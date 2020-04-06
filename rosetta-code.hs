import Control.Applicative ((<|>))
import Control.Monad.State.Lazy
import Data.Char    -- isAsciiLower, isAsciiUpper, isDigit, ord
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import System.Environment (getArgs)
import System.IO


-- Scanning ------------------------------------------------------------------------------------------------------------
-- Remaining text, line, column
-- ScannerState expects a null terminated string, for efficient character handling
type ScannerState = (Text, Int, Int)

scannerAdvance :: Int -> ScannerState -> ScannerState
scannerAdvance 1 (t, l, c)
    | ch == '\n' = (rest, l + 1, 0)
    | otherwise  = (rest, l, c + 1)

    where (ch, rest) = (T.head t, T.tail t)

scannerAdvance n ctx = scannerAdvance (n - 1) $ scannerAdvance 1 ctx


scannerPeek :: ScannerState -> Char
scannerPeek (t, _, _) = T.head t


scannerNext :: ScannerState -> (Char, ScannerState)
scannerNext ctx = (ch, new_ctx)
    where new_ctx @ (t, _, _) = scannerAdvance 1 ctx
          ch = T.head t


scannerLocation :: ScannerState -> (Int, Int)
scannerLocation (_, l, c) = (l, c)


scannerStrip :: ScannerState -> ScannerState
scannerStrip (t, l, c)
    | ch == ' '  = scannerStrip (rest, l, c + 1)
    | ch == '\n' = scannerStrip (rest, l + 1, 0)
    | otherwise  = (t, l, c)

    where (ch, rest) = (T.head t, T.tail t)


scannerLit :: String -> ScannerState -> (Bool, ScannerState)
scannerLit lexeme (t, l, c) =
    case T.stripPrefix (T.pack lexeme) t of
        Nothing -> (False, (t, l, c))
        Just t' -> (True, (t', l, c + (length lexeme)))


scannerStartsWith :: (Char -> Bool) -> ScannerState -> (Bool, ScannerState)
scannerStartsWith f (t, l, c) = (f (T.head t), (t, l, c))


scannerMany :: (Char -> Bool) -> ScannerState -> (Text, ScannerState)
scannerMany f (t, l, c) = (str, (t', l, c + T.length str))
    where (str, t') = T.span f t


-- Stateful Scanning ---------------------------------------------------------------------------------------------------
type Scanner = State ScannerState

advance :: Int -> Scanner ()
advance n = modify $ scannerAdvance n

peek :: Scanner Char
peek = gets scannerPeek

next :: Scanner Char
next = state scannerNext

location :: Scanner (Int, Int)
location = gets scannerLocation

skipWhitespace :: Scanner ()
skipWhitespace = modify scannerStrip

lit :: String -> Scanner Bool
lit lexeme = state $ scannerLit lexeme

startsWith :: (Char -> Bool) -> Scanner Bool
startsWith f = state $ scannerStartsWith f

many :: (Char -> Bool) -> Scanner Text
many f = state $ scannerMany f


-- Token ---------------------------------------------------------------------------------------------------------------
data TokenValue = IntValue Int | TextValue Text | None
    deriving (Show)


data Token = Token { tokenName   :: String,
                     tokenValue  :: TokenValue,
                     tokenLine   :: Int,
                     tokenColumn :: Int }


showToken :: Token -> String
showToken t = printf "%2d   %2d   %-17s%s\n" (tokenLine t) (tokenColumn t) (tokenName t) (show $ tokenValue t)


showTokens :: [Token] -> String
showTokens tokens =
    "Location     Token Name         Value\n" ++
    "-------------------------------------\n" ++
    concatMap showToken tokens


--          token context
lexError :: ScannerState -> String -> Scanner Token
lexError (t, l, c) msg = do
    (l', c') <- location    -- error context

    let code = T.unpack $ T.take (c' - c + 1) t
    let error_str = printf "(%2d,%2d): %s" l' c' code

    next

    let str = TextValue $ T.pack $ msg ++ "\n" ++ (replicate 13 ' ') ++ error_str
    return $ Token "Error" str l c


simpleToken :: String -> String -> Scanner (Maybe Token)
simpleToken str name = do
    (text, line, column) <- get

    if (T.isPrefixOf (T.pack str) text)
        then do advance $ length str
                return $ Just (Token name None line column)
        else return Nothing



-- Bespoke monadic choice
ifM :: Scanner Bool -> Scanner Token -> ScannerState -> (Maybe Token, ScannerState)
ifM ma mb ctx =
    if cond then (Just token, ctxB)
    else         (Nothing, ctxA)

    where (cond, ctxA)  = runState ma ctx
          (token, ctxB) = runState mb ctxA


(==>) :: Scanner Bool -> Scanner Token -> Scanner (Maybe Token)
ma ==> mb = state $ ifM ma mb


-- Tokenizing ----------------------------------------------------------------------------------------------------------
isIdStart :: Char -> Bool
isIdStart ch = isAsciiLower ch || isAsciiUpper ch || ch == '_'


isIdEnd :: Char -> Bool
isIdEnd ch = isIdStart ch || isDigit ch


makeIdentifier :: Scanner Token
makeIdentifier = do
    (line, column) <- location
    ch <- peek
    next

    back <- many isIdEnd
    let id = TextValue $ T.cons ch back

    return $ Token "Identifier" id line column


makeInteger :: Scanner Token
makeInteger = do
    ctx @ (_, line, column) <- get

    int_str <- many isDigit
    let num = IntValue (read (T.unpack int_str) :: Int)

    ch <- peek

    if (isIdStart ch) then
        lexError ctx "Invalid number. Starts like a number, but ends in non-numeric characters."
    else
        return $ Token "Integer" num line column


makeCharacter :: Scanner Token
makeCharacter = do
    ctx @ (text, _, _) <- get
    let str = T.unpack $ T.take 3 text

    parse_char ctx str
        where parse_char :: ScannerState -> String -> Scanner Token
              parse_char (_, l, c) (ch : '\'' : _) = do
                  advance 2
                  return $ Token "Integer" (IntValue $ ord ch) l c

              parse_char (_, l, c) "\\n'" = do
                  advance 3
                  return $ Token "Integer" (IntValue 10) l c

              parse_char (_, l, c) "\\\\'" = do
                  advance 3
                  return $ Token "Integer" (IntValue 92) l c

              parse_char ctx ('\\' : ch : "\'") = do
                  advance 2
                  lexError ctx $ printf "Unknown escape sequence \\%c" ch

              parse_char ctx ('\'' : _) = lexError ctx "gettok: empty character constant"

              parse_char ctx _ = do
                  advance 2
                  lexError ctx "Multi-character constant"


makeString :: Scanner Token
makeString = do
    ctx <- get
    ch <- peek
    build_str ctx (T.pack "") ch
        where
            build_str :: ScannerState -> Text -> Char -> Scanner Token
            build_str ctx s '\n' = lexError ctx $ "End-of-line while scanning string literal." ++
                                                  " Closing string character not found before end-of-line."

            build_str ctx s '\0' = lexError ctx $ "End-of-file while scanning string literal." ++
                                                  " Closing string character not found."

            build_str (_, line, column) s '"' = do
                next
                return $ Token "String" (TextValue s) line column

            build_str ctx s ch = do
                next_ch <- next
                build_str ctx (T.snoc s ch) next_ch


skipComment :: Scanner Token
skipComment = do
    ctx <- get
    ch <- peek

    loop ctx ch
        where loop :: ScannerState -> Char -> Scanner Token
              loop ctx '\0' = lexError ctx $ "End-of-file in comment. Closing comment characters not found."

              loop ctx '*' = do
                  next_ch <- next

                  if (next_ch == '/')
                      then nextToken
                      else loop ctx next_ch

              loop ctx ch = do
                  next_ch <- next
                  loop ctx next_ch


nextToken :: Scanner Token
nextToken = do
    skipWhitespace

    maybe_token <- id
        -- Keywords
         $  simpleToken "if"    "Keyword_if"
        <|> simpleToken "else"  "Keyword_else"
        <|> simpleToken "while" "Keyword_while"
        <|> simpleToken "print" "Keyword_while"
        <|> simpleToken "putc"  "Keyword_putc"

        -- End of Input
        <|> simpleToken "\0" "EOF"

        -- Patterns
        <|> startsWith isIdStart ==> makeIdentifier
        <|> startsWith isDigit   ==> makeInteger
        <|> lit "'"              ==> makeCharacter
        <|> lit "\""             ==> makeString
        <|> lit "/*"             ==> skipComment

        -- Operators
        <|> simpleToken "*"  "Op_multiply"
        <|> simpleToken "/"  "Op_divide"
        <|> simpleToken "%"  "Op_mod"
        <|> simpleToken "+"  "Op_add"
        <|> simpleToken "-"  "Op_subtract"
        <|> simpleToken "<=" "Op_lessequal"
        <|> simpleToken "<"  "Op_less"
        <|> simpleToken ">=" "Op_greaterequal"
        <|> simpleToken ">"  "Op_greater"
        <|> simpleToken "==" "Op_equal"
        <|> simpleToken "!=" "Op_notequal"
        <|> simpleToken "!"  "Op_not"
        <|> simpleToken "="  "Op_assign"
        <|> simpleToken "&&" "Op_and"
        <|> simpleToken "||" "Op_or"

        -- Symbols
        <|> simpleToken "(" "LeftParen"
        <|> simpleToken ")" "RightParen"
        <|> simpleToken "{" "LeftBrace"
        <|> simpleToken "}" "RightBrace"
        <|> simpleToken ";" "SemiColon"
        <|> simpleToken "," "Comma"

    case maybe_token of
        Nothing    -> (get >>= \ctx -> lexError ctx "Unrecognized character.")
        Just token -> return token



    -- -- Keywords
    -- if      (lit "if"   ) then do advance 2; return $ Token "Keyword_if"    None line column
    -- else if (lit "else" ) then do advance 4; return $ Token "Keyword_else"  None line column
    -- else if (lit "while") then do advance 5; return $ Token "Keyword_while" None line column
    -- else if (lit "print") then do advance 5; return $ Token "Keyword_print" None line column
    -- else if (lit "putc" ) then do advance 4; return $ Token "Keyword_putc"  None line column

    -- -- End of Input
    -- else if (lit "\0") then return Token "EOF" None line column

    -- -- Patterns
    -- else if (isIdStart ch) then makeIdentifier
    -- else if (isDigit ch)   then makeInteger
    -- else if (lit "\"")     then makeString
    -- else if (lit "/*")     then skipComment

    -- -- Operators
    -- else if (lit "*" ) then return $ Token "Op_multiply"     None line column
    -- else if (lit "/" ) then return $ Token "Op_divide"       None line column
    -- else if (lit "%" ) then return $ Token "Op_mod"          None line column
    -- else if (lit "+" ) then return $ Token "Op_add"          None line column
    -- else if (lit "-" ) then return $ Token "Op_subtract"     None line column
    -- else if (lit "<=") then return $ Token "Op_lessequal"    None line column
    -- else if (lit "<" ) then return $ Token "Op_less"         None line column
    -- else if (lit ">=") then return $ Token "Op_greaterequal" None line column
    -- else if (lit ">" ) then return $ Token "Op_greater"      None line column
    -- else if (lit "==") then return $ Token "Op_equal"        None line column
    -- else if (lit "!=") then return $ Token "Op_notequal"     None line column
    -- else if (lit "!" ) then return $ Token "Op_not"          None line column
    -- else if (lit "=" ) then return $ Token "Op_assign"       None line column
    -- else if (lit "&&") then return $ Token "Op_and"          None line column
    -- else if (lit "||") then return $ Token "Op_or"           None line column

    -- -- Symbols
    -- else if (lit "(") then return $ Token "LeftParen"  None line column
    -- else if (lit ")") then return $ Token "RightParen" None line column
    -- else if (lit "{") then return $ Token "LeftBrace"  None line column
    -- else if (lit "}") then return $ Token "RightBrace" None line column
    -- else if (lit ";") then return $ Token "SemiColon"  None line column
    -- else if (lit ",") then return $ Token "Comma"      None line column

    -- -- Bad Input
    -- else lexError ctx "Unrecognized character."


getTokens :: ScannerState -> [Token]
getTokens s
    | tokenName t == "EOF" = [t]
    | otherwise            = t : getTokens s'

    where (t, s') = runState nextToken s


tokenize :: String -> [Token]
tokenize s = getTokens (T.pack s, 0, 0)



-- Run -----------------------------------------------------------------------------------------------------------------
getIOHandles :: [String] -> IO (Handle, Handle)
getIOHandles [] = return (stdin, stdout)

getIOHandles [infile] = do
    inhandle <- openFile infile ReadMode
    return (inhandle, stdout)

getIOHandles [infile, outfile] = do
    inhandle  <- openFile infile ReadMode
    outhandle <- openFile outfile WriteMode
    return (inhandle, outhandle)


parseAsTokens :: Handle -> Handle -> IO ()
parseAsTokens in_handle out_handle = do
    contents <- hGetContents in_handle
    let contents = contents ++ "\0"

    hPutStr out_handle $ showTokens $ tokenize contents





main = do
    args <- getArgs
    (in_handle, out_handle) <- getIOHandles args

    parseAsTokens in_handle out_handle

    hClose in_handle
    hClose out_handle
