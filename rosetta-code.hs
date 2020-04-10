import Control.Applicative hiding (many)
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Char    -- isAsciiLower, isAsciiUpper, isDigit, ord
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import System.Environment (getArgs)
import System.IO


------------------------------------------------------------------------------------------------------------------------
-- Machinery
------------------------------------------------------------------------------------------------------------------------

-- File handling -------------------------------------------------------------------------------------------------------
getIOHandles :: [String] -> IO (Handle, Handle)
getIOHandles [] = return (stdin, stdout)

getIOHandles [infile] = do
    inhandle <- openFile infile ReadMode
    return (inhandle, stdout)

getIOHandles [infile, outfile] = do
    inhandle  <- openFile infile ReadMode
    outhandle <- openFile outfile WriteMode
    return (inhandle, outhandle)


withHandles :: Handle -> Handle -> (String -> String) -> IO a
withHandles in_handle out_handle f = do
    contents <- hGetContents in_handle

    hPutStr out_handle $ f contents

    hClose in_handle
    hClose out_handle


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
scannerNext ctx = (ch, ctx')
    where ctx' @ (t, _, _) = scannerAdvance 1 ctx
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


scannerStopBefore :: [Char] -> ScannerState -> ((Text, Char), ScannerState)
scannerStopBefore chars ctx @ (t, _, _) = until wrong build_text ((T.empty, T.head t), ctx)
    where wrong ((_, ch), _) = ch `elem` chars

          build_text ((text, ch), ctx) = ((text', ch'), ctx')
              where text' = T.snoc text ch
                    (ch', ctx') = scannerNext ctx


-- Stateful Interface --------------------------------------------------------------------------------------------------
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

stopBefore :: [Char] -> Scanner (Text, Char)
stopBefore chars = state $ scannerStopBefore chars


-- Bespoke monadic choice
-- ifM :: Scanner Bool -> Scanner Token -> ScannerState -> (Maybe Token, ScannerState)
-- ifM ma mb ctx =
--     if cond then (Just token, ctxB)
--     else         (Nothing, ctx)

--     where (cond, ctxA)  = runState ma ctx
--           (token, ctxB) = runState mb ctxA


-- Think about using the above definition and transform with monad functions, like elsewhere

(==>) :: Scanner Bool -> Scanner Token -> MaybeT Scanner Token
ma ==> mb = MaybeT $ do
    ctx <- get
    let (cond, ctxA) = runState ma ctx

    if (cond) then do
        let (token, ctxB) = runState mb ctxA
        put ctxB
        return $ Just token

    else return Nothing


------------------------------------------------------------------------------------------------------------------------
-- Language
------------------------------------------------------------------------------------------------------------------------

-- Token ---------------------------------------------------------------------------------------------------------------
data TokenValue = IntValue Int | TextValue Text | None

data Token = Token { tokenName   :: String,
                     tokenValue  :: TokenValue,
                     tokenLine   :: Int,
                     tokenColumn :: Int }


instance Show TokenValue where
    show (IntValue  v) = show v
    show (TextValue v) = T.unpack v
    show None          = ""


showToken :: Token -> String
showToken t = printf "%2d   %2d   %-17s%s\n" (tokenLine t) (tokenColumn t) (tokenName t) (show $ tokenValue t)


showTokens :: [Token] -> String
showTokens tokens =
    "Location     Token Name         Value\n" ++
    "-------------------------------------\n" ++
    (concatMap showToken tokens)


--          token context
lexError :: ScannerState -> String -> Scanner Token
lexError (t, l, c) msg = do
    (l', c') <- location    -- error context

    let code = T.unpack $ T.take (c' - c + 1) t
    let error_str = printf "(%d, %d): %s" l' c' code

    next

    let str = T.pack $ msg ++ "\n" ++ (replicate 27 ' ') ++ error_str
    return $ Token "Error" (TextValue str) l c


simpleToken :: String -> String -> MaybeT Scanner Token
simpleToken str name = MaybeT $ do
    (text, line, column) <- get

    if (T.isPrefixOf (T.pack str) text)
        then do advance $ length str
                return $ Just (Token name None line column)
        else return Nothing


-- Tokenizer -----------------------------------------------------------------------------------------------------------
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
    let id = T.cons ch back

    return $ Token "Identifier" (TextValue id) line column


makeInteger :: Scanner Token
makeInteger = do
    ctx @ (_, line, column) <- get

    int_str <- many isDigit
    next_ch <- peek

    if (isIdStart next_ch) then
        lexError ctx "Invalid number. Starts like a number, but ends in non-numeric characters."
    else do
        let num = read (T.unpack int_str) :: Int
        return $ Token "Integer" (IntValue num) line column


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

              parse_char ctx ('\'' : _) = lexError ctx "Empty character constant"

              parse_char ctx _ = do
                  advance 2
                  lexError ctx "Multi-character constant"


makeString :: Scanner Token
makeString = do
    ctx <- get

    -- This would be cleaner modelled after makeCharacter

    build_str ctx (T.pack "")
        where build_str :: ScannerState -> Text -> Scanner Token
              build_str ctx t = do
                  (lexeme, last) <- stopBefore ['\\', '"', '\n', '\0']

                  case last of
                      '\n' -> lexError ctx $ "End-of-line while scanning string literal." ++
                                             " Closing string character not found before end-of-line."
                      '\0' -> lexError ctx $ "End-of-file while scanning string literal. Closing string character not found."
                      '"'  -> do next; return $ Token "String" (TextValue lexeme) line column
                      '\\' -> do
                          ch <- next

                          case ch of
                              'n'  -> do next; return $ build_str (T.snoc lexeme '\n')
                              '\\' -> do next; return $ build_str (T.append lexeme "\\\\")
                              _    -> lexError ctx $ printf "Unknown escape sequence \\%c" ch


skipComment :: Scanner Token
skipComment = do
    ctx <- get
    ch <- peek

    loop ctx ch
        where loop :: ScannerState -> Char -> Scanner Token
              loop ctx '\0' = lexError ctx $ "End-of-file in comment. Closing comment characters not found."

              loop ctx '*' = do
                  next_ch <- next

                  if (next_ch == '/') then nextToken
                  else                     loop ctx next_ch

              loop ctx _ = next >>= loop ctx


nextToken :: Scanner Token
nextToken = do
    skipWhitespace

    maybe_token <- runMaybeT
        -- Keywords
         $  simpleToken "if"    "Keyword_if"
        <|> simpleToken "else"  "Keyword_else"
        <|> simpleToken "while" "Keyword_while"
        <|> simpleToken "print" "Keyword_while"
        <|> simpleToken "putc"  "Keyword_putc"

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

        -- End of Input
        <|> simpleToken "" "EOF"

    case maybe_token of
        Nothing    -> get >>= \ctx -> lexError ctx "Unrecognized character."
        Just token -> return token


getTokens :: ScannerState -> [Token]
getTokens s
    | T.null text = []
    | otherwise   = t : getTokens s'

    where (t, s') = runState nextToken s
          (text, _, _) = s'


tokenize :: String -> [Token]
tokenize s = getTokens (T.pack s, 0, 0)


main = do
    args <- getArgs
    (hin, hout) <- getIOHandles args

    withHandles hin hout $ showTokens . tokenize