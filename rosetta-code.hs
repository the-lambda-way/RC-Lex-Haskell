import Control.Monad.State.Lazy
import Data.Char                        -- isAsciiLower, isAsciiUpper, isDigit
import Data.Text.Lazy (Text)
import qualified Data.Text as T
import Text.Printf
import System.Environment               -- getArgs
import System.IO



-- Scanning ------------------------------------------------------------------------------------------------------------
-- Remaining text, line, column
type ScannerState = (Text, Int, Int)

advanceScanner :: Int -> ScannerState -> ScannerState
advanceScanner 1 (t, l, c)
    | ch == '\n' = (rest, l + 1, 0)
    | otherwise  = (rest, l, c + 1)

    where (ch, rest) = (T.head t, T.tail t)

advanceScanner n (t, l, c) = advanceScanner (n - 1) $ advanceScanner 1


peekScanner :: ScannerState -> Char
peekScanner (t, _, _) = T.head t


stripScanner :: ScannerState -> ScannerState
stripScanner (t, l, c)
    | ch == ' '  = stripScanner (rest, l, c + 1)
    | ch == '\n' = stripScanner (rest, l + 1, 0)
    | otherwise  = (t, l, c)

    where (ch, rest) = (T.head t, T.tail t)


matchScanner :: String -> ScannerState -> (Bool, ScannerState)
matchScanner lexeme (t, l, c) =
    case T.stripPrefix (T.pack lexeme) t of
        Nothing -> (False, (t, l, c))
        Just t' -> (True, (t', l, c + (length lexeme)))


manyScanner :: (Char -> Bool) -> ScannerState -> (Text, ScannerState)
manyScanner f (t, l, c) = (str, (t', l, c + T.length str))
    where (str, t') = T.span f t


-- Stateful Scanning ---------------------------------------------------------------------------------------------------
type Scanner = State ScannerState


advance :: Int -> Scanner ()
advance n = modify $ advanceScanner n


peek :: Scanner Char
peek = gets peekScanner


skipWhitespace :: Scanner ()
skipWhitespace = modify stripScanner


match :: String -> Scanner Bool
match lexeme = state $ matchScanner lexeme


many :: (Char -> Bool) -> Scanner Text
many f = state $ manyScanner f


-- Token ---------------------------------------------------------------------------------------------------------------
data TokenValue = Integer | Text | None deriving (Show)

data Token = Token { tokenName   :: String,
                     tokenValue  :: TokenValue,
                     tokenLine   :: Int,
                     tokenColumn :: Int }


showToken :: Token -> String
showToken t = printf "%2d   %2d   %-17s%s\n" (tokenLine t) (tokenColumn t) (tokenName t) (tokenValue t)


showTokens :: [Token] -> String
showTokens tokens =
    "Location     Token Name         Value\n" ++
    "-------------------------------------\n" ++
    concatMap showToken tokens


-- Lexical Errors ------------------------------------------------------------------------------------------------------
--          token context
lexError :: ScannerState -> String -> Scanner Token
lexError (t, l, c) msg = do
    (_, l', c') <- get    -- error context

    let code = T.take (c' - c + 1) t
    let error_str = printf "(%2d,%2d): %s" l' c' (T.unpack code)

    advance 1
    return Token "Error" (msg ++ "\n" ++ (replicate 13 ' ') ++ error_str) l c


-- Tokenizing ----------------------------------------------------------------------------------------------------------
isIdStart :: Char -> Bool
isIdStart ch = isAsciiLower ch || isAsciiUpper ch || ch == '_'


isIdEnd :: Char -> Bool
isIdEnd ch = isIdStart ch || isDigit ch


makeIdentifier :: Scanner Token
makeIdentifier = do
    (text, line, column) <- get
    advance 1

    back <- many isIdEnd
    let id = T.cons (T.head text) back

    return Token "Identifier" id line column


makeInteger :: Scanner Token
makeInteger = do
    ctx @ (_, line, column) <- get

    int_str <- many isDigit
    let num = read (T.unpack int_str) :: Integer

    ch <- peek

    if (isIdStart ch) then
        lexError ctx "Invalid number. Starts like a number, but ends in non-numeric characters."
    else
        return Token "Integer" num line column


makeString :: Scanner Token
makeString = do
    ctx @ (_, line, column) <- get

    let build_str s = do
        (text, _, _) <- get
        ch <- peek

        if (ch == '"') then do
            advance 1
            return $ Token "String" s line column

        else if (ch == '\n') then
            lexError ctx $ "End-of-line while scanning string literal." ++
                           " Closing string character not found before end-of-line."

        else if (text == T.pack "") then
            lexError ctx $ "End-of-file while scanning string literal. Closing string character not found."

        else do
            advance 1
            build_str (T.snoc s ch)

    return $ build_str (T.pack "")


skipComment :: Scanner Token
skipComment = do
    ctx @ (t, l, s) <- get
    let (c, rest) = T.breakOn (T.pack "*/" t)

    if (T.length rest == 0) then
        lexError ctx $ "End-of-file in comment. Closing comment characters not found."
    else
        advance $ T.length c
        getToken


-- Bespoke monadic choice
-- (==>) :: Scanner Bool -> Scanner Token -> Scanner (Maybe Token)
-- a @ (StateT ScannerState Identity cond) ==> StateT ScannerState Identity tok =
--     if (cond)
--         then a >>= \_ -> return Just tok
--         else a >>= \_ -> return Nothing


getToken :: Scanner Token
getToken = do
    skipWhitespace

    ctx @ (text, line, column) <- get
    ch <- peek

    -- Keywords
    if      (T.isPrefixOf "if"    text) then do advance 2; return $ Token "Keyword_if"    None line column
    else if (T.isPrefixOf "else"  text) then do advance 4; return $ Token "Keyword_else"  None line column
    else if (T.isPrefixOf "while" text) then do advance 5; return $ Token "Keyword_while" None line column
    else if (T.isPrefixOf "print" text) then do advance 5; return $ Token "Keyword_print" None line column
    else if (T.isPrefixOf "putc"  text) then do advance 4; return $ Token "Keyword_putc"  None line column

    -- End of Input
    else if (match "") then return Token "EOF" None line column

    -- Patterns
    else if (isIdStart $ ch) then makeIdentifier
    else if (isDigit $ ch)   then makeInteger
    else if (match "\"")     then makeString
    else if (match "/*")     then skipComment

    -- Operators
    else if (match "*" ) then return $ Token "Op_multiply"     None line column
    else if (match "/" ) then return $ Token "Op_divide"       None line column
    else if (match "%" ) then return $ Token "Op_mod"          None line column
    else if (match "+" ) then return $ Token "Op_add"          None line column
    else if (match "-" ) then return $ Token "Op_subtract"     None line column
    else if (match "<=") then return $ Token "Op_lessequal"    None line column
    else if (match "<" ) then return $ Token "Op_less"         None line column
    else if (match ">=") then return $ Token "Op_greaterequal" None line column
    else if (match ">" ) then return $ Token "Op_greater"      None line column
    else if (match "==") then return $ Token "Op_equal"        None line column
    else if (match "!=") then return $ Token "Op_notequal"     None line column
    else if (match "!" ) then return $ Token "Op_not"          None line column
    else if (match "=" ) then return $ Token "Op_assign"       None line column
    else if (match "&&") then return $ Token "Op_and"          None line column
    else if (match "||") then return $ Token "Op_or"           None line column

    -- Symbols
    else if (match "(") then return $ Token "LeftParen"  None line column
    else if (match ")") then return $ Token "RightParen" None line column
    else if (match "{") then return $ Token "LeftBrace"  None line column
    else if (match "}") then return $ Token "RightBrace" None line column
    else if (match ";") then return $ Token "SemiColon"  None line column
    else if (match ",") then return $ Token "Comma"      None line column

    -- Bad Input
    else lexError ctx "Unrecognized character."


getTokens :: ScannerState -> [Token]
getTokens s
    | tokenName t == "EOF" = [t]
    | otherwise            = t : getTokens s'

    where (t, s') = runState getToken s


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


parseTokens :: Handle -> Handle -> IO ()
parseTokens in_handle out_handle = do
    contents <- hGetContents in_handle
    hPutStr out_handle $ showTokens (tokenize contents)


main = do
    args <- getArgs
    (in_handle, out_handle) <- getIOHandles args

    parseTokens in_handle out_handle

    hClose in_handle
    hClose out_handle
