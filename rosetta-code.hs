import Control.Monad.State.Lazy
import Data.Char                        -- isAsciiLower, isAsciiUpper, isDigit
import qualified Data.Text.Lazy as T
import Text.Printf
import System.IO

type Text = T.Text


-- Scanning ------------------------------------------------------------------------------------------------------------
-- Remaining text, line, column
type ScannerState = (Text, Int, Int)


advanceScanner :: ScannerState -> ScannerState
advanceScanner (t, l, c)
    | ch == '\n' = (rest, l + 1, 0)
    | otherwise  = (rest, l, c + 1)

    where (ch, rest) = (T.head t, T.tail t)


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


grabWhileScanner :: (Char -> Bool) -> ScannerState -> (Text, ScannerState)
grabWhileScanner f (t, l, c) = (str, (t', l, c + T.length str))
    where (str, t') = T.span f t


-- Stateful Scanning ---------------------------------------------------------------------------------------------------
type Scanner = State ScannerState


advance :: Scanner Char
advance = modify advanceScanner


peek :: Scanner Char
peek = gets peekScanner


skipWhitespace :: Scanner ()
skipWhitespace = modify stripScanner


match :: String -> Scanner Bool
match lexeme = state $ matchScanner lexeme


grabWhile :: (Char -> Bool) -> Scanner Text
grabWhile f = state $ grabWhileScanner f


-- Token ---------------------------------------------------------------------------------------------------------------
data TokenValue = Integer | Text | None deriving (Show)

data Token = Token { tokenName   :: String,
                     tokenValue  :: TokenValue,
                     tokenLine   :: Int,
                     tokenColumn :: Int }


printToken :: Token -> IO ()
printToken t = printf "%2d   %2d   %-17s%s\n" (tokenLine t) (tokenColumn t) (tokenName t) (tokenValue t)


printTokens :: [Token] -> IO ()
printTokens tokens = do
    print "Location     Token Name         Value\n" ++
          "-------------------------------------\n"

    map printToken tokens


-- Lexical Errors ------------------------------------------------------------------------------------------------------
--          token context
lexError :: ScannerState -> Scanner Token
lexError (t, l, c) msg = do
    (_, l', c') <- get    -- error context

    let code = T.take (fromIntegral $ c' - c + 1) t
    let error_str = printf "(%2d,%2d): %s" l' c' (T.unpack code)

    advance
    return Token "Error" (msg ++ "\n" ++ (replicate 13 ' ')  ++ error_str) l c


-- Tokenizing ----------------------------------------------------------------------------------------------------------
isIdStart :: Char -> Bool
isIdStart ch = isAsciiLower ch || isAsciiUpper ch || ch == '_'


isIdEnd :: Char -> Bool
isIdEnd ch = isIdStart ch || isDigit ch


makeIdentifier :: Scanner Token
makeIdentifier = do
    (text, line, column) <- get
    advance

    back <- grabWhile isIdEnd
    let id = T.cons (T.head text) back

    return Token "Identifier" id line column


makeInteger :: Scanner Token
makeInteger = do
    ctx @ (_, line, column) <- get

    int_str <- grabWhile isDigit
    let num = read (T.unpack int_str) :: Integer

    ch <- peek

    if (isIdStart $ ch) then
        return lexError ctx "Invalid number. Starts like a number, but ends in non-numeric characters."
    else
        return Token "Integer" num line column


makeString :: Scanner Token
makeString = do
    ctx @ (_, line, column) <- get

    let build_str s = do
        ch <- peek

        if (match "\"" ) then return s

        else if (ch == '\n') then
            return lexError ctx "End-of-line while scanning string literal." ++
                                " Closing string character not found before end-of-line."

        else if (ch == "") then
            return lexError ctx "End-of-file while scanning string literal." ++
                                " Closing string character not found."

        else
            advance
            return $ build_str (T.snoc s ch)

    let str = build_str $ T.pack ""

    return Token "String" str line column


getToken :: Scanner Token
getToken = do
    skipWhitespace

    ctx @ (text, line, column) <- get
    ch <- peek

    -- Keywords
    if      (match "if"   ) then return Token "Keyword_if"    None line column
    else if (match "else" ) then return Token "Keyword_else"  None line column
    else if (match "while") then return Token "Keyword_while" None line column
    else if (match "print") then return Token "Keyword_print" None line column
    else if (match "putc" ) then return Token "Keyword_putc"  None line column

    -- End of Input
    else if (match "") then return Token "EOF" None line column

    -- Patterns
    else if (isIdStart $ ch) then return makeIdentifier
    else if (isDigit $ ch)   then return makeInteger
    else if (match "\"")     then return makeString
    else if (match "/*")     then return skipComment

    -- Operators
    else if (match "*" ) then return Token "Op_multiply"     None line column
    else if (match "/" ) then return Token "Op_divide"       None line column
    else if (match "%" ) then return Token "Op_mod"          None line column
    else if (match "+" ) then return Token "Op_add"          None line column
    else if (match "-" ) then return Token "Op_subtract"     None line column
    else if (match "<=") then return Token "Op_lessequal"    None line column
    else if (match "<" ) then return Token "Op_less"         None line column
    else if (match ">=") then return Token "Op_greaterequal" None line column
    else if (match ">" ) then return Token "Op_greater"      None line column
    else if (match "==") then return Token "Op_equal"        None line column
    else if (match "!=") then return Token "Op_notequal"     None line column
    else if (match "!" ) then return Token "Op_not"          None line column
    else if (match "=" ) then return Token "Op_assign"       None line column
    else if (match "&&") then return Token "Op_and"          None line column
    else if (match "||") then return Token "Op_or"           None line column

    -- Symbols
    else if (match "(") then return Token "LeftParen"  None line column
    else if (match ")") then return Token "RightParen" None line column
    else if (match "{") then return Token "LeftBrace"  None line column
    else if (match "}") then return Token "RightBrace" None line column
    else if (match ";") then return Token "SemiColon"  None line column
    else if (match ",") then return Token "Comma"      None line column

    -- Bad Input
    else return lexError ctx "Unrecognized character."


getTokens :: Scanner [Token]
getTokens = do
    t <- getToken

    if (tokenName t == "EOF") then return t
    else                           return (t : getTokens)


tokenize :: String -> [Token]
tokenize s = runState getTokens (T.pack s, 0, 0)



-- Run -----------------------------------------------------------------------------------------------------------------
getIOHandles :: [String] -> IO (Handle, Handle)
getIOHandles [] = (stdin, stdout)

getIOHandles [infile] = do
    inhandle <- openfile infile ReadMode
    return (inhandle, stdout)

getIOHandles [infile, outfile] = do
    inhandle  <- openfile infile ReadMode
    outhandle <- openfile outfile WriteMode
    return (inhandle, outhandle)


main = do
    args <- getArgs
    let (inhandle, outhandle) = getIOHandles args
    contents <- hGetContents inhandle

    map printToken $ tokenize contents

    hclose inhandle
    hclose outhandle
