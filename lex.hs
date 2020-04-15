import Control.Applicative hiding (many)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
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


withHandles :: Handle -> Handle -> (String -> String) -> IO ()
withHandles in_handle out_handle f = do
    contents <- hGetContents in_handle
    let contents' = contents ++ "\0"    -- adding \0 simplifies treatment of EOF

    hPutStr out_handle $ f contents'

    hClose in_handle
    hClose out_handle


-- Scanning ------------------------------------------------------------------------------------------------------------

-- Remaining text, line, column
type ParserState = (Text, Int, Int)
type Parser = State ParserState


parserAdvance :: Int -> ParserState -> ParserState
parserAdvance 0 ctx = ctx

parserAdvance 1 (t, l, c)
    | ch == '\n' = (rest, l + 1, 0)
    | otherwise  = (rest, l,     c + 1)
    where
        (ch, rest) = (T.head t, T.tail t)

parserAdvance n ctx = parserAdvance (n - 1) $ parserAdvance 1 ctx


advance :: Int -> Parser ()
advance n = modify $ parserAdvance n


peek :: Parser Char
peek = gets $ \(t, _, _) -> T.head t


next :: Parser Char
next = do
    advance 1
    return =<< peek


location :: Parser (Int, Int)
location = gets $ \(_, l, c) -> (l, c)


current :: Parser (Char, Int, Int)
current = gets $ \(t, l, c) -> (T.head t, l, c)


skipWhitespace :: Parser ()
skipWhitespace = do
    ch <- peek
    when (ch `elem` " \n") (next >> skipWhitespace)


lit :: String -> Parser Bool
lit lexeme = gets $ \(t, _, _) -> T.isPrefixOf (T.pack lexeme) t


startsWith :: (Char -> Bool) -> Parser Bool
startsWith f = return f <*> peek


parserMany :: (Char -> Bool) -> ParserState -> (Text, ParserState)
parserMany f (t, l, c) = (str, (t', l, c'))
    where (str, t') = T.span f t
          c' = c + T.length str


many :: (Char -> Bool) -> Parser Text
many f = state $ parserMany f


(?->) :: Parser Bool -> Parser Token -> MaybeT Parser Token
ma ?-> mb = MaybeT $ do
    cond <- ma

    if (cond) then return Just <*> mb
    else           return Nothing


-- unlike takeWhile and until, this takes the last value once the predicate is true
runUntilM :: (Token -> Bool) -> Parser Token -> ParserState -> [Token]
runUntilM f m s
    | f t       = [t]
    | otherwise = t : runUntilM f m s'

    where (t, s') = runState m s


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


instance PrintfArg TokenValue where
    formatArg = formatString . show


showToken :: Token -> String
showToken t = printf "%2d   %2d   %-17s%s\n" (tokenLine t) (tokenColumn t) (tokenName t) (tokenValue t)


showTokens :: [Token] -> String
showTokens tokens =
    "Location  Token Name       Value\n" ++
    "-------------------------------------\n" ++
    (concatMap showToken tokens)


--          token context
lexError :: ParserState -> String -> Parser Token
lexError (t, l, c) msg = do
    (ch, l', c') <- current    -- error context

    let code = T.unpack $ T.take (c' - c + 1) t
    let error_str = printf "(%d, %d): %s" l' c' code

    unless (ch == '\0') $ advance 1

    let str = T.pack $ msg ++ "\n" ++ (replicate 27 ' ') ++ error_str
    return $ Token "Error" (TextValue str) l c


simpleToken :: String -> String -> MaybeT Parser Token
simpleToken str name = MaybeT $ do
    (text, line, column) <- get

    if (T.isPrefixOf (T.pack str) text) then do
        advance $ length str
        return $ Just (Token name None line column)
    else
        return Nothing


-- Tokenizer -----------------------------------------------------------------------------------------------------------
isIdStart :: Char -> Bool
isIdStart ch = isAsciiLower ch || isAsciiUpper ch || ch == '_'


isIdEnd :: Char -> Bool
isIdEnd ch = isIdStart ch || isDigit ch


makeIdentifier :: Parser Token
makeIdentifier = do
    (front, line, column) <- current
    next

    back <- many isIdEnd
    let id = T.cons front back

    return $ Token "Identifier" (TextValue id) line column


makeInteger :: Parser Token
makeInteger = do
    ctx @ (_, line, column) <- get

    int_str <- many isDigit
    next_ch <- peek

    if (isIdStart next_ch) then
        lexError ctx "Invalid number. Starts like a number, but ends in non-numeric characters."
    else do
        let num = read (T.unpack int_str) :: Int
        return $ Token "Integer" (IntValue num) line column


makeCharacter :: Parser Token
makeCharacter = do
    ctx @ (text, line, column) <- get
    let str = T.unpack $ T.drop 1 (T.take 4 text)

    case str of
        (ch : '\'' : _)    -> do advance 3; return $ Token "Integer" (IntValue $ ord ch) line column
        "\\n'"             -> do advance 4; return $ Token "Integer" (IntValue 10) line column
        "\\\\'"            -> do advance 4; return $ Token "Integer" (IntValue 92) line column
        ('\\' : ch : "\'") -> do advance 3; lexError ctx $ printf "Unknown escape sequence \\%c" ch
        ('\'' : _)         -> lexError ctx "Empty character constant"
        _                  -> do advance 3; lexError ctx "Multi-character constant"


makeString :: Parser Token
makeString = do
    ctx <- get
    next

    build_str ctx (T.pack "")
        where build_str ctx t = do
                  let (_, line, column) = ctx
                  ch <- peek

                  case ch of
                      '\n' -> lexError ctx $ "End-of-line while scanning string literal." ++
                                             " Closing string character not found before end-of-line."

                      '\0' -> lexError ctx $ "End-of-file while scanning string literal." ++
                                             " Closing string character not found."

                      '\\' -> do
                           next_ch <- next

                           case next_ch of
                               'n'  -> do next; build_str ctx (T.snoc t '\n')
                               '\\' -> do next; build_str ctx (T.snoc t '\\')
                               _    -> lexError ctx $ printf "Unknown escape sequence \\%c" next_ch

                      '"'  -> do next; return $ Token "String" (TextValue t) line column

                      _    -> do next; build_str ctx (T.snoc t ch)


skipComment :: Parser Token
skipComment = do
    ctx <- get
    advance 2

    loop ctx =<< peek
        where loop ctx '\0' = lexError ctx $ "End-of-file in comment. Closing comment characters not found."

              loop ctx '*' = do
                  next_ch <- next

                  if (next_ch == '/') then next >> nextToken
                  else                     loop ctx next_ch

              loop ctx _ = loop ctx =<< next


nextToken :: Parser Token
nextToken = do
    skipWhitespace

    maybe_token <- runMaybeT
        -- Keywords
         $  simpleToken "if"    "Keyword_if"
        <|> simpleToken "else"  "Keyword_else"
        <|> simpleToken "while" "Keyword_while"
        <|> simpleToken "print" "Keyword_print"
        <|> simpleToken "putc"  "Keyword_putc"

        -- Patterns
        <|> startsWith isIdStart ?-> makeIdentifier
        <|> startsWith isDigit   ?-> makeInteger
        <|> lit "'"              ?-> makeCharacter
        <|> lit "\""             ?-> makeString
        <|> lit "/*"             ?-> skipComment

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
        <|> simpleToken "\0" "EOF"

    case maybe_token of
        Nothing    -> get >>= \ctx -> lexError ctx "Unrecognized character."
        Just token -> return token


tokenize :: String -> [Token]
tokenize s = runUntilM
    (\t -> tokenName t == "EOF")
    nextToken
    (T.pack s, 0, 0)


main = do
    args <- getArgs
    (hin, hout) <- getIOHandles args

    withHandles hin hout $ showTokens . tokenize