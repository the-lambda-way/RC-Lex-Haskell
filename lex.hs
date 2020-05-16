import Prelude hiding (lex)
import Control.Applicative hiding (many, some)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import System.Environment (getArgs)
import System.IO


-- Tokens --------------------------------------------------------------------------------------------------------------
data Token = IntToken    Int            -- value
           | TextToken   String Text    -- name value
           | SymbolToken String         -- name


instance Show Token where
    show (IntToken    name value) = printf "%-17s%d\n" "Integer" value
    show (TextToken   name value) = printf "%-17s%s\n" name (T.unpack value)
    show (SymbolToken name      ) = printf "%s\n"      name

instance Show LexError where
    show (LexError msg) = printf "%-17s%s\n" "Error" msg


printTokens :: [Token] -> String
printTokens tokens =
    "Location  Token Name       Value\n"      ++
    "-------------------------------------\n" ++
    (concatMap show tokens)


--                    token context
lexError :: String -> LexerState -> Lexer Token
lexError msg (t, l, c) = do
    (ch, l', c') <- current    -- error context

    let code = T.unpack $ T.take (c' - c + 1) t
    let error_str = printf "(%d, %d): %s" l' c' code

    unless (ch == '\0') $ advance 1

    let str = msg ++ "\n" ++ (replicate 27 ' ') ++ error_str
    throwError $ LexError str l c


-- Tokenizer -----------------------------------------------------------------------------------------------------------
simpleToken :: String -> String -> Lexer Token
simpleToken lexeme name = lexerSrc $ do
    lit lexeme
    return $ SymbolToken name


makeTokenizers :: Alternative f => [(String, String)] -> f a
makeTokenizers = afromList $ map (curry simpleToken)


keywords = makeTokenizers
    [("if",    "Keyword_if"),    ("else", "Keyword_else"), ("while", "Keyword_while"),
     ("print", "Keyword_print"), ("putc", "Keyword_putc")]

operators = makeTokenizers
    [("*", "Op_multiply"), ("/",  "Op_divide"),    ("%",  "Op_mod"),      ("+", "Op_add"),
     ("-", "Op_subtract"), ("<=", "Op_lessequal"), ("<",  "Op_less"),     (">=", "Op_greaterequal"),
     (">", "Op_greater"),  ("==", "Op_equal"),     ("!=", "Op_notequal"), ("!", "Op_not"),
     ("=", "Op_assign"),   ("&&", "Op_and"),       ("||", "Op_or")]

symbols = makeTokenizers
    [("(", "LeftParen"), (")", "RightParen"),
     ("{", "LeftBrace"), ("}", "RightBrace"),
     (";", "SemiColon"), (",", "Comma")]


isIdStart ch = isAsciiLower ch || isAsciiUpper ch || ch == '_'
isIdEnd ch = isIdStart ch || isDigit ch

identifier :: Lexer Token
identifier = lexerSrc $ do
    first <- some isIdStart
    rest <- many isIdEnd
    let lexeme = T.append first rest

    return $ TextToken "Identifier" lexeme

integer :: Lexer Token
integer = lexerSrc $ do
    ctx <- get
    lexeme <- some isDigit
    next_ch <- peek

    if (isIdStart next_ch) then
        lexError ctx "Invalid number. Starts like a number, but ends in non-numeric characters."
    else do
        let num = read (T.unpack lexeme) :: Int
        return $ IntToken num


makeCharacter :: Lexer Token
makeCharacter = lexerSrc $ do
    ctx <- get
    char '\''

    str <- lookahead 3

    case str of
        (ch : '\'' : _)    -> do advance 2; return $ IntToken (ord ch)
        "\\n'"             -> do advance 3; return $ IntToken 10
        "\\\\'"            -> do advance 3; return $ IntToken 92
        ('\\' : ch : "\'") -> do advance 2; lexError $ printf "Unknown escape sequence \\%c" ch
        ('\'' : _)         -> lexError "Empty character constant"
        _                  -> do advance 2; lexError "Multi-character constant"


makeString :: Lexer Token
makeString = lexerSrc $ do
    build_str (T.pack "") =<< next
        where build_str t ch = case ch of
                  '\\' -> do
                      next_ch <- next

                      case next_ch of
                          'n'  -> build_str (T.snoc t '\n') =<< next
                          '\\' -> build_str (T.snoc t '\\') =<< next
                          _    -> lexError $ printf "Unknown escape sequence \\%c" next_ch

                  '"' -> do
                      let (_, line, column) = ctx
                      next
                      return $ Token "String" (TextValue t)

                  '\n' -> lexError $ "End-of-line while scanning string literal." ++
                                     " Closing string character not found before end-of-line."

                  '\0' -> lexError $ "End-of-file while scanning string literal." ++
                                     " Closing string character not found."

                  _    -> build_str (T.snoc t ch) =<< next


skipComment :: Lexer Token
skipComment = lexerSrc $ do
    advance 2

    loop =<< peek
        where loop ch = case ch of
                  '\0' -> lexError $ "End-of-file in comment. Closing comment characters not found."

                  '*'  -> do
                      next_ch <- next

                      if (next_ch == '/') then next >> nextToken
                      else                     loop next_ch

                  _    -> loop =<< next


nextToken :: Lexer Token
nextToken = do
    skipWhitespace

    maybe_token <- keywords
               <|> identifier
               <|> integer
               <|> character
               <|> string
               <|> skipComment
               <|> operators
               <|> symbols
               <|> simpleToken "\0" "EOF"

    case maybe_token of
        Nothing    -> lexerSrc $ lexError "Unrecognized character."
        Just token -> return token


lex :: String -> [Token]
lex s = runUntil
    (\t -> tokenName t == "EOF")
    nextToken
    (T.pack s, 1, 1)


main = do
    args <- getArgs
    (hin, hout) <- getIOHandles args

    withHandles hin hout $ printTokens . lex


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


-- Parsing -------------------------------------------------------------------------------------------------------------

data SrcLoc     = SrcLoc Int Int       -- line column
data SrcToken a = SrcToken a SrcLoc    -- token location

data LexError = LexError String    -- message


instance Show SourceToken where
    show (SourceToken token (SrcLoc line column)) = printf "%2d   %2d   %s" line column (show token)


data LexerState = LexerState {
    LexerInput :: Text,
    LexerLoc   :: SrcLoc}

type Lexer = MaybeT State LexerState


lexerAdvance :: Int -> LexerState -> LexerState
lexerAdvance 0 ctx = ctx

lexerAdvance 1 (t, l, c)
    | ch == '\n' = (rest, l + 1, 1    )
    | otherwise  = (rest, l,     c + 1)
    where
        (ch, rest) = (T.head t, T.tail t)


lexerAdvance n ctx = lexerAdvance (n - 1) $ lexerAdvance 1 ctx


advance :: Int -> Lexer ()
advance n = modify $ LexerAdvance n


peek :: Lexer Char
peek = gets $ \(t, _, _) -> T.head t


next :: Lexer Char
next = do
    advance 1
    return =<< peek


location :: Lexer (Int, Int)
location = gets $ \(_, l, c) -> (l, c)


current :: Lexer (Char, Int, Int)
current = gets $ \(t, l, c) -> (T.head t, l, c)


skipWhitespace :: Lexer ()
skipWhitespace = do
    ch <- peek
    when (ch `elem` " \n") (next >> skipWhitespace)


lit :: String -> Lexer ()
lit lexeme = do
    (t, _, _) <- get
    guard $ T.isPrefixOf (T.pack lexeme) t
    advance $ length lexeme


startsWith :: (Char -> Bool) -> Lexer Bool
startsWith f = return . Just f <*> peek


one :: (Char -> Bool) -> Lexer Text
one f = do
    ch <- peek
    guard $ f ch
    advance 1
    return $ Just ch


LexerMany :: (Char -> Bool) -> LexerState -> (Text, LexerState)
LexerMany f (t, l, c) = (lexeme, (t', l', c'))
    where (lexeme, t') = T.span f t
          (_, l', c') = LexerAdvance (_, l, c) $ T.length lexeme


many :: (Char -> Bool) -> Lexer Text
many f = state $ LexerMany f


some :: (Char -> Bool) -> Lexer Text
some f = do
    first <- one f
    rest <- many f
    return $ T.append first rest


-- bespoke monadic conditional, helps the use of monadic style in the tokenizer
(?->) :: Lexer Bool -> Lexer Token -> MaybeT Lexer Token
ma ?-> mb = MaybeT $ do
    cond <- ma

    if (cond) then return Just <*> mb
    else           return Nothing


-- unlike takeWhile and until, this takes the last value once the predicate becomes true
runUntil :: (Token -> Bool) -> Lexer Token -> LexerState -> [Token]
runUntil f m s
    | f t       = [t]
    | otherwise = t : runUntil f m s'

    where (t, s') = runState m s