import Prelude hiding (lex)
import Control.Applicative hiding (many, some)
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf
import System.Environment (getArgs)
import System.IO hiding (isEOF)


-- Tokens --------------------------------------------------------------------------------------------------------------
data Val = IntVal    Int            -- value
         | TextVal   String Text    -- name value
         | SymbolVal String         -- name
         | NoVal                    -- (skip tokens)
         | LexError  String         -- message


instance Show Val where
    show (IntVal         value) = printf "%-17s%d\n" "Integer" value
    show (TextVal   name value) = printf "%-17s%s\n" name (T.unpack value)
    show (SymbolVal name      ) = printf "%s\n"      name
    show (LexError  msg)        = printf "%-17s%s\n" "Error" msg


printTokens :: [Token] -> String
printTokens tokens =
    "Location  Token Name       Value\n"      ++
    "-------------------------------------\n" ++
    (concatMap show tokens)


-- Tokenizer -----------------------------------------------------------------------------------------------------------
simpleToken :: String -> String -> Lexer (Maybe Val)
simpleToken lexeme name = do
    lit lexeme
    return $ Just $ SymbolVal name


makeTokenizers :: [(String, String)] -> Lexer (Maybe Val)
makeTokenizers = asum . map (uncurry simpleToken)


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

identifier :: Lexer (Maybe Val)
identifier = do
    first <- some isIdStart
    rest <- many isIdEnd
    let lexeme = T.append first rest

    return $ Just $ TextVal "Identifier" lexeme


integer :: Lexer (Maybe Val)
integer = do
    lexeme <- some isDigit
    next_ch <- peek

    if (isIdStart next_ch) then
        return $ Just $ LexError "Invalid number. Starts like a number, but ends in non-numeric characters."
    else do
        let num = read (T.unpack lexeme) :: Int
        return $ Just $ IntVal num


character :: Lexer (Maybe Val)
character = do
    lit "'"
    str <- lookahead 3

    case str of
        (ch : '\'' : _)    -> do advance 2; return $ Just $ IntVal (ord ch)
        "\\n'"             -> do advance 3; return $ Just $ IntVal 10
        "\\\\'"            -> do advance 3; return $ Just $ IntVal 92
        ('\\' : ch : "\'") -> do advance 2; return $ Just $ LexError $ printf "Unknown escape sequence \\%c" ch
        ('\'' : _)         -> return $ Just $ LexError "Empty character constant"
        _                  -> do advance 2; return $ Just $ LexError "Multi-character constant"


string :: Lexer (Maybe Val)
string = do
    lit "\""

    loop (T.pack "") =<< next
        where loop t ch = case ch of
                  '\\' -> do
                      next_ch <- next

                      case next_ch of
                          'n'  -> loop (T.snoc t '\n') =<< next
                          '\\' -> loop (T.snoc t '\\') =<< next
                          _    -> return $ Just $ LexError $ printf "Unknown escape sequence \\%c" next_ch

                  '"' -> do next; return $ Just $ TextVal "String" t

                  '\n' -> return $ Just $ LexError $ "End-of-line while scanning string literal." ++
                                                     " Closing string character not found before end-of-line."

                  '\0' -> return $ Just $ LexError $ "End-of-file while scanning string literal." ++
                                                     " Closing string character not found."

                  _    -> loop (T.snoc t ch) =<< next


skipComment :: Lexer (Maybe Val)
skipComment = do
    lit "/*"

    loop =<< peek
        where loop ch = case ch of
                  '\0' -> return $ Just $ LexError "End-of-file in comment. Closing comment characters not found."

                  '*'  -> do
                      next_ch <- next

                      if (next_ch == '/') then do next; return $ Just NoVal
                      else                     loop next_ch

                  _    -> loop =<< next


nextToken :: Lexer Token
nextToken = lexerSrc $ do
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

    return $ fromMaybe
        (LexError "Unrecognized character.")
        maybe_token


isEOF :: Token -> Bool
isEOF (Token (SymbolVal "EOF") _ _) = True
isEOF _ = False


lex :: String -> [Token]
lex s = runUntil isEOF nextToken (T.pack s, 1, 1)


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
--                 input line column
type LexerState = (Text, Int, Int)
type Lexer = MaybeT (State LexerState)
data Token = Token Val Int Int    -- token line column

instance Show Token where
    show (Token val line column) = printf "%2d   %2d   %s" line column (show val)


lexerAdvance :: Int -> LexerState -> LexerState
lexerAdvance 0 ctx = ctx

lexerAdvance 1 (t, l, c)
    | ch == '\n' = (rest, l + 1, 1    )
    | otherwise  = (rest, l,     c + 1)
    where
        (ch, rest) = (T.head t, T.tail t)


lexerAdvance n ctx = lexerAdvance (n - 1) $ lexerAdvance 1 ctx


advance :: Int -> Lexer ()
advance n = modify $ lexerAdvance n


peek :: Lexer Char
peek = gets $ \(t, _, _) -> T.head t


lookahead :: Int -> Lexer String
lookahead n = gets $ \(t, _, _) -> T.unpack $ T.take n t


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
startsWith f = return f <*> peek


one :: (Char -> Bool) -> Lexer Char
one f = do
    ch <- peek
    guard $ f ch
    advance 1
    return ch


lexerMany :: (Char -> Bool) -> LexerState -> (Text, LexerState)
lexerMany f (t, l, c) = (lexeme, (t', l', c'))
    where (lexeme, _) = T.span f t
          (t', l', c') = lexerAdvance (T.length lexeme) (t, l, c)


many :: (Char -> Bool) -> Lexer Text
many f = state $ lexerMany f


some :: (Char -> Bool) -> Lexer Text
some f = do
    first <- one f
    rest <- many f
    return $ T.cons first rest


lexerSrc :: Lexer Val -> Lexer Token
lexerSrc lexer = do
    (t, l, c) <- get
    val <- lexer

    case val of
        NoVal -> nextToken

        LexError msg -> do
            (l', c') <- location

            let code = T.unpack $ T.take (c' - c + 1) t
            let error_str = printf "(%d, %d): %s" l' c' code

            unless (T.head t == '\0') $ advance 1

            let str = msg ++ "\n" ++ (replicate 27 ' ') ++ error_str
            return $ Token (LexError str) l c

        otherwise -> return $ Token val l c


runUntil :: (Token -> Bool) -> Lexer Token -> LexerState -> [Token]
runUntil f lexer s
    | f t       = [t]
    | otherwise = t : runUntil f lexer s'

    where (Just t, s') = runState (runMaybeT lexer) s
