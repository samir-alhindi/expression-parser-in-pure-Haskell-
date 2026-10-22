module Parser where

import Data.Char (isDigit, isSpace, isAlpha)
import Control.Applicative

import Expr

newtype Parser a = Parser {
    run_parser :: String -> Either (String, String) (a, String)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, rest) <- p input
        return (f x, rest)

instance Applicative Parser where
    pure a = Parser $ \input -> Right (a, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (f, rest)  <- p1 input
        (x, rest') <- p2 rest
        return (f x, rest')

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> do
        (x, rest) <- p input
        run_parser (f x) rest

instance Alternative Parser where
    empty = Parser $ \input -> Left ("empty", input)
    (Parser p1) <|> (Parser p2) = Parser $ \input -> case (p1 input) of
        Right a -> Right a
        Left (err, rest)      -> if (length rest) < (length input)
            then Left (err, rest)
            else (p2 input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
    case input of
        []     -> Left ("No more input left", "")
        (c:cs) -> if predicate c
                    then Right (c, cs)
                    else Left (c : " Was not expected", input)

char :: Char -> Parser Char
char charToMatch = Parser $ \input ->
    case input of
        []     -> Left ("Expected " ++ [charToMatch] ++ " but input ran out", "")
        (c:cs) -> if charToMatch == c
                    then Right (c, cs)
                    else Left ("Expected " ++ [charToMatch] ++ " but got " ++ [c], input)

whitespace :: Parser ()
whitespace = (many (satisfy isSpace)) >> return ()

consume :: Char -> Parser ()
consume c = (char c) >> whitespace

try :: Parser a -> Parser a
try (Parser p) = Parser $ \input ->
    case p input of
        Right a       -> Right a
        Left (err, _) -> Left(err, input)

(<?>) :: Parser a -> String -> Parser a
(Parser p) <?> hint = Parser $ \input ->
    case p input of
        Right a          -> Right a
        Left (err, rest) -> if (length rest) < (length input)
            then Left (err, rest)
            else Left ("Expected " ++ hint, input)

digit :: Parser Char
digit = satisfy isDigit

word :: String -> Parser String
word s = mapM char s

keyword :: String -> Parser ()
keyword s = (word s >> whitespace) <?> ("keyword " ++ s)

natural :: Parser Int
natural = read <$> (some digit)

real :: Parser Float
real = do
    whole_part <- some digit
    consume '.'
    fraction_part <- some digit
    whitespace
    return (read (whole_part ++ "." ++ fraction_part))

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 element separator = do
    first <- element
    elements <- many (whitespace >> separator >> whitespace >> element)
    whitespace
    return (first : elements)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy element separator =  (element `sepBy1` separator) <|> (return [])

list :: Parser Expr
list = do
    consume '['
    elements <- expression `sepBy` (char ',')
    whitespace
    consume ']'
    return (ListExpr elements)

range :: Parser Expr
range = do
    consume '['
    start <- expression
    consume '.'
    consume '.'
    end <- expression
    consume ']'
    return (RangeExpr start end)

listComprehension :: Parser Expr
listComprehension = do
    consume '['
    output <- expression
    consume '|'
    element <- name
    consume '<'
    consume '-'
    source <- expression
    consume ']'
    return (ListComprehensionExpr output element source)

char' :: Parser Expr
char' = do
    consume '\''
    c <- satisfy (\_ -> True)
    consume '\''
    return (CharExpr c)

string :: Parser Expr
string = do
    consume '"'
    contents <- many (satisfy (/='"'))
    consume '"'
    return (StringExpr contents)

bool :: Parser Expr
bool = do
    b <- ((keyword "true" >> return True) <|> (keyword "false" >> return False))
    return (BoolExpr b)

binary :: [BinaryOp] -> Parser Expr -> Parser Expr
binary operators rule = do
    left   <- rule
    whitespace
    functions <- many operatorThenRight
    return (foldl (flip ($)) left functions)
        where
            operatorThenRight :: Parser (Expr -> Expr)
            operatorThenRight = do
                op    <- operators_parser
                whitespace
                right <- rule
                whitespace
                let flipped = flipArgs BinaryExpr
                return (flipped right op)

            operators_parser :: Parser BinaryOp
            operators_parser = foldl (\parser op -> parser <|> (op <$ (char (binaryOpToChar op)))) empty operators

            flipArgs :: (a -> b -> c -> d) -> (c -> b -> a -> d)
            flipArgs f = \ a b c -> f c b a

expression :: Parser Expr
expression = binary [Plus, Minus] term

term :: Parser Expr
term = binary [Multiply, Divide] negation

negation :: Parser Expr
negation = (consume '-' >> negation >>= \n -> return (UnaryExpr Negate n)) <|> factor

factor :: Parser Expr
factor = ((consume '(') *> expression <* (consume ')')) <|> atom

number :: Parser Expr
number = do
    n <- (try real) <|> (fromIntegral <$> natural)
    whitespace
    return (NumberExpr n)

ternary :: Parser Expr
ternary = do
    keyword "if"
    condition <- expression
    keyword "then"
    thenBranch <- expression
    keyword "else"
    elseBranch <- expression
    return (TernaryExpr condition thenBranch elseBranch)

name :: Parser String
name = do
    firstChar <- satisfy isAlphaOrUnderscore
    rest <- many (satisfy isAlphaOrUnderscoreOrDigit)
    whitespace
    return (firstChar : rest)
        where
            isAlphaOrUnderscore :: Char -> Bool
            isAlphaOrUnderscore c = (isAlpha c) || (c == '_')

            isAlphaOrUnderscoreOrDigit :: Char -> Bool
            isAlphaOrUnderscoreOrDigit c = (isAlphaOrUnderscore c) || (isDigit c)

name' :: Parser Expr
name' = NameExpr <$> (name)

atom :: Parser Expr
atom = number
    <|> char'
    <|> bool
    <|> (try range)
    <|> (try listComprehension)
    <|> list
    <|> string
    <|> ternary
    <|> name'
    <?> "atomic expression"

program :: Parser Expr
program = expression <* eof
    where
        eof :: Parser ()
        eof = Parser $ \input -> if (length input) == 0
            then Right ((), "")
            else Left ("Expected EOF", input)