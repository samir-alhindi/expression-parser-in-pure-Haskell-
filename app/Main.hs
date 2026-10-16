module Main where

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Control.Applicative

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


data BinaryOp = Plus | Minus | Multiply | Divide

data UnaryOp = Negate

binaryOpToChar :: BinaryOp -> Char
binaryOpToChar op = case op of
    Plus     -> '+'
    Minus    -> '-'
    Multiply -> '*'
    Divide   -> '/'


instance Show BinaryOp where
    show op = [binaryOpToChar op]

data Expr =
    NumberExpr Float
    | CharExpr Char
    | BoolExpr Bool
    | ListExpr [Expr]
    | TernaryExpr Expr Expr Expr
    | BinaryExpr Expr BinaryOp Expr
    | UnaryExpr UnaryOp Expr

instance Show Expr where
    show (NumberExpr n) = show n
    show (CharExpr c) = show c
    show (ListExpr l) = show l
    show (BoolExpr b) = show b
    show (BinaryExpr left op right) = (show left) ++ " " ++ (show op) ++ " " ++ (show right)
    show (UnaryExpr Negate e) = "-" ++ (show e)
    show (TernaryExpr condition thenBranch elseBranch) = intercalate ", " (map show  [condition, thenBranch, elseBranch])

data Val =
    NumberVal Float
    | CharVal Char
    | BoolVal Bool
    | ListVal [Val]

instance Show Val where
    show (NumberVal n) = show n
    show (CharVal c) = show c
    show (BoolVal b) = show b
    show (ListVal l) = show l

try :: Parser a -> Parser a
try (Parser p) = Parser $ \input ->
    case p input of
        Right a       -> Right a
        Left (err, _) -> Left(err, input)

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

(<?>) :: Parser a -> String -> Parser a
(Parser p) <?> message = Parser $ \input ->
    case p input of
        Right a          -> Right a
        Left (err, rest) -> if (length rest) < (length input)
            then Left (err, rest)
            else Left (message, input)

digit :: Parser Char
digit = satisfy isDigit

word :: String -> Parser String
word s = mapM char s

keyword :: String -> Parser ()
keyword s = (word s >> whitespace) <?> ("Expected keyword " ++ s)

natural :: Parser Int
natural = read <$> (some digit)

real :: Parser Float
real = do
    whole_part <- some digit
    _ <- char '.'
    fraction_part <- some digit
    return $ read (whole_part ++ "." ++ fraction_part)

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
    _ <- char '['
    whitespace
    elements <- expression `sepBy` (char ',')
    whitespace
    _ <- char ']'
    whitespace
    return (ListExpr elements)

char' :: Parser Expr
char' = singleQuote *> (CharExpr <$> satisfy (\_ -> True)) <* singleQuote
    where
        singleQuote :: Parser ()
        singleQuote = char '\'' >> return ()

bool :: Parser Expr
bool = BoolExpr <$> ((keyword "true" >> return True) <|> (keyword "false" >> return False))

whitespace :: Parser ()
whitespace = (many (satisfy isSpace)) >> return ()

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

ternary :: Parser Expr
ternary = do
    keyword "if"
    condition <- expression
    keyword "then"
    thenBranch <- expression
    keyword "else"
    elseBranch <- expression
    return (TernaryExpr condition thenBranch elseBranch)

expression :: Parser Expr
expression =
    arithmeticExpression
    <|> char'
    <|> bool
    <|> list
    <|> ternary
    <?> "Expceted Expression"

program :: Parser Expr
program = expression <* eof
    where
        eof :: Parser ()
        eof = Parser $ \input -> if (length input) == 0
            then Right ((), "")
            else Left ("Expected EOF", input)

arithmeticExpression :: Parser Expr
arithmeticExpression = binary [Plus, Minus] term

term :: Parser Expr
term = binary [Multiply, Divide] negation

negation :: Parser Expr
negation = whitespace >> ((char '-' >> negation >>= \n -> return (UnaryExpr Negate n)) <|> factor)

factor :: Parser Expr
factor = number <|> ((char '(') *> (whitespace) *> expression <* (whitespace) <* (char ')'))

number :: Parser Expr
number = NumberExpr <$> ((try real) <|> (fromIntegral <$> natural))

eval :: Expr -> Either String Val
eval (NumberExpr n) = return (NumberVal n)
eval (CharExpr c) = return (CharVal c)
eval (BoolExpr b) = return (BoolVal b)
eval (ListExpr l) = mapM eval l >>= \l' -> return (ListVal l')
eval (UnaryExpr Negate e) = do
    v <- eval e
    case v of
        (NumberVal n) -> return (NumberVal (-n))
        _ -> Left "Can only Negate number"
eval (BinaryExpr e1 op e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (NumberVal n1, NumberVal n2) -> case op of
            Plus     -> return (NumberVal (n1 + n2))
            Minus    -> return (NumberVal (n1 - n2))
            Multiply -> return (NumberVal (n1 * n2))
            Divide   -> if n2 == 0 then Left "Cannot divide by 0" else return (NumberVal (n1 / n2))
        _ -> Left "Both opperands must be numbers"
eval (TernaryExpr condition thenBranch elseBranch) = do
    conditionVal <- eval condition
    case conditionVal of
        (BoolVal b) -> if b then eval thenBranch else eval elseBranch
        _ -> Left "ternary condition must be a boolean"

main :: IO ()
main = do
    input <- getLine
    let ast = run_parser program input
    let val = ast >>= \(expr, _) -> return (eval expr)
    case val of
        Left (err, _)  -> print ("Parse Error: " ++ err)
        (Right v) -> case v of
            (Left err) -> print err
            (Right v') -> print v'
    main

thing :: IO ()
thing = print "hi"