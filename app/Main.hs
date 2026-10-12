module Main where

import Data.Char (isDigit, isSpace)
import Control.Applicative

newtype Parser a = Parser {
    run_parser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, rest) <- p input
        return (f x, rest)

instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
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
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> (p1 input) <|> (p2 input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
    case input of
        []     -> Nothing
        (c:cs) -> if predicate c
                    then Just (c, cs)
                    else Nothing

char :: Char -> Parser Char
char c = satisfy (c==)

digit :: Parser Char
digit = satisfy isDigit

string :: String -> Parser String
string s = mapM char s

natural :: Parser Int
natural = read <$> (some digit)

real :: Parser Float
real = do
    whole_part <- some digit
    _ <- char '.'
    fraction_part <- some digit
    return $ read (whole_part ++ "." ++ fraction_part)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy element separator = do
    first <- element
    elements <- many (whitespace >> separator >> whitespace >> element)
    whitespace
    return (first : elements)

array :: Parser [Float]
array = do
    _ <- char '['
    whitespace
    elements <- real `sepBy` (char ',')
    whitespace
    _ <- char ']'
    whitespace
    return elements

whitespace :: Parser ()
whitespace = (many (satisfy isSpace)) >> return ()

data BinaryOp = Plus | Minus | Multiply | Divide

data UnaryOp = Negate

binaryOpToChar :: BinaryOp -> Char
binaryOpToChar op = case op of
    Plus     -> '+'
    Minus    -> '-'
    Multiply -> '*'
    Divide   -> '/'

binaryOpToFunc :: BinaryOp -> (Float -> Float -> Float)
binaryOpToFunc op = case op of
    Plus     -> (+)
    Minus    -> (-)
    Multiply -> (*)
    Divide   -> (/)

instance Show BinaryOp where
    show op = [binaryOpToChar op]

data Expr =
    NumberExpr Float |
    BinaryExpr Expr BinaryOp Expr |
    UnaryExpr UnaryOp Expr

instance Show Expr where
    show (NumberExpr n) = show n
    show (BinaryExpr left op right) = (show left) ++ " " ++ (show op) ++ " " ++ (show right)
    show (UnaryExpr Negate e) = "-" ++ (show e)

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
negation = whitespace >> ((char '-' >> negation >>= \n -> return (UnaryExpr Negate n)) <|> factor)

factor :: Parser Expr
factor = number <|> ((char '(') *> (whitespace) *> expression <* (whitespace) <* (char ')'))

number :: Parser Expr
number = NumberExpr <$> (real <|> (fromIntegral <$> natural))

eval :: Expr -> Float
eval (NumberExpr n) = n
eval (BinaryExpr e1 op e2) = 
    let v1 = eval e1  in
    let v2 = eval e2 in
    let func = binaryOpToFunc op in
    v1 `func` v2
eval (UnaryExpr Negate e) = -(eval e)

main :: IO ()
main = do
    input <- getLine
    let ast = run_parser expression input
    let val = ast >>= \(expr, _) -> return (eval expr)
    print val
    main
