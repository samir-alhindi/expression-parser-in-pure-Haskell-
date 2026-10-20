module Expr where

import Data.List (intercalate)

data Expr =
    NumberExpr Float
    | CharExpr Char
    | BoolExpr Bool
    | NameExpr String
    | ListExpr [Expr]
    | ListComprehensionExpr Expr String Expr
    | RangeExpr Expr Expr
    | StringExpr String
    | TernaryExpr Expr Expr Expr
    | BinaryExpr Expr BinaryOp Expr
    | UnaryExpr UnaryOp Expr
    | LambdaExpr Expr [String]
    | CallExpr Expr [Expr]

data BinaryOp = Plus | Minus | Multiply | Divide

data UnaryOp = Negate

instance Show BinaryOp where
    show op = [binaryOpToChar op]

instance Show Expr where
    show (NumberExpr n) = show n
    show (CharExpr c) = show c
    show (ListExpr l) = show l
    show (StringExpr s) = ['"'] ++ s ++ ['"']
    show (BoolExpr b) = show b
    show (NameExpr name) = name
    show (BinaryExpr left op right) = (show left) ++ " " ++ (show op) ++ " " ++ (show right)
    show (UnaryExpr Negate e) = "-" ++ (show e)
    show (TernaryExpr condition thenBranch elseBranch) = intercalate ", " (map show  [condition, thenBranch, elseBranch])
    show (RangeExpr start end) = "[" ++ (show start) ++ ".." ++ (show end) ++ "]"
    show (CallExpr callee args) = (show callee) ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
    show (LambdaExpr _ parameters) = "lambda" ++ "(" ++ (intercalate ", " parameters) ++ ")"

binaryOpToChar :: BinaryOp -> Char
binaryOpToChar op = case op of
    Plus     -> '+'
    Minus    -> '-'
    Multiply -> '*'
    Divide   -> '/'

