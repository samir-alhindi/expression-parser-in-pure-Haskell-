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
    | SubscriptExpr Expr Expr

data BinaryOp = Plus
    | Minus | Multiply | Divide | Mod
    | LessThan | GreaterThan | IsEqual | NotEqual | LessThanOrEqual | GreaterThanOrEqual
    | And | Or

data UnaryOp = Negate | Not

instance Show BinaryOp where
    show op = case op of
        Plus     -> "+"
        Minus    -> "-"
        Multiply -> "*"
        Divide   -> "/"
        Mod      -> "%"

        LessThan -> "<"
        GreaterThan -> ">"
        IsEqual -> "=="
        NotEqual -> "!="
        LessThanOrEqual -> "<="
        GreaterThanOrEqual -> ">="

        And -> "and"
        Or -> "or"

instance Show UnaryOp where
    show op = case op of
        Negate -> "-"
        Not -> "not"

instance Show Expr where
    show (NumberExpr n) = show n
    show (CharExpr c) = show c
    show (ListExpr l) = show l
    show (StringExpr s) = ['"'] ++ s ++ ['"']
    show (BoolExpr b) = show b
    show (NameExpr name) = name
    show (BinaryExpr left op right) = (show left) ++ " " ++ (show op) ++ " " ++ (show right)
    show (UnaryExpr op e) = (show op) ++ (show e)
    show (TernaryExpr condition thenBranch elseBranch) = intercalate ", " (map show  [condition, thenBranch, elseBranch])
    show (RangeExpr start end) = "[" ++ (show start) ++ ".." ++ (show end) ++ "]"
    show (CallExpr callee args) = (show callee) ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
    show (LambdaExpr _ parameters) = "lambda" ++ "(" ++ (intercalate ", " parameters) ++ ")"
    show (ListComprehensionExpr _ _ _) = "ListComprehensionExpr"