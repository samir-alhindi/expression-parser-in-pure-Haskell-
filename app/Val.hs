module Val where

import Expr
import Environment (Environment)


data Val =
    NumberVal Float
    | CharVal Char
    | BoolVal Bool
    | ListVal [Val]
    | StringVal String
    | LambdaVal Expr [String] (Environment Val)

instance Show Val where
    show (NumberVal n) = show n
    show (CharVal c) = show c
    show (BoolVal b) = show b
    show (ListVal l) = show l
    show (StringVal s) = s
    show (LambdaVal _ _ _) = "lambda"