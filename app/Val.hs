module Val where

import Expr
import Environment (Environment)
import Data.List (isSuffixOf)

data Val =
    NumberVal Float
    | CharVal Char
    | BoolVal Bool
    | ListVal [Val]
    | StringVal String
    | LambdaVal Expr [String] (Environment Val)

instance Show Val where
    show (NumberVal n) =
        let s = show n in
            if ".0" `isSuffixOf` s
                then init (init s)
                else s
    show (CharVal c) = show c
    show (BoolVal b) = show b
    show (ListVal l) = show l
    show (StringVal s) = s
    show (LambdaVal _ _ _) = "lambda"

instance Eq Val where
    (NumberVal n1) == (NumberVal n2) = n1 == n2
    (CharVal c1)   == (CharVal c2)   = c1 == c2
    (BoolVal b1)   == (BoolVal b2)   = b1 == b2
    (ListVal l1)   == (ListVal l2)   = l1 == l2
    _ == _                         = False 