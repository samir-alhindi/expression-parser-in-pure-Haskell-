module Evaluator where

import Data.Fixed (mod')
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader

import Expr
import Val
import Environment

type Eval a = ReaderT (Environment Val) (Either String) a

eval :: Expr -> Eval Val
eval (NumberExpr n) = return (NumberVal n)
eval (CharExpr c) = return (CharVal c)
eval (BoolExpr b) = return (BoolVal b)
eval (NameExpr name) = do
    envi <- ask
    case find envi name of
        Just val -> return val
        Nothing  -> throwError ("Name '" ++ name ++ "' is not defined.")
eval (ListExpr es) = ListVal <$> (mapM eval es)
eval (StringExpr s) = return (StringVal s)
eval (UnaryExpr op e) = do
    v <- eval e
    case op of
        Negate -> case v of
            NumberVal n -> return (NumberVal (-n))
            _ -> throwError ("'-' operand must be a number")
        Not -> case v of
            BoolVal b -> return (BoolVal (not b))
            _ -> throwError ("'not' operand must be a boolean")
eval (BinaryExpr e1 op e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case op of
            IsEqual  -> return (BoolVal (v1 == v2))
            NotEqual -> return (BoolVal (v1 /= v2))
            o | o `elem` [And, Or] -> do
                (b1, b2) <- checkBoolOpperands v1 v2
                case o of
                    And -> return (BoolVal (b1 && b2))
                    Or  -> return (BoolVal (b1 || b2))
                    _        -> throwError ("Unhandled case")
                |  otherwise -> do
                    (n1, n2) <- checkNumberOpperands v1 v2
                    if o `elem` [Divide, Mod]
                        then if n2 == 0
                            then throwError "Cannot divide by 0"
                            else case o of
                                Divide -> return (NumberVal (n1 / n2))
                                Mod    -> return (NumberVal (n1 `mod'` n2))
                                _      -> throwError ("Unhandled case")
                        else case o of
                            Plus        -> return (NumberVal (n1 + n2))
                            Minus       -> return (NumberVal (n1 - n2))
                            Multiply    -> return (NumberVal (n1 * n2))
                            LessThan    -> return (BoolVal (n1 < n2))
                            GreaterThan -> return (BoolVal (n1 > n2))
                            GreaterThanOrEqual -> return (BoolVal (n1 >= n2))
                            LessThanOrEqual    -> return (BoolVal (n1 <= n2))
                            _        -> throwError ("Unhandled case")

eval (TernaryExpr condition thenBranch elseBranch) = do
    conditionVal <- eval condition
    case conditionVal of
        (BoolVal b) -> if b then eval thenBranch else eval elseBranch
        _ -> throwError "ternary condition must be a boolean"
eval (LetExpr varName varExpr bodyExpr) = do
    varValue <- eval varExpr
    local (extend [(varName, varValue)]) (eval bodyExpr)
eval (RangeExpr startExpr endExpr) = do
    startVal <- eval startExpr
    endVal   <- eval endExpr
    case (startVal, endVal) of
        (NumberVal start, NumberVal end) ->
            return (ListVal (map NumberVal (if start < end then [start..end] else [start, start-1..end])))
        _ -> throwError ("Range must all be numbers")
eval (LambdaExpr bodyExpr parameters) = do
    envi <- ask
    return (LambdaVal bodyExpr parameters envi)
eval (CallExpr calleeExpr argsExpr) = do
    calleeVal <- eval calleeExpr
    case calleeVal of
        (LambdaVal bodyExpr parameters closure) -> do
            argsVal <- mapM eval argsExpr
            let lambdaEnvi = extend (zip parameters argsVal) closure
            local (\_ -> lambdaEnvi) (eval bodyExpr)
        _ -> throwError ("can only call lambdas")
eval (ListComprehensionExpr outputExpr elementName sourceExpr gaurdExpr) = do
    sourceVal <- eval sourceExpr
    case sourceVal of
        (ListVal values) -> do
            vs <- evalListComprehension outputExpr elementName gaurdExpr values
            return (ListVal vs)
        _ -> throwError ("List comprehension source must evaluate to a list")
eval (SubscriptExpr sourceExpr indexExpr) = do
    sourceVal <- eval sourceExpr
    case sourceVal of
        (ListVal l) -> do
            indexVal <- eval indexExpr
            case indexVal of
                (NumberVal n) -> case properFraction n :: (Int, Float) of
                    (index, 0.0) -> if index < length l
                        then return (l !! index)
                        else throwError (show index ++ " is out of bounds")
                    _ -> throwError ("index must be whole number")
                _ -> throwError ("index must be a number")
        _ -> throwError ("can only subscript a list")

evalListComprehension :: Expr -> String -> Expr -> [Val] -> Eval [Val]
evalListComprehension _ _ _ [] = return []
evalListComprehension outputExpr elementName gaurdExpr (v:vs) = do
    b <- local (extend [(elementName, v)]) (eval gaurdExpr)
    case b of
        BoolVal (True) -> do
            v'  <- local (extend [(elementName, v)]) (eval outputExpr)
            vs' <- evalListComprehension outputExpr elementName gaurdExpr vs
            return (v':vs')
        _ -> evalListComprehension outputExpr elementName gaurdExpr vs

checkBoolOpperands :: Val -> Val -> Eval (Bool, Bool)
checkBoolOpperands (BoolVal b1) (BoolVal b2) = return (b1, b2)
checkBoolOpperands _ _ = throwError ("Opperands must both be booleans")

checkNumberOpperands :: Val -> Val -> Eval (Float, Float)
checkNumberOpperands (NumberVal n1) (NumberVal n2) = return (n1, n2)
checkNumberOpperands _ _ = throwError ("Opperands must both be numbers")