module Evaluator where

import Expr
import Val
import Environment(Environment)
import qualified Environment

eval :: (Environment Val) -> Expr -> Either String Val
eval _ (NumberExpr n) = return (NumberVal n)
eval _ (CharExpr c) = return (CharVal c)
eval _ (BoolExpr b) = return (BoolVal b)
eval envi (NameExpr name) = case Environment.get envi name of
    Just val -> return val
    Nothing  -> Left ("Name '" ++ name ++ "' is not defined.")
eval envi (ListExpr l) = mapM (eval envi) l >>= \l' -> return (ListVal l')
eval _ (StringExpr s) = return (StringVal s)
eval envi (UnaryExpr op e) = do
    v <- eval envi e
    case op of
        Negate -> case v of
            NumberVal n -> return (NumberVal (-n))
            _ -> Left ("'-' operand must be a number")
        Not -> case v of
            BoolVal b -> return (BoolVal (not b))
            _ -> Left ("'not' operand must be a boolean")
eval envi (BinaryExpr e1 op e2) = do
    v1 <- eval envi e1
    v2 <- eval envi e2
    case (v1, v2) of
        (NumberVal n1, NumberVal n2) -> case op of
            Plus        -> return (NumberVal (n1 + n2))
            Minus       -> return (NumberVal (n1 - n2))
            Multiply    -> return (NumberVal (n1 * n2))
            Divide      -> if n2 == 0 then Left "Cannot divide by 0" else return (NumberVal (n1 / n2))
            LessThan    -> return (BoolVal (n1 < n2))
            GreaterThan -> return (BoolVal (n1 > n2))
            GreaterThanOrEqual -> return (BoolVal (n1 >= n2))
            LessThanOrEqual -> return (BoolVal (n1 <= n2))
            IsEqual -> return (BoolVal (n1 == n2))
            NotEqual -> return (BoolVal (n1 /= n2))
        (BoolVal b1, BoolVal b2) -> case op of
            And -> return (BoolVal (b1 && b2))
            Or -> return (BoolVal (b1 || b2))
        _ -> Left ("Invalid opperands")
eval envi (TernaryExpr condition thenBranch elseBranch) = do
    conditionVal <- eval envi condition
    case conditionVal of
        (BoolVal b) -> if b then eval envi thenBranch else eval envi elseBranch
        _ -> Left "ternary condition must be a boolean"
eval envi (RangeExpr startExpr endExpr) = do
    startVal <- eval envi startExpr
    endVal   <- eval envi endExpr
    case (startVal, endVal) of
        (NumberVal start, NumberVal end) ->
            return (ListVal (map NumberVal (if start < end then [start..end] else [start, start-1..end])))
        _ -> Left ("Range must all be numbers")
eval envi (LambdaExpr bodyExpr parameters) = return (LambdaVal bodyExpr parameters envi)
eval envi (CallExpr calleeExpr argsExpr) = do
    calleeVal <- eval envi calleeExpr
    case calleeVal of
        (LambdaVal bodyExpr parameters closure) -> do
            argsVal <- mapM (eval envi) argsExpr
            let newEnvi = Environment.extend closure (zip parameters argsVal)
            eval newEnvi bodyExpr
        _ -> Left ("can only call lambdas")
eval envi (ListComprehensionExpr outputExpr elementName sourceExpr) = do
    sourceVal <- eval envi sourceExpr
    case sourceVal of
        (ListVal l) -> do
            let environments = map (\x -> Environment.extend envi [(elementName, x)]) l
            outputList <- mapM ((flip eval) outputExpr) environments
            return (ListVal outputList)
        _ ->
            Left ("List comprehension source must evaluate to a list")