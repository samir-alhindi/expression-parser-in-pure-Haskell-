module Evaluator where

import Data.Fixed (mod')

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
    case op of
            IsEqual  -> return (BoolVal (v1 == v2))
            NotEqual -> return (BoolVal (v1 /= v2))
            o | o `elem` [And, Or] -> do
                (b1, b2) <- checkBoolOpperands v1 v2
                case o of
                    And -> return (BoolVal (b1 && b2))
                    Or  -> return (BoolVal (b1 || b2))
                    _        -> Left ("Unhandled case")
                |  otherwise -> do
                    (n1, n2) <- checkNumberOpperands v1 v2
                    if o `elem` [Divide, Mod]
                        then if n2 == 0
                            then Left "Cannot divide by 0"
                            else case o of
                                Divide -> return (NumberVal (n1 / n2))
                                Mod    -> return (NumberVal (n1 `mod'` n2))
                                _      -> Left ("Unhandled case")
                        else case o of
                            Plus        -> return (NumberVal (n1 + n2))
                            Minus       -> return (NumberVal (n1 - n2))
                            Multiply    -> return (NumberVal (n1 * n2))
                            LessThan    -> return (BoolVal (n1 < n2))
                            GreaterThan -> return (BoolVal (n1 > n2))
                            GreaterThanOrEqual -> return (BoolVal (n1 >= n2))
                            LessThanOrEqual    -> return (BoolVal (n1 <= n2))
                            _        -> Left ("Unhandled case")

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
eval envi (ListComprehensionExpr outputExpr elementName sourceExpr gaurdExpr) = do
    sourceVal <- eval envi sourceExpr
    case sourceVal of
        (ListVal values) -> do
            let environments = map (\x -> Environment.extend envi [(elementName, x)]) values
            mappedValues <- mapM ((flip eval) outputExpr) environments
            gaurdValues  <- mapM ((flip eval) gaurdExpr)  environments
            bitmask      <- mapM (\v -> case v of
                BoolVal b -> Right b
                _         -> Left "gaurd must eval to boolean") gaurdValues
            let filteredValues = [v | (True, v) <- zip bitmask mappedValues]
            return (ListVal filteredValues)
        _ ->
            Left ("List comprehension source must evaluate to a list")
eval envi (SubscriptExpr sourceExpr indexExpr) = do
    sourceVal <- eval envi sourceExpr
    case sourceVal of
        (ListVal l) -> do
            indexVal <- eval envi indexExpr
            case indexVal of
                (NumberVal n) -> case properFraction n :: (Int, Float) of
                    (index, 0.0) -> if index < length l
                        then return (l !! index)
                        else Left (show index ++ " is out of bounds")
                    _ -> Left ("index must be whole number")
                _ -> Left ("index must be a number")
        _ -> Left ("can only subscript a list")

checkBoolOpperands :: Val -> Val -> Either String (Bool, Bool)
checkBoolOpperands (BoolVal b1) (BoolVal b2) = return (b1, b2)
checkBoolOpperands _ _ = Left ("Opperands must both be booleans")

checkNumberOpperands :: Val -> Val -> Either String (Float, Float)
checkNumberOpperands (NumberVal n1) (NumberVal n2) = return (n1, n2)
checkNumberOpperands _ _ = Left ("Opperands must both be numbers")