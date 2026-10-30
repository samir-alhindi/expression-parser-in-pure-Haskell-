module Main where

import Control.Monad.Reader

import Parser
import Evaluator
import Environment

main :: IO ()
main = do
    input <- getLine
    let ast = run_parser program input
    let val = ast >>= \(expr, _) -> return (runReaderT (eval expr) globalEnvironment)
    case val of
        Left (err, _)  -> print ("Parse Error: " ++ err)
        (Right v) -> case v of
            (Left err) -> print err
            (Right v') -> print v'
    main