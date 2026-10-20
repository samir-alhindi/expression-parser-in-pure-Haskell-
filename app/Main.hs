module Main where

import Parser
import Evaluator
import Environment

main :: IO ()
main = do
    input <- getLine
    let ast = run_parser program input
    let val = ast >>= \(expr, _) -> return (eval globalEnvironment expr)
    case val of
        Left (err, _)  -> print ("Parse Error: " ++ err)
        (Right v) -> case v of
            (Left err) -> print err
            (Right v') -> print v'
    main