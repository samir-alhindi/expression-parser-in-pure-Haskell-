module Environment where

import Data.Map (Map)
import qualified Data.Map as Map

type SymbolTable a = Map String a

data Environment a = Global (SymbolTable a) | Environment (SymbolTable a) (Environment a)

find :: Environment a -> String -> Maybe a
find (Environment table outer) name = case Map.lookup name table of
    Just a -> Just a
    Nothing -> find outer name
find (Global table) name = Map.lookup name table

extend ::  [(String, a)] -> Environment a -> Environment a
extend keyValuePairs (Global table) = Environment (Map.fromList keyValuePairs) (Global table)
extend keyValuePairs (Environment table outer) = Environment (Map.fromList keyValuePairs) (Environment table outer)

globalEnvironment :: Environment a
globalEnvironment = Global Map.empty