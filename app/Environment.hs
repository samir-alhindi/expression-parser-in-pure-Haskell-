module Environment where

import Data.Map (Map)
import qualified Data.Map as Map

type SymbolTable a = Map String a

data Environment a = Global (SymbolTable a) | Environment (SymbolTable a) (Environment a)

get :: Environment a -> String -> Maybe a
get (Environment table outer) name = case Map.lookup name table of
    Just a -> Just a
    Nothing -> get outer name
get (Global table) name = Map.lookup name table

extend :: Environment a -> [(String, a)] -> Environment a
extend (Global table) keyValuePairs = Environment (Map.fromList keyValuePairs) (Global table)
extend (Environment table outer) keyValuePairs = Environment (Map.fromList keyValuePairs) (Environment table outer)

globalEnvironment :: Environment a
globalEnvironment = Global Map.empty