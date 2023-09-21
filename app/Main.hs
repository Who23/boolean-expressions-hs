module Main where
import Data.IntSet (IntSet)
import Control.Monad.State

data QueryNode e = Term e
                    | And (QueryNode e) (QueryNode e) 
                    | Or  (QueryNode e) (QueryNode e) 
                    | Not (QueryNode e)

data Token = LParen | RParen | Text String

type TagMap = [(String, Int)]

-- tokenizes a query to be parsed
tokenizeQuery :: String -> [Token]
tokenizeQuery = undefined

-- parse a query to be executed against a set
parseQuery :: [Token] -> State TagMap (QueryNode Int)
parseQuery = undefined

-- execute a query against a single object
executeQuery :: TagMap -> QueryNode Int -> IntSet -> Bool
executeQuery = undefined

-- parse a set of values into a set object which a query can be executed against
parseSet :: String -> State TagMap IntSet
parseSet = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
