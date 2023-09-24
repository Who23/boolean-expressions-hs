module Main where
import Data.IntSet (IntSet)
import Control.Monad.State
import Data.Monoid
import Data.Char
import Control.Arrow (Arrow(first))

{-
  GRAMMAR:
  <expr> ::= <term>
            | <unaryop> <expr>
            | "(" <expr> ")"
            | <expr> <binop> <expr>
  <term> ::= term
  <binop> ::= "and" | "or"
  <unaryop> ::= "not"
-}

data QueryExpr e = TermExpr e
                  | AndExpr (QueryExpr e) (QueryExpr e) 
                  | OrExpr  (QueryExpr e) (QueryExpr e) 
                  | NotExpr (QueryExpr e)
                  deriving (Show)

data Token = LParen | RParen | AndTok | OrTok | NotTok | Text String
  deriving (Show, Eq)

type TagMap = [(String, Int)]
type ParserResult = Maybe (QueryExpr String, [Token])
type Parser = [Token] -> ParserResult

-- tokenizes a query to be parsed
tokenizeQuery :: String -> [Token]
tokenizeQuery ""       = []
tokenizeQuery (' ':cs) = tokenizeQuery cs
tokenizeQuery ('(':cs) = LParen : tokenizeQuery cs
tokenizeQuery (')':cs) = RParen : tokenizeQuery cs
tokenizeQuery ('&':cs) = AndTok : tokenizeQuery cs
tokenizeQuery ('|':cs) = OrTok : tokenizeQuery cs
tokenizeQuery ('!':cs) = NotTok : tokenizeQuery cs
tokenizeQuery (c:cs)       = tokenToOp token' : tokenizeQuery rest
  where (token, rest) = break (`elem` "() &!|") cs
        token' = toLower c : map toLower token
        tokenToOp :: String -> Token
        tokenToOp "and" = AndTok
        tokenToOp "or"  = OrTok
        tokenToOp "not" = NotTok
        tokenToOp s     = Text s
        
-- handling of recursive parser with left recursive grammar taken from
-- https://coursys.sfu.ca/2018fa-cmpt-384-d1/pages/Parsing1

-- parses an expression, trying non-left recursive paths.
-- on hitting a recursive expression, it attempts to "extend" the expression, checking if it
-- is part of a larger left recursive branch of the grammar.
-- essentially going from bottom up instead of top down.
parseExpr :: Parser
parseExpr ((Text s):ts) = extendExpr (TermExpr s) ts
parseExpr (NotTok:ts) =
  case parseExpr ts of
    Just (e, ts') -> extendExpr (NotExpr e) ts'
    Nothing -> Nothing
parseExpr (LParen:ts) =
  case parseExpr ts of
    Just (e, RParen:ts') -> extendExpr e ts'
    Nothing -> Nothing
parseExpr _ = Nothing

extendExpr :: QueryExpr String -> Parser
extendExpr e (AndTok:ts) =
  case parseExpr ts of
    Just (e', ts') -> extendExpr (AndExpr e e') ts'
    Nothing -> Nothing
extendExpr e (OrTok:ts) =
  case parseExpr ts of
    Just (e', ts') -> extendExpr (OrExpr e e') ts'
    Nothing -> Nothing
extendExpr e ts = Just (e, ts)
    
-- execute a query against a single object
executeQuery :: TagMap -> QueryExpr Int -> IntSet -> Bool
executeQuery = undefined

-- parse a set of values into a set object which a query can be executed against
parseSet :: String -> State TagMap IntSet
parseSet = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
