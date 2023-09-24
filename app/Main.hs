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

type TagMap = (Int, [(String, Int)])
type ParserResult = Maybe (QueryExpr Int, ([Token], TagMap))
type Parser = [Token] -> TagMap -> Maybe (QueryExpr Int, ([Token], TagMap))

getAndAddTag :: TagMap -> String -> (TagMap, Int)
getAndAddTag (i, m) s =
  case entries of 
    [] -> ((i + 1, (s, i) : m), i)
    [(s', i')] -> ((i, m), i')
  where entries = filter (\x -> s == fst x) m

-- tokenizes a query to be parsed
lexQuery :: String -> [Token]
lexQuery ""       = []
lexQuery (' ':cs) = lexQuery cs
lexQuery ('(':cs) = LParen : lexQuery cs
lexQuery (')':cs) = RParen : lexQuery cs
lexQuery ('&':cs) = AndTok : lexQuery cs
lexQuery ('|':cs) = OrTok : lexQuery cs
lexQuery ('!':cs) = NotTok : lexQuery cs
lexQuery (c:cs)       = tokenToOp token' : lexQuery rest
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
parseExpr :: [Token] -> TagMap -> Maybe (QueryExpr Int, ([Token], TagMap))
parseExpr ((Text s):ts) m = extendExpr (TermExpr i') ts m'
  where (m', i') = getAndAddTag m s
parseExpr (NotTok:ts) m =
  case parseExpr ts m of
    Just (e, (ts', m')) -> extendExpr (NotExpr e) ts' m'
    Nothing -> Nothing
parseExpr (LParen:ts) m =
  case parseExpr ts m of
    Just (e, (RParen:ts', m')) -> extendExpr e ts' m'
    Nothing -> Nothing
parseExpr _ _ = Nothing

extendExpr :: QueryExpr Int -> [Token] -> TagMap -> Maybe (QueryExpr Int, ([Token], TagMap))
extendExpr e (AndTok:ts) m =
  case parseExpr ts m of
    Just (e', (ts', m')) -> extendExpr (AndExpr e e') ts' m'
    Nothing -> Nothing
extendExpr e (OrTok:ts) m =
  case parseExpr ts m of
    Just (e', (ts', m')) -> extendExpr (OrExpr e e') ts' m'
    Nothing -> Nothing
extendExpr e ts m = Just (e, (ts, m))

parseQuery :: [Token] -> Maybe (QueryExpr Int)
parseQuery ts = fst <$> parseExpr ts (0, [])
    
-- execute a query against a single object
executeQuery :: TagMap -> QueryExpr Int -> IntSet -> Bool
executeQuery = undefined

-- parse a set of values into a set object which a query can be executed against
parseSet :: String -> State TagMap IntSet
parseSet = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
