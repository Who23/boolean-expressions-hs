{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Data.IntSet (IntSet, member, insert)
import qualified Data.IntSet as IntSet (empty)
import Control.Monad.State
import Control.Monad (guard)
import Control.Applicative (Alternative(..))
import Data.Monoid
import Data.Char
import Control.Arrow (Arrow(first))
import qualified Data.Foldable as IntSet
import qualified Data.IntMap as Prelude
import System.Environment

{-
  GRAMMAR:
  <terminal> ::= <unaryop> <disjunction> | "(" <disjunction> ")" | <term>
  <conjunction> ::= <terminal> <and> <conjunction> | <terminal>
  <disjunction> ::= <conjunction> <or> <conjunction> | <conjunction>
  <term> ::= term
  <unaryop> ::= "not"
  <and> ::= "and" | "&"
  <or> ::= "and" | "&"
-}

{-
  GRAMMAR:
  <expr> ::= <beta> <expr'>
  <expr'> ::= <binop> <expr> <expr'> | e
  <beta> ::= <term> | <unaryop> <expr> | "(" <expr> ")"
  <term> ::= term
  <binop> ::= "and" | "or"
  <unaryop> ::= "not"
-}

data Expr e = Term e
                  | And (Expr e) (Expr e) 
                  | Or (Expr e) (Expr e) 
                  | Not (Expr e)
                  deriving (Show)

type PExpr = Expr String

instance Functor Expr where
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Term e)  = Term (f e)
  fmap f (And l r) = And (fmap f l) (fmap f r)
  fmap f (Or l r) = Or (fmap f l) (fmap f r)
  fmap f (Not t) = Not (fmap f t)

type TagMap = (Int, [(String, Int)])
newtype Parser a = Parser { unParser :: StateT String Maybe a }

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT . unParser

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f x = Parser $ f <$> unParser x

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ pure x
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> x = Parser $ unParser pf <*> unParser x

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  -- this do block runs in the context of the Maybe monad
  x >>= f = Parser $ StateT $ \s -> do
    (a, s') <- runParser x s
    runParser (f a) s'

instance Alternative Parser where
  empty :: Parser a
  empty = Parser empty

  (<|>) :: Parser a -> Parser a -> Parser a
  x <|> y = Parser $ unParser x <|> unParser y

item :: Parser Char
item = Parser $ StateT $ \case
    [] -> Nothing
    (c:cs) -> Just (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- item
  guard (f c)
  return c

char :: Char -> Parser Char
char c = satisfy (c == )

string :: String -> Parser String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

token :: Parser a -> Parser a
token p = many (satisfy isSpace) *> p

sym :: String -> Parser String
sym = token . string

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  a <- p
  rest a
  where
    rest a = (do 
              f <- op
              b <- p
              rest (f a b)) <|> pure a

unaryop :: Parser (PExpr -> PExpr)
unaryop = sym "not" *> pure Not

orop :: Parser (PExpr -> PExpr -> PExpr)
orop = (sym "or" <|> sym "|") *> pure Or

andop :: Parser (PExpr -> PExpr -> PExpr)
andop = (sym "and" <|> sym "&") *> pure And

term :: Parser PExpr
term = Term <$> token (some $ satisfy $ not . isSpace)

terminal :: Parser PExpr
terminal = (unaryop <*> disjunction) <|> (sym "(" *> disjunction <* sym ")") <|> term

conjunction :: Parser PExpr
conjunction = chainl1 terminal andop <|> terminal

disjunction :: Parser PExpr
disjunction = chainl1 conjunction orop <|> conjunction

getAndAddTag :: TagMap -> String -> (TagMap, Int)
getAndAddTag (i, m) s =
  case entries of 
    [] -> ((i + 1, (s, i) : m), i)
    [(s', i')] -> ((i, m), i')
  where entries = filter (\x -> s == fst x) m
    
-- execute a query against a single object
executeQuery :: Expr Int -> IntSet -> Bool
executeQuery (Term i)  s = i `member` s
executeQuery (Not e)   s = not $ executeQuery e s
executeQuery (And l r) s = executeQuery l s && executeQuery r s
executeQuery (Or l r)  s = executeQuery l s || executeQuery r s

parseTag :: String -> (IntSet, TagMap) -> (IntSet, TagMap)
parseTag t (s, m) = (insert i s, m')
  where
    (m', i) = getAndAddTag m t

parseSets' :: String -> ([IntSet], TagMap) -> ([IntSet], TagMap)
parseSets' s (is, m) = (i:is, m')
  where 
    (i, m') = foldr parseTag (IntSet.empty, m) (words s)

parseSets :: TagMap -> [String] -> ([IntSet], TagMap)
parseSets m = foldr parseSets' ([], m)

main :: IO ()
main = undefined
-- main = do
--   args <- getArgs
--   let ([rawQuery], objects) = splitAt 1 args
--   let Just (q, (_, m)) = parseExpr (lexQuery rawQuery) (0, [])
--   let (os, _) = parseSets m objects
--   let results = map (executeQuery q) os
--   mapM_ print results
  
  
