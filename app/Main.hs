{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Data.Set (Set, member, insert, fromList)
import Control.Monad.State
import Control.Monad (guard)
import Control.Applicative (Alternative(..))
import Data.Monoid
import Control.Arrow (Arrow(first))
import System.Environment
import Data.Char

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

unaryop :: Parser (Bool -> Bool)
unaryop = (sym "not" <|> sym "!") *> pure not

orop :: Parser (Bool -> Bool -> Bool)
orop = (sym "or" <|> sym "|") *> pure (||)

andop :: Parser (Bool -> Bool -> Bool)
andop = (sym "and" <|> sym "&") *> pure (&&)


term :: Set String -> Parser Bool
term s = do
  t <- token (some $ satisfy validChar)
  return (t `member` s)
  where
    validChar :: Char -> Bool
    validChar c = not $ isSpace c || (c == ')') || (c == '(')

terminal :: Set String -> Parser Bool
terminal s = (unaryop <*> disjunction s) <|> (sym "(" *> disjunction s <* sym ")") <|> term s

conjunction :: Set String -> Parser Bool
conjunction s = chainl1 (terminal s) andop <|> terminal s

disjunction :: Set String -> Parser Bool
disjunction s = chainl1 (conjunction s) orop <|> conjunction s

parse :: Set String -> Parser Bool
parse = disjunction

main :: IO ()
main = do
  args <- getArgs
  let query = head args
  let set' = fromList $ words $ map toLower $ unwords $ tail args
  showResult $ runParser (parse set') query
  where
    showResult :: Maybe (Bool, String) -> IO ()
    showResult (Just (b, s))
      | null s = print b
      | otherwise = print $ "Parsing error: left over tokens are " ++ s
    showResult Nothing = print "Parsing error: no match"
