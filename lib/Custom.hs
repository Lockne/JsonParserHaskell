{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module Custom where


import Data.Char
import Control.Applicative

newtype Parser a =
  Parser
    { runParser :: String -> Maybe (String, a)
    }
  deriving (Functor)


-- | Our Parser is essentially a newtype that wraps around a
--   function. This function takes a string, and returns a tuple
--   that contains a the remaining string stream, and the parsed
--   value. If the parser fails to parse the string, then it returns
--   Nothing. This is why we embed our tuple in the Maybe context.
-- | Now a Parser is an instance of a Functor, and Applicative.
--   It is also an instance of the Alternative typeclass.
--   The reason why we wish to implement our Parser as an
--   instance of Alternative is so that if something fails to
--   parse then our Parser will choose an alternative way to parse
--   that thing.
instance Applicative (Parser) where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser pa) =
    Parser $ \input -> do
      (input', f) <- pf input
      (input'', a) <- pa input'
      Just (input'', f a)

instance Alternative (Parser) where
  empty :: Parser a
  empty = Parser $ \input -> Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

notEmpty :: Parser [a] -> Parser [a]
notEmpty (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    case null xs of
      True -> Nothing
      False -> Just (input', xs)

whitespaceP :: Parser String
whitespaceP = spanP isSpace

stringLiteral :: Parser String
stringLiteral = spanP (/= '"')

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = liftA2 (:) p $ many (sep *> p) <|> pure []
