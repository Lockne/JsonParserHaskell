module JsonParser where

import AST
import Custom
import Control.Applicative
import Data.Char

-- | Okay! Let's make our parser from scratch.
--   Baby steps is what we'll take.


-- | We'll tackle the null value in Json first.
--   I want to write a function that takes an
--   input string "null" and returns a value JsonNull

jsonNull :: Parser Json
jsonNull = (\_ -> JsonNull) <$> stringP "null"

-- | Next we'll tackle the boolean values in Json.
--   Our goal is to write a function that takes an
--   input string saying "true" and returns JsonBool True
--   , and takes an input string "false" and returns
--   JsonBool False
jsonBool :: Parser Json
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False

-- | Next in line are numbers! We'll only parse integers for now.
--   The deal with integers is that we want to parse something
--   like "1234onetwo" and return ("1234", JsonNumber Integer)
--   Also remember that our function might try to parse the
--   empty string "". Since we're using read, this will spit
--   out an Exception error! To fix that we'll make use of a
--   helper function notEmpty.
jsonNumber :: Parser Json
jsonNumber = (\xs -> JsonNumber $ read xs) <$> notEmpty (spanP isDigit)

-- | Having made a number parsers, let's turn our attention to parsing strings!
--   In Json, strings are a bunch of literals enclosed in quotes.
--   This is a string: "i am a string"
jsonString :: Parser Json
jsonString = JsonString <$> (charP '"' *> stringLiteral <* charP '"')

-- | Now we can successfully parse null values, boolean values, numbers and strings.
--   The next two types of values are a bit harder to parse.
--   We'll tackle the simpler one of the two. The JsonArray.
jsonArray :: Parser Json
jsonArray =
  JsonArray <$> (charP '[' *> whitespaceP *> value <* whitespaceP <* charP ']')
  where
    value = sepBy (whitespaceP *> charP ',' <* whitespaceP) jsonValue


-- | Before we tackle JsonObjects, we will define a helper function pair that
--   a string that looks like " \"name\": value " to (name, Json)
keyValuePair :: Parser (String, Json)
keyValuePair = liftA3 (\key _ value -> (key, value)) stringLiteral (whitespaceP *> charP ':' <* whitespaceP) jsonValue

jsonObject :: Parser Json
jsonObject =
  JsonObject <$>
  (charP '{' *> whitespaceP *>
   sepBy (whitespaceP *> charP ',' <* whitespaceP) keyValuePair <*
   whitespaceP <*
   charP '}')

jsonValue :: Parser Json
jsonValue =
  jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|>
  jsonObject
