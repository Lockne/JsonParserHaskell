module AST where

-- | Define the abstract syntax tree for the parser

data Json = JsonNull
          | JsonBool Bool
          | JsonNumber Integer
          | JsonString String
          | JsonArray [Json]
          | JsonObject [(String, Json)]
          deriving (Eq, Show)
