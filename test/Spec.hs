{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DataKinds #-}

import Parser
import GHC.Generics
import Data.Char

data Expr = Add Term (StrLit "+") Expr
          | Sub Term (StrLit "-") Expr
          | Term Term
    deriving (Show, Generic, Parser)

data Term = Mul Fact (StrLit "*") Term
          | Div Fact (StrLit "/") Term
          | Fact Fact
    deriving (Show, Generic, Parser)

data Fact = Expr (StrLit "(") Expr (StrLit ")")
          | Num Int
          | Var Letters
    deriving (Show, Generic, Parser)

data Letters = Letters String
    deriving (Show, Generic)

instance Parser Letters where
    parse s = return $ (\(x, y) -> (Letters x, y)) (parseLetter "" s) where
        parseLetter :: String -> String -> (String, String)
        parseLetter v (c:s) | isLetter c = parseLetter (v ++ [c]) s
        parseLetter v s = (v, s)

main :: IO ()
main = do
    print $ (parse "1+2-3" :: Maybe (Expr, String))
    print $ (parse "4/5*6" :: Maybe (Expr, String))
    print $ (parse "(1+2)*(3/4)-5" :: Maybe (Expr, String))
    print $ (parse "abc/(xyz+1-2)" :: Maybe (Expr, String))
