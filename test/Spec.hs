{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DataKinds #-}

import Parser
import GHC.Generics

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
    deriving (Show, Generic, Parser)

main :: IO ()
main = do
    print $ (parse "1+2-3" :: Maybe (Expr, String))
    print $ (parse "4/5*6" :: Maybe (Expr, String))
    print $ (parse "(1+2)*(3/4)-5" :: Maybe (Expr, String))
