{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Parser
import GHC.Generics

data Expr = Add Int Plus Expr
          | Num Int
    deriving (Show, Generic, Parser)

data Plus = Plus
    deriving (Show, Generic)

instance Parser Plus where
    parse ('+':s) = Just (Plus, s)
    parse _ = Nothing

main :: IO ()
main = do
    print $ (parse "123+28+35" :: Maybe (Expr, String))
