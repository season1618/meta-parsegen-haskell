{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Lib
import GHC.Generics
import Data.Char

data Expr = Add Int Plus Expr
          | Num Int
    deriving (Show, Generic, Parser)

data Plus = Plus
    deriving (Show, Generic)

instance Parser Int where
    parse (c:s) | isDigit c = Just (digitToInt c, s)
    parse _ = Nothing

instance Parser Plus where
    parse ('+':s) = Just (Plus, s)
    parse _ = Nothing

main :: IO ()
main = do
    print $ (parse "1+2+3" :: Maybe (Expr, String))
