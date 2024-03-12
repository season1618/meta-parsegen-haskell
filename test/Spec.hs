{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DataKinds #-}

import Parser
import GHC.Generics

data Expr = Add Int (StrLit "+") Expr
          | Num Int
    deriving (Show, Generic, Parser)

main :: IO ()
main = do
    print $ (parse "123+28+35" :: Maybe (Expr, String))
