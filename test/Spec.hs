{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DataKinds #-}

import Parser
import Grammar
import AutoExpr
import GHC.Generics
import Data.Char

data Expr = Expr Term [HOr [HCon [StrLit "+", Term], HCon [StrLit "-", Term]]]
    deriving (Show, Generic, Parser)

data Term = Term Fact [HOr [HCon [StrLit "*", Fact], HCon [StrLit "/", Fact]]]
    deriving (Show, Generic, Parser)

data Fact = ParenExpr (StrLit "(") Expr (StrLit ")")
          | Num Int
          | Var Letters (Maybe (StrLit "[]"))
    deriving (Show, Generic, Parser)

data Prim' = ParenExpr' (StrLit "(") Expr' (StrLit ")")
           | Num' Int
    deriving (Show, Generic, Parser)
type Expr' = AutoExpr [ HOr [StrLit "+", StrLit "-"], HOr [StrLit "*", StrLit "/"] ] Prim'

data Letters = Letters String
    deriving (Show, Generic)

instance Parser Letters where
    parse s = return $ (\(x, y) -> (Letters x, y)) (parseLetter "" s) where
        parseLetter :: String -> String -> (String, String)
        parseLetter v (c:s) | isLetter c = parseLetter (v ++ [c]) s
        parseLetter v s = (v, s)

main :: IO ()
main = do
    print $ (parse "1+2-3+4-5" :: Maybe (Expr, String))
    print $ (parse "4/5*6/7*8" :: Maybe (Expr, String))
    print $ (parse "(1+2)*(3/4)-5" :: Maybe (Expr, String))
    print $ (parse "abc/(xyz[]-1+2)" :: Maybe (Expr, String))
    print $ (parse "1+2*3/(4-5*6)+7" :: Maybe (Expr', String))
    print $ (parse "1*2*3+4-5-6/7" :: Maybe (Expr', String))
