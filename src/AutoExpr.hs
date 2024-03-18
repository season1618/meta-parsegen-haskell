{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module AutoExpr (AutoExpr) where

import Data.Kind
import Parser

data AutoExpr (ops :: [Type]) prim where
    Term :: AutoExpr as prim -> AutoExpr (a ': as) prim
    Prim :: prim -> AutoExpr '[] prim
    Op :: a -> AutoExpr (a ': as) prim -> AutoExpr as prim -> AutoExpr (a ': as) prim

instance Show prim => Show (AutoExpr '[] prim) where
    show (Prim p) = show p

instance (Show prim, Show a, Show (AutoExpr as prim)) => Show (AutoExpr (a ': as) prim) where
    show (Term t) = show t
    show (Op op lhs rhs) = "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"

instance Parser prim => Parser (AutoExpr '[] prim) where
    parse s = do
        (res, s') <- parse s
        return (Prim res, s')

instance (Parser a, Parser (AutoExpr as prim)) => Parser (AutoExpr (a ': as) prim) where
    parse s = do
        (e, s') <- parse s :: Maybe (AutoExpr as prim, String)
        return $ parseExpr (Term e) s'
        where
            parseExpr :: AutoExpr (a ': as) prim -> String -> (AutoExpr (a ': as) prim, String)
            parseExpr e s = case parseTerm s of
                Just (op, e', s') -> parseExpr (Op op e e') s'
                Nothing -> (e, s)
                where
                    parseTerm :: String -> Maybe (a, AutoExpr as prim, String)
                    parseTerm s = do
                        (op, s1) <- parse s
                        (e', s2) <- parse s1
                        return (op, e', s2)
