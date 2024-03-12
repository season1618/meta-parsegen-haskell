{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser (
    Parser,
    parse,
) where

import GHC.Generics
import Data.Char

class Parser a where
    parse :: String -> Maybe (a, String)
    default parse :: (Generic a, Parser' (Rep a)) => String -> Maybe (a, String)
    parse s = do
        (res, t) <- parse' s
        return (to res, t)

class Parser' f where
    parse' :: String -> Maybe (f p, String)

instance Parser' V1 where
    parse' _ = undefined

instance Parser' U1 where
    parse' s = Just (U1, s)

instance (Parser' f, Parser' g) => Parser' (f :+: g) where
    parse' s = case parse' s of
        Just (res1, s1) -> Just (L1 res1, s1)
        Nothing -> case parse' s of
            Just (res2, s2) -> Just (R1 res2, s2)
            Nothing -> Nothing

instance (Parser' f, Parser' g) => Parser' (f :*: g) where
    parse' s = do
        (res1, s1) <- parse' s
        (res2, s2) <- parse' s1
        return (res1 :*: res2, s2)

instance Parser c => Parser' (K1 i c) where
    parse' s = do
        (res, t) <- parse s
        return (K1 res, t)

instance Parser' f => Parser' (M1 i t f) where
    parse' s = do
        (res, t) <- parse' s
        return (M1 res, t)

instance Parser Int where
    parse s@(c:_) | isDigit c = parseInt 0 s where
        parseInt :: Int -> String -> Maybe (Int, String)
        parseInt v (c:cs) | isDigit c = parseInt (10 * v + digitToInt c) cs
        parseInt v s = Just (v, s)
    parse _ = Nothing
