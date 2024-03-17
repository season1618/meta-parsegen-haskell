{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser (
    Parser,
    parse,
    StrLit,
    HCon,
    HOr,
) where

import GHC.Generics
import GHC.TypeLits
import Data.Char
import Data.Proxy
import Data.Kind

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

data StrLit (s :: Symbol) = StrLit

instance KnownSymbol t => Show (StrLit t) where
    show s = (symbolVal (Proxy :: Proxy t))

instance KnownSymbol t => Parser (StrLit t) where
    parse s = parseStrLit s (symbolVal (Proxy :: Proxy t)) where
        parseStrLit :: String -> String -> Maybe (StrLit t, String)
        parseStrLit (c:cs) (d:ds) | c == d = parseStrLit cs ds
        parseStrLit s "" = Just (StrLit, s)
        parseStrLit _ _ = Nothing

data HCon (as :: [Type]) where
    HNil :: HCon '[]
    HCons :: a -> HCon as -> HCon (a ': as)

instance Show (HCon '[]) where
    show _ = "[]"

instance (Show a, Show (HCon as)) => Show (HCon (a ': as)) where
    show (HCons x xs) = "[" ++ show x ++ ", " ++ tail (show xs)

instance Parser (HCon '[]) where
    parse s = Just (HNil, s)

instance (Parser a, Parser (HCon as)) => Parser (HCon (a ': as)) where
    parse s = do
        (x, s1) <- parse s :: Maybe (a, String)
        (xs, s2) <- parse s1
        return (HCons x xs, s2)

data HOr (as :: [Type]) where
    Head :: a -> HOr (a ': as)
    Tail :: HOr as -> HOr (a ': as)

instance Show (HOr '[]) where
    show _ = "Nothing"

instance (Show a, Show (HOr as)) => Show (HOr (a ': as)) where
    show (Head x) = show x
    show (Tail x) = show x

instance Parser (HOr '[]) where
    parse s = Nothing

instance (Parser a, Parser (HOr as)) => Parser (HOr (a ': as)) where
    parse s = disjunct (parse s) (parse s) where
        disjunct :: Maybe (a, String) -> Maybe (HOr as, String) -> Maybe (HOr (a ': as), String)
        disjunct (Just (res, s)) _ = Just (Head res, s)
        disjunct _ (Just (res, s)) = Just (Tail res, s)
        disjunct _ _ = Nothing

instance Parser a => Parser (Maybe a) where
    parse s = case parse s of
        Just (res, s') -> Just (Just res, s')
        Nothing -> Just (Nothing, s)

instance Parser a => Parser [a] where
    parse s = parseList [] s where
        parseList :: [a] -> String -> Maybe ([a], String)
        parseList v s = case parse s of
            Just (res, s') -> parseList (v ++ [res]) s'
            Nothing -> Just (v, s)
