{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Grammar (
    HCon,
    HOr,
) where

import Data.Kind
import Parser

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
        (x, s1) <- parse s
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
    parse _ = Nothing

instance (Parser a, Parser (HOr as)) => Parser (HOr (a ': as)) where
    parse s = join (parse s) (parse s) where
        join :: Maybe (a, String) -> Maybe (HOr as, String) -> Maybe (HOr (a ': as), String)
        join (Just (res, s)) _ = Just (Head res, s)
        join _ (Just (res, s)) = Just (Tail res, s)
        join _ _ = Nothing

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
