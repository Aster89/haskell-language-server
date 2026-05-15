{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T1 where

data X = X | Y

x :: Int
x = case undefined :: X of
