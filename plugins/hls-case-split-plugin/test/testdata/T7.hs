{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T7 where

data X = A
       | B
       | C { foo :: Int }
       | D { bar :: Int, baz :: Int }
       | E
       | F

x :: Int
x = case undefined :: X of
