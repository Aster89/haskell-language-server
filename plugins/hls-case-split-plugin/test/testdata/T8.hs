{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T8 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: Int
foo = bar (A :: X)
  where
    bar s = case s of
