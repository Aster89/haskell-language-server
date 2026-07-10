{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T4 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

x :: Int
x = case undefined :: X of
        {}
