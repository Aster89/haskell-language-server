{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T1 where

data X = A
       | B
       | C Int
       | D Int Int
       | E

x :: Int
x = case undefined :: X of
            A -> _
            B -> _
            C _ -> _
            D _ _ -> _
            E -> _
