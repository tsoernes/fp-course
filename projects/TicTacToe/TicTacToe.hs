module TicTacToe where

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}

-- :set -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XUndecidableInstances -XDataKinds -XGADTs -XTypeFamilies -XDeriveAnyClass
-- import Data.Maybe

-- data Player = X | O

-- data Pos = Pos Int Int

-- data Nat = Zero | Succ Nat

-- data Board = A | B

-- data BoardState = Empty | InPlay | Finished

-- data Game = Game BoardState Board

-- type family Move game pos where
--   Move ('Game 'Empty b) pos = 'Game 'InPlay b
--   -- Move ('GInPlay b) pos = 'GInPlay b

-- isFinished :: BBoard n s Board -> Bool
-- isFinished bbrd = isJust $ whoWon' bbrd

-- whoWon' :: BBoard n s Board -> Maybe Player
-- whoWon' BEmpty = Nothing
-- -- whoWon' (BInPlay _ _) = Just X
-- whoWon' (BNonEmpty_ _) = Just X
--
--data BStarted = BStarted BInPlay | BFinished
--data BBoard = BEmpty | BInPlay | BFinished
--
-- move :: BB-> Pos -> BBoard
-- move = undefined
--
--whoWon :: BFinished -> Maybe Player
--whoWon = undefined
--
--playerAt :: BBoard -> Pos -> Maybe Player
--playerAt = undefined
--
--takeBack :: BNotFinished -> Pos -> BBoard
--takeBack = undefined
----------------------------------------------------------

nil = undefined

data Nil
data Cons x xs

data MList a where
  MNil :: MList a
  MCons :: a -> MList a -> MList a

class First list x | list -> x
instance First Nil Nil
instance First (Cons x more) x

-- Is it possible to map a type family?
type family MFirst list where
  MFirst 'MNil = 'MNil
  MFirst ('MCons x _) = x

type family MRest list where
  MRest 'MNil = 'MNil
  MRest ('MCons _ r) = r

class ListConcat a b c | a b -> c
instance ListConcat Nil x x
instance (ListConcat as bs cs)
  => ListConcat (Cons a as) bs (Cons a cs)

type family MListConcat a b where
  MListConcat 'MNil x = x
  MListConcat ('MCons a as) bs = 'MCons a (MListConcat as bs)

-- Concatenate all lists in a list
class ListConcatAll ls l | ls -> l
instance ListConcatAll Nil Nil
instance (ListConcat chunk acc result,
          ListConcatAll rest acc)
  => ListConcatAll (Cons chunk rest) result

type family MListConcatAll ls where
  MListConcatAll 'MNil = 'MNil
  MListConcatAll ('MCons chunk rest) = MListConcat chunk (MListConcatAll rest)

data True
data False

data MBool = MTrue | MFalse

-- Is any element of this list True?
class AnyTrue list t | list -> t
instance AnyTrue Nil              False
instance AnyTrue (Cons True more) True
instance (AnyTrue list t)
  => AnyTrue (Cons False list) t

type family MAnyTrue list where
  MAnyTrue 'MNil = 'MFalse
  MAnyTrue ('MCons 'MTrue _) = 'MTrue
  MAnyTrue ('MCons 'MFalse list) = MAnyTrue list

class Not b1 b | b1 -> b
instance Not False True
instance Not True  False

type family MNot b where
  MNot 'MFalse = 'MTrue
  MNot 'MTrue = 'MFalse

class Or b1 b2 b | b1 b2 -> b
instance Or True  True  True
instance Or True  False True
instance Or False True  True
instance Or False False False

type family MOr b1 b2 where
  MOr 'MTrue 'MTrue   = 'MTrue
  MOr 'MTrue 'MFalse  = 'MTrue
  MOr 'MFalse 'MTrue  = 'MTrue
  MOr 'MFalse 'MFalse = 'MFalse

type family MAnd b1 b2 where
  MAnd 'MTrue 'MTrue   = 'MTrue
  MAnd 'MTrue 'MFalse  = 'MFalse
  MAnd 'MFalse 'MTrue  = 'MFalse
  MAnd 'MFalse 'MFalse = 'MFalse

type family And b1 b2 where
  And True True   = True
  And True False  = False
  And False True  = False

data Z
data S n

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8

data Nat = MZ | MS Nat

type MN0 = 'MZ
type MN1 = 'MS MN0
type MN2 = 'MS MN1
type MN3 = 'MS MN2
type MN4 = 'MS MN3
type MN5 = 'MS MN4
type MN6 = 'MS MN5
type MN7 = 'MS MN6
type MN8 = 'MS MN7
type MN9 = 'MS MN8

-- Equality
class PeanoEqual a b t | a b -> t
instance PeanoEqual Z     Z     True
instance PeanoEqual (S a) Z     False
instance PeanoEqual Z     (S b) False
instance (PeanoEqual a b t)
  => PeanoEqual (S a) (S b) t

type family MEqual a b where
  MEqual 'MZ 'MZ = 'MTrue
  MEqual ('MS _) 'MZ = 'MFalse
  MEqual 'MZ ('MS _) = 'MTrue
  MEqual ('MS a) ('MS b) = MEqual a b

-- Comparison (<)
class PeanoLT a b t | a b -> t
instance PeanoLT Z      Z     False
instance PeanoLT (S x)  Z     False
instance PeanoLT Z      (S x) True
instance (PeanoLT a b t)
  => PeanoLT (S a) (S b) t

type family MLT a b where
  MLT 'MZ 'MZ = 'MFalse
  MLT ('MS _) 'MZ = 'MFalse
  MLT 'MZ ('MS _) = 'MTrue
  MLT ('MS a) ('MS b) = MLT a b

-- Absolute difference
class PeanoAbsDiff a b c | a b -> c
instance PeanoAbsDiff Z Z Z
instance PeanoAbsDiff Z (S b) (S b)
instance PeanoAbsDiff (S a) Z (S a)
instance (PeanoAbsDiff a b c)
  => PeanoAbsDiff (S a) (S b) c

type family MAbsDiff a b where
  MAbsDiff 'MZ 'MZ = 'MZ
  MAbsDiff 'MZ a = a
  MAbsDiff a 'MZ = a
  MAbsDiff ('MS a) ('MS b) = MAbsDiff a b

-- Integers from n to 0
class Range n xs | n -> xs
instance Range Z Nil
instance (Range n xs)
  => Range (S n) (Cons n xs)

type family MRange n where
  MRange 'MZ = 'MNil
  MRange ('MS a) = 'MCons a (MRange a)

class Apply f a r | f a -> r

data MConj1 list = MConj1 list

data Conj1 list
instance Apply (Conj1 list) x (Cons x list)

type family MApply f a where
  MApply ('MConj1 list) x = 'MCons x list

-- Map f over a list
class Map f xs ys | f xs -> ys
instance Map f Nil Nil
instance (Apply f x y, Map f xs ys)
      => Map f (Cons x xs) (Cons y ys)

type family MMap f xs where
  MMap f ('MCons x xs) = 'MCons (MApply f x) (MMap f xs)

-- Map f over list and concatenate results together
class MapCat f xs zs | f xs -> zs
instance MapCat f Nil Nil
instance (Map f xs chunks, ListConcatAll chunks ys)
      => MapCat f xs ys

type family MMapCat f xs where
  MMapCat f 'MNil = 'MNil
  MMapCat f xs = MListConcatAll (MMap f xs)

-- Filter a list with an Apply-able predicate function
class AppendIf pred x ys zs | pred x ys -> zs
instance AppendIf True x ys (Cons x ys)
instance AppendIf False x ys ys

class Filter f xs ys | f xs -> ys
instance Filter f Nil Nil
instance (Apply f x t,
          Filter f xs ys,
          AppendIf t x ys zs)
  => Filter f (Cons x xs) zs

-------------------------------------------------------
data CellType = None | X | O

data Cell (a :: CellType) where
  CellOf :: a -> Cell a

data Row a where
  EmptyRow :: Row ('MCons 'None ('MCons 'None ('MCons 'None 'MNil)))

-- data Game a where
--   EmptyGame :: Game (MCons EmptyRow (MCons EmptyRow (MCons EmptyRow MNil)))

type family CellEq c1 c2 where
  CellEq 'None 'None = True
  CellEq 'X 'X       = True
  CellEq 'O 'O       = True
  CellEq _ _         = False

type family ZWonLine line z where
  ZWonLine 'MNil _ = True
  ZWonLine ('MCons x xs) z = And (CellEq x z) (ZWonLine xs z)

data Id a = Id a
zWonRow :: Row a -> Cell -> Id (ZWonLine a b)
zWonRow = undefined

-- type family ZWon board z where
--   ZWon board z = MOr (ZWonLine (MMapCat MFirst board) z) (ZWon (MMap MRest board) z)
  -- todo: diags

-- type family Move game pos where
--   Move

data Queen x y
data Queen1 x
instance Apply (Queen1 x) y (Queen x y)

-- A list of queens in row x with y from 0 to n.
class QueensInRow n x queens | n x -> queens
instance (Range n ys, Map (Queen1 x) ys queens)
  => QueensInRow n x queens

-- Does queen a threaten queen b?
class Threatens a b t | a b -> t
instance (PeanoEqual ax bx xeq,
          PeanoEqual ay by yeq,
          Or xeq yeq xyeq,
          PeanoAbsDiff ax bx dx,
          PeanoAbsDiff ay by dy,
          PeanoEqual dx dy deq,
          Or xyeq deq res)
  => Threatens (Queen ax ay) (Queen bx by) res

-- Partial application of Threatens
data Threatens1 a
instance (Threatens a b t)
  => Apply (Threatens1 a) b t

-- Is queen b compatible with all queen as?
class Safe config queen t | config queen -> t
instance (Map (Threatens1 queen) config m1,
          AnyTrue m1 t1,
          Not     t1 t2)
  => Safe config queen t2

data Safe1 config
instance (Safe config queen t)
  => Apply (Safe1 config) queen t

-- Add a queen with the given x coordinate to a legal configuration, returning
-- a set of legal configurations.
class AddQueen n x c cs | n x c -> cs
instance (QueensInRow n x candidates,
          Filter (Safe1 c) candidates filtered,
          Map (Conj1 c) filtered cs)
  => AddQueen n x c cs

data AddQueen2 n x
instance (AddQueen n x c cs)
  => Apply (AddQueen2 n x) c cs

-- Add a queen at x to every configuration, returning a set of legal
-- configurations.
class AddQueenToAll n x cs cs' | n x cs -> cs'
instance (MapCat (AddQueen2 n x) cs cs')
  => AddQueenToAll n x cs cs'


-- Add queens recursively
class AddQueensIf pred n x cs cs' | pred n x cs -> cs'
instance AddQueensIf False n x cs cs
instance (AddQueenToAll n x cs cs2,
          AddQueens n (S x) cs2 cs')
  => AddQueensIf True n x cs cs'

class AddQueens n x cs cs' | n x cs -> cs'
instance (PeanoLT x n pred,
          AddQueensIf pred n x cs cs')
  => AddQueens n x cs cs'

-- Solve
class Solution n c | n -> c where
  solution :: n -> c
instance (AddQueens n Z (Cons Nil Nil) cs,
          First cs c)
  => Solution n c where solution = nil



