{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Core(
  Eq(..)
, Ord(..)
, Show(..)
, Enum(..)
, Integral(..)
, Bounded(..)
, RealFrac(..)
, Num(..)
, Fractional(..)
, Bool(..)
, Either(..)
, Int
, Integer
, IO
, Rational
, seq
, error
, undefined
, const
, flip
, curry
, uncurry
, id
, otherwise
, (.)
, ($)
, (&&)
, (||)
, not
, even
, odd
, fst
, snd
, getChar
, on
, first
, second
, (&&&)
, (***)
, IsString(..)
, module Data.Char
, ifThenElse
) where


import           Data.String   (IsString (..))
import           Prelude       (Bool (..), Bounded (..), Char, Either (..),
                                Enum (..), Eq (..), Fractional (..), IO, Int,
                                Integer, Integral (..), Num (..), Ord (..),
                                Rational, RealFrac (..), Show (..), const,
                                curry, error, even, flip, fst, id, not, odd,
                                otherwise, seq, snd, uncurry, undefined, ($),
                                (&&), (.), (||))

import           Control.Arrow (first, second, (&&&), (***))
import           Data.Char
import           Data.Function (on)
import           System.IO     (getChar)


ifThenElse ::
  Bool
  -> a
  -> a
  -> a
ifThenElse True t _ =
  t
ifThenElse False _ f =
  f
