-- |
-- Module      : NCTUMPC.Loc
-- Description : Source code location
-- Copyright   : (c) Po-Yi Tsai, 2021
-- License     : Unlicense
-- Maintainer  : abt8601@protonmail.ch
-- Stability   : experimental
-- Portability : portable
--
-- Types related to the source code location.
module NCTUMPC.Loc (Loc (..), LocSpan (..)) where

-- | A location of a program source.
data Loc = Loc
  { -- | Line number.
    locLn :: Int,
    -- | Column number.
    locCol :: Int
  }
  deriving (Eq, Ord, Show, Read)

-- | A span of location of a program source.
data LocSpan = LocSpan
  { -- | Starting location.
    locFirst :: Loc,
    -- | Ending location.
    locLast :: Loc
  }
  deriving (Eq, Ord, Show, Read)

instance Semigroup LocSpan where
  LocSpan {locFirst = f} <> LocSpan {locLast = l} =
    LocSpan {locFirst = f, locLast = l}
