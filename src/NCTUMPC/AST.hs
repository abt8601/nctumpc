-- |
-- Module      : NCTUMPC.AST
-- Description : Abstract syntax tree
-- Copyright   : (c) Po-Yi Tsai, 2021
-- License     : Unlicense
-- Maintainer  : abt8601@protonmail.ch
-- Stability   : experimental
-- Portability : portable
--
-- Abstract syntax tree of a MiniPascal source.
module NCTUMPC.AST
  ( MPVar (..),
    MPExpr (..),
    MPBinOp (..),
    MPUnOp (..),
    MPN (..),
  )
where

import qualified Data.ByteString as B

-- | Variable.
data MPVar = MPVar
  { -- | Name.
    mpvName :: B.ByteString,
    -- | Subscripts.
    mpvSubscrs :: [MPExpr]
  }
  deriving (Eq, Ord, Show, Read)

-- | Expression.
data MPExpr
  = -- | Binary operator.
    MPEBinOp
      { -- | Left hand side.
        mpeLHS :: MPExpr,
        -- | Operator.
        mpeBOp :: MPBinOp,
        -- | Right hand side.
        mpeRHS :: MPExpr
      }
  | -- | Unary operator.
    MPEUnOp
      { -- | Operator.
        mpeUOp :: MPUnOp,
        -- | Operand.
        mpeOperand :: MPExpr
      }
  | -- | Variable, or function call with no arguments.
    MPEVar MPVar
  | -- | Function call.
    MPEFnCall
      { -- | Name of the called function.
        mpeFnName :: B.ByteString,
        -- | Arguments.
        mpeArgs :: [MPExpr]
      }
  | -- | Number.
    MPEN MPN
  | -- | String literal. The value is quoted.
    MPELitStr B.ByteString
  deriving (Eq, Ord, Show, Read)

-- | Binary operator.
data MPBinOp
  = -- | "@and@".
    MPOAnd
  | -- | "@or@".
    MPOOr
  | -- | "@<@".
    MPOLt
  | -- | "@>@".
    MPOGt
  | -- | "@=@".
    MPOEq
  | -- | "@<=@".
    MPOLet
  | -- | "@>=@".
    MPOGet
  | -- | "@!=@".
    MPONeq
  | -- | "@+@".
    MPOAdd
  | -- | "@-@".
    MPOSub
  | -- | "@*@".
    MPOMul
  | -- | "@/@".
    MPODiv
  deriving (Eq, Ord, Show, Read)

-- | Unary operator.
data MPUnOp
  = -- | "@not@".
    MPONot
  | -- | "@-@".
    MPONeg
  deriving (Eq, Ord, Show, Read)

-- | Number.
data MPN
  = -- | Number of type @integer@.
    MPNInteger Integer
  | -- | Number of type @real@.
    MPNReal Double
  deriving (Eq, Ord, Show, Read)
