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
  ( MPStmt (..),
    MPVar (..),
    MPExpr (..),
    MPBinOp (..),
    MPUnOp (..),
    MPN (..),
  )
where

import qualified Data.ByteString as B

-- | Statement.
data MPStmt
  = -- | Variable assignment.
    MPSAssign
      { -- | Variable to be assigned.
        mpsVarName :: MPVar,
        -- | Expression to be assigned to the variable.
        mpsVarExpr :: MPExpr
      }
  | -- | Function/procedure call.
    MPSFnCall
      { -- | Name of the called procedure.
        mpsFnName :: B.ByteString,
        -- | Arguments.
        mpsArgs :: [MPExpr]
      }
  | -- | Compound statement.
    MPSCompound [MPStmt]
  | -- | "@if@-@then@-@else@".
    MPSIf
      { -- | Condition.
        mpsCond :: MPExpr,
        -- | Statement for the "then" branch.
        mpsStmtThen :: Maybe MPStmt,
        -- | Statement for the "else" branch.
        mpsStmtElse :: Maybe MPStmt
      }
  | -- | "@while@-@do@".
    MPSWhile
      { -- | Condition.
        mpsCond :: MPExpr,
        -- | Statement to run repeatedly.
        mpsStmtDo :: Maybe MPStmt
      }

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
