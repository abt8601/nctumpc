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
  ( MPProg (..),
    MPDecl,
    MPType (..),
    MPStdType (..),
    MPSubProgDecl (..),
    MPStmt (..),
    MPVar (..),
    MPExpr (..),
    MPBinOp (..),
    MPUnOp (..),
    MPN (..),
  )
where

import qualified Data.ByteString as B

-- | Program.
data MPProg = MPProg
  { -- | Program name.
    mppName :: B.ByteString,
    -- | External file descriptors.
    mppExtFds :: [B.ByteString],
    -- | Declarations.
    mppDecls :: [MPDecl],
    -- | Subprogram declarations.
    mppSubProgDecls :: [MPSubProgDecl],
    -- | Program body.
    mppBody :: [MPStmt]
  }

-- | Declaration.
type MPDecl = (B.ByteString, MPType)

-- | Type.
data MPType
  = -- | Standard type.
    MPTStdType MPStdType
  | -- | Array.
    MPTArray
      { -- | Lower bound of array.
        mptArrayLBound :: MPN,
        -- | Upper bound of array.
        mptArrayUBound :: MPN,
        -- | Element type.
        mptElemTy :: MPType
      }
  deriving (Eq, Ord, Show, Read)

-- | Standard type.
data MPStdType = MPTInteger | MPTReal | MPTString deriving (Eq, Ord, Show, Read)

-- | Subprogram declaration.
data MPSubProgDecl = MPSubProgDecl
  { -- | Name of the subprogram.
    mpspName :: B.ByteString,
    -- | Parameters.
    mpspParams :: [(B.ByteString, MPType)],
    -- | Return type.
    mpspRetTy :: Maybe MPStdType,
    -- | Declarations.
    mpspDecls :: [MPDecl],
    -- | Subprogram declarations.
    mpspSubProgDecls :: [MPSubProgDecl],
    -- | Body of the subprogram.
    mpspBody :: [MPStmt]
  }
  deriving (Eq, Ord, Show, Read)

-- | Statement.
data MPStmt
  = -- | Variable assignment.
    MPSAssign
      { -- | Variable to be assigned.
        mpsAssignVar :: MPVar,
        -- | Expression to be assigned to the variable.
        mpsAssignExpr :: MPExpr
      }
  | -- | Function/procedure call.
    MPSFnCall
      { -- | Name of the called function/procedure.
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
  deriving (Eq, Ord, Show, Read)

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
  | -- | Variable, or function call with no arguments. (They are syntactically
    -- identical.)
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
  | -- | String literal. The value is exactly as written in the source.
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
