-- |
-- Module      : NCTUMPC.Parser.Types
-- Description : Types and their operations used by the scanner and the parser
-- Copyright   : (c) Po-Yi Tsai, 2021
-- License     : Unlicense
-- Maintainer  : abt8601@protonmail.ch
-- Stability   : experimental
-- Portability : portable
--
-- Types and their operations used by the scanner and the parser.
module NCTUMPC.Parser.Types
  ( -- * Parser Monad
    -- $parserMonad
    P,

    -- ** Parser State
    -- $parserState
    PState (..),
    POpts (..),
    mkPState,

    -- *** AlexInput
    -- $alexInput
    AlexInput (..),
    alexGetByte,
    alexInputPrevChar,
    mkInput,

    -- ** Parser Log
    -- $parserLog
    PLog (..),

    -- * Token
    -- $token
    Token (..),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.RWS (RWS)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)
import Data.Word (Word8)
import NCTUMPC.Loc (Loc)

-- $parserMonad

-- | Parser monad. The monad in which the scanner and the parser resides.
type P = ExceptT () (RWS () [PLog] PState)

-- $parserState

-- | Parser state.
data PState = PState
  { -- | Scanner input.
    psInput :: AlexInput,
    -- | Start code of the scanner.
    psStartCode :: Int,
    -- | Text of the last read token.
    psTokenText :: B.ByteString,
    -- | Options.
    psOpts :: POpts,
    -- | Line number of the last read token.
    psLineNo :: Int,
    -- | First column number of the last read token.
    psFirstColNo :: Int,
    -- | Last column number of the last read token.
    psLastColNo :: Int,
    -- | The content of the current line being read.
    psLineBuffer :: Builder
  }

-- | Parser options.
data POpts = POpts
  { -- | Controls whether or not source code listing is enabled.
    pOptList :: Bool,
    -- | Controls whether or not token logging is enabled.
    pOptToken :: Bool
  }
  deriving (Eq, Ord, Show, Read)

-- | Make a new 'PState'.
mkPState ::
  -- | Initial options.
  POpts ->
  -- | Program source.
  BL.ByteString ->
  PState
mkPState opts bs =
  PState
    { psInput = mkInput bs,
      psStartCode = 0,
      psTokenText = B.empty,
      psOpts = opts,
      psLineNo = 1,
      psFirstColNo = 1,
      psLastColNo = 1,
      psLineBuffer = mempty
    }

-- $alexInput

-- | Input of the Alex-generated scanner.
data AlexInput = AlexInput
  { -- | Bytes of the program source.
    aiBytes :: BL.ByteString,
    -- | Last seen byte of the program source.
    aiPrevByte :: Word8
  }
  deriving (Eq, Ord, Show, Read)

-- | Get a byte.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {aiBytes = bs} =
  case BL.uncons bs of
    Nothing -> Nothing
    Just (b, bs') -> Just (b, AlexInput {aiBytes = bs', aiPrevByte = b})

-- | Return the last seen character.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar AlexInput {aiPrevByte = b} =
  chr $ fromIntegral b -- We assume the program source be encoded in ASCII.

-- | Make a new 'AlexInput'.
mkInput ::
  -- | Program source.
  BL.ByteString ->
  AlexInput
mkInput bs = AlexInput {aiBytes = bs, aiPrevByte = 0x0a}

-- $parserLog

-- | Log message produced by the scanner and the parser.
data PLog
  = -- | A line of the source code listing.
    List
      { -- | The content of the line.
        plSrcLn :: Builder
      }
  | -- | Token log.
    LogToken
      { -- | Location of the token, comment delimiter, or error.
        plLoc :: Loc,
        -- | Token type.
        plTokenType :: String,
        -- | Token text.
        plTokenText :: B.ByteString
      }
  | -- | Line comment log.
    LogLineComment
      { -- | Location of the token, comment delimiter, or error.
        plLoc :: Loc
      }
  | -- | Start of block comment.
    LogBlockCommentStart
      { -- | Location of the token, comment delimiter, or error.
        plLoc :: Loc
      }
  | -- | End of block comment.
    LogBlockCommentEnd
      { -- | Location of the token, comment delimiter, or error.
        plLoc :: Loc
      }
  | -- | Lexical analyser error.
    LogLexError
      { -- | Location of the token, comment delimiter, or error.
        plLoc :: Loc,
        -- | Token text.
        plTokenText :: B.ByteString
      }

-- $token

-- | Token of a MiniPascal source.
data Token
  = -- | End of file.
    TokenEOF
  | -- | Reserved word "@program@".
    TokenProgram
  | -- | Reserved word "@var@".
    TokenVar
  | -- | Reserved word "@array@".
    TokenArray
  | -- | Reserved word "@of@".
    TokenOf
  | -- | Reserved word "@integer@".
    TokenInteger
  | -- | Reserved word "@real@".
    TokenReal
  | -- | Reserved word "@string@".
    TokenString
  | -- | Reserved word "@function@".
    TokenFunction
  | -- | Reserved word "@procedure@".
    TokenProcedure
  | -- | Reserved word "@begin@".
    TokenBegin
  | -- | Reserved word "@end@".
    TokenEnd
  | -- | Reserved word "@if@".
    TokenIf
  | -- | Reserved word "@then@".
    TokenThen
  | -- | Reserved word "@else@".
    TokenElse
  | -- | Reserved word "@while@".
    TokenWhile
  | -- | Reserved word "@do@".
    TokenDo
  | -- | Reserved word "@not@".
    TokenNot
  | -- | Reserved word "@and@".
    TokenAnd
  | -- | Reserved word "@or@".
    TokenOr
  | -- | "@(@".
    TokenLParen
  | -- | "@)@".
    TokenRParen
  | -- | "@;@".
    TokenSemicolon
  | -- | "@.@".
    TokenDot
  | -- | "@,@".
    TokenComma
  | -- | "@:@".
    TokenColon
  | -- | "@[@".
    TokenLBrace
  | -- | "@]@".
    TokenRBrace
  | -- | "@..@".
    TokenDotDot
  | -- | "@:=@".
    TokenAssignment
  | -- | Operator "@+@".
    TokenAddOp
  | -- | Operator "@-@".
    TokenSubOp
  | -- | Operator "@*@".
    TokenMulOp
  | -- | Operator "@/@".
    TokenDivOp
  | -- | Operator "@<@".
    TokenLtOp
  | -- | Operator "@>@".
    TokenGtOp
  | -- | Operator "@=@".
    TokenEqOp
  | -- | Operator "@>=@".
    TokenGetOp
  | -- | Operator "@<=@".
    TokenLetOp
  | -- | Operator "@!=@".
    TokenNeqOp
  | -- | Identifier.
    TokenIdentifier B.ByteString
  | -- | Number with a decimal point.
    TokenRealNumber Double
  | -- | Integer.
    TokenIntegerNum Integer
  | -- | Number in scientific notation.
    TokenScientific Double
  | -- | String literal.
    TokenLiteralStr B.ByteString
  deriving (Eq, Ord, Show, Read)
