{
-- |
-- Module      : NCTUMPC.Parser
-- Description : MiniPascal parser
-- Copyright   : (c) Po-Yi Tsai, 2021
-- License     : Unlicense
-- Maintainer  : abt8601@protonmail.ch
-- Stability   : experimental
-- Portability : portable
--
-- MiniPascal parser.
module NCTUMPC.Parser (parse) where

import qualified Data.ByteString as B
import NCTUMPC.AST
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
import NCTUMPC.Loc (LocSpan (..))
import NCTUMPC.Parser.Internal.Actions
import NCTUMPC.Parser.Scanner (scanCont)
import NCTUMPC.Parser.Types (P, Token (..))
}

%name parse
%tokentype { (LocSpan, Token) }
%error { parseError }

%monad { P }
%lexer { scanCont } { (_, TokenEOF) }

%token
  program    { (_, TokenProgram)      }
  var        { (_, TokenVar)          }
  array      { (_, TokenArray)        }
  of         { (_, TokenOf)           }
  integer    { (_, TokenInteger)      }
  real       { (_, TokenReal)         }
  string     { (_, TokenString)       }
  function   { (_, TokenFunction)     }
  procedure  { (_, TokenProcedure)    }
  begin      { (_, TokenBegin)        }
  end        { (_, TokenEnd)          }
  if         { (_, TokenIf)           }
  then       { (_, TokenThen)         }
  else       { (_, TokenElse)         }
  while      { (_, TokenWhile)        }
  do         { (_, TokenDo)           }
  not        { (_, TokenNot)          }
  and        { (_, TokenAnd)          }
  or         { (_, TokenOr)           }
  '('        { (_, TokenLParen)       }
  ')'        { (_, TokenRParen)       }
  ';'        { (_, TokenSemicolon)    }
  '.'        { (_, TokenDot)          }
  ','        { (_, TokenComma)        }
  ':'        { (_, TokenColon)        }
  '['        { (_, TokenLBrace)       }
  ']'        { (_, TokenRBrace)       }
  '..'       { (_, TokenDotDot)       }
  ':='       { (_, TokenAssignment)   }
  '+'        { (_, TokenAddOp)        }
  '-'        { (_, TokenSubOp)        }
  '*'        { (_, TokenMulOp)        }
  '/'        { (_, TokenDivOp)        }
  '<'        { (_, TokenLtOp)         }
  '>'        { (_, TokenGtOp)         }
  '='        { (_, TokenEqOp)         }
  '>='       { (_, TokenGetOp)        }
  '<='       { (_, TokenLetOp)        }
  '!='       { (_, TokenNeqOp)        }
  id         { (_, TokenIdentifier _) }
  realNumber { (_, TokenRealNumber _) }
  integerNum { (_, TokenIntegerNum _) }
  scientific { (_, TokenScientific _) }
  literalStr { (_, TokenLiteralStr _) }

%nonassoc and or
%nonassoc '<' '>' '=' '<=' '>=' '!='
%left '+' '-'
%left '*' '/'
%left not NEG

%%

prog  :: { MPProg }
      : program id '(' identifier_list ')' ';'
        declarations
        subprogram_declarations
        compound_statement
        '.'
        { let (_, TokenIdentifier name) = $2
           in MPProg
                { mppName = name,
                  mppExtFds = reverse $4,
                  mppDecls = reverse $7,
                  mppSubProgDecls = reverse $8,
                  mppBody = $9
                } }

identifier_list
      :: { [B.ByteString] }
      : id
        { let (_, TokenIdentifier name) = $1 in [name] }
      | identifier_list ',' id
        { let (_, TokenIdentifier name) = $3 in name : $1 }

declarations
      :: { [MPDecl] }
      : declarations declaration
        { $1 ++ $2 }
      | {- empty -}
        { [] }

declaration
      :: { [MPDecl] }
      : var identifier_list ':' type ';'
        { map (\x -> (x, $4)) (reverse $2) }

type  :: { MPType }
      : standard_type
        { MPTStdType $1 }
      | array '[' num '..' num ']' of type
        { MPTArray
            { mptArrayLBound = $3,
              mptArrayUBound = $5,
              mptElemTy = $8
            } }

standard_type
      :: { MPStdType }
      : integer
        { MPTInteger }
      | real
        { MPTReal }
      | string
        { MPTString }

subprogram_declarations
      :: { [MPSubProgDecl] }
      : subprogram_declarations subprogram_declaration ';'
        { $2 : $1 }
      | {- empty -}
        { [] }

subprogram_declaration
      :: { MPSubProgDecl }
      : subprogram_head
        declarations
        subprogram_declarations
        compound_statement
        { let (name, params, retty) = $1
           in MPSubProgDecl
                { mpspName = name,
                  mpspParams = params,
                  mpspRetTy = retty,
                  mpspDecls = reverse $2,
                  mpspSubProgDecls = reverse $3,
                  mpspBody = $4
                } }

subprogram_head
      :: { (B.ByteString, [(B.ByteString, MPType)], Maybe MPStdType) }
      : function id arguments ':' standard_type ';'
        { let (_, TokenIdentifier name) = $2 in (name, $3, Just $5) }
      | procedure id arguments ';'
        { let (_, TokenIdentifier name) = $2 in (name, $3, Nothing) }

arguments
      :: { [(B.ByteString, MPType)] }
      : '(' parameter_list ')'
        { $2 }
      | {- empty -}
        { [] }

parameter_list
      :: { [(B.ByteString, MPType)] }
      : parameter_group
        { $1 }
      | parameter_list ';' parameter_group
        { $1 ++ $3 }

parameter_group
      :: { [(B.ByteString, MPType)] }
      : optional_var identifier_list ':' type
        { map (\x -> (x, $4)) (reverse $2) }

optional_var
      :: { () }
      : var
        {}
      | {- empty -}
        {}

compound_statement
      :: { [MPStmt] }
      : begin
        statement_list
        end
        { reverse $2 }

statement_list
      :: { [MPStmt] }
      : statement_list ';' statement
        { $3 : $1 }
      | statement_list ';'
        { $1 }
      | statement
        { [$1] }
      | {- empty -}
        { [] }

optional_statement
      :: { Maybe MPStmt }
      : statement
        { Just $1 }
      | {- empty -}
        { Nothing }

statement
      :: { MPStmt }
      : variable ':=' expression
        { MPSAssign {mpsAssignVar = $1, mpsAssignExpr = $3} }
      | procedure_statement
        { $1 }
      | compound_statement
        { MPSCompound $1 }
      | if expression then optional_statement else optional_statement
        { MPSIf {mpsCond = $2, mpsStmtThen = $4, mpsStmtElse = $6} }
      | while expression do optional_statement
        { MPSWhile {mpsCond = $2, mpsStmtDo = $4} }

variable
      :: { MPVar }
      : id tail
        { let (_, TokenIdentifier name) = $1
           in MPVar {mpvName = name, mpvSubscrs = reverse $2}
        }

tail  :: { [MPExpr] }
      : tail '[' expression ']'
        { $3 : $1 }
      | {- empty -}
        { [] }

procedure_statement
      :: { MPStmt }
      : id
        { let (_, TokenIdentifier name) = $1
           in MPSFnCall {mpsFnName = name, mpsArgs = []}
        }
      | id '(' expression_list ')'
        { let (_, TokenIdentifier name) = $1
           in MPSFnCall {mpsFnName = name, mpsArgs = reverse $3}
        }

expression_list
      :: { [MPExpr] }
      : expression
        { [$1] }
      | expression_list ',' expression
        { $3 : $1 }

expression
      :: { MPExpr }
      : expression and  expression
        { MPEBinOp $1 MPOAnd $3 }
      | expression or   expression
        { MPEBinOp $1 MPOOr  $3 }
      | expression '<'  expression
        { MPEBinOp $1 MPOLt  $3 }
      | expression '>'  expression
        { MPEBinOp $1 MPOGt  $3 }
      | expression '='  expression
        { MPEBinOp $1 MPOEq  $3 }
      | expression '<=' expression
        { MPEBinOp $1 MPOLet $3 }
      | expression '>=' expression
        { MPEBinOp $1 MPOGet $3 }
      | expression '!=' expression
        { MPEBinOp $1 MPONeq $3 }
      | expression '+'  expression
        { MPEBinOp $1 MPOAdd $3 }
      | expression '-'  expression
        { MPEBinOp $1 MPOSub $3 }
      | expression '*'  expression
        { MPEBinOp $1 MPOMul $3 }
      | expression '/'  expression
        { MPEBinOp $1 MPODiv $3 }
      | id tail
        { let (_, TokenIdentifier name) = $1
           in MPEVar $ MPVar {mpvName = name, mpvSubscrs = reverse $2}
        }
      | id '(' expression_list ')'
        { let (_, TokenIdentifier name) = $1
           in MPEFnCall {mpeFnName = name, mpeArgs = reverse $3}
        }
      | num
        { MPEN $1 }
      | literalStr
        { let (_, TokenLiteralStr value) = $1 in MPELitStr value }
      | '(' expression ')'
        { $2 }
      | not expression
        { MPEUnOp MPONot $2 }
      | '-' expression %prec NEG
        { MPEUnOp MPONeg $2 }

num   :: { MPN }
      : integerNum
        { let (_, TokenIntegerNum n) = $1 in MPNInteger n }
      | realNumber
        { let (_, TokenRealNumber n) = $1 in MPNReal n }
      | scientific
        { let (_, TokenScientific n) = $1 in MPNReal n }
