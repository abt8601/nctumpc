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

prog  : program id '(' identifier_list ')' ';'
        declarations
        subprogram_declarations
        compound_statement
        '.'
        {}

identifier_list
      : id
        {}
      | identifier_list ',' id
        {}

declarations
      : declarations declaration
        {}
      | {- empty -}
        {}

declaration
      : var identifier_list ':' type ';'
        {}

type  : standard_type
        {}
      | array '[' num '..' num ']' of type
        {}

standard_type
      : integer
        {}
      | real
        {}
      | string
        {}

subprogram_declarations
      : subprogram_declarations subprogram_declaration ';'
        {}
      | {- empty -}
        {}

subprogram_declaration
      : subprogram_head
        declarations
        subprogram_declarations
        compound_statement
        {}

subprogram_head
      : function id arguments ':' standard_type ';'
        {}
      | procedure id arguments ';'
        {}

arguments
      : '(' parameter_list ')'
        {}
      | {- empty -}
        {}

parameter_list
      : parameter_group
        {}
      | parameter_list ';' parameter_group
        {}

parameter_group
      : optional_var identifier_list ':' type
        {}

optional_var
      : var
        {}
      | {- empty -}
        {}

compound_statement
      : begin
        statement_list
        end
        {}

statement_list
      : statement_list ';' statement
        {}
      | statement_list ';'
        {}
      | statement
        {}
      | {- empty -}
        {}

optional_statement
      : statement
        {}
      | {- empty -}
        {}

statement
      : variable ':=' expression
        {}
      | procedure_statement
        {}
      | compound_statement
        {}
      | if expression then optional_statement else optional_statement
        {}
      | while expression do optional_statement
        {}

variable
      : id tail
        {}

tail  : tail '[' expression ']'
        {}
      | {- empty -}
        {}

procedure_statement
      : id
        {}
      | id '(' expression_list ')'
        {}

expression_list
      : expression
        {}
      | expression_list ',' expression
        {}

expression
      : expression and  expression
        {}
      | expression or   expression
        {}
      | expression '<'  expression
        {}
      | expression '>'  expression
        {}
      | expression '='  expression
        {}
      | expression '<=' expression
        {}
      | expression '>=' expression
        {}
      | expression '!=' expression
        {}
      | expression '+'  expression
        {}
      | expression '-'  expression
        {}
      | expression '*'  expression
        {}
      | expression '/'  expression
        {}
      | id tail
        {}
      | id '(' expression_list ')'
        {}
      | num
        {}
      | literalStr
        {}
      | '(' expression ')'
        {}
      | not expression
        {}
      | '-' expression %prec NEG
        {}

num   : integerNum
        {}
      | realNumber
        {}
      | scientific
        {}
