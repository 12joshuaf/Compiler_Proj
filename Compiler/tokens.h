/* A Bison parser, made by GNU Bison 3.7.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_TOKENS_H_INCLUDED
# define YY_YY_TOKENS_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    TOKEN_ARRAY = 258,             /* TOKEN_ARRAY  */
    TOKEN_BOOLEAN = 259,           /* TOKEN_BOOLEAN  */
    TOKEN_CHAR = 260,              /* TOKEN_CHAR  */
    TOKEN_ELSE = 261,              /* TOKEN_ELSE  */
    TOKEN_FALSE = 262,             /* TOKEN_FALSE  */
    TOKEN_FOR = 263,               /* TOKEN_FOR  */
    TOKEN_FUNCTION = 264,          /* TOKEN_FUNCTION  */
    TOKEN_IF = 265,                /* TOKEN_IF  */
    TOKEN_INTEGER = 266,           /* TOKEN_INTEGER  */
    TOKEN_PRINT = 267,             /* TOKEN_PRINT  */
    TOKEN_RETURN = 268,            /* TOKEN_RETURN  */
    TOKEN_STRING = 269,            /* TOKEN_STRING  */
    TOKEN_TRUE = 270,              /* TOKEN_TRUE  */
    TOKEN_VOID = 271,              /* TOKEN_VOID  */
    TOKEN_WHILE = 272,             /* TOKEN_WHILE  */
    TOKEN_LITERAL_INT = 273,       /* TOKEN_LITERAL_INT  */
    TOKEN_LITERAL_CHAR = 274,      /* TOKEN_LITERAL_CHAR  */
    TOKEN_LITERAL_STRING = 275,    /* TOKEN_LITERAL_STRING  */
    TOKEN_IDENT = 276,             /* TOKEN_IDENT  */
    TOKEN_POSTFIX_ADD = 277,       /* TOKEN_POSTFIX_ADD  */
    TOKEN_POSTFIX_MINUS = 278,     /* TOKEN_POSTFIX_MINUS  */
    TOKEN_GREATER_EQUAL = 279,     /* TOKEN_GREATER_EQUAL  */
    TOKEN_LESS_EQUAL = 280,        /* TOKEN_LESS_EQUAL  */
    TOKEN_EQUAL = 281,             /* TOKEN_EQUAL  */
    TOKEN_NOT_EQUAL = 282,         /* TOKEN_NOT_EQUAL  */
    TOKEN_NOT = 283,               /* TOKEN_NOT  */
    TOKEN_LOGICAL_AND = 284,       /* TOKEN_LOGICAL_AND  */
    TOKEN_LOGICAL_OR = 285,        /* TOKEN_LOGICAL_OR  */
    TOKEN_LPAREN = 286,            /* TOKEN_LPAREN  */
    TOKEN_RPAREN = 287,            /* TOKEN_RPAREN  */
    TOKEN_LBRACKET = 288,          /* TOKEN_LBRACKET  */
    TOKEN_RBRACKET = 289,          /* TOKEN_RBRACKET  */
    TOKEN_LBRACE = 290,            /* TOKEN_LBRACE  */
    TOKEN_RBRACE = 291,            /* TOKEN_RBRACE  */
    TOKEN_EXPONENTIATION = 292,    /* TOKEN_EXPONENTIATION  */
    TOKEN_MUL = 293,               /* TOKEN_MUL  */
    TOKEN_DIV = 294,               /* TOKEN_DIV  */
    TOKEN_MOD = 295,               /* TOKEN_MOD  */
    TOKEN_PLUS = 296,              /* TOKEN_PLUS  */
    TOKEN_MINUS = 297,             /* TOKEN_MINUS  */
    TOKEN_COMMA = 298,             /* TOKEN_COMMA  */
    TOKEN_GREATER = 299,           /* TOKEN_GREATER  */
    TOKEN_LESS = 300,              /* TOKEN_LESS  */
    TOKEN_ASSIGN = 301,            /* TOKEN_ASSIGN  */
    TOKEN_COLON = 302,             /* TOKEN_COLON  */
    TOKEN_SEMI = 303,              /* TOKEN_SEMI  */
    TOKEN_ERROR = 304,             /* TOKEN_ERROR  */
    NO_ELSE = 305                  /* NO_ELSE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 76 "b-minor-parser.y"

  struct decl *decl;
  struct stmt *stmt;
  struct expr *expr;
  struct type *type;
  struct param_list *param_list;
  char *name;
  int value;

#line 124 "tokens.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_TOKENS_H_INCLUDED  */
