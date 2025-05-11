/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
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
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "b-minor-parser.y"

#include "ast_library.h"
#include <stdlib.h>
#include <string.h>


/* The result of parsing (root of tree)
    is a declaration node */
struct decl * parser_result = 0;

/* Error reporting function */
extern void yyerror(const char *);

/* Lexing function and lexeme from
    Flex-generated scanner */
extern int yylex();
extern char *yytext;

#line 90 "b-minor-parser-ast.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "tokens.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_TOKEN_ARRAY = 3,                /* TOKEN_ARRAY  */
  YYSYMBOL_TOKEN_BOOLEAN = 4,              /* TOKEN_BOOLEAN  */
  YYSYMBOL_TOKEN_CHAR = 5,                 /* TOKEN_CHAR  */
  YYSYMBOL_TOKEN_ELSE = 6,                 /* TOKEN_ELSE  */
  YYSYMBOL_TOKEN_FALSE = 7,                /* TOKEN_FALSE  */
  YYSYMBOL_TOKEN_FOR = 8,                  /* TOKEN_FOR  */
  YYSYMBOL_TOKEN_FUNCTION = 9,             /* TOKEN_FUNCTION  */
  YYSYMBOL_TOKEN_IF = 10,                  /* TOKEN_IF  */
  YYSYMBOL_TOKEN_INTEGER = 11,             /* TOKEN_INTEGER  */
  YYSYMBOL_TOKEN_PRINT = 12,               /* TOKEN_PRINT  */
  YYSYMBOL_TOKEN_RETURN = 13,              /* TOKEN_RETURN  */
  YYSYMBOL_TOKEN_STRING = 14,              /* TOKEN_STRING  */
  YYSYMBOL_TOKEN_TRUE = 15,                /* TOKEN_TRUE  */
  YYSYMBOL_TOKEN_VOID = 16,                /* TOKEN_VOID  */
  YYSYMBOL_TOKEN_WHILE = 17,               /* TOKEN_WHILE  */
  YYSYMBOL_TOKEN_LITERAL_INT = 18,         /* TOKEN_LITERAL_INT  */
  YYSYMBOL_TOKEN_LITERAL_CHAR = 19,        /* TOKEN_LITERAL_CHAR  */
  YYSYMBOL_TOKEN_LITERAL_STRING = 20,      /* TOKEN_LITERAL_STRING  */
  YYSYMBOL_TOKEN_IDENT = 21,               /* TOKEN_IDENT  */
  YYSYMBOL_TOKEN_POSTFIX_ADD = 22,         /* TOKEN_POSTFIX_ADD  */
  YYSYMBOL_TOKEN_POSTFIX_MINUS = 23,       /* TOKEN_POSTFIX_MINUS  */
  YYSYMBOL_TOKEN_GREATER_EQUAL = 24,       /* TOKEN_GREATER_EQUAL  */
  YYSYMBOL_TOKEN_LESS_EQUAL = 25,          /* TOKEN_LESS_EQUAL  */
  YYSYMBOL_TOKEN_EQUAL = 26,               /* TOKEN_EQUAL  */
  YYSYMBOL_TOKEN_NOT_EQUAL = 27,           /* TOKEN_NOT_EQUAL  */
  YYSYMBOL_TOKEN_NOT = 28,                 /* TOKEN_NOT  */
  YYSYMBOL_TOKEN_LOGICAL_AND = 29,         /* TOKEN_LOGICAL_AND  */
  YYSYMBOL_TOKEN_LOGICAL_OR = 30,          /* TOKEN_LOGICAL_OR  */
  YYSYMBOL_TOKEN_LPAREN = 31,              /* TOKEN_LPAREN  */
  YYSYMBOL_TOKEN_RPAREN = 32,              /* TOKEN_RPAREN  */
  YYSYMBOL_TOKEN_LBRACKET = 33,            /* TOKEN_LBRACKET  */
  YYSYMBOL_TOKEN_RBRACKET = 34,            /* TOKEN_RBRACKET  */
  YYSYMBOL_TOKEN_LBRACE = 35,              /* TOKEN_LBRACE  */
  YYSYMBOL_TOKEN_RBRACE = 36,              /* TOKEN_RBRACE  */
  YYSYMBOL_TOKEN_EXPONENTIATION = 37,      /* TOKEN_EXPONENTIATION  */
  YYSYMBOL_TOKEN_MUL = 38,                 /* TOKEN_MUL  */
  YYSYMBOL_TOKEN_DIV = 39,                 /* TOKEN_DIV  */
  YYSYMBOL_TOKEN_MOD = 40,                 /* TOKEN_MOD  */
  YYSYMBOL_TOKEN_PLUS = 41,                /* TOKEN_PLUS  */
  YYSYMBOL_TOKEN_MINUS = 42,               /* TOKEN_MINUS  */
  YYSYMBOL_TOKEN_COMMA = 43,               /* TOKEN_COMMA  */
  YYSYMBOL_TOKEN_GREATER = 44,             /* TOKEN_GREATER  */
  YYSYMBOL_TOKEN_LESS = 45,                /* TOKEN_LESS  */
  YYSYMBOL_TOKEN_ASSIGN = 46,              /* TOKEN_ASSIGN  */
  YYSYMBOL_TOKEN_COLON = 47,               /* TOKEN_COLON  */
  YYSYMBOL_TOKEN_SEMI = 48,                /* TOKEN_SEMI  */
  YYSYMBOL_TOKEN_ERROR = 49,               /* TOKEN_ERROR  */
  YYSYMBOL_NO_ELSE = 50,                   /* NO_ELSE  */
  YYSYMBOL_YYACCEPT = 51,                  /* $accept  */
  YYSYMBOL_program = 52,                   /* program  */
  YYSYMBOL_globals = 53,                   /* globals  */
  YYSYMBOL_global = 54,                    /* global  */
  YYSYMBOL_var_decl = 55,                  /* var_decl  */
  YYSYMBOL_length = 56,                    /* length  */
  YYSYMBOL_array_init = 57,                /* array_init  */
  YYSYMBOL_val_list = 58,                  /* val_list  */
  YYSYMBOL_function_decl = 59,             /* function_decl  */
  YYSYMBOL_function_def = 60,              /* function_def  */
  YYSYMBOL_params = 61,                    /* params  */
  YYSYMBOL_param_list = 62,                /* param_list  */
  YYSYMBOL_param = 63,                     /* param  */
  YYSYMBOL_stmts = 64,                     /* stmts  */
  YYSYMBOL_stmt = 65,                      /* stmt  */
  YYSYMBOL_stmt_block = 66,                /* stmt_block  */
  YYSYMBOL_function_call = 67,             /* function_call  */
  YYSYMBOL_args = 68,                      /* args  */
  YYSYMBOL_arg_list = 69,                  /* arg_list  */
  YYSYMBOL_arg = 70,                       /* arg  */
  YYSYMBOL_assignment = 71,                /* assignment  */
  YYSYMBOL_identifier = 72,                /* identifier  */
  YYSYMBOL_expr = 73,                      /* expr  */
  YYSYMBOL_lo_expr = 74,                   /* lo_expr  */
  YYSYMBOL_pm_expr = 75,                   /* pm_expr  */
  YYSYMBOL_term = 76,                      /* term  */
  YYSYMBOL_expon = 77,                     /* expon  */
  YYSYMBOL_factor = 78,                    /* factor  */
  YYSYMBOL_not = 79,                       /* not  */
  YYSYMBOL_array_element = 80,             /* array_element  */
  YYSYMBOL_if_stmt = 81,                   /* if_stmt  */
  YYSYMBOL_for_stmt = 82,                  /* for_stmt  */
  YYSYMBOL_return_stmt = 83,               /* return_stmt  */
  YYSYMBOL_print_stmt = 84,                /* print_stmt  */
  YYSYMBOL_postfix = 85,                   /* postfix  */
  YYSYMBOL_opt_assignment = 86,            /* opt_assignment  */
  YYSYMBOL_opt_expr = 87,                  /* opt_expr  */
  YYSYMBOL_opt_increment = 88,             /* opt_increment  */
  YYSYMBOL_literal = 89,                   /* literal  */
  YYSYMBOL_atomic_type = 90,               /* atomic_type  */
  YYSYMBOL_return_type = 91,               /* return_type  */
  YYSYMBOL_relational_op = 92,             /* relational_op  */
  YYSYMBOL_logical_op = 93                 /* logical_op  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  8
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   220

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  51
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  43
/* YYNRULES -- Number of rules.  */
#define YYNRULES  101
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  170

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   305


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    97,    97,   101,   102,   106,   107,   111,   112,   113,
     114,   118,   122,   126,   127,   131,   132,   136,   140,   141,
     145,   146,   150,   151,   155,   156,   160,   161,   162,   163,
     164,   165,   166,   167,   168,   172,   173,   177,   181,   182,
     186,   187,   191,   195,   196,   200,   204,   205,   209,   210,
     214,   215,   216,   220,   221,   222,   223,   227,   228,   232,
     233,   234,   235,   236,   237,   238,   239,   243,   247,   252,
     253,   257,   261,   265,   269,   270,   274,   275,   279,   280,
     284,   285,   286,   290,   291,   292,   293,   294,   298,   299,
     300,   301,   305,   306,   310,   311,   312,   313,   314,   315,
     319,   320
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "TOKEN_ARRAY",
  "TOKEN_BOOLEAN", "TOKEN_CHAR", "TOKEN_ELSE", "TOKEN_FALSE", "TOKEN_FOR",
  "TOKEN_FUNCTION", "TOKEN_IF", "TOKEN_INTEGER", "TOKEN_PRINT",
  "TOKEN_RETURN", "TOKEN_STRING", "TOKEN_TRUE", "TOKEN_VOID",
  "TOKEN_WHILE", "TOKEN_LITERAL_INT", "TOKEN_LITERAL_CHAR",
  "TOKEN_LITERAL_STRING", "TOKEN_IDENT", "TOKEN_POSTFIX_ADD",
  "TOKEN_POSTFIX_MINUS", "TOKEN_GREATER_EQUAL", "TOKEN_LESS_EQUAL",
  "TOKEN_EQUAL", "TOKEN_NOT_EQUAL", "TOKEN_NOT", "TOKEN_LOGICAL_AND",
  "TOKEN_LOGICAL_OR", "TOKEN_LPAREN", "TOKEN_RPAREN", "TOKEN_LBRACKET",
  "TOKEN_RBRACKET", "TOKEN_LBRACE", "TOKEN_RBRACE", "TOKEN_EXPONENTIATION",
  "TOKEN_MUL", "TOKEN_DIV", "TOKEN_MOD", "TOKEN_PLUS", "TOKEN_MINUS",
  "TOKEN_COMMA", "TOKEN_GREATER", "TOKEN_LESS", "TOKEN_ASSIGN",
  "TOKEN_COLON", "TOKEN_SEMI", "TOKEN_ERROR", "NO_ELSE", "$accept",
  "program", "globals", "global", "var_decl", "length", "array_init",
  "val_list", "function_decl", "function_def", "params", "param_list",
  "param", "stmts", "stmt", "stmt_block", "function_call", "args",
  "arg_list", "arg", "assignment", "identifier", "expr", "lo_expr",
  "pm_expr", "term", "expon", "factor", "not", "array_element", "if_stmt",
  "for_stmt", "return_stmt", "print_stmt", "postfix", "opt_assignment",
  "opt_expr", "opt_increment", "literal", "atomic_type", "return_type",
  "relational_op", "logical_op", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-149)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      12,  -149,    87,  -149,    12,    34,  -149,    56,  -149,  -149,
    -149,   160,    73,  -149,  -149,    94,  -149,  -149,    63,    97,
    -149,  -149,    86,    65,  -149,    90,    12,  -149,  -149,  -149,
    -149,  -149,    65,    65,    65,  -149,   144,    -6,     3,   114,
      36,    84,  -149,  -149,  -149,  -149,  -149,   168,    95,  -149,
      92,   100,    -6,   -14,    -6,  -149,  -149,    65,    65,  -149,
    -149,    65,  -149,  -149,  -149,  -149,  -149,  -149,    65,    65,
      65,    65,    65,    65,    65,    83,    42,    12,   109,  -149,
     120,  -149,   107,    -6,   151,     3,   114,    36,    36,    84,
      84,    84,  -149,   135,   141,  -149,  -149,   145,  -149,  -149,
      65,  -149,    82,  -149,   118,  -149,  -149,   149,  -149,   148,
     143,   156,   157,    65,    65,  -149,   142,   153,    31,  -149,
     146,   147,    48,   -23,   150,  -149,  -149,   152,   154,   168,
    -149,    82,    12,    65,  -149,    -6,  -149,  -149,  -149,  -149,
    -149,    65,   137,  -149,    65,  -149,  -149,  -149,  -149,  -149,
       7,   150,   155,    25,    -6,    -6,    65,    31,    -6,   158,
     185,    12,    31,  -149,    -1,  -149,   161,  -149,    31,  -149
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       4,    45,     0,     2,     4,     0,     6,     0,     1,     3,
       5,     0,     0,    88,    90,     0,    89,    91,     7,     0,
      93,    92,     0,     0,    11,     0,    19,    84,    83,    85,
      86,    87,     0,     0,     0,    66,    62,     8,    47,    49,
      52,    56,    58,    64,    65,    61,    63,     0,     0,    18,
      20,     0,    67,     0,    59,    74,    75,    39,     0,   100,
     101,     0,    97,    99,    94,    95,    96,    98,     0,     0,
       0,     0,     0,     0,     0,     9,     0,     0,     0,    60,
       0,    38,    40,    42,     0,    46,    48,    50,    51,    53,
      54,    55,    57,     0,     0,    15,    21,     0,    22,    37,
       0,    68,     0,    10,     0,    16,    17,     0,    41,     0,
      13,     0,     0,    39,     0,    36,     0,     0,    24,    33,
      66,     0,    62,     0,    65,    31,    32,     0,     0,     0,
      12,     0,    77,     0,    73,    72,    27,    35,    25,    26,
      28,     0,     0,    30,     0,    34,    29,    23,    14,    76,
       0,     0,     0,     0,    43,    44,    78,     0,    79,     0,
      69,    82,     0,    80,     0,    81,     0,    70,     0,    71
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -149,  -149,   188,  -149,    64,  -149,  -149,    66,  -149,  -149,
    -149,   121,  -149,    81,  -148,   110,   -99,    88,   105,  -149,
    -124,     0,   -22,   159,   139,    89,    72,   134,  -149,  -101,
    -149,  -149,  -149,  -149,    49,  -149,  -149,  -149,   -89,   -13,
    -149,  -149,  -149
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     2,     3,     4,   116,    25,   103,   109,     6,   105,
      48,    49,    50,   117,   118,   119,    35,    80,    81,    82,
     121,    36,   123,    38,    39,    40,    41,    42,    43,    44,
     125,   126,   127,   128,    45,   152,   159,   166,    46,    18,
      22,    68,    61
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
       7,    37,    21,   124,     7,   120,    59,    60,   149,   160,
      52,    53,    54,   110,   167,    59,    60,   124,    79,   120,
     169,    55,    56,    59,    60,   143,    51,    62,    63,    64,
      65,   151,    58,     1,    75,    83,    84,   163,    27,   111,
      58,   112,   110,   113,   114,   141,    28,    66,    67,    29,
      30,    31,     1,   141,    59,    60,   124,   157,   120,    32,
     151,   124,    33,   120,     5,    98,   104,   124,     5,   120,
      55,    56,    27,    34,    71,    72,    73,    51,    83,    57,
      28,    58,    10,    29,    30,    31,     1,     8,    94,    27,
      95,    83,   135,    32,   141,   142,    33,    28,    13,    14,
      29,    30,    31,    11,   122,    16,    19,    34,    17,    23,
      20,   153,    97,    13,    14,    24,   147,    26,   122,   154,
      16,    74,   155,    17,    47,    27,   111,    76,   112,    93,
     113,   114,   150,    28,   158,    77,    29,    30,    31,     1,
      12,    13,    14,    89,    90,    91,    32,    78,    16,    33,
     100,    17,    99,   104,   115,    69,    70,   122,    87,    88,
      34,   164,   122,    12,    13,    14,    55,    56,   122,    15,
     102,    16,    13,    14,    17,    57,   104,    58,   107,    16,
      59,    60,    17,   129,   130,   101,   131,   132,   133,   137,
     136,   162,     9,   168,   139,   140,   144,   148,    96,   138,
     145,   134,   146,   156,   106,   108,   161,    86,    92,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85
};

static const yytype_int16 yycheck[] =
{
       0,    23,    15,   104,     4,   104,    29,    30,   132,   157,
      32,    33,    34,   102,   162,    29,    30,   118,    32,   118,
     168,    22,    23,    29,    30,    48,    26,    24,    25,    26,
      27,   132,    33,    21,    47,    57,    58,   161,     7,     8,
      33,    10,   131,    12,    13,    46,    15,    44,    45,    18,
      19,    20,    21,    46,    29,    30,   157,    32,   157,    28,
     161,   162,    31,   162,     0,    78,    35,   168,     4,   168,
      22,    23,     7,    42,    38,    39,    40,    77,   100,    31,
      15,    33,    48,    18,    19,    20,    21,     0,    46,     7,
      48,   113,   114,    28,    46,    47,    31,    15,     4,     5,
      18,    19,    20,    47,   104,    11,    33,    42,    14,    46,
      16,   133,     3,     4,     5,    18,   129,    31,   118,   141,
      11,    37,   144,    14,    34,     7,     8,    32,    10,    46,
      12,    13,   132,    15,   156,    43,    18,    19,    20,    21,
       3,     4,     5,    71,    72,    73,    28,    47,    11,    31,
      43,    14,    32,    35,    36,    41,    42,   157,    69,    70,
      42,   161,   162,     3,     4,     5,    22,    23,   168,     9,
      35,    11,     4,     5,    14,    31,    35,    33,    33,    11,
      29,    30,    14,    34,    36,    34,    43,    31,    31,    36,
      48,     6,     4,    32,    48,    48,    46,   131,    77,   118,
      48,   113,    48,    48,    94,   100,    48,    68,    74,    -1,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      61
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    21,    52,    53,    54,    55,    59,    72,     0,    53,
      48,    47,     3,     4,     5,     9,    11,    14,    90,    33,
      16,    90,    91,    46,    18,    56,    31,     7,    15,    18,
      19,    20,    28,    31,    42,    67,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    85,    89,    34,    61,    62,
      63,    72,    73,    73,    73,    22,    23,    31,    33,    29,
      30,    93,    24,    25,    26,    27,    44,    45,    92,    41,
      42,    38,    39,    40,    37,    90,    32,    43,    47,    32,
      68,    69,    70,    73,    73,    74,    75,    76,    76,    77,
      77,    77,    78,    46,    46,    48,    62,     3,    90,    32,
      43,    34,    35,    57,    35,    60,    66,    33,    69,    58,
      89,     8,    10,    12,    13,    36,    55,    64,    65,    66,
      67,    71,    72,    73,    80,    81,    82,    83,    84,    34,
      36,    43,    31,    31,    68,    73,    48,    36,    64,    48,
      48,    46,    47,    48,    46,    48,    48,    90,    58,    71,
      72,    80,    86,    73,    73,    73,    48,    32,    73,    87,
      65,    48,     6,    71,    72,    85,    88,    65,    32,    65
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    51,    52,    53,    53,    54,    54,    55,    55,    55,
      55,    56,    57,    58,    58,    59,    59,    60,    61,    61,
      62,    62,    63,    63,    64,    64,    65,    65,    65,    65,
      65,    65,    65,    65,    65,    66,    66,    67,    68,    68,
      69,    69,    70,    71,    71,    72,    73,    73,    74,    74,
      75,    75,    75,    76,    76,    76,    76,    77,    77,    78,
      78,    78,    78,    78,    78,    78,    78,    79,    80,    81,
      81,    82,    83,    84,    85,    85,    86,    86,    87,    87,
      88,    88,    88,    89,    89,    89,    89,    89,    90,    90,
      90,    90,    91,    91,    92,    92,    92,    92,    92,    92,
      93,    93
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     2,     0,     2,     1,     3,     5,     7,
       9,     1,     3,     1,     3,     8,     9,     1,     1,     0,
       1,     3,     3,     6,     1,     2,     2,     2,     2,     2,
       2,     1,     1,     1,     2,     3,     2,     4,     1,     0,
       1,     3,     1,     3,     3,     1,     3,     1,     3,     1,
       3,     3,     1,     3,     3,     3,     1,     3,     1,     2,
       3,     1,     1,     1,     1,     1,     1,     2,     4,     5,
       7,     9,     2,     2,     2,     2,     1,     0,     0,     1,
       1,     1,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: globals  */
#line 97 "b-minor-parser.y"
                          { parser_result = (yyvsp[0].decl); printf(">=================>program\n"); }
#line 1310 "b-minor-parser-ast.c"
    break;

  case 3: /* globals: global globals  */
#line 101 "b-minor-parser.y"
                           { (yyval.decl) = (yyvsp[-1].decl); (yyvsp[-1].decl)->next = (yyvsp[0].decl); printf(">=================>global globals\n");}
#line 1316 "b-minor-parser-ast.c"
    break;

  case 4: /* globals: %empty  */
#line 102 "b-minor-parser.y"
                          { (yyval.decl) = 0; printf(">=================>globals: epsilon\n"); }
#line 1322 "b-minor-parser-ast.c"
    break;

  case 5: /* global: var_decl TOKEN_SEMI  */
#line 106 "b-minor-parser.y"
                                { (yyval.decl) = (yyvsp[-1].decl); }
#line 1328 "b-minor-parser-ast.c"
    break;

  case 6: /* global: function_decl  */
#line 107 "b-minor-parser.y"
                          { (yyval.decl) = (yyvsp[0].decl); printf(">=================>global: function decl\n"); }
#line 1334 "b-minor-parser-ast.c"
    break;

  case 7: /* var_decl: identifier TOKEN_COLON atomic_type  */
#line 111 "b-minor-parser.y"
                                                      { (yyval.decl) = decl_create((yyvsp[-2].name), (yyvsp[0].type), 0, 0, 0); printf(">=================>var_decl: declaration only %s\n", (yyvsp[-2].name)); }
#line 1340 "b-minor-parser-ast.c"
    break;

  case 8: /* var_decl: identifier TOKEN_COLON atomic_type TOKEN_ASSIGN expr  */
#line 112 "b-minor-parser.y"
                                                                  { (yyval.decl) = decl_create((yyvsp[-4].name), (yyvsp[-2].type), (yyvsp[0].expr), 0, 0); printf(">=================>var_decl: declare and initialize %s\n", (yyvsp[-4].name)); }
#line 1346 "b-minor-parser-ast.c"
    break;

  case 9: /* var_decl: identifier TOKEN_COLON TOKEN_ARRAY TOKEN_LBRACKET length TOKEN_RBRACKET atomic_type  */
#line 113 "b-minor-parser.y"
                                                                                                 { (yyval.decl) = decl_create((yyvsp[-6].name), type_create(TYPE_ARRAY, (yyvsp[0].type), 0), expr_create_integer_literal((yyvsp[-2].value)), 0, 0); printf(">=================>var_decl: declare array %s [%d]\n", (yyvsp[-6].name), (yyvsp[-2].value)); }
#line 1352 "b-minor-parser-ast.c"
    break;

  case 10: /* var_decl: identifier TOKEN_COLON TOKEN_ARRAY TOKEN_LBRACKET length TOKEN_RBRACKET atomic_type TOKEN_ASSIGN array_init  */
#line 114 "b-minor-parser.y"
                                                                                                                         { (yyval.decl) = decl_create((yyvsp[-8].name), type_create(TYPE_ARRAY, (yyvsp[-2].type), 0), expr_create_integer_literal((yyvsp[-4].value)), 0, 0); (yyval.decl)->value->right=(yyvsp[0].expr); printf(">=================>var_decl: declare and initialize array %s [%d] \n", (yyvsp[-8].name), (yyvsp[-4].value)); }
#line 1358 "b-minor-parser-ast.c"
    break;

  case 11: /* length: TOKEN_LITERAL_INT  */
#line 118 "b-minor-parser.y"
                           { (yyval.value) = atoi(yytext); }
#line 1364 "b-minor-parser-ast.c"
    break;

  case 12: /* array_init: TOKEN_LBRACE val_list TOKEN_RBRACE  */
#line 122 "b-minor-parser.y"
                                                { (yyval.expr) = (yyvsp[-1].expr); printf(">=================>array inititialization\n"); }
#line 1370 "b-minor-parser-ast.c"
    break;

  case 13: /* val_list: literal  */
#line 126 "b-minor-parser.y"
                   { (yyval.expr) = (yyvsp[0].expr); printf(">=================>value for list\n"); }
#line 1376 "b-minor-parser-ast.c"
    break;

  case 14: /* val_list: literal TOKEN_COMMA val_list  */
#line 127 "b-minor-parser.y"
                                     { (yyval.expr) = (yyvsp[-2].expr); (yyvsp[-2].expr)->right=(yyvsp[0].expr); printf(">=================>value list addition\n"); }
#line 1382 "b-minor-parser-ast.c"
    break;

  case 15: /* function_decl: identifier TOKEN_COLON TOKEN_FUNCTION return_type TOKEN_LPAREN params TOKEN_RPAREN TOKEN_SEMI  */
#line 131 "b-minor-parser.y"
                                                                                                              { (yyval.decl) = decl_create((yyvsp[-7].name), type_create(TYPE_FUNCTION, (yyvsp[-4].type), (yyvsp[-2].param_list)), 0, 0, 0); printf(">=================>function decl\n"); }
#line 1388 "b-minor-parser-ast.c"
    break;

  case 16: /* function_decl: identifier TOKEN_COLON TOKEN_FUNCTION return_type TOKEN_LPAREN params TOKEN_RPAREN TOKEN_ASSIGN function_def  */
#line 132 "b-minor-parser.y"
                                                                                                                             { (yyval.decl) = decl_create((yyvsp[-8].name), type_create(TYPE_FUNCTION, (yyvsp[-5].type), (yyvsp[-3].param_list)), 0, (yyvsp[0].stmt), 0); printf(">=================>function decl and def\n"); }
#line 1394 "b-minor-parser-ast.c"
    break;

  case 17: /* function_def: stmt_block  */
#line 136 "b-minor-parser.y"
                           { (yyval.stmt) = (yyvsp[0].stmt); printf(">=================>function def\n"); }
#line 1400 "b-minor-parser-ast.c"
    break;

  case 18: /* params: param_list  */
#line 140 "b-minor-parser.y"
                      { (yyval.param_list) = (yyvsp[0].param_list); printf(">=================>params\n"); }
#line 1406 "b-minor-parser-ast.c"
    break;

  case 19: /* params: %empty  */
#line 141 "b-minor-parser.y"
                        { (yyval.param_list) = 0; printf(">=================>empty params\n"); }
#line 1412 "b-minor-parser-ast.c"
    break;

  case 20: /* param_list: param  */
#line 145 "b-minor-parser.y"
                    { (yyval.param_list) = (yyvsp[0].param_list); printf(">=================>param_list: param\n"); }
#line 1418 "b-minor-parser-ast.c"
    break;

  case 21: /* param_list: param TOKEN_COMMA param_list  */
#line 146 "b-minor-parser.y"
                                           { (yyval.param_list) = (yyvsp[-2].param_list), (yyvsp[-2].param_list)->next = (yyvsp[0].param_list); printf(">=================>param_list: multiple params\n"); }
#line 1424 "b-minor-parser-ast.c"
    break;

  case 22: /* param: identifier TOKEN_COLON atomic_type  */
#line 150 "b-minor-parser.y"
                                           { (yyval.param_list) = param_list_create((yyvsp[-2].name), (yyvsp[0].type), 0); printf(">=================>param %s\n", (yyvsp[-2].name)); }
#line 1430 "b-minor-parser-ast.c"
    break;

  case 23: /* param: identifier TOKEN_COLON TOKEN_ARRAY TOKEN_LBRACKET TOKEN_RBRACKET atomic_type  */
#line 151 "b-minor-parser.y"
                                                                                      { (yyval.param_list) = param_list_create((yyvsp[-5].name), type_create(TYPE_ARRAY, (yyvsp[0].type), 0), 0); printf(">=================>param (array): %s\n", (yyvsp[-5].name)); }
#line 1436 "b-minor-parser-ast.c"
    break;

  case 24: /* stmts: stmt  */
#line 155 "b-minor-parser.y"
             { (yyval.stmt) = (yyvsp[0].stmt); printf(">=================>stmt in stmts\n"); }
#line 1442 "b-minor-parser-ast.c"
    break;

  case 25: /* stmts: stmt stmts  */
#line 156 "b-minor-parser.y"
                   { (yyval.stmt) = (yyvsp[-1].stmt); (yyvsp[-1].stmt)->next = (yyvsp[0].stmt); printf(">=================>multiple stmts\n"); }
#line 1448 "b-minor-parser-ast.c"
    break;

  case 26: /* stmt: function_call TOKEN_SEMI  */
#line 160 "b-minor-parser.y"
                                { (yyval.stmt) = stmt_create(STMT_EXPR, 0, 0, (yyvsp[-1].expr), 0, 0, 0, 0); printf(">=================>stmt: function call\n"); }
#line 1454 "b-minor-parser-ast.c"
    break;

  case 27: /* stmt: var_decl TOKEN_SEMI  */
#line 161 "b-minor-parser.y"
                          { (yyval.stmt) = stmt_create(STMT_DECL, (yyvsp[-1].decl), 0, 0, 0, 0, 0, 0); printf(">=================>stmt: variable declaration\n"); }
#line 1460 "b-minor-parser-ast.c"
    break;

  case 28: /* stmt: assignment TOKEN_SEMI  */
#line 162 "b-minor-parser.y"
                            { (yyval.stmt) = stmt_create(STMT_EXPR, 0, 0, (yyvsp[-1].expr), 0, 0, 0, 0); printf(">=================>stmt: assignment\n"); }
#line 1466 "b-minor-parser-ast.c"
    break;

  case 29: /* stmt: print_stmt TOKEN_SEMI  */
#line 163 "b-minor-parser.y"
                            { (yyval.stmt) = (yyvsp[-1].stmt); printf(">=================>print statement\n"); }
#line 1472 "b-minor-parser-ast.c"
    break;

  case 30: /* stmt: expr TOKEN_SEMI  */
#line 164 "b-minor-parser.y"
                      { (yyval.stmt) = stmt_create(STMT_EXPR, 0, 0, (yyvsp[-1].expr), 0, 0, 0, 0); printf(">=================>stmt: expr\n");}
#line 1478 "b-minor-parser-ast.c"
    break;

  case 31: /* stmt: if_stmt  */
#line 165 "b-minor-parser.y"
              { (yyval.stmt) = (yyvsp[0].stmt); printf(">=================>stmt: if matched\n"); }
#line 1484 "b-minor-parser-ast.c"
    break;

  case 32: /* stmt: for_stmt  */
#line 166 "b-minor-parser.y"
               { (yyval.stmt) = (yyvsp[0].stmt); printf(">=================>stmt: for\n"); }
#line 1490 "b-minor-parser-ast.c"
    break;

  case 33: /* stmt: stmt_block  */
#line 167 "b-minor-parser.y"
                 { (yyval.stmt) = (yyvsp[0].stmt); printf(">=================>stmt: block\n"); }
#line 1496 "b-minor-parser-ast.c"
    break;

  case 34: /* stmt: return_stmt TOKEN_SEMI  */
#line 168 "b-minor-parser.y"
                             { (yyval.stmt) = (yyvsp[-1].stmt); printf(">=================>stmt: return\n"); }
#line 1502 "b-minor-parser-ast.c"
    break;

  case 35: /* stmt_block: TOKEN_LBRACE stmts TOKEN_RBRACE  */
#line 172 "b-minor-parser.y"
                                             { (yyval.stmt) = stmt_create(STMT_BLOCK, 0, 0, 0, 0, (yyvsp[-1].stmt), 0, 0); printf(">=================>statement block\n"); }
#line 1508 "b-minor-parser-ast.c"
    break;

  case 36: /* stmt_block: TOKEN_LBRACE TOKEN_RBRACE  */
#line 173 "b-minor-parser.y"
                                    { (yyval.stmt) = 0; printf(">=================>empty statement block\n"); }
#line 1514 "b-minor-parser-ast.c"
    break;

  case 37: /* function_call: identifier TOKEN_LPAREN args TOKEN_RPAREN  */
#line 177 "b-minor-parser.y"
                                                          { (yyval.expr) = expr_create(EXPR_FUNCTIONCALL, expr_create_name((yyvsp[-3].name)), (yyvsp[-1].expr)); printf(">=================>function call %s\n", (yyvsp[-3].name)); }
#line 1520 "b-minor-parser-ast.c"
    break;

  case 38: /* args: arg_list  */
#line 181 "b-minor-parser.y"
                { (yyval.expr) = (yyvsp[0].expr); printf(">=================>args: argument list\n"); }
#line 1526 "b-minor-parser-ast.c"
    break;

  case 39: /* args: %empty  */
#line 182 "b-minor-parser.y"
                        { (yyval.expr) = 0; printf(">=================>empty args\n"); }
#line 1532 "b-minor-parser-ast.c"
    break;

  case 40: /* arg_list: arg  */
#line 186 "b-minor-parser.y"
               { (yyval.expr) = (yyvsp[0].expr); printf(">=================>arg_list: arg\n"); }
#line 1538 "b-minor-parser-ast.c"
    break;

  case 41: /* arg_list: arg TOKEN_COMMA arg_list  */
#line 187 "b-minor-parser.y"
                                   { (yyval.expr) = (yyvsp[-2].expr); (yyvsp[-2].expr)->right = (yyvsp[0].expr); printf(">=================>arg_list: list of args\n"); }
#line 1544 "b-minor-parser-ast.c"
    break;

  case 42: /* arg: expr  */
#line 191 "b-minor-parser.y"
           { (yyval.expr) = (yyvsp[0].expr); printf(">=================>arg: expr\n"); }
#line 1550 "b-minor-parser-ast.c"
    break;

  case 43: /* assignment: identifier TOKEN_ASSIGN expr  */
#line 195 "b-minor-parser.y"
                                         { (yyval.expr) = expr_create(EXPR_ASSIGN, expr_create_name((yyvsp[-2].name)), (yyvsp[0].expr)); printf(">=================>assignment: non-array\n"); }
#line 1556 "b-minor-parser-ast.c"
    break;

  case 44: /* assignment: array_element TOKEN_ASSIGN expr  */
#line 196 "b-minor-parser.y"
                                             { (yyval.expr) = expr_create(EXPR_ASSIGN, (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>assignment: array element\n"); }
#line 1562 "b-minor-parser-ast.c"
    break;

  case 45: /* identifier: TOKEN_IDENT  */
#line 200 "b-minor-parser.y"
                        { (yyval.name) = strdup(yytext); printf(">=================>identifier %s\n", yytext); }
#line 1568 "b-minor-parser-ast.c"
    break;

  case 46: /* expr: expr logical_op lo_expr  */
#line 204 "b-minor-parser.y"
                                { (yyval.expr) = expr_create((yyvsp[-1].value), (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>expr: logical_op\n"); }
#line 1574 "b-minor-parser-ast.c"
    break;

  case 47: /* expr: lo_expr  */
#line 205 "b-minor-parser.y"
                  { (yyval.expr) = (yyvsp[0].expr); printf(">=================>expr: pm_expr\n"); }
#line 1580 "b-minor-parser-ast.c"
    break;

  case 48: /* lo_expr: lo_expr relational_op pm_expr  */
#line 209 "b-minor-parser.y"
                                       { (yyval.expr) = expr_create((yyvsp[-1].value), (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>expr: relational_op\n"); }
#line 1586 "b-minor-parser-ast.c"
    break;

  case 49: /* lo_expr: pm_expr  */
#line 210 "b-minor-parser.y"
                  { (yyval.expr) = (yyvsp[0].expr); printf(">=================>factor: logical_boolean\n"); }
#line 1592 "b-minor-parser-ast.c"
    break;

  case 50: /* pm_expr: pm_expr TOKEN_PLUS term  */
#line 214 "b-minor-parser.y"
                                        { (yyval.expr) = expr_create(EXPR_ADD, (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>pm_expr: addition\n"); }
#line 1598 "b-minor-parser-ast.c"
    break;

  case 51: /* pm_expr: pm_expr TOKEN_MINUS term  */
#line 215 "b-minor-parser.y"
                                     { (yyval.expr) = expr_create(EXPR_SUB, (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>pm_expr: subtraction\n");}
#line 1604 "b-minor-parser-ast.c"
    break;

  case 52: /* pm_expr: term  */
#line 216 "b-minor-parser.y"
             { (yyval.expr) = (yyvsp[0].expr); printf(">=================>pm_expr: term\n"); }
#line 1610 "b-minor-parser-ast.c"
    break;

  case 53: /* term: term TOKEN_MUL expon  */
#line 220 "b-minor-parser.y"
                                  { (yyval.expr) = expr_create(EXPR_MUL, (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>term: multiplication\n"); }
#line 1616 "b-minor-parser-ast.c"
    break;

  case 54: /* term: term TOKEN_DIV expon  */
#line 221 "b-minor-parser.y"
                                  { (yyval.expr) = expr_create(EXPR_DIV, (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>term: division\n"); }
#line 1622 "b-minor-parser-ast.c"
    break;

  case 55: /* term: term TOKEN_MOD expon  */
#line 222 "b-minor-parser.y"
                                  { (yyval.expr) = expr_create(EXPR_MODULUS, (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>term: modulus\n"); }
#line 1628 "b-minor-parser-ast.c"
    break;

  case 56: /* term: expon  */
#line 223 "b-minor-parser.y"
              { (yyval.expr) = (yyvsp[0].expr); printf(">=================>term: expon\n");}
#line 1634 "b-minor-parser-ast.c"
    break;

  case 57: /* expon: expon TOKEN_EXPONENTIATION factor  */
#line 227 "b-minor-parser.y"
                                          { (yyval.expr) = expr_create(EXPR_EXPONENTIATION, (yyvsp[-2].expr), (yyvsp[0].expr)); printf(">=================>expon: exponentiation\n"); }
#line 1640 "b-minor-parser-ast.c"
    break;

  case 58: /* expon: factor  */
#line 228 "b-minor-parser.y"
               { (yyval.expr) = (yyvsp[0].expr); printf(">=================>expon: factor\n"); }
#line 1646 "b-minor-parser-ast.c"
    break;

  case 59: /* factor: TOKEN_MINUS expr  */
#line 232 "b-minor-parser.y"
                           { (yyval.expr) = expr_create(EXPR_SUB, expr_create_integer_literal(0), (yyvsp[0].expr)); printf(">=================>factor: negate\n");}
#line 1652 "b-minor-parser-ast.c"
    break;

  case 60: /* factor: TOKEN_LPAREN expr TOKEN_RPAREN  */
#line 233 "b-minor-parser.y"
                                          { (yyval.expr) = (yyvsp[-1].expr); printf(">=================>factor: parenthesized expression\n"); }
#line 1658 "b-minor-parser-ast.c"
    break;

  case 61: /* factor: postfix  */
#line 234 "b-minor-parser.y"
                  { (yyval.expr) = (yyvsp[0].expr); printf(">=================>factor: postfix\n"); }
#line 1664 "b-minor-parser-ast.c"
    break;

  case 62: /* factor: identifier  */
#line 235 "b-minor-parser.y"
                     { (yyval.expr) = expr_create_name((yyvsp[0].name)); printf(">=================>factor: identifier\n"); }
#line 1670 "b-minor-parser-ast.c"
    break;

  case 63: /* factor: literal  */
#line 236 "b-minor-parser.y"
                  { (yyval.expr) = (yyvsp[0].expr); printf(">=================>factor: literal\n"); }
#line 1676 "b-minor-parser-ast.c"
    break;

  case 64: /* factor: not  */
#line 237 "b-minor-parser.y"
              { (yyval.expr) = (yyvsp[0].expr); printf(">=================>factor: not\n");}
#line 1682 "b-minor-parser-ast.c"
    break;

  case 65: /* factor: array_element  */
#line 238 "b-minor-parser.y"
                        { (yyval.expr) = (yyvsp[0].expr); printf(">=================>factor: array element\n"); }
#line 1688 "b-minor-parser-ast.c"
    break;

  case 66: /* factor: function_call  */
#line 239 "b-minor-parser.y"
                        { (yyval.expr) = (yyvsp[0].expr); printf(">=================>factor: function call\n"); }
#line 1694 "b-minor-parser-ast.c"
    break;

  case 67: /* not: TOKEN_NOT expr  */
#line 243 "b-minor-parser.y"
                    { (yyval.expr) = expr_create(EXPR_NOT, (yyvsp[0].expr), 0); printf(">=================>not\n"); }
#line 1700 "b-minor-parser-ast.c"
    break;

  case 68: /* array_element: identifier TOKEN_LBRACKET expr TOKEN_RBRACKET  */
#line 247 "b-minor-parser.y"
                                                             { (yyval.expr) = expr_create(EXPR_SUBSCRIPT, expr_create_name((yyvsp[-3].name)), (yyvsp[-1].expr)); printf(">=================>element of array %s\n", (yyvsp[-3].name)); }
#line 1706 "b-minor-parser-ast.c"
    break;

  case 69: /* if_stmt: TOKEN_IF TOKEN_LPAREN expr TOKEN_RPAREN stmt  */
#line 252 "b-minor-parser.y"
                                                                    { (yyval.stmt) = stmt_create(STMT_IF_ELSE, 0, 0, (yyvsp[-2].expr), 0, (yyvsp[0].stmt), 0, 0); printf(">=================>if without else\n"); }
#line 1712 "b-minor-parser-ast.c"
    break;

  case 70: /* if_stmt: TOKEN_IF TOKEN_LPAREN expr TOKEN_RPAREN stmt TOKEN_ELSE stmt  */
#line 253 "b-minor-parser.y"
                                                                     { (yyval.stmt) = stmt_create(STMT_IF_ELSE, 0, 0, (yyvsp[-4].expr), 0, (yyvsp[-2].stmt), (yyvsp[0].stmt), 0); printf(">=================>if with else\n"); }
#line 1718 "b-minor-parser-ast.c"
    break;

  case 71: /* for_stmt: TOKEN_FOR TOKEN_LPAREN opt_assignment TOKEN_SEMI opt_expr TOKEN_SEMI opt_increment TOKEN_RPAREN stmt  */
#line 257 "b-minor-parser.y"
                                                                                                               { (yyval.stmt) = stmt_create(STMT_FOR, 0, (yyvsp[-6].expr), (yyvsp[-4].expr), (yyvsp[-2].expr), (yyvsp[0].stmt), 0, 0); printf(">=================>for statement\n"); }
#line 1724 "b-minor-parser-ast.c"
    break;

  case 72: /* return_stmt: TOKEN_RETURN expr  */
#line 261 "b-minor-parser.y"
                               { (yyval.stmt) = stmt_create(STMT_RETURN, 0, 0, (yyvsp[0].expr), 0, 0, 0, 0); printf(">=================>return\n"); }
#line 1730 "b-minor-parser-ast.c"
    break;

  case 73: /* print_stmt: TOKEN_PRINT args  */
#line 265 "b-minor-parser.y"
                             { (yyval.stmt) = stmt_create(STMT_PRINT, 0, 0, (yyvsp[0].expr), 0, 0, 0, 0); printf(">=================>print statement\n"); }
#line 1736 "b-minor-parser-ast.c"
    break;

  case 74: /* postfix: identifier TOKEN_POSTFIX_ADD  */
#line 269 "b-minor-parser.y"
                                      { (yyval.expr) = expr_create(EXPR_INCREMENT, expr_create_name((yyvsp[-1].name)), 0); printf(">=================>postfix increment\n"); }
#line 1742 "b-minor-parser-ast.c"
    break;

  case 75: /* postfix: identifier TOKEN_POSTFIX_MINUS  */
#line 270 "b-minor-parser.y"
                                         { (yyval.expr) = expr_create(EXPR_DECREMENT, expr_create_name((yyvsp[-1].name)), 0); printf(">=================>postfix decrement\n"); }
#line 1748 "b-minor-parser-ast.c"
    break;

  case 76: /* opt_assignment: assignment  */
#line 274 "b-minor-parser.y"
                           { (yyval.expr) = (yyvsp[0].expr); printf(">=================>opt_assignment: assignment\n"); }
#line 1754 "b-minor-parser-ast.c"
    break;

  case 77: /* opt_assignment: %empty  */
#line 275 "b-minor-parser.y"
                          { (yyval.expr) = 0; printf(">=================>opt_assignment epsilon \n"); }
#line 1760 "b-minor-parser-ast.c"
    break;

  case 78: /* opt_expr: %empty  */
#line 279 "b-minor-parser.y"
                        { (yyval.expr) = 0;  printf(">=================>opt_expr: epsilon\n"); }
#line 1766 "b-minor-parser-ast.c"
    break;

  case 79: /* opt_expr: expr  */
#line 280 "b-minor-parser.y"
               { (yyval.expr) = (yyvsp[0].expr);  printf(">=================>opt_expr: expr\n"); }
#line 1772 "b-minor-parser-ast.c"
    break;

  case 80: /* opt_increment: assignment  */
#line 284 "b-minor-parser.y"
                          { (yyval.expr) = (yyvsp[0].expr); printf(">=================>opt_increment: assignment\n"); }
#line 1778 "b-minor-parser-ast.c"
    break;

  case 81: /* opt_increment: postfix  */
#line 285 "b-minor-parser.y"
                    { (yyval.expr) = (yyvsp[0].expr); printf(">=================>opt_increment: postfix\n"); }
#line 1784 "b-minor-parser-ast.c"
    break;

  case 82: /* opt_increment: %empty  */
#line 286 "b-minor-parser.y"
                          { (yyval.expr) = 0; printf(">=================>opt_increment epsilon\n"); }
#line 1790 "b-minor-parser-ast.c"
    break;

  case 83: /* literal: TOKEN_TRUE  */
#line 290 "b-minor-parser.y"
                     { (yyval.expr) = expr_create_boolean_literal(1); printf(">=================>boolean literal (true)\n"); }
#line 1796 "b-minor-parser-ast.c"
    break;

  case 84: /* literal: TOKEN_FALSE  */
#line 291 "b-minor-parser.y"
                      { (yyval.expr) = expr_create_boolean_literal(0); printf(">=================>boolean literal (false)\n"); }
#line 1802 "b-minor-parser-ast.c"
    break;

  case 85: /* literal: TOKEN_LITERAL_INT  */
#line 292 "b-minor-parser.y"
                            { (yyval.expr) = expr_create_integer_literal(atoi(yytext)); printf(">=================>integer literal %s\n", yytext); }
#line 1808 "b-minor-parser-ast.c"
    break;

  case 86: /* literal: TOKEN_LITERAL_CHAR  */
#line 293 "b-minor-parser.y"
                             { (yyval.expr) = expr_create_char_literal(*(yytext + 1)); printf(">=================>character literal %s\n", yytext);  }
#line 1814 "b-minor-parser-ast.c"
    break;

  case 87: /* literal: TOKEN_LITERAL_STRING  */
#line 294 "b-minor-parser.y"
                               { (yyval.expr) = expr_create_string_literal(strdup(yytext)); printf(">=================>string literal %s\n", yytext);  }
#line 1820 "b-minor-parser-ast.c"
    break;

  case 88: /* atomic_type: TOKEN_BOOLEAN  */
#line 298 "b-minor-parser.y"
                             { (yyval.type) = type_create(TYPE_BOOLEAN, 0, 0); printf(">=================>Boolean type created\n"); }
#line 1826 "b-minor-parser-ast.c"
    break;

  case 89: /* atomic_type: TOKEN_INTEGER  */
#line 299 "b-minor-parser.y"
                          { (yyval.type) = type_create(TYPE_INTEGER, 0, 0); printf(">=================>Integer type created\n"); }
#line 1832 "b-minor-parser-ast.c"
    break;

  case 90: /* atomic_type: TOKEN_CHAR  */
#line 300 "b-minor-parser.y"
                        { (yyval.type) = type_create(TYPE_CHARACTER, 0, 0); printf(">=================>Character type created\n"); }
#line 1838 "b-minor-parser-ast.c"
    break;

  case 91: /* atomic_type: TOKEN_STRING  */
#line 301 "b-minor-parser.y"
                          { (yyval.type) = type_create(TYPE_STRING, 0, 0); printf(">=================>String type created\n"); }
#line 1844 "b-minor-parser-ast.c"
    break;

  case 92: /* return_type: atomic_type  */
#line 305 "b-minor-parser.y"
                           { (yyval.type) = (yyvsp[0].type); printf(">=================>return_type: atomic_type\n"); }
#line 1850 "b-minor-parser-ast.c"
    break;

  case 93: /* return_type: TOKEN_VOID  */
#line 306 "b-minor-parser.y"
                     { (yyval.type) = type_create(TYPE_VOID, 0, 0); printf(">=================>return_type: void created\n"); }
#line 1856 "b-minor-parser-ast.c"
    break;

  case 94: /* relational_op: TOKEN_EQUAL  */
#line 310 "b-minor-parser.y"
                            { (yyval.value) = EXPR_EQUAL; printf(">=================>relational_op: equal\n"); }
#line 1862 "b-minor-parser-ast.c"
    break;

  case 95: /* relational_op: TOKEN_NOT_EQUAL  */
#line 311 "b-minor-parser.y"
                            { (yyval.value) = EXPR_NOT_EQUAL; printf(">=================>relational_op: not equal\n"); }
#line 1868 "b-minor-parser-ast.c"
    break;

  case 96: /* relational_op: TOKEN_GREATER  */
#line 312 "b-minor-parser.y"
                          { (yyval.value) = EXPR_GREATER_THAN; printf(">=================>relational_op: greater\n"); }
#line 1874 "b-minor-parser-ast.c"
    break;

  case 97: /* relational_op: TOKEN_GREATER_EQUAL  */
#line 313 "b-minor-parser.y"
                                { (yyval.value) = EXPR_GREATER_EQUAL; printf(">=================>relational_op: greater equal\n"); }
#line 1880 "b-minor-parser-ast.c"
    break;

  case 98: /* relational_op: TOKEN_LESS  */
#line 314 "b-minor-parser.y"
                       { (yyval.value) = EXPR_LESS_THAN; printf(">=================>relational_op: less\n"); }
#line 1886 "b-minor-parser-ast.c"
    break;

  case 99: /* relational_op: TOKEN_LESS_EQUAL  */
#line 315 "b-minor-parser.y"
                             { (yyval.value) = EXPR_LESS_EQUAL; printf(">=================>relational_op: less equal\n"); }
#line 1892 "b-minor-parser-ast.c"
    break;

  case 100: /* logical_op: TOKEN_LOGICAL_AND  */
#line 319 "b-minor-parser.y"
                              { (yyval.value) = EXPR_AND; printf(">=================>logical_op: and\n"); }
#line 1898 "b-minor-parser-ast.c"
    break;

  case 101: /* logical_op: TOKEN_LOGICAL_OR  */
#line 320 "b-minor-parser.y"
                             { (yyval.value) = EXPR_OR; printf(">=================>logical_op: or\n"); }
#line 1904 "b-minor-parser-ast.c"
    break;


#line 1908 "b-minor-parser-ast.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 322 "b-minor-parser.y"


/* Display an error message in the console */
void yyerror(char const *s) {
   fprintf(stderr, "%s\n", s);
}
