/*
 Header file for the library of AST node creation functions.
*/

#ifndef AST_LIBRARY_H

#define AST_LIBRARY_H

#include "symbol.h"

extern void yyerror(const char *);

/* Structure (metadata) for a declaration */
struct decl {
  char *name;
  struct type *type;
  struct expr *value;
  struct stmt *code;
  struct decl *next;

  // Used by name_resolution to add the symbol to this AST node (added in chapter 7)
  struct symbol *symbol;
};

/* Function prototype for creating a declaration */
struct decl *decl_create(char *name, struct type *type, struct expr *value, struct stmt *code, struct decl *next);

/* Statement types */
typedef enum {
  STMT_DECL,
  STMT_EXPR,
  STMT_IF_ELSE,
  STMT_FOR,
  STMT_PRINT,
  STMT_RETURN,
  STMT_BLOCK
} stmt_t;

/* Structure (metadata) for a statement */
struct stmt {
    stmt_t kind;
    struct decl *decl;
    struct expr *init_expr;
    struct expr *expr;
    struct expr *next_expr;
    struct stmt *body;
    struct stmt *else_body;
    struct stmt *next;
};

/* Function prototype for creating a statement */
struct stmt * stmt_create(stmt_t kind, struct decl *decl, struct expr *init_expr,
        struct expr *expr, struct expr *next_expr, struct stmt *body,
        struct stmt *else_body, struct stmt *next);

/* Expression types */
typedef enum {
  EXPR_ASSIGN,
  EXPR_FUNCTIONCALL,
  EXPR_ADD,
  EXPR_SUB,
  EXPR_MUL,
  EXPR_DIV,
  EXPR_MODULUS,
  EXPR_EXPONENTIATION,
  EXPR_NAME,
  EXPR_EQUAL,
  EXPR_GREATER_THAN,
  EXPR_GREATER_EQUAL,
  EXPR_LESS_THAN,
  EXPR_LESS_EQUAL,
  EXPR_NOT_EQUAL,
  EXPR_NOT,
  EXPR_AND,
  EXPR_OR,
  EXPR_BOOLEAN_LITERAL,
  EXPR_CHAR_LITERAL,
  EXPR_INTEGER_LITERAL,
  EXPR_STRING_LITERAL,
  EXPR_INCREMENT,
  EXPR_DECREMENT,
  EXPR_SUBSCRIPT
} expr_t;

struct expr {
  expr_t kind;
  struct expr *left;
  struct expr *right;
  const char *name;
  int integer_value;
  const char *string_literal;

  // Used by name_resolution to add the symbol to this AST node (added in chapter 7)
  struct symbol *symbol;

  // Added in chapter 11 for keeping track of associated register
  int reg;
};

/* Function prototype to create an expression */
struct expr *expr_create(expr_t kind, struct expr *left, struct expr *right);

/* Function prototype to create a variable (name) */
struct expr *expr_create_name(const char *name);

/* Function prototype to create an integer literal */
struct expr *expr_create_integer_literal(int i);

/* Function prototype to create a Boolean literal */
struct expr *expr_create_boolean_literal(int b);

/* Function prototype to create a character literal */
struct expr *expr_create_char_literal(char c);

/* Function prototype to create a string literal */
struct expr *expr_create_string_literal(const char *str);

/* Data types */
typedef enum {
  TYPE_VOID,
  TYPE_BOOLEAN,
  TYPE_CHARACTER,
  TYPE_INTEGER,
  TYPE_STRING,
  TYPE_ARRAY,
  TYPE_FUNCTION
} type_t;

/* Structure (metadata) for a type */
struct type {
  type_t kind;
  struct type *subtype;
  struct param_list *params;
};

/* Function prototype to create a type */
struct type *type_create(type_t kind, struct type *subtype, struct param_list *params);

/* Structure (metadata) for params */
struct param_list {
  const char *name;
  struct type *type;
  struct param_list *next;

  // Used by name_resolution to add the symbol to this AST node (added in chapter 7)
  struct symbol *symbol;
};

/* Function prototype to create a parameter */
struct param_list *param_list_create(const char *name, struct type *type, struct param_list *next);

#endif // AST_LIBRARY_H
