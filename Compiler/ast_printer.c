/*
 Library of AST printing functions.
*/

#include <stdio.h>
#include <stdarg.h>
#include "ast_library.h"
#include "ast_printer.h"

// Configuration for the AST printing
// TODO override on the command line
#define INDENT_CHARACTER ' '
#define DISPLAY_POINTER_ADDRESSES 0
#define NEST_NEXT_DECL 0
#define NEST_NEXT_STMT 0

// Vector of type names associated with the constants
char *TYPE_T_TEXT[] = {
  "TYPE_VOID",
  "TYPE_BOOLEAN",
  "TYPE_CHARACTER",
  "TYPE_INTEGER",
  "TYPE_STRING",
  "TYPE_ARRAY",
  "TYPE_FUNCTION"
};

// Vector of expression names associated with the constants
char *EXPR_T_TEXT[] = {
  "EXPR_ASSIGN",
  "EXPR_FUNCTIONCALL",
  "EXPR_ADD",
  "EXPR_SUB",
  "EXPR_MUL",
  "EXPR_DIV",
  "EXPR_MODULUS",
  "EXPR_EXPONENTIATION",
  "EXPR_NAME",
  "EXPR_EQUAL",
  "EXPR_GREATER_THAN",
  "EXPR_GREATER_EQUAL",
  "EXPR_LESS_THAN",
  "EXPR_LESS_EQUAL",
  "EXPR_NOT_EQUAL",
  "EXPR_NOT",
  "EXPR_AND",
  "EXPR_OR",
  "EXPR_BOOLEAN_LITERAL",
  "EXPR_CHAR_LITERAL",
  "EXPR_INTEGER_LITERAL",
  "EXPR_STRING_LITERAL",
  "EXPR_INCREMENT",
  "EXPR_DECREMENT",
  "EXPR_SUBSCRIPT"
};

// Vector of statement names associated with the constants
char *STMT_T_TEXT[] = {
  "STMT_DECL",
  "STMT_EXPR",
  "STMT_IF_ELSE",
  "STMT_FOR",
  "STMT_PRINT",
  "STMT_RETURN",
  "STMT_BLOCK"
};

#if PRINT_SYMBOL_NODES
// Vector of symbol names associated with the constants
char *SYMBOL_T_TEXT[] = {
  "SYMBOL_LOCAL",
  "SYMBOL_PARAM",
  "SYMBOL_GLOBAL"
};
#endif // PRINT_SYMBOL_NODES

// Print a declaration node
void decl_print(struct decl *node) {
  if (!node) {
    return;
  }

  iprintf("decl>");
  if (DISPLAY_POINTER_ADDRESSES) {
    printf(" ptr:%p", node);
  }
  printf("\n");

  indent(0, 1);

  if (DISPLAY_POINTER_ADDRESSES) {
    iprintf("name ptr:%p ", node->name);
  } else {
    iprintf("");
  }

  printf("name:%s  ", node->name);
  type_print(node->type);

  #if PRINT_SYMBOL_NODES
    symbol_print(node->symbol);
  #endif

  expr_print(NULL, node->value);
  stmt_print(NULL, node->code);

  if (node->next) {
    if (!NEST_NEXT_DECL) {
      indent(0, -1);
    }
    decl_print(node->next);
    if (!NEST_NEXT_DECL) {
      indent(0, 1);
    }
  }

  indent(0, -1);
}

// Print an expression node
void expr_print(char *prefix, struct expr *node) {
  if (!node) {
    return;
  }

  iprintf("expr");
  if (prefix) {
    printf("-%s", prefix);
  }
  printf(">");
  if (DISPLAY_POINTER_ADDRESSES) {
    printf(" ptr:%p", node);
  }
  printf("\n");

  indent(0, 1);

  iprintf("kind:%d (%s)", node->kind, getExprName(node->kind));

  if (node->name) {
    if (DISPLAY_POINTER_ADDRESSES) {
      printf(" name ptr:%p", node->name);
    }
    printf(" name:%s", node->name);
  }

  switch (node->kind) {
    case EXPR_INTEGER_LITERAL:
      printf(" value:%d", node->integer_value);
      break;
    case EXPR_CHAR_LITERAL:
      printf(" value:%c", node->integer_value);
      break;
    case EXPR_BOOLEAN_LITERAL:
      printf(" value:%d (%s)", node->integer_value, node->integer_value? "true" : "false");
      break;
    case EXPR_STRING_LITERAL:
      if (DISPLAY_POINTER_ADDRESSES) {
        printf(" value ptr:%p\n", node->string_literal);
      }
      printf(" value:%s", node->string_literal);
      break;
  }

  printf("\n");

  #if PRINT_SYMBOL_NODES
    symbol_print(node->symbol);
  #endif

  expr_print("left", node->left);
  expr_print("right", node->right);

  indent(0, -1);
}

// Print a statement node
void stmt_print(char *prefix, struct stmt *node) {
    if (!node) {
      return;
    }

    iprintf("stmt");
    if (prefix) {
      printf("-%s", prefix);
    }
    printf(">");
    if (DISPLAY_POINTER_ADDRESSES) {
      printf(" ptr:%p", node);
    }
    printf("\n");

    indent(0, 1);

    iprintf("kind:%d (%s)\n", node->kind, getStmtName(node->kind));

    decl_print(node->decl);
    expr_print("init", node->init_expr);
    expr_print(NULL, node->expr);
    expr_print("next", node->next_expr);
    stmt_print("body", node->body);
    stmt_print("else", node->else_body);

    if (!NEST_NEXT_STMT) {
      indent(0, -1);
    }
    stmt_print(NULL, node->next);
    if (!NEST_NEXT_STMT) {
      indent(0, 1);
    }

    indent(0, -1);
}

// Print a type node
void type_print(struct type *node) {
  if (!node) {
    return;
  }

  if (DISPLAY_POINTER_ADDRESSES) {
    printf("type ptr:%p ", node);
  }

  printf("type:%d (%s)", node->kind, getTypeName(node->kind));
  if (node->subtype) {
    if (DISPLAY_POINTER_ADDRESSES) {
      printf(" subtype ptr:%p", node->subtype);
    }

    printf(" subtype:%d (%s)", node->subtype->kind, getTypeName(node->subtype->kind));
  }
  printf("\n");

  param_list_print(node->params);
}

// Print a parameter list node
void param_list_print(struct param_list *node) {
  if (!node) {
    return;
  }

  iprintf("param>");
  if (DISPLAY_POINTER_ADDRESSES) {
    printf(" ptr:%p", node);
  }
  printf("\n");

  indent(0, 1);

  if (DISPLAY_POINTER_ADDRESSES) {
    iprintf("name ptr:%p\n", node->name);
  }
  iprintf("name:%s ", node->name);

  type_print(node->type);

  #if PRINT_SYMBOL_NODES
    symbol_print(node->symbol);
  #endif

  param_list_print(node->next);

  indent(0, -1);
}

#if PRINT_SYMBOL_NODES
void symbol_print(struct symbol *node) {
  if (!node) {
    return;
  }

  iprintf("symbol>");
  if (DISPLAY_POINTER_ADDRESSES) {
    printf(" ptr:%p", node);
  }
  printf("\n");

  indent(0, 1);

  iprintf("kind:%d (%s)\n", node->kind, getSymbolName(node->kind));

  iprintf("");
  type_print(node->type);

  if (DISPLAY_POINTER_ADDRESSES) {
    iprintf("name ptr:%p\n", node->name);
  }
  iprintf("name:%s\n", node->name);

  indent(0, -1);
}
#endif // PRINT_SYMBOL_NODES

// Retrieve a type name
// See the TYPE_T_TEXT vector at the top of this file
char *getTypeName(type_t kind) {
  if (kind < 0 || kind >= sizeof(TYPE_T_TEXT) / sizeof(char *)) {
    return "UNDEFINED TYPE NAME";
  }

  return TYPE_T_TEXT[kind];
}

// Retrieve an expression name
// See the EXPR_T_TEXT vector at the top of this file
char *getExprName(expr_t kind) {
  if (kind < 0 || kind >= sizeof(EXPR_T_TEXT) / sizeof(char *)) {
    return "UNDEFINED EXPR NAME";
  }

  return EXPR_T_TEXT[kind];
}

// Retrieve a statement name
// See the STMT_T_TEXT vector at the top of this file
char *getStmtName(stmt_t kind) {
  if (kind < 0 || kind >= sizeof(STMT_T_TEXT) / sizeof(char *)) {
    return "UNDEFINED STMT NAME";
  }

  return STMT_T_TEXT[kind];
}

#if PRINT_SYMBOL_NODES
// Retrieve a symbol name
// See the SYMBOL_T_TEXT vector at the top of this file
char *getSymbolName(symbol_t kind) {
  if (kind < 0 || kind >= sizeof(SYMBOL_T_TEXT) / sizeof(char *)) {
    return "UNDEFINED SYMBOL NAME";
  }

  return SYMBOL_T_TEXT[kind];
}
#endif // PRINT_SYMBOL_NODES

// A printf() that uses the current indent setting
void iprintf(char* format, ...) {
  va_list arg_list;
  va_start(arg_list, format);

  char *ch = format;

  union {
    int d;
    double f;
    void *p;
    char *s;
  } arg;

  indent(1, 0);

  while (*ch) {
    if (*ch == '%') {
      ch++;
      switch (*ch) {
        case 'c': // char
          arg.d = va_arg(arg_list, int);
          printf("%c", arg.d);
          break;
        case 'd': // int
          arg.d = va_arg(arg_list, int);
          printf("%d", arg.d);
          break;
        case 'f': // float
          arg.f = va_arg(arg_list, double);
          printf("%lf", arg.f);
          break;
        case 'p': // pointer
          arg.p = va_arg(arg_list, void *);
          printf("%p", arg.p);
          break;
        case 's': // string (null-terminated char array)
          arg.s = va_arg(arg_list, char *);
          printf("%s", arg.s);
          break;
        default: // unsupported format
          printf("\nUnsupported format %%%c\n", *ch);
      }
    } else {
      putchar(*ch);
    }
    ch++;
  }
}

// Adjust the current level of indentation for iprintf()
void indent(int do_print, int adjust_spaces) {
  static int depth = -1;
  depth += adjust_spaces;
  if (depth < 0) {
    depth = 0;
  }

  if (do_print) {
    for (int i = 0;i < depth;i++) {
      putchar(INDENT_CHARACTER);
    }
  }
}
