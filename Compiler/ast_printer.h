/*
 Header file for the library of AST printing functions.
*/

#ifndef AST_PRINTER_H

#define AST_PRINTER_H

#include "ast_library.h"

// Set PRINT_SYMBOL_STRUCTURE to 1 once the symbol enum (constants)
//   and symbol structure have been defined
// Note expected order and names of constants in the
//   SYMBOL_T_TEXT vector (in ast_printer.c)
#define PRINT_SYMBOL_NODES 1

// Recursive functions for printing the decorated AST
void decl_print(struct decl *node);
void expr_print(char *prefix, struct expr *node);
void stmt_print(char *prefix, struct stmt *node);
void param_list_print(struct param_list *node);
void type_print(struct type *node);

#if PRINT_SYMBOL_NODES
void symbol_print(struct symbol *node);
#endif

// Lookup functions for getting constant names
char *getTypeName(type_t value);
char *getExprName(expr_t kind);
char *getStmtName(stmt_t kind);

#if PRINT_SYMBOL_NODES
char *getSymbolName(symbol_t kind);
#endif

// Specialised output for managing indentation
void indent(int do_print, int adjust_spaces);
void iprintf(char* format, ...);

#endif // AST_PRINTER_H
