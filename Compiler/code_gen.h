/* Code generation functions - Chapter 11 */

#ifndef CODE_GEN_H

#define CODE_GEN_H

#include <stdio.h>
#include "ast_library.h"

/* Must be setup in main */
extern FILE *ASM_CODE;

/* Generate assembly code for declarations */
void decl_codegen(struct decl *d);

/* Generate assembly code for expressions */
void expr_codegen(struct expr *e);

/* Generate assembly code for statements */
void stmt_codegen(struct stmt *s);

/* Lookup the x86 register name assosicated with an argument */
const char *abi_arg_register_name(int arg_position);

/* Output the assembly code without newline */
void emit_asm(const char *s);

/* Output the assembly code with a newline */
void emit_asm_line(const char *s);


void asm_global_intvalue(struct decl *d);
void asm_local_intvalue(struct decl *d);
void asm_global_stringvalue(struct decl *d);
void asm_local_stringvalue(struct decl *d);
void asm_function(struct decl *d);
int count_local_variables(struct stmt *s);
void print_codegen(struct stmt *s);


#endif // CODEGEN_H
