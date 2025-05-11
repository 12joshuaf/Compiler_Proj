/* Utilities for code generation.
    Scratch register allocation tracking - book page 181
    Label management - book page 182
    Symbol code generation - book page 183 */
#ifndef CODE_GEN_UTIL_H

#define CODE_GEN_UTIL_H

#include "symbol.h"

/* Allocate an unused scratch register. */
int scratch_alloc();

/* Free an allocated scratch register */
void scratch_free(int r);

/* Obtain the name of a scratch register */
const char *scratch_name(int r);

/* Generate the next unique label value */
int label_create();

/* Create a label of the form .L#
    where # is the label argument's value */
const char *label_name(int label);

/* Generate assembler code for address computation
    for the supplied symbol */
const char *symbol_codegen(struct symbol *s);

#endif // CODE_GEN_UTIL_H
