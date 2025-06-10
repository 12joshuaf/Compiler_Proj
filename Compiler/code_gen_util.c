// Utilities for code generation. Built for x86 CPUs.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "code_gen_util.h"

/* The scratch register index.
    0 indicates the register is available.
    1 indicates the register is already in use. */
int _scratch_registers[] = { 0, 0, 0, 0, 0, 0, 0 };

/* The number of available scratch registers */
#define NUM_REGISTERS (sizeof(_scratch_registers) / sizeof(int))

/* The latest label. */
int _label = 0;

/* Locate an unused scratch register.
    Stop the program if none are available. */
int scratch_alloc() {
  int avail_register = -1;
  int i;
  for (i = 0;i < NUM_REGISTERS;i++) {
    if (_scratch_registers[i] == 0) {
      _scratch_registers[i] = 1;
      avail_register = i;
      break;
    }
  }

  if (avail_register == -1) {
    printf("FATAL: Unable to find a free scratch register\n");
    exit(1);
  }

  return avail_register;
}

/* Free an allocated scratch register. */
void scratch_free(int r) {
  if (r >= 0 && r < NUM_REGISTERS) {
    _scratch_registers[r] = 0;
  } else {
    printf("WARN: Attempt to free non-existant scratch register (%d)\n", r);
  }
}

/* Obtain the name of a scratch register. */
const char *scratch_name(int r) {
  switch (r) {
    case 0: return "%rbx";
    case 1: return "%r10";
    case 2: return "%r11";
    case 3: return "%r12";
    case 4: return "%r13";
    case 5: return "%r14";
    case 6: return "%r15";
  }

  printf("FATAL: Undefined scratch register (%d)\n", r);
  exit(1);
}

/* Generates next unique label value */
int label_create() {
  ++_label;
  return _label;
}

/* Create label of the form .L# where # is the label argument's value
    This code uses a static char array so the value returned is only
    available prior to this function being called again. */
const char *label_name(int label) {
  static char __label_name[20];
  sprintf(__label_name, ".L%d", label);
  return __label_name;
}

/* Generate assembler code for address computation
    for the supplied symbol.

    TODO Fix MEMORY LEAK - Nothing is freeing the
      allocated memory for the return value */
const char *symbol_codegen(struct symbol *s) {
  char *__asm_code;

  if (s->kind == SYMBOL_GLOBAL) {
    __asm_code = strdup(s->name);
  } else {
    __asm_code = malloc(20);
    sprintf(__asm_code, "-%d(%%rbp)", s->which * 8);
  }

  return __asm_code;
}
