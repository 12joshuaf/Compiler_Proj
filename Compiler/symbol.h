/* See chapter 7
  Symbol management code */

  /* Chapter 7 - symbol table management */

  #ifndef SYMBOL_H

  #define SYMBOL_H

  typedef enum {
    SYMBOL_LOCAL,
    SYMBOL_PARAM,
    SYMBOL_GLOBAL
  } symbol_t;

  struct symbol {
    symbol_t kind;
    struct type *type;
    const char *name;
    int which;
  };

  struct symbol *symbol_create(symbol_t kind, struct type *type, const char *name);

  #endif // SYMBOL_H
