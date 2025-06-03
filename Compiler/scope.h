

  #ifndef SCOPE_H

  #define SCOPE_H

  #include "hash_table.h"
  #include "symbol.h"

  struct scope_stack_element {
    struct hash_table *table;
    struct scope_stack_element *prev;
  };

  void scope_enter();
  void scope_exit();
  int scope_level();

  void scope_bind(const char *name, struct symbol *sym);
  struct symbol *scope_lookup(const char *name);
  struct symbol *scope_lookup_current(const char *name);

  #endif // SCOPE_H
