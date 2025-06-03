
  #ifndef   TYPE_CHECK_H

  #include "ast_library.h"

  #define TYPE_CHECK_H

  int typecheck_error_count();



  void decl_resolve(struct decl *d);
  void expr_resolve(struct expr *e);
  void param_list_resolve(struct param_list *p);
  void stmt_resolve(struct stmt *s);


  int type_equals(struct type *a, struct type *b);
  struct type *type_copy(struct type *t);
  struct param_list *param_list_copy(struct param_list *p);
  void type_delete(struct type *t);
  void param_list_delete(struct param_list *p);

  struct type *expr_typecheck(struct expr *e);
  void decl_typecheck(struct decl *d);
  void stmt_typecheck(struct stmt *s);

  #endif // TYPE_CHECK_H
