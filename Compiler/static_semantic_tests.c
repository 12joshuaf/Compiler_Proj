// Test functions for the scoping and type comparison operations

#include <stdio.h>
#include <stdlib.h>
#include <sys/sysinfo.h>

#include "static_semantic_tests.h"
#include "symbol.h"
#include "scope.h"
#include "type_check.h"

// As the scoping and type managing code is written, uncomment the macros
// in the .h file to enable this code to run

// See the main function (in main.c) for calls to test functions

#if TEST_SCOPES
void test_scopes() {
	printf("Should be no scopes yet. Num scopes: %d\n", scope_level());
	scope_enter();
	printf("Should be one scope. Num scopes: %d\n", scope_level());
	scope_enter();
	printf("Should be two scopes. Num scopes: %d\n", scope_level());
	scope_enter();
	printf("Should be three scopes. Num scopes: %d\n", scope_level());
	scope_exit();
	printf("Should be two scopes. Num scopes: %d\n", scope_level());
	scope_enter();
	printf("Should be three scopes. Num scopes: %d\n", scope_level());
	scope_exit();
	printf("Should be two scopes. Num scopes: %d\n", scope_level());
	scope_exit();
	printf("Should be one scope. Num scopes: %d\n", scope_level());
	scope_exit();
	printf("Should be zero scopes. Num scopes: %d\n", scope_level());
	scope_enter();
	printf("Should be one scope. Num scopes: %d\n", scope_level());
	scope_exit();
	printf("Should be zero scopes. Num scopes: %d\n", scope_level());
	scope_exit();
	printf("Exit scope should have been ignored, should still be zero scopes. Num scopes: %d\n", scope_level());
}
#endif

#if TEST_SCOPE_SYMBOLS
void test_scopes_symbols() {
	scope_enter();

	struct type *t = type_create(TYPE_INTEGER, 0, 0);
	struct symbol *a = symbol_create(SYMBOL_GLOBAL, t, "a");
	scope_bind("a", a);

	printf("Should find a in any scope: a found? %s\n", scope_lookup("a")? "Pass" : "FAIL");
	printf("Should find a in current scope: a found? %s\n", scope_lookup_current("a")? "Pass" : "FAIL");

	scope_enter();

	t = type_create(TYPE_INTEGER, 0, 0);
	struct symbol *b = symbol_create(SYMBOL_LOCAL, t, "b");
	scope_bind("b", b);

	printf("Should find a in any scope: a found? %s\n", scope_lookup("a")? "Pass" : "No");
	printf("Should NOT find a in current scope: a found? %s\n", scope_lookup_current("a")? "FAIL" : "Pass");
	printf("Should find b in any scope: b found? %s\n", scope_lookup("b")? "Pass" : "FAIL");
	printf("Should find b in current scope: b found? %s\n", scope_lookup_current("b")? "Pass" : "FAIL");

	scope_exit();

	printf("Should find a in any scope: a found? %s\n", scope_lookup("a")? "Pass" : "FAIL");
	printf("Should find a in current scope: a found? %s\n", scope_lookup_current("a")? "Pass" : "FAIL");
	printf("Should NOT find b in any scope: b found? %s\n", scope_lookup("b")? "FAIL" : "Pass");
	printf("Should NOT find b in current scope: b found? %s\n", scope_lookup_current("b")? "FAIL" : "Pass");
}
#endif

#if TEST_TYPE_EQUALS
void test_type_equals() {
	struct type *a = type_create(TYPE_INTEGER, 0, 0);
	struct type *b = type_create(TYPE_INTEGER, 0, 0);

	int result = type_equals(a, b);
	printf("Types should be equal: %s\n", result? "Pass" : "FAIL");

	b = type_create(TYPE_STRING, 0, 0);
 	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	b = type_create(TYPE_ARRAY, type_create(TYPE_INTEGER, 0, 0), 0);
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	a = type_create(TYPE_ARRAY, type_create(TYPE_STRING, 0, 0), 0);
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	a = type_create(TYPE_ARRAY, type_create(TYPE_INTEGER, 0, 0), 0);
	result = type_equals(a, b);
	printf("Types should be equal: %s\n", result? "Pass" : "FAIL");

	a = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("a", type_create(TYPE_STRING, 0, 0), 0));
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	b = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("b", type_create(TYPE_STRING, 0, 0), 0));
	result = type_equals(a, b);
	printf("Types should be equal: %s\n", result? "Pass" : "FAIL");

	a = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("a", type_create(TYPE_CHARACTER, 0, 0), 0));
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	b = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("b", type_create(TYPE_CHARACTER, 0, 0), 0));
	result = type_equals(a, b);
	printf("Types should be equal: %s\n", result? "Pass" : "FAIL");

	a = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("a", type_create(TYPE_CHARACTER, 0, 0), param_list_create("b", type_create(TYPE_INTEGER, 0, 0), 0)));
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");
}
#endif

#if TEST_TYPE_COPY
void test_type_copy() {
	struct type *a = type_create(TYPE_INTEGER, 0, 0);
	struct type *b = type_copy(a);

	int result = type_equals(a, b);
	printf("Types should be equal: %s\n", result? "Pass" : "FAIL");

	b = type_create(TYPE_STRING, 0, 0);
 	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	b = type_create(TYPE_ARRAY, type_create(TYPE_INTEGER, 0, 0), 0);
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	a = type_create(TYPE_ARRAY, type_create(TYPE_STRING, 0, 0), 0);
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	a = type_copy(b);
	result = type_equals(a, b);
	printf("Types should be equal: %s\n", result? "Pass" : "FAIL");

	a = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("a", type_create(TYPE_STRING, 0, 0), 0));
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	b = type_copy(a);
	result = type_equals(a, b);
	printf("Types should be equal: %s\n", result? "Pass" : "FAIL");

	a = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("a", type_create(TYPE_CHARACTER, 0, 0), 0));
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");

	b = type_copy(a);
	result = type_equals(a, b);
	printf("Types should be equal: %s\n", result? "Pass" : "FAIL");

	a = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("a", type_create(TYPE_CHARACTER, 0, 0), param_list_create("b", type_create(TYPE_INTEGER, 0, 0), 0)));
	result = type_equals(a, b);
	printf("Types should NOT be equal: %s\n", result? "FAIL" : "Pass");
}
#endif

#if TEST_TYPE_DELETE
void test_type_delete() {
	long int before;
	long int after;
	struct sysinfo *info = malloc(sizeof(*info));
	sysinfo(info);
	before = info->freeram;
	printf("Free RAM: %ld\n", before);

	int x = 1000000;
	struct type *a[x];

	struct type *template = type_create(TYPE_FUNCTION, type_create(TYPE_VOID, 0, 0), param_list_create("a", type_create(TYPE_CHARACTER, 0, 0), param_list_create("b", type_create(TYPE_INTEGER, 0, 0), 0)));

	for (int j = 0;j < 10;j++) {
		for (int i = 0;i < x;i++) {
			a[i] = type_copy(template);
		}

		sysinfo(info);
		after = info->freeram;
		printf("Free RAM: %ld\n", after);
		printf("Diff: %ld\n", before - after);

		for (int i = 0;i < x;i++) {
			type_delete(a[i]);
		}
	}
}
#endif
