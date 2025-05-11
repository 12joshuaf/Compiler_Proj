#include <stdio.h>
#include <stdlib.h>
#include <sys/sysinfo.h>

#include "tokens.h"
#include "ast_library.h"
#include "ast_printer.h"
#include "static_semantic_tests.h"
#include "scope.h"
#include "symbol.h"
#include "type_check.h"
#include "code_gen.h"

/* File used for generated assembly code */
#define ASSEMBLY_CODE_OUTPUT_FILE "generated_assembly_code.s"

/* Use the yyin FILE pointer created by Bison to
		support accepting the file to compile on
		the command line, rather than from standard input.*/
extern FILE *yyin;

extern int yylex();
extern char *yytext;

extern struct decl *parser_result;

/* The file used to write the generated assembly code */
FILE *ASM_CODE;

/* Test the lexer */
int just_lex() {
	while (1) {
		int t = yylex();
		if (t == 0) {
			break;
		}
		if (t == TOKEN_ERROR) {
			printf("ERROR. Unrecognized token: %s\n", yytext);
		} else {
			printf("token: %d  text: %s\n", t, yytext);
		}
	}
	return 0;
}

/* Print the AST */
void print_ast(char *description, struct decl *ast_root) {
	printf("\n************AST Output************\n");
	printf("***%s***\n", description);
	decl_print(ast_root);
}

/* Compile the code. This calls the functions to:
		1. Parse the source code (including lexical analysis)
		2. Resolve data types (decorate the AST)
		3. Check static semantics (type agreement)
		4. Generate assembly code */
int compile() {
	// Set this to 1 to see the stack data, 0 to suppress it
	yydebug = 0;
	if (yyparse() == 0) {
		printf("Parse successful\n");

		// Output a text-representation of the AST
		// Don't uncomment the following line until nodes are being placed in the AST
		print_ast("After Parse", parser_result);

		// *** Semantic analysis ***
		// Uncomment as you build out the resolver and the type checks
		printf("\nType Checking Begins...\n");
		scope_enter();	// initialize scope
		printf("\n Resolver\n");
		decl_resolve(parser_result);
		// Print the AST with symbols added (decorated)
		print_ast("After Decorating", parser_result);
		printf("\n Type checks\n");
		decl_typecheck(parser_result);
		if (typecheck_error_count()) {
		 	printf("!!Static semantic checks failed\n");
			return 1;
		} else {
			printf(">>Static semantics checks passed\n");
		}
		ASM_CODE = fopen(ASSEMBLY_CODE_OUTPUT_FILE, "w");
		decl_codegen(parser_result);
		printf("Assembly code written to %s\n", ASSEMBLY_CODE_OUTPUT_FILE);
	} else {
		printf("Parse failed.\n");
		return 1;
	}

	return 0;
}

int main(int argc, char **argv) {
	//return just_lex();

	// See static_semantic_tests.c and static_semantic_tests.h for testing code
	//test_scopes();
	//test_scopes_symbols();
	//test_type_equals();
	//test_type_copy();
	//test_type_delete();

	if (argc != 2) {
		fprintf(stderr, "Error: source file name must be supplied\n");
		fprintf(stderr, "       Usage: %s SOURCE_FILE_NAME\n", *argv);
		exit(1);
	}
	++argv;
	yyin = fopen(*argv, "r");
	if (!yyin) {
		fprintf(stderr, "Error: Source file not found: %s\n", *argv);
		exit(1);
	}
	return compile();
}
