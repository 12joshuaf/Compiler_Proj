# Makefile for the semantic analysis

PROGRAM=b-minor-compiler

OBJS=b-minor-parser-ast.o b-minor-scanner.o ast_printer.o main.o static_semantic_tests.o hash_table.o code_gen_util.o code_gen.o

PRIOR_OBJS=ast_library.o scope.o symbol.o type_check.o

SRCS=b-minor-scanner.c b-minor-parser-ast.c ast_printer.c main.c static_semantic_tests.c hash_table.c code_gen_util.c code_gen.c

CC=cc

all:$(PROGRAM)

ast_printer.o: ast_printer.c ast_printer.h
	$(CC) -c -o $@ ast_printer.c

static_semantic_tests.o: static_semantic_tests.c static_semantic_tests.h
	$(CC) -c -o $@ static_semantic_tests.c

hash_table.o: hash_table.c hash_table.h
	$(CC) -c -o $@ hash_table.c

code_gen_util.o: code_gen_util.c code_gen_util.h
		$(CC) -c -o $@ code_gen_util.c

code_gen.o: code_gen.c code_gen.h
			$(CC) -c -o $@ code_gen.c

b-minor-compiler:$(OBJS)  $(PRIOR_OBJS)
	$(CC) $(OBJS) $(PRIOR_OBJS) -o $@ -lm

clean:;	rm -f $(OBJS) b-minor-program generated_assembly_code.s core ∗~ *.output *.tab.? b-minor-compiler

b-minor-program: generated_assembly_code.s b_minor_library.c
		$(CC) -o $@ generated_assembly_code.s b_minor_library.c
