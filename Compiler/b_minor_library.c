#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

/*
This is an extended version of the standard library
of functions for B-Minor, implemented in C.

The print statement in B-Minor expects there to exist a function
for each type that can be printed.  So, the following B-Minor code:

x: int = 10;
b: boolean = true;
x: string = "hello";

print x, b, s;

Is effectively translated to the following C code:

print_integer(x);
print_boolean(b);
print_string(s);

And the following B-Minor code:

x = a ^ b;

Is effectively this C code:

x = integer_power(a,b);
*/

/* Integer printing to standard output */
void print_integer( long x )
{
	printf("%ld",x);
}

/* String printing to standard output */
void print_string( const char *s )
{
	printf("%s",s);
}

/* Boolean printing to standard output */
void print_boolean( int b )
{
	printf("%s",b?"true":"false");
}

/* Character printing to standard output */
void print_character( char c )
{
	printf("%c",c);
}

/* Integer-based exponentiation */
long integer_power( long x, long y )
{
	long result = 1;
	while(y>0) {
		result = result * x;
		y = y -1;
	}
	return result;
}

/* DSR extensions to the library to support basic
    input from the keyboard

    There is a function for obtaining keyboard
    input for each scalar type */

/* Function prototype for retrieving a string
    from the standard input */
char *get_string_from_keyboard(int max_chars);

/* Retrieve an integer from the standard input */
long get_int_from_keyboard() {
  char *sval = get_string_from_keyboard(100);
  long val = atoi(sval);
  free(sval);
  return val;
}

/* Retrieve a Boolean from the standard input */
long get_boolean_from_keyboard() {
  char *sval = get_string_from_keyboard(100);
  long val = atoi(sval);
  free(sval);
  return val != 0;
}

/* Retrieve a character from the standard input */
char get_character_from_keyboard() {
  char *sval = get_string_from_keyboard(100);
  char val = *sval;
  free(sval);
  return val;
}

/*
  Function to retrieve a String from the standard input.

  The caller must pass the maximum number
  of characters allowed.

  The caller should free the returned string
  since it is dynamically allocated.
*/
char *get_string_from_keyboard(int max_chars) {
  if (max_chars <= 0) {
    return "";
  }

  char *val = malloc(sizeof(char) * (max_chars + 1));
  if (!val) {
    printf("Failed to allocated memory to read input\n");
    return "";
  }

  fgets(val, max_chars + 1, stdin);

  return val;
}

/*
  Function to obtain a random integer in the range 0 to n-1
*/
long randint(long n) {
    static int initialized = 0;
    if (!initialized) {
        srand(time(NULL));
        initialized = 1;
    }

    if (n < 2) {
        printf("\n\nERROR: randint requires a parameter greater than 1. Using default of 100\n\n");
        n = 100;
    }

    return rand() % n;
}

/*
    Function to obtain a character from a string.

    The caller must pass the string and the desired
    position. If the position is beyond the length,
    a null character ('\0') is returned.
*/
char get_char_at(const char *s, int position) {
    if (position < strlen(s)) {
        return s[position];
    } else {
        printf("ERROR: position for character outside bounds of string (len:%d)\n", strlen(s));
        return '\0';
    }
}

/*
    Function to insert a character into a string.

    The caller must pass the string, the desired
    position, and the character to insert.
    If the position is beyond the length,
    of the string. If update is successful
    one (1) is returned, otherwise zero (0)
    is returned.
*/
int set_char_at(char *s, int position, char new_char) {
    if (position < strlen(s)) {
        s[position] = new_char;
        return 1;
    } else {
        printf("ERROR: position for insertion outside bounds of string (len:%d)\n", strlen(s));
        return 0;
    }
}

/*
    Function to emit \a, the alert character,
    to emit an audible signal.
*/
void beep() {
    putchar('\a');
}
