#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>

#include "array.h"

typedef double float64_t;



/* typedef enum { */
/*     TOKEN_STRING, */
/*     TOKEN_CHARACTER, */
/*     TOKEN_FLOAT, */
/*     TOKEN_ATOM, */
/*     TOKEN_NEWLINE, */
/*     TOKEN_PARENTHESIS, */
/*     TOKEN_BRACKET */
/* } TokenType; */

/* const char* token_type_name(TokenType type) { */
/*     switch (type) { */
/*     case TOKEN_STRING:      return "TOKEN_STRING"; */
/*     case TOKEN_CHARACTER:   return "TOKEN_CHARACTER"; */
/*     case TOKEN_FLOAT:       return "TOKEN_FLOAT"; */
/*     case TOKEN_ATOM:        return "TOKEN_ATOM"; */
/*     case TOKEN_NEWLINE:     return "TOKEN_NEWLINE"; */
/*     case TOKEN_PARENTHESIS: return "TOKEN_PARENTHESIS"; */
/*     case TOKEN_BRACKET:     return "TOKEN_BRACKET"; */
/*     default:                assert(false && "Unhandled token type"); */
/*     } */
/* } */

/* #define MAX_TOKEN_SIZE 256 */

/* typedef union { */
/*     sstring_t as_string; */
/*     char      as_character; */
/*     double    as_float; */
/*     sstring_t as_atom; */
/*     char      as_newline; */
/*     char      as_parenthesis; */
/*     char      as_bracket; */
/* } Token; */

/* typedef struct { */
/*     TokenType type; */
/*     Token     token; */
/*     size_t    line; */
/*     size_t    column; */
/* } Lexeme; */

/* void lexeme_free(Lexeme* lexeme, array_free_t free) { */
/*     switch (lexeme->type) { */
/*     case TOKEN_STRING:      sstring_free(&lexeme->token.as_string, free); break; */
/*     case TOKEN_CHARACTER:   break; */
/*     case TOKEN_FLOAT:       break; */
/*     case TOKEN_ATOM:        sstring_free(&lexeme->token.as_atom, free); break; */
/*     case TOKEN_NEWLINE:     break; */
/*     case TOKEN_PARENTHESIS: break; */
/*     case TOKEN_BRACKET:     break; */
/*     default:                break; */
/*     } */
/* } */

/* typedef ARRAY_OF(Lexeme) LexemeArray; */

/* void lexeme_array_free(LexemeArray* lexemes, array_free_t free) { */
/*     for (size_t i = 0; i < lexemes->count; ++i) */
/*         lexeme_free(&array_at(lexemes, i), free); */
/*     array_free(lexemes, free); */
/* } */

/* typedef struct { */
/*     const sstring_t* program; */
/*     size_t           program_index; */
/*     const char*      program_name; */
/*     sstring_t        token_buffer; */
/*     LexemeArray      lexemes; */
/*     size_t           line; */
/*     size_t           column; */
/*     size_t           multibyte_line; */
/*     size_t           multibyte_column; */
/*     array_realloc_t  realloc; */
/*     bool             failed; */
/* } LexerContext; */

/* void try_append_multibyte_lexeme(LexerContext* context) { */
/*     if (0 == context->token_buffer.count) return; */

/*     Lexeme lexeme = {0}; */
/*     lexeme.line   = context->multibyte_line; */
/*     lexeme.column = context->multibyte_column; */

/*     SstringConvertResult result; */

/*     // We try to parse it as a float. */
/*     double as_float = sstring_to_double(&context->token_buffer, &result); */
/*     switch (result) { */
/*     case SSTRING_CONVERT_SUCCESS: { */
/*         lexeme.type           = TOKEN_FLOAT; */
/*         lexeme.token.as_float = as_float; */
/*         goto lend; */
/*     } break; */
/*     case SSTRING_CONVERT_UNDERFLOW: { */
/*         (void)fprintf( */
/*              stderr, */
/*              "%s(%zu:%zu): Error: Float conversion of '%" SSTRING_PRINT */
/*              "' results in underflow", */
/*              context->program_name, */
/*              lexeme.line, lexeme.column, */
/*              SSTRING_FORMAT(&context->token_buffer) */
/*         ); */
/*         context->failed = true; */
/*     } break; */
/*     case SSTRING_CONVERT_OVERFLOW: { */
/*         (void)fprintf( */
/*              stderr, */
/*              "%s(%zu:%zu): Error: Float conversion of '%" SSTRING_PRINT */
/*              "' results in overflow", */
/*              context->program_name, */
/*              lexeme.line, lexeme.column, */
/*              SSTRING_FORMAT(&context->token_buffer) */
/*         ); */
/*         context->failed = true; */
/*     } break; */
/*     case SSTRING_CONVERT_PARSE_FAIL: break; */
/*     default: assert(false && "Unhandled sstring conversion result"); */
/*     } */

/*     // If that fails, we assume it is an atom. */
/*     // TODO make this an array.h function. */
/*     lexeme.type = TOKEN_ATOM; */
/*     sstring_resize(&lexeme.token.as_atom, context->token_buffer.count, context->realloc); */
/*     (void)memcpy( */
/*          lexeme.token.as_atom.elements, */
/*          context->token_buffer.elements, */
/*          context->token_buffer.count */
/*     ); */
/*     lexeme.token.as_atom.count = context->token_buffer.count; */
/*     context->token_buffer.count = 0; */
/*     goto lend; */

/*  lend: */
/*     array_append(&context->lexemes, lexeme, context->realloc); */
/*     context->token_buffer.count = 0; */
/*     context->multibyte_line     = SIZE_MAX; */
/*     context->multibyte_column   = SIZE_MAX; */
/* } */

/* void lex_string(LexerContext* context) { */
/*     context->multibyte_line   = context->line; */
/*     context->multibyte_column = context->column; */

/*     // Skips past first quote. */
/*     ++context->column; */
/*     ++context->program_index; */

/*     sstring_t string          = {0}; */
/*     bool      found_end_quote = false; */

/*     for (; context->program_index < context->program->count; ++context->program_index) { */
/*         if (found_end_quote) break; */

/*         char character = sstring_at(context->program, context->program_index); */

/*         switch (character) { */
/*         case '"': { */
/*             found_end_quote = true; */
/*             ++context->column; */
/*         } break; */

/*         case '\n': { */
/*             ++context->line; */
/*             context->column = 0; */
/*         } break; */

/*         case '\\': { */
/*             if (++context->program_index >= context->program->count) { */
/*                 goto lunterminated_string; */
/*             } */
/*             character = sstring_at(context->program, context->program_index); */

/*             switch (character) { */
/*             case '"':  sstring_append(&string, character, context->realloc); break; */
/*             case '\\': sstring_append(&string, character, context->realloc); break; */
/*             case 'n':  sstring_append(&string, '\n',      context->realloc); break; */
/*             case 't':  sstring_append(&string, '\t',      context->realloc); break; */

/*             default: { */
/*                 (void)fprintf( */
/*                      stderr, */
/*                      "%s(%zu:%zu): Error: Unknown escape sequence '\\%c'", */
/*                      context->program_name, */
/*                      context->line, context->column, */
/*                      character */
/*                 ); */
/*                 context->failed = true; */
/*             } break; */
/*             }; */

/*             context->column += 2; */
/*         } break; */

/*         default: { */
/*             sstring_append(&string, character, context->realloc); */
/*             ++context->column; */
/*         } break; */
/*         }; */
/*     } */

/*     if (!found_end_quote) goto lunterminated_string; */

/*     Lexeme lexeme = { */
/*         .type   = TOKEN_STRING, */
/*         .token  = { .as_string = string }, */
/*         .line   = context->multibyte_line, */
/*         .column = context->multibyte_column */
/*     }; */
/*     array_append(&context->lexemes, lexeme, context->realloc); */

/*     context->multibyte_line   = SIZE_MAX; */
/*     context->multibyte_column = SIZE_MAX; */

/*     return; */

/*  lunterminated_string: */
/*     (void)fprintf( */
/*          stderr, */
/*          "%s(%zu:%zu): Error: Unterminated string", */
/*          context->program_name, */
/*          context->multibyte_line, context->multibyte_column */
/*     ); */
/*     exit(1); */
/* } */

/* void lex_character_literal(LexerContext* context) { */
/*     context->multibyte_line   = context->line; */
/*     context->multibyte_column = context->column; */

/*     // Skips past first quote. */
/*     ++context->column; ++context->program_index; */
/*     if (context->program_index >= context->program->count) { */
/*         goto lunterminated_character_literal; */
/*     } */

/*     char character = sstring_at(context->program, context->program_index); */

/*     switch (character) { */
/*     case '\\': { */
/*         ++context->column; ++context->program_index; */
/*         if (context->program_index >= context->program->count) { */
/*             goto lunterminated_character_literal; */
/*         } */

/*         character = sstring_at(context->program, context->program_index); */

/*         switch (character) { */
/*         case '\'':  character = '\''; break; */
/*         case '\\': character = '\\';  break; */
/*         case 'n':  character = '\n';  break; */
/*         case 't':  character = '\t';  break; */

/*         default: { */
/*             (void)fprintf( */
/*                  stderr, */
/*                  "%s(%zu:%zu): Error: Unknown escape sequence '\\%c'", */
/*                  context->program_name, */
/*                  context->line, context->column, */
/*                  character */
/*             ); */
/*             context->failed = true; */
/*         } break; */
/*         } */
/*     } break; */

/*     default: break; */
/*     }; */

/*     ++context->column; ++context->program_index; */
/*     if (context->program_index >= context->program->count) { */
/*         goto lunterminated_character_literal; */
/*     } */
/*     if ('\'' != sstring_at(context->program, context->program_index)) { */
/*         goto lunterminated_character_literal; */
/*     }; */

/*     ++context->column; ++context->program_index; */

/*     Lexeme lexeme = { */
/*         .type   = TOKEN_CHARACTER, */
/*         .token  = { .as_character = character }, */
/*         .line   = context->multibyte_line, */
/*         .column = context->multibyte_column */
/*     }; */
/*     array_append(&context->lexemes, lexeme, context->realloc); */

/*     context->multibyte_line   = SIZE_MAX; */
/*     context->multibyte_column = SIZE_MAX; */

/*     return; */

/*  lunterminated_character_literal: */
/*     (void)fprintf( */
/*          stderr, */
/*          "%s(%zu:%zu): Error: Unterminated character literal", */
/*          context->program_name, */
/*          context->multibyte_line, context->multibyte_column */
/*     ); */
/*     exit(1); */
/* } */

/* LexemeArray lex_program( const sstring_t *restrict program */
/*                        , const char *restrict program_name */
/*                        , array_free_t free */
/*                        , array_realloc_t realloc) { */
/*     LexerContext context = { */
/*         .program           = program, */
/*         .program_index     = 0, */
/*         .program_name      = program_name, */
/*         .token_buffer      = {0}, */
/*         .lexemes           = {0}, */
/*         .line              = 1, */
/*         .column            = 0, */
/*         .multibyte_line    = SIZE_MAX, */
/*         .multibyte_column  = SIZE_MAX, */
/*         .realloc           = realloc, */
/*         .failed            = false */
/*     }; */
/*     // Since we have a limit on how large tokens can be we can preallocate the */
/*     // buffer used to construct them. */
/*     sstring_resize(&context.token_buffer, MAX_TOKEN_SIZE, context.realloc); */

/*     for (; context.program_index < context.program->count; ++context.program_index) { */
/*         char character = sstring_at(context.program, context.program_index); */

/*         switch (character) { */
/*         case '\n': { */
/*             try_append_multibyte_lexeme(&context); */

/*             Lexeme lexeme = { */
/*                 .type   = TOKEN_NEWLINE, */
/*                 .token  = { .as_newline = character }, */
/*                 .line   = context.line, */
/*                 .column = context.column */
/*             }; */
/*             array_append(&context.lexemes, lexeme, context.realloc); */

/*             ++context.line; */
/*             context.column = 0; */
/*         } break; */

/*         case '(': */
/*         case ')': { */
/*             try_append_multibyte_lexeme(&context); */

/*             Lexeme lexeme = { */
/*                 .type   = TOKEN_PARENTHESIS, */
/*                 .token  = { .as_parenthesis = character }, */
/*                 .line   = context.line, */
/*                 .column = context.column */
/*             }; */
/*             array_append(&context.lexemes, lexeme, context.realloc); */

/*             ++context.column; */
/*         } break; */

/*         case '{': */
/*         case '}': { */
/*             try_append_multibyte_lexeme(&context); */

/*             Lexeme lexeme = { */
/*                 .type   = TOKEN_BRACKET, */
/*                 .token  = { .as_bracket = character }, */
/*                 .line   = context.line, */
/*                 .column = context.column */
/*             }; */
/*             array_append(&context.lexemes, lexeme, context.realloc); */

/*             ++context.column; */
/*         } break; */

/*         case '"': { */
/*             try_append_multibyte_lexeme(&context); */
/*             lex_string(&context); */
/*         } break; */

/*         case '\'': { */
/*             try_append_multibyte_lexeme(&context); */
/*             lex_character_literal(&context); */
/*         } break; */

/*         case ' ': */
/*         case '\t': { */
/*             try_append_multibyte_lexeme(&context); */
/*             ++context.column; */
/*         } break; */

/*         default: { */
/*             if (MAX_TOKEN_SIZE <= context.token_buffer.count) { */
/*                 (void)fprintf( */
/*                      stderr, */
/*                      "%s(%zu:%zu): Error: Encountered token larger than the" */
/*                      "maximum allowed size %zu: %" SSTRING_PRINT, */
/*                      context.program_name, */
/*                      context.line, context.column, */
/*                      (size_t)MAX_TOKEN_SIZE, */
/*                      SSTRING_FORMAT(&context.token_buffer) */
/*                 ); */
/*                 exit(1); */
/*             } */

/*             if (SIZE_MAX == context.multibyte_line) { */
/*                 context.multibyte_line = context.line; */
/*             } */
/*             if (SIZE_MAX == context.multibyte_column) { */
/*                 context.multibyte_column = context.column; */
/*             } */
/*             sstring_append(&context.token_buffer, character, context.realloc); */

/*             ++context.column; */
/*         } break; */
/*         } */
/*     } */

/*     try_append_multibyte_lexeme(&context); */

/*     if (context.failed) exit(1); */

/*     sstring_free(&context.token_buffer, free); */
/*     return context.lexemes; */
/* } */

/* void dump_lexemes( FILE *restrict stream */
/*                  , const LexemeArray *restrict lexemes */
/*                  , const char *restrict program_name) { */
/*     for (size_t i = 0; i < lexemes->count; ++i) { */
/*         const Lexeme* lexeme = &array_at(lexemes, i); */

/*         switch (lexeme->type) { */
/*         case TOKEN_STRING: { */
/*             (void)fprintf( */
/*                  stream, */
/*                  "%s(%zu:%zu): %s: \"", */
/*                  program_name, lexeme->line, lexeme->column, */
/*                  token_type_name(lexeme->type) */
/*             ); */

/*             for (size_t k = 0; k < lexeme->token.as_string.count; ++k) { */
/*                 char character = sstring_at(&lexeme->token.as_string, k); */
/*                 switch (character) { */
/*                 case '"': */
/*                 case '\\': { */
/*                     (void)fputc('\\', stream); */
/*                     (void)fputc(character, stream); */
/*                 } break; */

/*                 case '\n': { */
/*                     (void)fputs("\\n", stream); */
/*                 } break; */

/*                 case '\t': { */
/*                     (void)fputs("\\t", stream); */
/*                 } break; */

/*                 default: { */
/*                     (void)fputc(character, stream); */
/*                 } break; */
/*                 } */
/*             } */

/*             (void)fputs("\"\n", stream); */
/*         } break; */

/*         case TOKEN_CHARACTER: { */
/*             (void)fprintf( */
/*                  stream, */
/*                  "%s(%zu:%zu): %s: '", */
/*                  program_name, lexeme->line, lexeme->column, */
/*                  token_type_name(lexeme->type) */
/*             ); */

/*             char character = lexeme->token.as_character; */

/*             switch (character) { */
/*             case '\'': */
/*             case '\\': { */
/*                 (void)fputc('\\', stream); */
/*                 (void)fputc(character, stream); */
/*             } break; */

/*             case '\n': { */
/*                 (void)fputs("\\n", stream); */
/*             } break; */

/*             case '\t': { */
/*                 (void)fputs("\\t", stream); */
/*             } break; */

/*             default: { */
/*                 (void)fputc(character, stream); */
/*             } break; */
/*             } */

/*             (void)fputs("'\n", stream); */
/*         } break; */

/*         case TOKEN_FLOAT: { */
/*             (void)fprintf( */
/*                  stream, */
/*                  "%s(%zu:%zu): %s: %f\n", */
/*                  program_name, lexeme->line, lexeme->column, */
/*                  token_type_name(lexeme->type), */
/*                  lexeme->token.as_float */
/*             ); */
/*         } break; */

/*         case TOKEN_ATOM: { */
/*             (void)fprintf( */
/*                  stream, */
/*                  "%s(%zu:%zu): %s: %" SSTRING_PRINT "\n", */
/*                  program_name, lexeme->line, lexeme->column, */
/*                  token_type_name(lexeme->type), */
/*                  SSTRING_FORMAT(&lexeme->token.as_atom) */
/*             ); */
/*         } break; */

/*         case TOKEN_NEWLINE: { */
/*             (void)fprintf( */
/*                  stream, */
/*                  "%s(%zu:%zu): %s\n", */
/*                  program_name, lexeme->line, lexeme->column, */
/*                  token_type_name(lexeme->type) */
/*             ); */
/*         } break; */

/*         case TOKEN_PARENTHESIS: { */
/*             (void)fprintf( */
/*                  stream, */
/*                  "%s(%zu:%zu): %s: %c\n", */
/*                  program_name, lexeme->line, lexeme->column, */
/*                  token_type_name(lexeme->type), */
/*                  lexeme->token.as_parenthesis */
/*             ); */
/*         } break; */

/*         case TOKEN_BRACKET: { */
/*             (void)fprintf( */
/*                  stream, */
/*                  "%s(%zu:%zu): %s: %c\n", */
/*                  program_name, lexeme->line, lexeme->column, */
/*                  token_type_name(lexeme->type), */
/*                  lexeme->token.as_bracket */
/*             ); */
/*         } break; */

/*         default: assert(false && "Unhandled token type"); */
/*         }; */
/*     } */
/* } */



typedef enum {
    VALUE_NUMBER,
    VALUE_CHARACTER,
    VALUE_ARRAY
} ValueType;

typedef struct Value Value;

typedef ARRAY_OF(Value) ValueArray;

struct Value {
    ValueType type;
    union {
        float64_t  as_number;
        uint8_t    as_character;
        ValueArray as_array;
    };
};

/**
 * Frees the underlying memory of the value, if there is any.
 */
void value_free(Value* value) {
    switch (value->type) {
    case VALUE_ARRAY: {
        ValueArray* value_array = &value->as_array;
        for (size_t i = 0; i < value_array->count; ++i) {
            value_free(&value_array->elements[i]);
        }

        ARRAY_FREE(value_array, &array_stdlib_allocator);
    } break;

    case VALUE_NUMBER:
    case VALUE_CHARACTER: break;

    default: assert(0 && "Unreachable");
    }
}

/**
 * Performs a deep copy of the given value, recursively copying any nested
 * arrays.
 */
Value value_deep_copy(const Value* value) {
    switch (value->type) {
    case VALUE_ARRAY: {
        Value new_value = {
            .type     = VALUE_ARRAY,
            .as_array = {0}
        };
        new_value.as_array.count    = value->as_array.count;
        new_value.as_array.capacity = value->as_array.count;
        ARRAY_REALLOCATE(&new_value.as_array, &array_stdlib_allocator);

        for (size_t i = 0; i < value->as_array.count; ++i) {
            new_value.as_array.elements[i] = value_deep_copy(&value->as_array.elements[i]);
        }

        return new_value;
    } break;

    case VALUE_NUMBER:
    case VALUE_CHARACTER: return *value;

    default: assert(0 && "Unreachable");
    }
}



typedef enum {
    ERROR_OK,
    ERROR_DOMAIN,
    ERROR_SHAPE,
    ERROR_STACK_UNDERFLOW
} Error;

typedef enum {
    FUNCTION_NATIVE,
    FUNCTION_DEFUN,
    FUNCTION_LITERAL
} FunctionType;

typedef struct Function Function;

typedef ARRAY_OF(Function) FunctionArray;

struct Function {
    FunctionType type;
    union {
        Error(*as_native)(ValueArray*);
        FunctionArray as_defun;
        Value         as_literal;
    };
};

/**
 * Frees the underlying memory of the function, if there is any.
 */
void function_free(Function* function) {
    switch (function->type) {
    case FUNCTION_DEFUN: {
        for (size_t i = 0; i < function->as_defun.count; ++i) {
            function_free(&function->as_defun.elements[i]);
        }
    } break;

    case FUNCTION_LITERAL: {
        value_free(&function->as_literal);
    } break;

    case FUNCTION_NATIVE: break;

    default: assert(0 && "Unreachable");
    };
}



/**
 * Returns true if the arrays are of the same shape.
 * Being of the same "shape" means that the arrays are of the same length, and
 * any nested arrays are located at the same index in both arrays and are also
 * of the same length, so on so forth, recursively, for all nested arrays.
 */
bool compare_array_shapes(const ValueArray* array1, const ValueArray* array2) {
    if (array1->count != array2->count) return false;

    for (size_t i = 0; i < array1->count; ++i) {
        const Value* array1_element = &array1->elements[i];
        const Value* array2_element = &array2->elements[i];

        if (VALUE_ARRAY != array1_element->type && VALUE_ARRAY != array2_element->type) {
            continue;
        }

        if (VALUE_ARRAY != array1_element->type || VALUE_ARRAY != array2_element->type) {
            return false;
        }

        if (!compare_array_shapes(array1, array2)) {
            return false;
        }
    }

    return true;
}



/**
 * Performs a dyadic native function that works with numbers.
 *
 * On number,number - performs operation on numbers.
 * On character,* or *,character - domain error.
 * On array,number - recursively perform operation with argument 1 as the
 * elements of array and argument 2 as number.
 * On array,number - recursively perform operation with argument 1 as number and
 * argument 2 as the elements of array.
 * On array,array - if arrays of same shape, recursively perform operation with
 * the elements of array 1 as argument 1 and the elements of array 2 as argument
 * 2, else shape error.
 */
Error native_numeric_dyadic(ValueArray* stack, float64_t(*operation)(float64_t,float64_t)) {
    if (stack->count < 2) return ERROR_STACK_UNDERFLOW;

    Value* a = &stack->elements[stack->count - 2];
    Value* b = &stack->elements[stack->count - 1];

    switch (a->type) {
    case VALUE_NUMBER: {
        switch (b->type) {
        case VALUE_NUMBER: {
            a->as_number = operation(a->as_number, b->as_number);
            --stack->count;
        } break;
        case VALUE_CHARACTER: return ERROR_DOMAIN;
        case VALUE_ARRAY: {
            for (size_t i = 0; i < b->as_array.count; ++i) {
                Value* element = &b->as_array.elements[i];
                ARRAY_APPEND(stack, &array_stdlib_allocator, *a);
                ARRAY_APPEND(stack, &array_stdlib_allocator, *element);
                Error result = native_numeric_dyadic(stack, operation);
                if (ERROR_OK != result) return result;
                *element = stack->elements[stack->count - 1];
                --stack->count;
            }
            *a = *b;
            --stack->count;
        } break;
        default: assert(0 && "Unreachable");
        }
    } break;

    case VALUE_CHARACTER: return ERROR_DOMAIN;

    case VALUE_ARRAY: {
        switch (b->type) {
        case VALUE_NUMBER: {
            for (size_t i = 0; i < a->as_array.count; ++i) {
                Value* element = &a->as_array.elements[i];
                ARRAY_APPEND(stack, &array_stdlib_allocator, *element);
                ARRAY_APPEND(stack, &array_stdlib_allocator, *b);
                Error result = native_numeric_dyadic(stack, operation);
                if (ERROR_OK != result) return result;
                *element = stack->elements[stack->count - 1];
                --stack->count;
            }
            --stack->count;
        } break;
        case VALUE_CHARACTER: return ERROR_DOMAIN;
        case VALUE_ARRAY: {
            if (!compare_array_shapes(&a->as_array, &b->as_array)) {
                return ERROR_SHAPE;
            }
            for (size_t i = 0; i < a->as_array.count; ++i) {
                Value* a_element = &a->as_array.elements[i];
                Value* b_element = &b->as_array.elements[i];
                ARRAY_APPEND(stack, &array_stdlib_allocator, *a_element);
                ARRAY_APPEND(stack, &array_stdlib_allocator, *b_element);
                Error result = native_numeric_dyadic(stack, operation);
                if (ERROR_OK != result) return result;
                *a_element = stack->elements[stack->count - 1];
                --stack->count;
            }
            value_free(b);
            --stack->count;
        } break;
        default: assert(0 && "Unreachable");
        }
    } break;

    default: assert(0 && "Unreachable");
    }

    return ERROR_OK;
}

float64_t native_pona_operation(float64_t a, float64_t b) { return a + b; }
/**
 * Addition - dyadic.
 *
 * On number,number - add numbers
 * On character,* or *,character - domain error.
 * On array,number or number,array - Recursively add number to the elements of
 * the array.
 * On array,array - if arrays of same shape, recursively adds individual
 * elements together, else
 * shape error.
 */
Error native_pona(ValueArray* stack) {
    return native_numeric_dyadic(stack, &native_pona_operation);
}

float64_t native_ike_operation(float64_t a, float64_t b) { return a - b; }
/**
 * Subtraction - dyadic.
 *
 * On number,number - subtract numbers
 * On character,* or *,character - domain error.
 * On array,number - recursively subtract number from elements of array.
 * On number,array - recursively set elements of array to number subtract
 * element.
 * On array,array - if arrays of same shape, recursively subtracts individual
 * elements from eachother, else shape error.
 */
Error native_ike(ValueArray* stack) {
    return native_numeric_dyadic(stack, &native_ike_operation);
}

float64_t native_mute_operation(float64_t a, float64_t b) { return a * b; }
/**
 * Multiplication - dyadic.
 *
 * On number,number - multiply numbers
 * On character,* or *,character - domain error.
 * On array,number or number,array - Recursively multiply number to the elements
 * of the array.
 * On array,array - if arrays of same shape, recursively multiplies individual
 * elements together, else shape error.
 */
Error native_mute(ValueArray* stack) {
    return native_numeric_dyadic(stack, &native_mute_operation);
}

float64_t native_kipisi_operation(float64_t a, float64_t b) { return a / b; }
/**
 * Divide - dyadic.
 *
 * On number,number - divide numbers
 * On character,* or *,character - domain error.
 * On array,number - recursively divide elements of array by number.
 * On number,array - recursively set elements of array to number divided by
 * element.
 * On array,array - if arrays of same shape, recursively divides individual
 * elements by eachother, else shape error.
 */
Error native_kipisi(ValueArray* stack) {
    return native_numeric_dyadic(stack, &native_kipisi_operation);
}

/**
 * Index generator - monadic
 *
 * On number - generate array of numbers [ 1, 2, 3, ..., n ].
 * On character - domain error.
 * On array - TODO.
 */
Error native_nanpa(ValueArray* stack) {
    if (stack->count < 1) return ERROR_STACK_UNDERFLOW;

    Value* a = &stack->elements[stack->count - 1];

    switch (a->type) {
    case VALUE_NUMBER: {
        float64_t max_index = a->as_number;

        Value index_array = {
            .type = VALUE_ARRAY,
            .as_array = {0}
        };
        Value index = {0};
        index.type = VALUE_NUMBER;

        for (float64_t i = 1; i <= max_index; ++i) {
            index.as_number = i;
            // TODO: make preallocate memory.
            ARRAY_APPEND(&index_array.as_array, &array_stdlib_allocator, index);
        }

        stack->elements[stack->count - 1] = index_array;
    } break;

    case VALUE_CHARACTER: return ERROR_DOMAIN;

    case VALUE_ARRAY: assert(0 && "TODO");

    default: assert(0 && "Unreachable");
    }

    return ERROR_OK;
}

/**
 * Concatenate - dyadic.
 *
 * On *,* - joins the elements/values into a single array.
 */
Error native_olin(ValueArray* stack) {
    if (stack->count < 2) return ERROR_STACK_UNDERFLOW;

    Value* a = &stack->elements[stack->count - 2];
    Value* b = &stack->elements[stack->count - 1];

    switch (a->type) {
    case VALUE_NUMBER: {
        switch (b->type) {
        case VALUE_NUMBER:
        case VALUE_CHARACTER: {
            Value result = {
                .type     = VALUE_ARRAY,
                .as_array = {0}
            };
            ARRAY_APPEND(&result.as_array, &array_stdlib_allocator, *a);
            ARRAY_APPEND(&result.as_array, &array_stdlib_allocator, *b);
            *a = result;
            --stack->count;
        } break;

        case VALUE_ARRAY: {
            ARRAY_PREPEND(&b->as_array, &array_stdlib_allocator, *a);
            *a = *b;
            --stack->count;
        } break;

        default: assert(0 && "Unreachable");
        }
    } break;

    case VALUE_CHARACTER: {
        switch (b->type) {
        case VALUE_NUMBER:
        case VALUE_CHARACTER: {
            Value result = {
                .type     = VALUE_ARRAY,
                .as_array = {0}
            };
            ARRAY_APPEND(&result.as_array, &array_stdlib_allocator, *a);
            ARRAY_APPEND(&result.as_array, &array_stdlib_allocator, *b);
            *a = result;
            --stack->count;
        } break;

        case VALUE_ARRAY: {
            ARRAY_PREPEND(&b->as_array, &array_stdlib_allocator, *a);
            *a = *b;
            --stack->count;
        } break;

        default: assert(0 && "Unreachable");
        }
    } break;

    case VALUE_ARRAY: {
        switch (b->type) {
        case VALUE_NUMBER:
        case VALUE_CHARACTER: {
            ARRAY_APPEND(&a->as_array, &array_stdlib_allocator, *b);
            --stack->count;
        } break;

        case VALUE_ARRAY: {
            ARRAY_CONCATENATE(&b->as_array, &array_stdlib_allocator, &a->as_array);
            ARRAY_FREE(&b->as_array, &array_stdlib_allocator);
            --stack->count;
        } break;

        default: assert(0 && "Unreachable");
        }
    } break;

    default: assert(0 && "Unreachable");
    }

    return ERROR_OK;
}

/**
 * Call command - monadic.
 *
 * On character array - call the system shell with the supplied text as the
 * command.
 * On *,* - domain error.
 */
Error native_o(ValueArray* stack) {
    if (stack->count < 1) return ERROR_STACK_UNDERFLOW;

    Value* a = &stack->elements[stack->count - 1];
    if (VALUE_ARRAY != a->type) return ERROR_DOMAIN;

    char command[a->as_array.count + 1];
    command[a->as_array.count] = '\0';

    for (size_t i = 0; i < a->as_array.count; ++i) {
        Value* element = &a->as_array.elements[i];
        if (VALUE_CHARACTER != element->type) {
            return ERROR_DOMAIN;
        }

        command[i] = (char)element->as_character;
    }

    system(command);

    --stack->count;
    return ERROR_OK;
}

Error execute_functions(const FunctionArray* functions, ValueArray* stack) {
    for (size_t i = 0; i < functions->count; ++i) {
        const Function* function = &functions->elements[i];
        Error result;

        switch (function->type) {
        case FUNCTION_DEFUN: {
            result = execute_functions(&function->as_defun, stack);
        } break;
        case FUNCTION_NATIVE: {
            result = function->as_native(stack);
        } break;
        case FUNCTION_LITERAL: {
            ARRAY_APPEND(
                stack,
                &array_stdlib_allocator,
                value_deep_copy(&function->as_literal)
            );
            result = ERROR_OK;
        } break;
        default: assert(0 && "Unreachable");
        };

        if (ERROR_OK != result) {
            return result;
        }
    }

    return ERROR_OK;
}



/* #define SOURCE_FILE     "test.tlpin" */
/* #define READ_CHUNK_SIZE 1024 */

void dump_stack(const ValueArray* stack) {
    for (size_t i = 0; i < stack->count; ++i) {
        const Value* value = &stack->elements[i];

        switch (value->type) {
        case VALUE_NUMBER:    (void)printf("%lf ", value->as_number);    break;
        case VALUE_CHARACTER: (void)printf("%c ",  value->as_character); break;

        case VALUE_ARRAY: {
            (void)printf("{ ");
            dump_stack(&value->as_array);
            (void)printf("} ");
        } break;

        default: assert(0 && "Unreachable");
        };
    }
}

#define ARRAY_SIZE(array) sizeof(array)/sizeof(array[0])

const Function initial_program[] = {
    {
        .type = FUNCTION_LITERAL,
        .as_literal = {
            .type = VALUE_CHARACTER,
            .as_character = 'l'
        }
    },
    {
        .type = FUNCTION_LITERAL,
        .as_literal = {
            .type = VALUE_CHARACTER,
            .as_character = 's'
        }
    },
    {
        .type = FUNCTION_NATIVE,
        .as_native = &native_olin
    },
    {
        .type = FUNCTION_NATIVE,
        .as_native = &native_o
    }
};

int main(void) {
    /* FILE* source = fopen(SOURCE_FILE, "r"); */
    /* if (NULL == source) { */
    /*     perror("Error: Unable to open file '" SOURCE_FILE "'"); */
    /*     return 1; */
    /* }; */
    /* sstring_t contents = sstring_read_file(source, READ_CHUNK_SIZE, &realloc); */
    /* (void)fclose(source); */

    /* LexemeArray lexemes = lex_program(&contents, SOURCE_FILE, &free, &realloc); */
    /* sstring_free(&contents, &free); */

    /* dump_lexemes(stdout, &lexemes, SOURCE_FILE); */
    /* //cparse_program(&lexemes); */
    /* lexeme_array_free(&lexemes, &free); */

    ValueArray stack = {0};

    FunctionArray program = {0};
    ARRAY_APPEND_MANY(
        &program,
        &array_stdlib_allocator,
        initial_program,
        ARRAY_SIZE(initial_program)
    );
    /* Value test = { */
    /*     .type = VALUE_NUMBER, */
    /*     .as_number = 12 */
    /* }; */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */
    /* ARRAY_APPEND(&program.elements[1].as_literal.as_array, &array_stdlib_allocator, test); */

    Error result = execute_functions(&program, &stack);
    switch (result) {
    case ERROR_DOMAIN:          fprintf(stderr, "DOMAIN ERROR\n");     exit(1);
    case ERROR_SHAPE:           fprintf(stderr, "SHAPE ERROR\n");      exit(1);
    case ERROR_STACK_UNDERFLOW: fprintf(stderr, "STACK UNDERFLOW\n");  exit(1);
    case ERROR_OK:     break;
    default:           assert(0 && "Unreachable");
    }

    (void)printf("Stack dump: ");
    dump_stack(&stack);

    // Cleanup.
    for (size_t i = 0; i < stack.count; ++i) {
        value_free(&stack.elements[i]);
    }
    ARRAY_FREE(&stack, &array_stdlib_allocator);
    for (size_t i = 0; i < program.count; ++i) {
        function_free(&program.elements[i]);
    }
    ARRAY_FREE(&program, &array_stdlib_allocator);

    return 0;
}
