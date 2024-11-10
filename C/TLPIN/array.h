/*
 * zlib license
 *
 * Copyright (c) 2024 ona-li-toki-e-jan-Epiphany-tawa-mi
 *
 * This software is provided ‘as-is’, without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 * claim that you wrote the original software. If you use this software
 * in a product, an acknowledgment in the product documentation would be
 * appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 * distribution.
 */

/*
 * Dead simple dynamic arrays.
 *
 * Preprocessor parameters:
 * - ARRAY_INITIAL_CAPACITY - The initial capacity of dynamic arrays. Has
 *   default value.
 * - ARRAY_CAPACITY_MULTIPLIER - How much to scale the size of dynamic arrays by
 *   on resizing. Has default value.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>



#ifndef ARRAY_INITIAL_CAPACITY
#define ARRAY_INITIAL_CAPACITY 10
#endif // ARRAY_INITIAL_CAPACITY

#ifndef ARRAY_CAPACITY_MULTIPLIER
#define ARRAY_CAPACITY_MULTIPLIER 2
#endif // ARRAY_CAPACITY_MULTIPLIER



/**
 * Allocater type to pass to array functions to perform the memory allocations.
 * In most cases, you'll just want to use array_stdlib_allocator.
 */
typedef struct {
    void*(*realloc)(void*, size_t);
    void(*free)(void*);
} array_allocator_t;

const array_allocator_t array_stdlib_allocator = {
    .realloc = &realloc,
    .free    = &free
};



/**
 * Creates an array type that stores the given type. Intantiations of this type
 * should be zero-initialized to start.
 */
#define ARRAY_OF(type)                          \
    struct {                                    \
        type*  elements;                        \
        size_t count;                           \
        size_t capacity;                        \
    }

/**
 * Frees the memory allocated by the dynamic array and resets it.
 * @param array     (ARRAY_OF(type)*).
 * @param allocator (array_allocator_t*) - memory allocator to use.
 */
#define ARRAY_FREE(array, allocator)            \
    do {                                        \
        (allocator)->free((array)->elements);   \
        (array)->elements = NULL;               \
        (array)->count    = 0;                  \
        (array)->capacity = 0;                  \
    } while (0)

/**
 * Computes the size, in bytes, of the dynamic array's elements.
 * @param array (ARRAY_OF(type)*).
 */
#define ARRAY_ELEMENT_BYTE_SIZE(array) (sizeof(*(array)->elements))

/**
 * Computes the size, in bytes, of the occupied portion of the dynamic array.
 * @param array (ARRAY_OF(type)*).
 */
#define ARRAY_OCCUPIED_BYTE_SIZE(array) ((array)->count * ARRAY_ELEMENT_BYTE_SIZE((array)))

/**
 * Computes the size, in bytes, of the dynamic array.
 * @param array (ARRAY_OF(type)*).
 */
#define ARRAY_BYTE_SIZE(array) ((array)->capacity * ARRAY_ELEMENT_BYTE_SIZE((array)))

/**
 * Swaps the contents of the dynamic arrays.
 * @param array1 (ARRAY_OF(type)*).
 * @param array2 (ARRAY_OF(type)*).
 */
#define ARRAY_SWAP(array1, array2)                      \
    do {                                                \
        void* array1_elements = (array1)->elements;     \
        (array1)->elements    = (array2)->elements;     \
        (array2)->elements    = array1_elements;        \
                                                        \
        size_t array1_count = (array1)->count;          \
        (array1)->count     = (array2)->count;          \
        (array2)->count     = array1_count;             \
                                                        \
        size_t array1_capacity = (array1)->capacity;    \
        (array1)->capacity     = (array2)->capacity;    \
        (array2)->capacity     = array1_capacity;       \
    } while (0)

/**
 * Reallocates the memory of the dynamic array if it's capacity has changed.
 * @param array     (ARRAY_OF(type)*).
 * @param allocator (array_allocator_t*) - memory allocator to use.
 */
#define ARRAY_REALLOCATE(array, allocator)                                      \
    do {                                                                        \
        (array)->elements = (allocator)->realloc(                               \
            (array)->elements,                                                  \
            ARRAY_BYTE_SIZE((array))                                            \
        );                                                                      \
        if (NULL == (array)->elements) {                                        \
            (void)fputs(                                                        \
                 "Error: Unable to reallocate dynamic array; buy more RAM lol", \
                 stderr                                                         \
            );                                                                  \
            exit(1);                                                            \
        }                                                                       \
    } while (0)

/**
 * Resizes the dynamic array to the given size. If the size specified is smaller
 * than the array's current size, the array will be truncated.
 * Has no effect if the current and specified size are the same.
 * @param array     (ARRAY_OF(type)*).
 * @param allocator (array_allocator_t*) - memory allocator to use.
 * @param size      (size_t).
 */
#define ARRAY_RESIZE(array, allocator, size)        \
    do {                                            \
        if ((size) != (array)->capacity) {          \
            if ((size) < (array)->count)            \
                (array)->count = (size);            \
            (array)->capacity = (size);             \
            ARRAY_REALLOCATE((array), (allocator)); \
        }                                           \
    } while (0)

/**
 * Makes an identical copy of the source array with the same size and capacity,
 * but with it's own copy of the memory.
 * @param array_from (ARRAY_OF(type)*).
 * @param allocator  (array_allocator_t*) - memory allocator to use.
 * @param array_to   (ARRAY_OF(type)*).
 */
#define ARRAY_COPY(array_from, allocator, array_to)    \
    do {                                               \
        (array_to)->count    = (array_from)->count;    \
        (array_to)->capacity = (array_from)->capacity; \
        ARRAY_REALLOCATE((array_to), (allocator));     \
        (void)memcpy(                                  \
            (array_to)->elements,                      \
            (array_from)->elements,                    \
            ARRAY_OCCUPIED_BYTE_SIZE(array_from)       \
        );                                             \
    } while (0)

/**
 * Makes an identical copy of the source array with the same size, but the
 * capacity is trimmed to the occupied portion and with it's own copy of the
 * memory.
 * @param array_from (ARRAY_OF(type)*).
 * @param allocator  (array_allocator_t*) - memory allocator to use.
 * @param array_to   (ARRAY_OF(type)*).
 */
#define ARRAY_COPY_TRIM(array_from, allocator, array_to) \
    do {                                                     \
        (array_to)->count    = (array_from)->count;          \
        (array_to)->capacity = (array_from)->count;          \
        ARRAY_REALLOCATE((array_to), (allocator));           \
        (void)memcpy(                                        \
            (array_to)->elements,                            \
            (array_from)->elements,                          \
            ARRAY_OCCUPIED_BYTE_SIZE(array_from)             \
        );                                                   \
    } while (0)

/**
 * Increases the size of the dynamic array by the given amount.
 * @param array     (ARRAY_OF(type)*).
 * @param allocator (array_allocator_t*) - memory allocator to use.
 * @param size      (size_t).
 */
#define ARRAY_EXPAND(array, allocator, size)      \
    do {                                          \
        (array)->capacity += (size);              \
        ARRAY_REALLOCATE((array), (allocator));   \
    } while (0)

/**
 * Appends an element to the dynamic array.
 * @param array     (ARRAY_OF(type)*).
 * @param allocator (array_allocator_t*) - memory allocator to use.
 * @param element   (type).
 */
#define ARRAY_APPEND(array, allocator, element)                                \
    do {                                                                       \
        if ((array)->count >= (array)->capacity) {                             \
            (array)->capacity = 0 == (array)->capacity                         \
                              ? ARRAY_INITIAL_CAPACITY                         \
                              : ARRAY_CAPACITY_MULTIPLIER * (array)->capacity; \
                                                                               \
            ARRAY_REALLOCATE((array), (allocator));                            \
        }                                                                      \
        (array)->elements[(array)->count++] = (element);                       \
    } while (0)

/**
 * Appends multiple elements to the dynamic array.
 * @param array         (ARRAY_OF(type)*).
 * @param allocator     (array_allocator_t*) - memory allocator to use.
 * @param buffer        (type*).
 * @param element_count (size_t).
 */
#define ARRAY_APPEND_MANY(array, allocator, buffer, element_count)          \
    do {                                                                    \
        if ((element_count) + (array)->count >= (array)->capacity) {        \
            if (0 == (array)->capacity) {                                   \
                (array)->capacity = ARRAY_INITIAL_CAPACITY;                 \
            }                                                               \
            while ((element_count) + (array)->count >= (array)->capacity) { \
                (array)->capacity *= ARRAY_CAPACITY_MULTIPLIER;             \
            }                                                               \
            ARRAY_REALLOCATE((array), (allocator));                         \
        }                                                                   \
                                                                            \
        (void)memcpy(                                                       \
             (array)->elements + (array)->count,                            \
             (buffer),                                                      \
             (element_count)*sizeof(*(buffer))                              \
         );                                                                 \
        (array)->count += (element_count);                                  \
    } while (0)

/**
 * Appends the elements of the source array to the destination array.
 * @param array_from (ARRAY_OF(type)*).
 * @param allocator  (array_allocator_t*) - memory allocator to use.
 * @param array_to   (ARRAY_OF(type)*).
 */
#define ARRAY_CONCATENATE(array_from, allocator, array_to) \
    ARRAY_APPEND_MANY(                                     \
        (array_to),                                        \
        (allocator),                                       \
        (array_from)->elements,                            \
        (array_from)->count                                \
    )

/**
 * Prepends an element to the dynamic array.
 * @param array     (ARRAY_OF(type)*).
 * @param allocator (array_allocator_t*) - memory allocator to use.
 * @param element   (type).
 */
#define ARRAY_PREPEND(array, allocator, element)                               \
    do {                                                                       \
        if ((array)->count >= (array)->capacity) {                             \
            (array)->capacity = 0 == (array)->capacity                         \
                              ? ARRAY_INITIAL_CAPACITY                         \
                              : ARRAY_CAPACITY_MULTIPLIER * (array)->capacity; \
                                                                               \
            ARRAY_REALLOCATE((array), (allocator));                            \
        }                                                                      \
        (void)memmove(                                                         \
             (array)->elements+1,                                              \
             (array)->elements,                                                \
             ARRAY_OCCUPIED_BYTE_SIZE((array))                                 \
        );                                                                     \
        ++(array)->count;                                                      \
        (array)->elements[0] = (element);                                      \
    } while (0)
