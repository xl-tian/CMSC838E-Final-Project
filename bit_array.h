#ifndef BIT_ARRAY_H
#define BIT_ARRAY_H

#include <stdio.h>
#include <stdlib.h>

// This bit map code is not written by me. It is obtained online with slight modifications.
// Bit map is a very common data structure with well known implementation. Furthermore,
// because using a bit map is only an optional memory optimization, it is not essential
// to the cache friendly garbage collector algorithms. Hence, I think it is okay 
// to use online code as a library for the bit map.


// Structure to represent a bit array
typedef struct {
    unsigned int *data; // Array to store bits
    size_t size;        // Number of bits in the array
} BitArray;

// Function to create a bit array
BitArray *bit_array_create(size_t size) {
    BitArray *bit_array = (BitArray *)malloc(sizeof(BitArray));
    if (bit_array == NULL) {
        return NULL;
    }

    // Calculate the number of unsigned integers needed
    size_t num_ints = (size + 31) / 32; // Assuming 32-bit integers

    bit_array->data = (unsigned int *)calloc(num_ints, sizeof(unsigned int));
    if (bit_array->data == NULL) {
        free(bit_array);
        return NULL;
    }

    bit_array->size = size;
    return bit_array;
}

// Function to set a bit in the bit array
void bit_array_set(BitArray *bit_array, size_t index) {
    if (index >= bit_array->size) {
        return; // Index out of bounds
    }

    size_t int_index = index / 32;
    size_t bit_index = index % 32;
    bit_array->data[int_index] |= (1 << bit_index);
}

// Function to clear a bit in the bit array
void bit_array_clear(BitArray *bit_array, size_t index) {
     if (index >= bit_array->size) {
        return; // Index out of bounds
    }

    size_t int_index = index / 32;
    size_t bit_index = index % 32;
    bit_array->data[int_index] &= ~(1 << bit_index);
}

// Function to test a bit in the bit array
int bit_array_test(BitArray *bit_array, size_t index) {
    if (index >= bit_array->size) {
        return 0; // Index out of bounds, return 0
    }
  size_t int_index = index / 32;
    size_t bit_index = index % 32;
    return (bit_array->data[int_index] >> bit_index) & 1;
}

// Function to destroy a bit array
void bit_array_destroy(BitArray *bit_array) {
    free(bit_array->data);
    free(bit_array);
}

void bit_array_zero(BitArray *bit_array) {
    for (size_t i = 0; i < bit_array->size; ++i) {
        bit_array->data[i] = 0;
    }
}

#endif

// int main() {
//     BitArray *bit_array = bit_array_create(100);
//     if (bit_array == NULL) {
//         printf("Failed to create bit array.\n");
//         return 1;
//     }

//     bit_array_set(bit_array, 5);
//     bit_array_set(bit_array, 20);
//     bit_array_set(bit_array, 75);

//     printf("Bit 5: %d\n", bit_array_test(bit_array, 5));   // Output: 1
//     printf("Bit 10: %d\n", bit_array_test(bit_array, 10));  // Output: 0
//     printf("Bit 20: %d\n", bit_array_test(bit_array, 20));  // Output: 1
//     printf("Bit 75: %d\n", bit_array_test(bit_array, 75));  // Output: 1

//   bit_array_clear(bit_array, 20);
//     printf("Bit 20 after clearing: %d\n", bit_array_test(bit_array, 20)); // Output: 0

//     bit_array_destroy(bit_array);
//     return 0;
// }