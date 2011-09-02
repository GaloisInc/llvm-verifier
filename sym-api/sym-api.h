#ifndef _SYM_API_H_
#define _SYM_API_H_

#include <stdint.h>
#include <stdlib.h>

/* Note: the following uses uint32_t for sizes, rather than size_t, so
 * that the types of these functions are not dependent on the target
 * architecture. Similarly, we use uint8_t instead of char. */

uint8_t  fresh_uint8  (uint8_t def);
uint16_t fresh_uint16 (uint16_t def);
uint32_t fresh_uint32 (uint32_t def);
uint64_t fresh_uint64 (uint64_t def);

uint8_t  *fresh_array_uint8  (uint32_t size, uint8_t def);
uint16_t *fresh_array_uint16 (uint32_t size, uint16_t def);
uint32_t *fresh_array_uint32 (uint32_t size, uint32_t def);
uint64_t *fresh_array_uint64 (uint32_t size, uint64_t def);

void write_aiger_uint8  (uint8_t  sym, uint8_t *filename);
void write_aiger_uint16 (uint16_t sym, uint8_t *filename);
void write_aiger_uint32 (uint32_t sym, uint8_t *filename);
void write_aiger_uint64 (uint64_t sym, uint8_t *filename);

void write_aiger_array_uint8  (uint8_t  *sym, uint32_t size,
                               uint8_t *filename);
void write_aiger_array_uint16 (uint16_t *sym, uint32_t size,
                               uint8_t *filename);
void write_aiger_array_uint32 (uint32_t *sym, uint32_t size,
                               uint8_t *filename);
void write_aiger_array_uint64 (uint64_t *sym, uint32_t size,
                               uint8_t *filename);

uint8_t  eval_aiger_uint8  (uint8_t  sym, uint8_t *input_bits);
uint16_t eval_aiger_uint16 (uint16_t sym, uint8_t *input_bits);
uint32_t eval_aiger_uint32 (uint32_t sym, uint8_t *input_bits);
uint64_t eval_aiger_uint64 (uint64_t sym, uint8_t *input_bits);

uint8_t *eval_aiger_array_uint8  (uint8_t  *sym, uint32_t size,
                                   uint8_t *input_bits);
uint16_t *eval_aiger_array_uint16 (uint16_t *sym, uint32_t size,
                                   uint8_t *input_bits);
uint32_t *eval_aiger_array_uint32 (uint32_t *sym, uint32_t size,
                                   uint8_t *input_bits);
uint64_t *eval_aiger_array_uint64 (uint64_t *sym, uint32_t size,
                                   uint8_t *input_bits);

void override_function_by_name(uint8_t *from, uint8_t *to);
void override_function_by_addr(void *from, void *to);

void print_symbolic(void *sym);

#endif
