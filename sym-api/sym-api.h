#ifndef _SYM_API_H_
#define _SYM_API_H_

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#include <stdint.h>
#include <stdlib.h>

/* Note: the following uses uint32_t for sizes, rather than size_t, so
 * that the types of these functions are not dependent on the target
 * architecture. */

uint8_t  lss_fresh_uint8  (uint8_t def);
uint16_t lss_fresh_uint16 (uint16_t def);
uint32_t lss_fresh_uint32 (uint32_t def);
uint64_t lss_fresh_uint64 (uint64_t def);

uint8_t  *lss_fresh_array_uint8  (uint32_t size, uint8_t def);
uint16_t *lss_fresh_array_uint16 (uint32_t size, uint16_t def);
uint32_t *lss_fresh_array_uint32 (uint32_t size, uint32_t def);
uint64_t *lss_fresh_array_uint64 (uint32_t size, uint64_t def);

void lss_write_aiger_uint8  (uint8_t  sym, char *filename);
void lss_write_aiger_uint16 (uint16_t sym, char *filename);
void lss_write_aiger_uint32 (uint32_t sym, char *filename);
void lss_write_aiger_uint64 (uint64_t sym, char *filename);

void lss_write_aiger_array_uint8  (uint8_t  *sym, uint32_t size,
                               char *filename);
void lss_write_aiger_array_uint16 (uint16_t *sym, uint32_t size,
                               char *filename);
void lss_write_aiger_array_uint32 (uint32_t *sym, uint32_t size,
                               char *filename);
void lss_write_aiger_array_uint64 (uint64_t *sym, uint32_t size,
                               char *filename);

uint8_t  lss_eval_aiger_uint8  (uint8_t  sym, uint8_t *input_bits,
                            uint32_t input_size);
uint16_t lss_eval_aiger_uint16 (uint16_t sym, uint8_t *input_bits,
                            uint32_t input_size);
uint32_t lss_eval_aiger_uint32 (uint32_t sym, uint8_t *input_bits,
                            uint32_t input_size);
uint64_t lss_eval_aiger_uint64 (uint64_t sym, uint8_t *input_bits,
                            uint32_t input_size);

void lss_eval_aiger_array_uint8  (uint8_t  *sym, uint8_t *out, uint32_t size,
                              uint8_t *input_bits, uint32_t input_size);
void lss_eval_aiger_array_uint16 (uint16_t *sym, uint16_t *out, uint32_t size,
                              uint8_t *input_bits, uint32_t input_size);
void lss_eval_aiger_array_uint32 (uint32_t *sym, uint32_t *out, uint32_t size,
                              uint8_t *input_bits, uint32_t input_size);
void lss_eval_aiger_array_uint64 (uint64_t *sym, uint64_t *out, uint32_t size,
                              uint8_t *input_bits, uint32_t input_size);

void lss_override_function_by_name(char *from, char *to);
void lss_override_function_by_addr(void *from, void *to);

void lss_print_symbolic(void *sym);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

void lss_abort(const char* msg);

#endif
