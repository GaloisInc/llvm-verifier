#ifndef _SYM_API_H_
#define _SYM_API_H_

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#include <stdint.h>
#include <stdlib.h>

#define NO_INL __attribute__ ((noinline))

/* Note: the following uses uint32_t for sizes, rather than size_t, so
 * that the types of these functions are not dependent on the target
 * architecture. */

uint8_t  lss_fresh_uint8  (uint8_t def) NO_INL;
uint16_t lss_fresh_uint16 (uint16_t def) NO_INL;
uint32_t lss_fresh_uint32 (uint32_t def) NO_INL;
uint64_t lss_fresh_uint64 (uint64_t def) NO_INL;

/* Note: the lss_fresh_array_* functions take an optional third
   parameter for supplying array values during concrete execution.  If
   'defs' is null, the 'def' parameter is used to populate the array
   entirely with the same value.  If 'defs' non-null, the 'def'
   parameter is ignored, and 'defs' is expected to point to an array of
   the appropriate concrete size.
 */
uint8_t  *lss_fresh_array_uint8  (uint32_t size, uint8_t def, uint8_t *defs) NO_INL;
uint16_t *lss_fresh_array_uint16 (uint32_t size, uint16_t def, uint16_t *defs) NO_INL;
uint32_t *lss_fresh_array_uint32 (uint32_t size, uint32_t def, uint32_t *defs) NO_INL;
uint64_t *lss_fresh_array_uint64 (uint32_t size, uint64_t def, uint64_t *defs) NO_INL;

/* This familiy of functions allows incremental designation of AIG
   outputs; the collected AIG outputs are cleared upon writing the AIG
   via the lss_write_aiger() function. */
void lss_aiger_add_output_uint8( uint8_t  sym) NO_INL;
void lss_aiger_add_output_uint16(uint16_t sym) NO_INL;
void lss_aiger_add_output_uint32(uint32_t sym) NO_INL;
void lss_aiger_add_output_uint64(uint64_t sym) NO_INL;
void lss_aiger_add_output_array_uint8 (uint8_t *sym, uint32_t size)  NO_INL;
void lss_aiger_add_output_array_uint16(uint16_t *sym, uint32_t size) NO_INL;
void lss_aiger_add_output_array_uint32(uint32_t *sym, uint32_t size) NO_INL;
void lss_aiger_add_output_array_uint64(uint64_t *sym, uint32_t size) NO_INL;
    
/* Write collected outputs (i.e., those collected via the
   lss_aiger_add_output* functions).  Note that upon return from this
   function, the list of collected outputs is cleared. */
void lss_write_aiger(char *filename) NO_INL;

typedef struct SMTLIB1_script SMTLIB1_script;

/**
 * Create a new SMTLIB1 script.
 */
SMTLIB1_script* lss_SMTLIB1_create(const char* name) NO_INL;

/**
 * Add assumption that @v@ is non-zero.
 *
 * @param s Script to add to.
 * @param v Value to check.
 */
void lss_SMTLIB1_assumption_nonzero_uint8(SMTLIB1_script* s,  uint8_t v) NO_INL;

/**
 * Add formula that @v@ is non-zero.
 *
 * @param s Script to add to.
 * @param v Value to check.
 */
void lss_SMTLIB1_formula_nonzero_uint8(SMTLIB1_script* s,  uint8_t v) NO_INL;

/**
 * Write SMTLIB1 Script to file.
 * 
 * @param s Script to write.
 * @param path Path to write to.
 */
void lss_SMTLIB1_write(SMTLIB1_script* s, const char* path) NO_INL;

/**
 * Free the SMTLIB1 Script.
 */ 
void lss_SMTLIB1_free(SMTLIB1_script* s) NO_INL;

static
void lss_SMTLIB1_write_nonzero_uint8(const char* name, const char* path, uint8_t v) {
  SMTLIB1_script* s;
  s = lss_SMTLIB1_create(name);
  lss_SMTLIB1_formula_nonzero_uint8(s, v);
  lss_SMTLIB1_write(s, path);
  lss_SMTLIB1_free(s);
}

typedef struct SMTLIB2_script SMTLIB2_script;

/**
 * Create a new SMTLIB2 script.
 */
SMTLIB2_script* lss_SMTLIB2_create(void) NO_INL;

/**
 * Add assertion that @v@ is non-zero.
 *
 * @param s Script to add assertion to.
 * @param v Value to check.
 */
void lss_SMTLIB2_assert_nonzero_uint8(SMTLIB2_script* s,  uint8_t v) NO_INL;

/**
 * Add a check-sat command to SMTLIB file.
 */
void lss_SMTLIB2_check_sat(SMTLIB2_script* s) NO_INL;

/**
 * Write SMTLIB2 Script to file.
 * 
 * @param s Script to write.
 * @param path Path to write to.
 */
void lss_SMTLIB2_write(SMTLIB2_script* s, const char* path) NO_INL;

/**
 * Free the SMTLIB2 Script.
 */ 
void lss_SMTLIB2_free(SMTLIB2_script* s) NO_INL;

static
void lss_SMTLIB2_write_nonzero_uint8(const char* path, uint8_t v) {
  SMTLIB2_script* s;
  s = lss_SMTLIB2_create();
  lss_SMTLIB2_assert_nonzero_uint8(s, v);
  lss_SMTLIB2_check_sat(s);
  lss_SMTLIB2_write(s, path);
  lss_SMTLIB2_free(s);
}

void lss_write_aiger_uint8  (uint8_t  sym, char *filename) NO_INL;
void lss_write_aiger_uint16 (uint16_t sym, char *filename) NO_INL;
void lss_write_aiger_uint32 (uint32_t sym, char *filename) NO_INL;
void lss_write_aiger_uint64 (uint64_t sym, char *filename) NO_INL;

void lss_write_aiger_array_uint8  (uint8_t  *sym, uint32_t size,
                                   char *filename) NO_INL;
void lss_write_aiger_array_uint16 (uint16_t *sym, uint32_t size,
                                   char *filename) NO_INL;
void lss_write_aiger_array_uint32 (uint32_t *sym, uint32_t size,
                                   char *filename) NO_INL;
void lss_write_aiger_array_uint64 (uint64_t *sym, uint32_t size,
                                   char *filename) NO_INL;

uint8_t  lss_eval_aiger_uint8  (uint8_t  sym, uint8_t *input_bits,
                                uint32_t input_size) NO_INL;
uint16_t lss_eval_aiger_uint16 (uint16_t sym, uint8_t *input_bits,
                                uint32_t input_size) NO_INL;
uint32_t lss_eval_aiger_uint32 (uint32_t sym, uint8_t *input_bits,
                                uint32_t input_size) NO_INL;
uint64_t lss_eval_aiger_uint64 (uint64_t sym, uint8_t *input_bits,
                                uint32_t input_size) NO_INL;

void lss_eval_aiger_array_uint8  (uint8_t  *sym, uint8_t *out, uint32_t size,
                                  uint8_t *input_bits, uint32_t input_size)
    NO_INL;
void lss_eval_aiger_array_uint16 (uint16_t *sym, uint16_t *out, uint32_t size,
                                  uint8_t *input_bits, uint32_t input_size)
    NO_INL;
void lss_eval_aiger_array_uint32 (uint32_t *sym, uint32_t *out, uint32_t size,
                                  uint8_t *input_bits, uint32_t input_size)
    NO_INL;
void lss_eval_aiger_array_uint64 (uint64_t *sym, uint64_t *out, uint32_t size,
                                  uint8_t *input_bits, uint32_t input_size)
    NO_INL;


void lss_write_cnf(uint32_t sym, const char *filename) NO_INL;

void lss_override_function_by_name(char *from, char *to) NO_INL;
void lss_override_function_by_addr(void *from, void *to) NO_INL;
void lss_override_llvm_intrinsic(char* name, void* impl) NO_INL;
void lss_override_reset_by_name(char *name) NO_INL; /* Equivalent to lss_override_function_by_name(x,x) */
void lss_override_reset_by_addr(void* fp) NO_INL; /* Equivalent to lss_override_function_by_addr(x,x) */

/* NB: This resets all user-overridden functions AND intrinsics */
void lss_override_reset_all(void) NO_INL;

void lss_print_symbolic(void *sym) NO_INL;

void lss_abort(const char* msg) NO_INL;

void lss_show_path(void) NO_INL;

void lss_show_mem(void) NO_INL;

void lss_set_verbosity(int v) NO_INL;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
