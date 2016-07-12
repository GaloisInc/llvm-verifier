#include <sym-api.h>
#include <stdio.h>

#define FRESH(ty, def) { return def; }
#define EVAL(ty, sym)  { return sym; }
#define EVAL_ARRAY(ty, sym, out, size) {                \
        int i;                                          \
        for(i = 0; i < size; i++) out[i] = sym[i];      \
    }
#define FRESH_ARRAY(ty, size, def, defs) {                  \
        int i;                                              \
        ty *buf = malloc(size * sizeof(ty));                \
        for(i = 0; i < size; ++i)                           \
            buf[i] = defs ? defs[i] : def;                  \
        return buf;                                         \
    }

uint8_t  lss_fresh_uint8  (uint8_t def)  FRESH(uint8_t,  def);
uint16_t lss_fresh_uint16 (uint16_t def) FRESH(uint16_t, def);
uint32_t lss_fresh_uint32 (uint32_t def) FRESH(uint32_t, def);
uint64_t lss_fresh_uint64 (uint64_t def) FRESH(uint64_t, def);

uint8_t*  lss_fresh_array_uint8  (uint32_t size, uint8_t def, uint8_t *defs)
    FRESH_ARRAY(uint8_t,  size, def, defs);
uint16_t* lss_fresh_array_uint16 (uint32_t size, uint16_t def, uint16_t *defs)
    FRESH_ARRAY(uint16_t, size, def, defs);
uint32_t* lss_fresh_array_uint32 (uint32_t size, uint32_t def, uint32_t *defs) 
    FRESH_ARRAY(uint32_t, size, def, defs);
uint64_t* lss_fresh_array_uint64 (uint32_t size, uint64_t def, uint64_t *defs)
    FRESH_ARRAY(uint64_t, size, def, defs);

void lss_aiger_add_output_uint8(uint8_t sym)   {}
void lss_aiger_add_output_uint16(uint16_t sym) {}
void lss_aiger_add_output_uint32(uint32_t sym) {}
void lss_aiger_add_output_uint64(uint64_t sym) {}
void lss_aiger_add_output_array_uint8 (uint8_t *sym, uint32_t size)  {}
void lss_aiger_add_output_array_uint16(uint16_t *sym, uint32_t size) {}
void lss_aiger_add_output_array_uint32(uint32_t *sym, uint32_t size) {}
void lss_aiger_add_output_array_uint64(uint64_t *sym, uint32_t size) {}
void lss_write_aiger(char *filename) {}

void lss_write_aiger_uint8  (uint8_t  sym, char *filename) {}
void lss_write_aiger_uint16 (uint16_t sym, char *filename) {}
void lss_write_aiger_uint32 (uint32_t sym, char *filename) {}
void lss_write_aiger_uint64 (uint64_t sym, char *filename) {}

void lss_write_aiger_array_uint8  (uint8_t*  sym, uint32_t size,
                                   char *filename) {}
void lss_write_aiger_array_uint16 (uint16_t* sym, uint32_t size,
                                   char *filename) {}
void lss_write_aiger_array_uint32 (uint32_t* sym, uint32_t size,
                                   char *filename) {}
void lss_write_aiger_array_uint64 (uint64_t* sym, uint32_t size,
                                   char *filename) {}

uint8_t  lss_eval_aiger_uint8  (uint8_t  sym, uint8_t *input_bits,
                                uint32_t input_size)
    EVAL(uint8_t, sym);
uint16_t lss_eval_aiger_uint16 (uint16_t sym, uint8_t *input_bits,
                                uint32_t input_size)
    EVAL(uint16_t, sym);
uint32_t lss_eval_aiger_uint32 (uint32_t sym, uint8_t *input_bits,
                                uint32_t input_size)
    EVAL(uint32_t, sym);
uint64_t lss_eval_aiger_uint64 (uint64_t sym, uint8_t *input_bits,
                                uint32_t input_size)
    EVAL(uint64_t, sym);

void lss_eval_aiger_array_uint8  (uint8_t  *sym, uint8_t  *out, uint32_t size,
                                  uint8_t *input_bits, uint32_t input_size)
    EVAL_ARRAY(uint8_t, sym, out, size);
void lss_eval_aiger_array_uint16 (uint16_t *sym, uint16_t *out, uint32_t size,
                                  uint8_t *input_bits, uint32_t input_size)
    EVAL_ARRAY(uint16_t, sym, out, size);
void lss_eval_aiger_array_uint32 (uint32_t *sym, uint32_t *out, uint32_t size,
                                  uint8_t *input_bits, uint32_t input_size)
    EVAL_ARRAY(uint32_t, sym, out, size);
void lss_eval_aiger_array_uint64 (uint64_t *sym, uint64_t *out, uint32_t size,
                                  uint8_t *input_bits, uint32_t input_size)
    EVAL_ARRAY(uint64_t, sym, out, size);

void lss_write_cnf(uint32_t sym, const char *filename) {}
void lss_write_cnf_tmp(uint32_t sym) {}
void lss_write_sawcore_uint32(uint32_t sym, const char *filename) {}
void lss_write_smtlib2(uint32_t sym, const char *filename) {}
void lss_write_smtlib2_tmp(uint32_t sym) {}

void lss_override_function_by_name (char *from, char *to) {}
void lss_override_function_by_addr (void *from, void *to) {}
void lss_override_llvm_intrinsic(char* name, void* impl) {}
void lss_override_reset_by_name(char *name) {}
void lss_override_reset_by_addr(void* fp) {}
void lss_override_reset_all() {}

void lss_print_symbolic (void *sym) {}
void lss_abort (const char *msg) {}
void lss_show_path() {}
void lss_show_mem() {}
void lss_set_verbosity(int v) {}
