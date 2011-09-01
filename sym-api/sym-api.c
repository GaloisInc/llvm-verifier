#include <sym-api.h>

#define FRESH(ty, def) { return def; }
#define EVAL(ty, sym)  { return sym; }
#define FRESH_ARRAY(ty, size, def) { \
        ty *buf = malloc(size * sizeof(ty));            \
        int i;                                          \
        for (i = 0; i < size; i++) buf[i] = def;        \
        return buf;                                     \
    }

uint8_t  fresh_uint8  (uint8_t def)  FRESH(uint8_t,  def);
uint16_t fresh_uint16 (uint16_t def) FRESH(uint16_t, def);
uint32_t fresh_uint32 (uint32_t def) FRESH(uint32_t, def);
uint64_t fresh_uint64 (uint64_t def) FRESH(uint64_t, def);

uint8_t*  fresh_array_uint8  (uint32_t size, uint8_t def)
    FRESH_ARRAY(uint8_t,  size, def);
uint16_t* fresh_array_uint16 (uint32_t size, uint16_t def)
    FRESH_ARRAY(uint16_t, size, def);
uint32_t* fresh_array_uint32 (uint32_t size, uint32_t def)
    FRESH_ARRAY(uint32_t, size, def);
uint64_t* fresh_array_uint64 (uint32_t size, uint64_t def)
    FRESH_ARRAY(uint64_t, size, def);

void write_aiger_uint8  (uint8_t  sym, uint8_t *filename) {}
void write_aiger_uint16 (uint16_t sym, uint8_t *filename) {}
void write_aiger_uint32 (uint32_t sym, uint8_t *filename) {}
void write_aiger_uint64 (uint64_t sym, uint8_t *filename) {}

void write_aiger_array_uint8  (uint8_t*  sym, uint32_t size,
                               uint8_t *filename) {}
void write_aiger_array_uint16 (uint16_t* sym, uint32_t size,
                               uint8_t *filename) {}
void write_aiger_array_uint32 (uint32_t* sym, uint32_t size,
                               uint8_t *filename) {}
void write_aiger_array_uint64 (uint64_t* sym, uint32_t size,
                               uint8_t *filename) {}

uint8_t  eval_aiger_uint8  (uint8_t  sym, uint8_t *input_bits)
    EVAL(uint8_t, sym);
uint16_t eval_aiger_uint16 (uint16_t sym, uint8_t *input_bits)
    EVAL(uint16_t, sym);
uint32_t eval_aiger_uint32 (uint32_t sym, uint8_t *input_bits)
    EVAL(uint32_t, sym);
uint64_t eval_aiger_uint64 (uint64_t sym, uint8_t *input_bits)
    EVAL(uint64_t, sym);

uint8_t*  eval_aiger_array_uint8  (uint8_t  *sym, uint32_t size,
                                   uint8_t *input_bits) EVAL(uint8_t, sym);
uint16_t* eval_aiger_array_uint16 (uint16_t *sym, uint32_t size,
                                   uint8_t *input_bits) EVAL(uint16_t, sym);
uint32_t* eval_aiger_array_uint32 (uint32_t *sym, uint32_t size,
                                   uint8_t *input_bits) EVAL(uint32_t, sym);
uint64_t* eval_aiger_array_uint64 (uint64_t *sym, uint32_t size,
                                   uint8_t *input_bits) EVAL(uint64_t, sym);

void override_function_by_name(uint8_t *from, uint8_t *to) {}
void override_function_by_addr(void *from, void *to) {}

void print_symbolic(void *sym) {}
