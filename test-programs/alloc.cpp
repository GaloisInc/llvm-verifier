#include <stdlib.h>
#include <string>

void* allocate_memory(size_t sz) { return malloc(sz); }


char* new_char(size_t sz) { return new char[sz]; }

std::string* new_string(size_t sz) { return new std::string[sz]; }

