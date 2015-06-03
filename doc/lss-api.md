Symbolic API Reference
===============

The functions listed in this document can be called by programs that are  running inside the lss simulator.
They allow the user to specify symbolic inputs and produce formal models of programs which can be
output in the AIGER format, SMTLib versions 1 or 2, CNF format, or as SAWCore terms.  In addition,
they allow more sophisticated interaction with the symbolic simulator, including installing
override functions and getting debugging information from the simulator.

Bit Packing
---------

When writing files in AIGER format or when concretely evaluating symbolic values,
it is important to understand how the C/LLVM datatypes are encoded into a flat sequence
of bits.  Basically, every type is packed into a list of bits in the order you would expect
(earliest declared inputs first) with primitive integer and floating-point values packed
with most-significant bit first.  We follow the rules listed below:

* Symbolic inputs are packed in the order they are created.  Note, this follows the dynamic
 symbolic program execution, not necessarily the static program order.
* Arrays are packed in index order, with low indices first.
* Structures are packed with their fields in the order they are declared.  No padding is added.
* Integer values are packed via their 2's complement representation in most-significant-bit first order.
* Floating point values are treated as their IEEE 754 bit-level representations, packed in most-significant bit first order.


lss_fresh_\*
---------

<pre>
<code>uint8_t  lss_fresh_uint8  (uint8_t def)</code>
<code>uint16_t  lss_fresh_uint16  (uint16_t def)</code>
<code>uint32_t  lss_fresh_uint32  (uint32_t def)</code>
<code>uint64_t  lss_fresh_uint64 (uint64_t def)</code>
</pre>

When run inside the symbolic simulator, the lss_fresh family of functions
generate a fresh symbolic input variable 8, 16, 32, or 64 bits wide; the
input argument is ignored.  When compiled directly via a C compiler, these
functions simply return their argument unchanged.

lss_fresh_array_\*
--------------

<pre>
<code>uint8_t  *lss_fresh_array_uint8  (uint32_t size, uint8_t def, uint8_t *defs)</code>
<code>uint16_t *lss_fresh_array_uint16 (uint32_t size, uint16_t def, uint16_t *defs)</code>
<code>uint32_t *lss_fresh_array_uint32 (uint32_t size, uint32_t def, uint32_t *defs)</code>
<code>uint64_t *lss_fresh_array_uint64 (uint32_t size, uint64_t def, uint64_t *defs)</code>
</pre>

When run inside the symbolic simulator, the lss_fresh_array family of functions generates
arrays of length `size`, each element of which consists of fresh a symbolic input variable
of 8, 16, 32, or 64 bits; the second and third argument are ignored.

When compiled via a C compiler, an array of `size` length is allocated and filled according to
the values `def` and `defs`.  If `defs` is non-NULL, its values are copied into the newly
allocated array; if it is NULL, `def` is used to fill in the values.  If non-NULL `defs` must
be at least `size` long.

lss_aiger_add_output_\*
-------------------
<pre>
<code>void lss_aiger_add_output_uint8(uint8_t  sym)</code>
<code>void lss_aiger_add_output_uint16(uint16_t sym)</code>
<code>void lss_aiger_add_output_uint32(uint32_t sym)</code>
<code>void lss_aiger_add_output_uint64(uint64_t sym)</code>
</pre>

The lss_aiger_add_output_* family of functions allows incremental designation of AIG
outputs.  The designated outputs are buffered until the next invocation of `lss_write_aiger`,
after which the output buffer is cleared.


lss_aiger_add_output_array_\*
-----------------------
<pre>
<code>void lss_aiger_add_output_array_uint8 (uint8_t *sym, uint32_t size)</code>
<code>void lss_aiger_add_output_array_uint16(uint16_t *sym, uint32_t size)</code>
<code>void lss_aiger_add_output_array_uint32(uint32_t *sym, uint32_t size)</code>
<code>void lss_aiger_add_output_array_uint64(uint64_t *sym, uint32_t size)</code>
</pre>

The lss_aiger_add_output_array_* family of functions allows incremental designation
of AIG outputs for an entire array.  The given pointers are treated as arrays of length `size`
and added to the AIG output buffer.

lss_write_aiger
------------
<pre>
<code>void lss_write_aiger(char *filename)</code>
</pre>

Write a formal model in AIGER format into `filename` consisting of all the outputs
previously designated using lss_aiger_add_* functions.  After this write, pending
outputs will be cleared.


lss_write_aiger_\*
-------------
<pre>
<code>void lss_write_aiger_uint8  (uint8_t  sym, char *filename)</code>
<code>void lss_write_aiger_uint16 (uint16_t sym, char *filename)</code>
<code>void lss_write_aiger_uint32 (uint32_t sym, char *filename)</code>
<code>void lss_write_aiger_uint64 (uint64_t sym, char *filename)</code>

<code>void lss_write_aiger_array_uint8  (uint8_t  *sym, uint32_t size, char *filename)</code>
<code>void lss_write_aiger_array_uint16 (uint16_t *sym, uint32_t size, char *filename)</code>
<code>void lss_write_aiger_array_uint32 (uint32_t *sym, uint32_t size, char *filename)</code>
<code>void lss_write_aiger_array_uint64 (uint64_t *sym, uint32_t size, char *filename)</code>
</pre>

Designate outputs and write an AIGER file in one step.  These operations do not affect
the currently-buffered outputs.


lss_eval_aiger_\*
-------------
<pre>
<code>uint8_t  lss_eval_aiger_uint8  (uint8_t  sym, uint8_t *input_bits, uint32_t input_size)</code>
<code>uint16_t lss_eval_aiger_uint16 (uint16_t sym, uint8_t *input_bits, uint32_t input_size)</code>
<code>uint32_t lss_eval_aiger_uint32 (uint32_t sym, uint8_t *input_bits, uint32_t input_size)</code>
<code>uint64_t lss_eval_aiger_uint64 (uint64_t sym, uint8_t *input_bits, uint32_t input_size)</code>
</pre>

Concretely evaluate a symbolic value by providing values to the symbolic inputs.  `input_bits` is expected
to be an array of `input_size`, each byte of which is interpreted as a boolean value corresponding to an
input bit.  See  the bit packing section above for an explanation of how bits are interpreted as higher-level values.

lss_eval_aiger_array_\*
------------------
<pre>
<code>void lss_eval_aiger_array_uint8  (uint8_t  *sym, uint8_t *out, uint32_t size, uint8_t *input_bits, uint32_t input_size)</code>
<code>void lss_eval_aiger_array_uint16 (uint16_t *sym, uint16_t *out, uint32_t size, uint8_t *input_bits, uint32_t input_size)</code>
<code>void lss_eval_aiger_array_uint32 (uint32_t *sym, uint32_t *out, uint32_t size, uint8_t *input_bits, uint32_t input_size)</code>
<code>void lss_eval_aiger_array_uint64 (uint64_t *sym, uint64_t *out, uint32_t size, uint8_t *input_bits, uint32_t input_size)</code>
</pre>

Concretely evaluate an array of symbolic values by providing values for the symbolic inputs.  `input_bits` is expected
to be an array of `input_size`, each byte of which is interpreted as a boolean value corresponding to an
input bit.  See the bit packing section above for an explanation of how bits are interpreted as higher-level values.  `sym` should be an
array of `size` symbolic elements to evaluate. `out` should be an allocated array of `size` elements into which the
evaluated values will be written.

lss_write_smtlib1
--------------
<pre>
<code>void lss_write_smtlib1(uint32_t sym, const char *filename)</code>
</pre>

Write an SMTLib version 1 file into `filename` which asserts that `sym`
is equal to  the constant value 0.

lss_write_smtlib2
--------------
<pre>
<code>void lss_write_smtlib2(uint32_t sym, const char *filename)</code>
</pre>

Write an SMTLib version 2 file into `filename` which asserts that `sym`
is equal to  the constant value 0.

lss_write_cnf
-----------
<pre>
<code>void lss_write_cnf(uint32_t sym, const char *filename)</code>
</pre>

Write a CNF file in DIMACS format asserting that `sym` is equal to the constant
value 0.

lss_write_sawcore_unit32
---------------------
<pre>
<code>void lss_write_sawcore_uint32(uint32_t sym, const char *filename)</code>
</pre>

Write a SAWCore term representing the value of `sym` into `filename` using the
SAWCore external term representation.

lss_override_function_by_name
-------------------------
TODO

lss_override_function_by_addr
------------------------
TODO

lss_override_llvm_intrinsic
----------------------
TODO

lss_override_reset_by_name
-----------------------
TODO

lss_override_reset_by_addr
-----------------------
TODO

lss_override_reset_all
-----------------
TODO

lss_print_symbolic
---------------
<pre>
<code>void lss_print_symbolic(void *sym)</code>
</pre>

Given a symbolic expression, print a representation of it on standard output.


lss_abort
---------------
<pre>
<code>void lss_abort(const char* msg)</code>
</pre>

Abort the current path of execution with message.

lss_show_path
------------
TODO


lss_show_mem
-------------
TODO


lss_set_verbosity
--------------
<pre>
<code>void lss_set_verbosity(int v)</code>
</pre>

Change the verbosity value of the simulator.   Higher verbosity
levels generate more informational and debugging output
while the program is being executed.
