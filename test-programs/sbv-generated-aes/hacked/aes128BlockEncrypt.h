/* Header file for aes128BlockEncrypt. Automatically generated by SBV. Do not edit! */

#ifndef __aes128BlockEncrypt__HEADER_INCLUDED__
#define __aes128BlockEncrypt__HEADER_INCLUDED__

#include <inttypes.h>
#include <stdint.h>

/* Unsigned bit-vectors */
typedef uint8_t  SBool  ;
typedef uint8_t  SWord8 ;
typedef uint16_t SWord16;
typedef uint32_t SWord32;
typedef uint64_t SWord64;

/* Signed bit-vectors */
typedef int8_t  SInt8 ;
typedef int16_t SInt16;
typedef int32_t SInt32;
typedef int64_t SInt64;

/* Entry point prototype: */
void aes128BlockEncrypt(const SWord32 *pt, const SWord32 *key,
                        SWord32 *ct);

#endif /* __aes128BlockEncrypt__HEADER_INCLUDED__ */
