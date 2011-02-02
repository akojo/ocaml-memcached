//-----------------------------------------------------------------------------
// MurmurHash2, by Austin Appleby
//
// From the murmurhash www page:
//
// All code is released to the public domain. For business purposes, Murmurhash
// is under the MIT license.
// 

// Note - This code makes a few assumptions about how your machine behaves -

// 1. We can read a 4-byte value from any address without crashing
// 2. sizeof(int) == 4

// And it has a few limitations -

// 1. It will not work incrementally.
// 2. It will not produce the same results on little-endian and big-endian
//    machines.

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <stdio.h>

static unsigned int Murmurhash2(const void *key, int len, unsigned int seed)
{
    // 'm' and 'r' are mixing constants generated offline.
    // They're not really 'magic', they just happen to work well.
    const unsigned int m = 0x5bd1e995;
    const int r = 24;

    // Initialize the hash to a 'random' value
    unsigned int h = seed ^ len;

    // Mix 4 bytes at a time into the hash
    const unsigned char * data = (const unsigned char *)key;

    while (len >= 4) {
        unsigned int k = *(unsigned int *)data;

        k *= m; 
        k ^= k >> r; 
        k *= m; 

        h *= m; 
        h ^= k;

        data += 4;
        len -= 4;
    }

    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.
    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;

    return h;
} 

value mm_hash2(value data)
{
    CAMLparam1(data);
    unsigned int h = Murmurhash2(String_val(data),
                                 Wosize_val(data) * sizeof(long), 0)
    CAMLreturn(Long_val(h));
}
