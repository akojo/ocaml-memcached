/*
 * A tool for generating hash values using functions from libhashkit.
 *
 * Takes as its sole argument the name of the hash function to apply to the
 * input.
 *
 * For each input line, calculates the hash value of the line (sans newline)
 * and outputs the original line and its hash value separated by a tab.
 *
 * Surprisingly, this program requires libhashkit (or libmemcached) to work.
 *
 * License follows:
 *
 * Copyright (c) 2011 Atte Kojo <atte.kojo@gmail.com>
 *
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libhashkit/hashkit.h>

typedef struct {
    char *name;
    uint32_t (*hashfunc)(const char *key, size_t key_length);
} hashdesc_t;

static hashdesc_t hashfuncs[] = {
    { "crc32", libhashkit_crc32 },
    { "murmur", libhashkit_murmur },
};
#define NHASHES (sizeof(hashfuncs) / sizeof(hashdesc_t))

static void usage(void)
{
    int i;

    printf("Usage: genhash <hashname>\n");
    printf("Available hash functions:\n");
    for (i = 0; i < NHASHES; i++)
        printf("\t%s\n", hashfuncs[i].name);
}

int main(int argc, char *argv[])
{
    int i;
    char buf[BUFSIZ];
    uint32_t (*hash)(const char *key, size_t key_length);

    if (argc != 2) {
        usage();
        exit(1);
    }

    for (i = 0; i < NHASHES; i++)
        if (!strcmp(hashfuncs[i].name, argv[1])) {
            hash = hashfuncs[i].hashfunc;
            break;
        }

    if (i == NHASHES) {
        usage();
        exit(2);
    }

    while (fgets(buf, BUFSIZ, stdin) != NULL) {
        buf[strlen(buf) - 1] = '\0';
        uint32_t h = hash(buf, strlen(buf));
        printf("%s\t%d\n", buf, h);
    }
    return 0;
}
