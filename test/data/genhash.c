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
