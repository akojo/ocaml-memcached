#include <stdio.h>
#include <string.h>
#include <libhashkit/hashkit.h>

int main(void)
{
    char buf[BUFSIZ];

    while (fgets(buf, BUFSIZ, stdin) != NULL) {
        buf[strlen(buf) - 1] = '\0';
        uint32_t hash = libhashkit_murmur(buf, strlen(buf));
        printf("%s\t%d\n", buf, hash);
    }
    return 0;
}
