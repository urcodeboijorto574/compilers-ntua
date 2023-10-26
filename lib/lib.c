#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

extern void writeInteger(int x) {
    printf("%d\n", x);
}

extern void writeChar(char x) {
    printf("%c\n", x);
}

extern void writeString(char *s) {
    printf("%s\n", s);
}

extern int64_t readInteger() {
    int64_t x;
    if (scanf("%ld", &x) == 1) {
        return x;
    } 
    else {
        return 0;
    }
}