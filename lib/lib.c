#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>

extern void writeInteger(int x) {
    printf("%d", x);
}

extern void writeChar(char x) {
    printf("%c", x);
}

extern void writeString(const char *s) {
    printf("%s", s);
}

extern int64_t readInteger() {
    int64_t x;
    return (scanf("%ld", &x) == 1) ? x : 0;
}

extern char readChar() {
    char c;
    return (scanf("%c", &c) == 1) ? c : '\0';
}

extern void readString(int n, char *s) {
    char c;
    int i = 0, empty_char_position = n - 1;
    while (scanf("%c", &c) == 1 && c != '\n' && c != '\0') {
        s[i++] = c;
        if (i == empty_char_position)
            break;
    }
    s[i] = '\0';
    return;
}

extern int ascii (char c) {
    return (int)c;
}

extern char chr (int n) {
    return (char)n;
}
