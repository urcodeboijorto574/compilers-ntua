#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

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

extern int strlen (const char *s) {
    if (!s) {
        fprintf(stderr, "Error: Null pointer detected in strlen function.\n");
        return -1;
    }

    int i = -1, counter = 0;
    while (s[++i] != '\0')
        ++counter;
    return counter;
}

extern int strcmp (const char *s1, const char *s2) {
    if (!s1 || !s2) {
        fprintf(stderr, "Error: Null pointer detected in strcmp function.\n");
        return !s1 ? -2 : 2;
    }

    int i = 0;
    while (s1[i] == s2[i])
        if (s1[i++] == '\0')
            return 0;

    return (int)s1[i] - (int)s2[i] > 0 ? 1 : -1;
}

extern void strcpy (char * trg, const char *src) {
    if (!trg || !src) {
        fprintf(stderr, "Error: Null pointer detected in strcpy function.\n");
        return;
    }

    int i = 0;
    do trg[i] = src[i];
    while (src[i++] != '\0');
    return;
}

extern void strcat (char * trg, const char *src) {
    if (!trg || !src) {
        fprintf(stderr, "Error: Null pointer detected in strcat function.\n");
        return;
    }

    int i = -1;
    while (trg[++i] != '\0') ;
    int j = 0;
    do trg[i] = src[j];
    while (src[j++] != '\0') ;
    return;
}
