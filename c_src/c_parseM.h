#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {false, true};
enum {top, bottom, id, and, or, not, to, box, diamond};

typedef struct MODALITY {
    char *s;
    unsigned u;
} MODALITY;
typedef struct MODALITIES {
    MODALITY *mp;
    struct MODALITIES *mps;
} MODALITIES;
struct WFFMS;
typedef struct WFFM {
    unsigned u;
    char *s;
    struct WFFM *fp1, *fp2;
    MODALITY *mp;
    struct WFFMS *fps;
} WFFM;
typedef struct WFFMS {
    unsigned u;
    WFFM *fp;
    struct WFFMS *fps;
} WFFMS;
typedef union TYPE {
    char *s;
    unsigned u;
    WFFM *fp;
    WFFMS *fps;
} TYPE;

#define MODALITYSIZE sizeof(MODALITY)
#define MODALITIESSIZE sizeof(MODALITIES)
#define WFFMSIZE sizeof(WFFM)
#define WFFMSSIZE sizeof(WFFMS)
#define YYSTYPE TYPE

char *parseM(char *);

#include "c_parseM.tab.h"

extern FILE *yyin, *yyout;
extern char *yytext;
extern int yylex();
extern void yyrestart(FILE *);