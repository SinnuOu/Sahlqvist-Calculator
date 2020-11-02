%{
#include "c_parseM.h"
WFFM *fp = NULL;
int yyerror(char *msg)
{
    fp = NULL;
}
MODALITIES ms = {NULL, NULL};
MODALITIES *mps = &ms;
MODALITY *lookupModality(char *s, unsigned u)
{
    MODALITIES *tmp = mps;
    while (tmp->mp != NULL)
    {
        if (strcmp(tmp->mp->s,s) == 0)
            if (tmp->mp->u == u)
                return tmp->mp;
            else
                yyerror(NULL);
        tmp = tmp->mps;
    }
    tmp = malloc(MODALITIESSIZE);
    tmp->mp = malloc(MODALITYSIZE);
    tmp->mp->s = s;
    tmp->mp->u = u;
    tmp->mps = mps;
    mps = tmp;
    return tmp->mp;
}
void output(FILE *, WFFM *);
void outputs(FILE *out, WFFMS *fps)
{
    if (fps->fp != NULL)
    {
        fprintf(out," ");
        output(out,fps->fp);
        WFFMS *next = fps->fps;
        free(fps);
        outputs(out,next);
    }
    else
        free(fps);
}
void outputm(FILE *out, MODALITY *mp)
{
    fprintf(out,"%s %u",mp->s,mp->u);
}
void output(FILE *out, WFFM *fp)
{
    switch (fp->u)
    {
        case top:
            fprintf(out,"Top");
            break;
        case bottom:
            fprintf(out,"Bottom");
            break;
        case id:
            fprintf(out,fp->s);
            break;
        case and:
            fprintf(out,"(And ");
            output(out,fp->fp1);
            fprintf(out," ");
            output(out,fp->fp2);
            fprintf(out,")");
            break;
        case or:
            fprintf(out,"(Or ");
            output(out,fp->fp1);
            fprintf(out," ");
            output(out,fp->fp2);
            fprintf(out,")");
            break;
        case not:
            fprintf(out,"(Not ");
            output(out,fp->fp1);
            fprintf(out,")");
            break;
        case to:
            fprintf(out,"(To ");
            output(out,fp->fp1);
            fprintf(out," ");
            output(out,fp->fp2);
            fprintf(out,")");
            break;
        case box:
            fprintf(out,"([");
            outputm(out,fp->mp);
            fprintf(out,"]");
            outputs(out,fp->fps);
            fprintf(out,")");
            break;
        case diamond:
            fprintf(out,"(<");
            outputm(out,fp->mp);
            fprintf(out,">");
            outputs(out,fp->fps);
            fprintf(out,")");
            break;
    }
    free(fp);
}
%}
%token NEWLINE TOP BOTTOM EQUAL AND OR NOT TO FORALL EXISTS LPARENTHESES RPARENTHESES LBOX RBOX LDIAMOND RDIAMOND LBRACE RBRACE ERROR
%token <s> ID
%token <u> NUMBER
%type <fp> wffM
%type <fps> wffMs
%%
target: wffM NEWLINE {fp = $1;}
      ;
wffM: TOP {$$ = malloc(WFFMSIZE); $$->u = top;}
    | BOTTOM {$$ = malloc(WFFMSIZE); $$->u = bottom;}
    | ID {$$ = malloc(WFFMSIZE); $$->u = id; $$->s = $1;}
    | AND wffM wffM {$$ = malloc(WFFMSIZE); $$->u = and; $$->fp1 = $2; $$->fp2 = $3;}
    | OR wffM wffM {$$ = malloc(WFFMSIZE); $$->u = or; $$->fp1 = $2; $$->fp2 = $3;}
    | NOT wffM {$$ = malloc(WFFMSIZE); $$->u = not; $$->fp1 = $2;}
    | TO wffM wffM {$$ = malloc(WFFMSIZE); $$->u = to; $$->fp1 = $2; $$->fp2 = $3;}
    | LPARENTHESES wffM RPARENTHESES {$$ = $2;}
    | LPARENTHESES LBOX ID RBOX wffMs RPARENTHESES {$$ = malloc(WFFMSIZE); $$->u = box; $$->mp = lookupModality($3,$5->u); $$->fps = $5;}
    | LPARENTHESES LBOX ID NUMBER RBOX wffMs RPARENTHESES {if ($4 != $6->u) yyerror(NULL); $$ = malloc(WFFMSIZE); $$->u = box; $$->mp = lookupModality($3,$4); $$->fps = $6;}
    | LPARENTHESES LDIAMOND ID RDIAMOND wffMs RPARENTHESES {$$ = malloc(WFFMSIZE); $$->u = diamond; $$->mp = lookupModality($3,$5->u); $$->fps = $5;}
    | LPARENTHESES LDIAMOND ID NUMBER RDIAMOND wffMs RPARENTHESES {if ($4 != $6->u) yyerror(NULL); $$ = malloc(WFFMSIZE); $$->u = diamond; $$->mp = lookupModality($3,$4); $$->fps = $6;}
    ;
wffMs: wffM wffMs {$$ = malloc(WFFMSSIZE); $$->u = $2->u + 1; $$->fp = $1; $$->fps = $2;}
     | {$$ = malloc(WFFMSSIZE); $$->u = 0; $$->fp = NULL; $$->fps = NULL;}
     ;
%%
char *parseM(char *input)
{
    fp = NULL;
    while (mps->mps != NULL)
    {
        MODALITIES *next = mps->mps;
        free(mps->mp);
        free(mps);
        mps = next;
    }
    FILE *tmpin = fopen(".tmpfile~","w+");
    fprintf(tmpin,"%s\n",input);
    fclose(tmpin);
    yyin = fopen(".tmpfile~","r");
    yyrestart(yyin);
    yyparse();
    fclose(yyin);
    if (fp == NULL)
        return "";
    yyout = fopen(".tmpfile~","w+");
    output(yyout,fp);
    unsigned size = ftell(yyout) + 1;
    fprintf(yyout,"\n");
    fclose(yyout);
    FILE *tmpout = fopen(".tmpfile~","r");
    char *buffer = malloc(size);
    fgets(buffer,size,tmpout);
    fclose(tmpout);
    return buffer;
}