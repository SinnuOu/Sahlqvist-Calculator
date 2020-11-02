%{
#include "c_parseM.h"
%}
delim  [ \f\r\t\v]
digit  [0-9]
letter [^0-9 \f\n\r\t\v\(\)\[\]<>\{\}]
ws     {delim}+
id     {letter}({letter}|{digit})*
number {digit}+
%%
"\n"     {return NEWLINE;}
{ws}     {;}
"Top"    {return TOP;}
"Bottom" {return BOTTOM;}
"Equal"  {return EQUAL;}
"And"    {return AND;}
"Or"     {return OR;}
"Not"    {return NOT;}
"To"     {return TO;}
"Forall" {return FORALL;}
"Exists" {return EXISTS;}
{id}     {yylval.s = strdup(yytext); return ID;}
{number} {yylval.u = atoi(yytext); return NUMBER;}
"("      {return LPARENTHESES;}
")"      {return RPARENTHESES;}
"["      {return LBOX;}
"]"      {return RBOX;}
"<"      {return LDIAMOND;}
">"      {return RDIAMOND;}
"{"      {return LBRACE;}
"}"      {return RBRACE;}
.        {return ERROR;}
%%
int yywrap()
{
    return 1;
}