%{
/* Exercise 1: Identify integers */
%}

%option noyywrap

%%
[0-9]+ {printf("INTEGER(%s)\n", yytext);}
[^0-9\n]+ {printf("UNKNOWN(%s)\n", yytext);}
%%

int main(void) {
    yylex();
    return 0;
}
