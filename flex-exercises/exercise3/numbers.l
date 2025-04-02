%{
/* Exercise 3: Integers and real numbers

The following lexical analyzer identifies integers, floats and poorly formatted
floats. White space is ignored and all other strings are marked as "unknown".
*/
%}

%option noyywrap

%%
[-|+]?[0-9]{1,10}     { printf("<integer, %s>", yytext); }
[-|+]?[0-9]+\.[0-9]+  { printf("<float, %s>", yytext); }
[-|+]?[0-9]*\.[0-9]*  { printf("<float_error, %s>", yytext); }

[ \t\n]+              { /* ignore whitespace */ }
.+                    { printf("<unknown, %s>", yytext); }
%%

int main(void) {
    yylex();
    return 0;
}
