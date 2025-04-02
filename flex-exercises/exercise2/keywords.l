%{
/* Exercise 2: Keywords and ids

Assuming Lua's programming language syntax, this program should:

1. recognize the if/then/elseif/else/end keywords;
2. variable names;
3. ignore whitespaces
4. Mark anything other as an "unknown"
*/
%}

%option noyywrap

%%
"elseif"                      { printf("<kw_%s, %s>", yytext, yytext); }
"else"                        { printf("<kw_%s, %s>", yytext, yytext); }
"if"                          { printf("<kw_%s, %s>", yytext, yytext); }
"end"                         { printf("<kw_%s, %s>", yytext, yytext); }
"then"                        { printf("<kw_%s, %s>", yytext, yytext); }

[0-9_]+[a-zA-Z][a-zA-Z0-9_]*  { printf("<id_error, %s>", yytext); }
[a-zA-Z0-9_]{64,}             { printf("<id_error, %s>", yytext); }
[a-zA-Z][a-zA-Z0-9_]          { printf("<id, %s>", yytext); }
[ \t\n]+                      { /* ignore whitespace */ }
.                             { printf("<unknown, %s>", yytext); }
%%

int main(void) {
    yylex();
    return 0;
}
