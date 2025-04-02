%{
/*
Exercise 5
Create a modular approach to lexical analysis:
1. On INITIAL mode integers, ids, keywords and so on are identified. Upon finding
a ", switches to STRING mode.
2. On STRING mode, content between markers is matched to <string, "%s"> and mode
is recet to INITIAL.
3. STRING mode does not support multiline strings by design, it should be latter
implemented using a LONGSTRING mode.
*/
%}

%option noyywrap

%x STRING

%%
<INITIAL>{
    "print"                 { printf("<kw_print, %s>", yytext); }
    [+|-]?[0-9]+            { printf("<integer, %s>", yytext); }
    [a-zA-Z_][a-zA-Z0-9_]*  { printf("<id, %s>", yytext); }

    \"                      { BEGIN(STRING); }
    [ \t\n]+                { /* ignora espaços em branco */ }
    .                      { printf("<unknown, %s>", yytext); }
}

<STRING>{
    \n  { BEGIN(INITIAL); printf("<string_error>"); }
    (\\+\"|[^\"\n])* { printf("<string, \"%s\">", yytext); }
    \"  { BEGIN(INITIAL); }
}
%%

int main(void) {
    yylex();
    return 0;
}
