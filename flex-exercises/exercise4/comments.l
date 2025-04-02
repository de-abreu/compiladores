%{
/* Exercise 4: Commentaries

1. Recognize commentaries prefixed by "//" by consuming and ignoring those
characters and every other character that follows it towards the end of that
line;
2. Recognize commentaries between "/*" and "*\/", consume and ignore its
characters;
3. Recognize the "while" and "return" keywords;
4. Recognize ids according to C syntax;
5. Ignore whitespace
*/
%}

%option noyywrap
%%
"while"                       { printf("<kw_%s, %s>", yytext, yytext); }
"return"                      { printf("<kw_%s, %s>", yytext, yytext); }
[0-9_]+[a-zA-Z][a-zA-Z0-9_]*  { printf("<id_error, %s>", yytext); }
[a-zA-Z0-9_]{64,}             { printf("<id_error, %s>", yytext); }
[a-zA-Z][a-zA-Z0-9_]*         { printf("<id, %s>", yytext); }

([ \t\n]+)|(\/\/.+)           { /* ignore either */ }
"/*"([^*]|\*+[^\/])*\*+\/\*?  { /* Flex does not support lazy matching, so this
is a more explicit pattern to match any sequence of characters between the 
commentary start and end markers */ }
.                             { printf("<unknown, %s>", yytext); }
%%

int main(void) {
    yylex();
    return 0;
}

