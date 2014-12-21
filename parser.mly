%token <string> ID
%token EOF
%start main
%type <string> main
%%
main: 
	expr {$1}
;

expr:
	ID { $1 }
; 
