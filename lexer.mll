{
open Parser
exception Eof
}


rule token = parse
	| [' ' '\t' '\n'] { token lexbuf }
	| "(*" { big_comment lexbuf } (* Deleting multi-lines comments *)
	| "\n#" { comment lexbuf }  (* Deleting # comments -anywhere in the document except beginning -> # *)
	| ("")+'#' { comment lexbuf }  (* Deleting # comments -beginning of file-  *)
	| "//" { comment lexbuf } (* Deleting end line comments -> // *)
	| '<' | '>' { token lexbuf }  (* Deleting HTML brackets *)
	| ['g' 'G']['r' 'R']['a' 'A']['p' 'P']['h' 'H'] { GRAPH } (* graph, strict and subgraph are case-independent *)
	| ['s' 'S']['u' 'U']['b' 'B']['g' 'G']['r' 'R']['a' 'A']['p' 'P']['h' 'H'] { SUBGRAPH }
	| ['s' 'S']['t' 'T']['r' 'R']['i' 'I']['c' 'C']['t' 'T'] { STRICT }
	| ['n' 'N']['o' 'O']['d' 'D']['e' 'E'] { NODE }
	| ['e' 'E']['d' 'D']['g' 'G']['e' 'E'] { EDGE }
	| "--" { EDGEOP }
	| '[' { LB }
	| ']' { RB }
	| ';' { SEMICOLON }
	| ':' { COLON }
	| '=' { EQUAL }
	| ',' { COMMA }
	| '{' { LCB }
	| '}' { RCB }
	| [^'0'-'9' '\n' '\t' ' ' '[' ']' ';' ':' '=' ',' '{' '}' '#' '"'][^'\n' '\t' ' ' '[' ']' ';' ':' '=' ',' '{' '}' '#' '"']* | ['0'-'9']+ | ['"'][^ '"']*['"']  as value { ID value }
	| eof { raise End_of_file } 

and comment = parse
	| '\n' { token lexbuf }
	| _ { comment lexbuf }
	| eof { raise End_of_file }

and big_comment = parse
	| "*)" { token lexbuf }
	| _ { big_comment lexbuf }
	| eof { raise End_of_file }
