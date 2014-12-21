{
open Parser
exception Eof
}


rule token = parse
	| [' ' '\t' '\n'] { token lexbuf }
	| "(*" { big_comment lexbuf } (* Gestion des commentaires sur plusieurs lignes *)
	| '\n'?'#' { comment lexbuf }  (* Gestion des commentaires en début de ligne -> # *)
	| "//" { comment lexbuf } (* Gestion des commentaires en fin de ligne -> // *)
	| '<' | '>' { token lexbuf }  (* Banalisation des balises HTML *)
	| [ 'a'-'z' 'A'-'Z' ](['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*) | ['0'-'9']+  as value { ID value } (* Récupération des identifiants -> ID *)
	| eof { raise End_of_file } 

and comment = parse
	| '\n' { token lexbuf }
	| _ { comment lexbuf }
	| eof { raise End_of_file }

and big_comment = parse
	| "*)" { token lexbuf }
	| _ { big_comment lexbuf }
	| eof { raise End_of_file }
