(* dot2D3.ml *)
open Filename

let _ =
	if (Array.length Sys.argv) == 2 then begin  
		if check_suffix (Sys.argv.(1))  ".dot" then begin
			let file = open_in Sys.argv.(1) in 
			try
				let lexbuf = Lexing.from_channel file in
				while true do
					let result = Parser.main Lexer.token lexbuf in
						print_endline result;flush stdout
				done
			with Lexer.Eof ->
				close_in file;
				exit 0
		end
		else begin
			print_endline "Mauvaise extension de fichier: .dot attendu";exit 0
		end
	end else begin 
		print_endline "Commande attendue: ./dot2D3 fichier.dot"; exit 0 
	end
