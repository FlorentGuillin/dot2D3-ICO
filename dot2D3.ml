(* dot2D3.ml *)
open Filename
open Syntax

let id_id_to_string id = match id with
	| (ID(a), ID(b)) -> a
	| _ -> "" 
;; 

let get_node_list_from_couple couple = match couple with
	| (a, b) -> a
	| _ -> []
;;

let get_edge_list_from_couple couple = match couple with
	| (a, b) -> b
	| _ -> []
;; 

let id_id_to_string_string id = match id with
	| (ID(a), ID(b)) -> (a, b)
	| _ -> ("", "")
;;

let rec a_list_to_string_string_list a_list = match a_list with
	| [] -> []
	| [a] -> [id_id_to_string_string a]
	| head :: tail -> [(id_id_to_string_string head)]@(a_list_to_string_string_list tail)
;;

let rec attr_list_to_string_string_list params = match params with 
	| [] -> []
	| head :: tail -> (a_list_to_string_string_list head)@(attr_list_to_string_string_list tail) 
;;


let compare_nodes node id = match node with
	| NODE (node_id, node_params) -> if id = node_id then true else false
	| _ -> false;
;;

let get_param_name param = match param with
	| (a, b) -> a
	| _ -> ""
;;

let replace_param_value param1 param2 = match (param1, param2) with
	| ((a,b), (c,d)) -> (a,d)
	| _ -> param2
;;

let compare_params param1 param2 = 
	if (get_param_name param1) = (get_param_name param2) 
	then true 
	else false
;;

let rec sweep_param_list node_params param = match node_params with
	| [] -> [param]
	| head :: tail -> if (compare_params head param) then [(replace_param_value head param)]@tail else [head]@(sweep_param_list tail param)
;;

(* Parcourt la liste des paramètres d'un noeud pour déterminer si les paramètres d'une seconde liste sont à ajouter
	ou modifier dans la première *)
let rec sweep_both_param_list node_params params = match params with
	| [] -> []
	| head :: tail -> (sweep_param_list node_params head)@(sweep_both_param_list node_params tail)
;;

let analyze_param_list head id params = match head with
	| NODE (node_id, node_params) -> NODE(node_id, (sweep_both_param_list node_params params))  
	| _ -> NODE(id, params)
;;

(* La liste des noeuds est parcourue, si un nom n'est pas présent, on l'ajoute avec ses paramètres, 
	sinon, on ne crée pas de noeud mais on parcourt les paramètres pour voir s'il y en a des nouveaux ou des modifiables *)

let rec sweep_node_list couple id params = match (get_node_list_from_couple couple) with 
	| [] -> ([NODE(id, params)], get_edge_list_from_couple couple)
	|	head :: tail -> if (compare_nodes head id) 
		then ([(analyze_param_list head id params)]@tail, (get_edge_list_from_couple couple))
		else ([head]@(get_node_list_from_couple(sweep_node_list (tail, (get_edge_list_from_couple couple)) id params)), (get_edge_list_from_couple couple)) 
	| _ -> couple
;;

let analyze_stmt stmt couple = match stmt with 
	|	NODE_STMT(id, params) -> sweep_node_list couple (id_id_to_string id) (attr_list_to_string_string_list params)
;;

let rec sweep_stmt_list stmt_list couple = match stmt_list with
	| [] -> couple
	| head :: tail -> sweep_stmt_list tail (analyze_stmt head couple) 
;;	

let call_create_nodes_edges graph =
	let couple = ([NODE("", [])], [EDGE("", "", [])]) in  
		match graph with
		| GRAPH (id, stmt_list) -> sweep_stmt_list stmt_list couple
		| _ -> couple;;

let _ =
	if (Array.length Sys.argv) == 2 then begin  
		if check_suffix (Sys.argv.(1))  ".dot" then begin
			let file = open_in Sys.argv.(1) in 
			try
				let lexbuf = Lexing.from_channel file in
				while true do
					let graph = Parser.graph Lexer.token lexbuf in
						let couple = call_create_nodes_edges graph in	
							flush stdout
				done
			with Lexer.Eof ->
				close_in file;
				exit 0
		end
		else begin
			print_endline "Bad file extension: .dot expected";exit 0
		end
	end else begin 
		print_endline "Expected command: ./dot2D3 file.dot"; exit 0 
	end;;
