(* dot2D3.ml *)
open Filename
open Syntax

(* Affiche le contenu d'un stmt sous forme de string *)

let stmt_to_string stmt = match stmt with
	| NODE_STMT(_, _) -> "node_stmt"
	| ATTR_STMT (_, _) -> "attr_stmt"
	| ID_ID _ -> "ID = ID"
	| EDGE_STMT(_, _, _) -> "edge_stmt"
	| SUBGRAPH (_, _) -> "subgraph"
;;


(* Affiche le contenu d'une stmt_list sous forme de string*)
let rec stmt_list_to_string stmt_list = match stmt_list with
	| [] -> "" 
	| head :: tail -> (stmt_to_string head) ^ ";" ^ (stmt_list_to_string tail)
;;

(* Affiche le contenu d'un graph sous forme de string *)
let graph_to_string graph = match graph with
	| GRAPH(id, stmt_list) -> (stmt_list_to_string stmt_list)
;;

(* Affiche le contenu d'un paramêtre sous forme de string *)
let param_to_string param = match param with
	| (a, b) -> a ^ "=" ^ b
;;

(* Affiche le contenu d'une liste de paramètres sous forme de  string *)
let rec params_to_string params = match params with
	| [] -> ""
	| head :: tail -> (param_to_string head) ^ "; " ^ (params_to_string tail)
;;

(* Affiche le contenu d'un noeud sous forme de string *)
let node_to_string node = match node with
	| NODE(node_id, node_params) -> "(ID= " ^ node_id ^ "; PARAMS= " ^ (params_to_string node_params) ^ ")"
;;

(* Affiche une liste de noeuds sous forme de string *)
let rec nodes_to_string nodes = match nodes with
	| [] -> ""
	| head :: tail -> (node_to_string head) ^ (nodes_to_string tail)
;;

(* Affiche une arête sous forme de string *)
let edge_to_string edge = match edge with
	| EDGE(start_node, end_node, params) -> "(START= " ^ start_node ^ "; END= " ^ end_node ^ "; PARAMS= " ^ (params_to_string params) ^ ")"
;;

(* Affiche une liste d'arêtes sous forme de string *)
let rec edges_to_string edges = match edges with
	| [] -> ""
	| head :: tail -> (edge_to_string head) ^ (edges_to_string tail) 
;;

(* Affiche l'ensemble des noeuds et des arêtes *)
let couple_to_string couple = match couple with 
	| (nodes, edges) -> "[" ^ (nodes_to_string nodes) ^ "] [" ^ (edges_to_string edges) ^ "]\n" 
;;

(* Convertit un couple id et port en une string composé de l'id, le port n'étant pas implémenté dans le sujet *)
let id_id_to_string id = match id with
	| (ID(a), ID(b)) -> a
;; 


(* Renvoie la liste de noeuds *)
let get_node_list_from_couple couple = match couple with
	| (a, b) -> a
;;

(* Renvoie la liste d'arrêtes *)

let get_edge_list_from_couple couple = match couple with
	| (a, b) -> b
;; 

(* Un couple d'id correspond à un paramètre avant l'analyse par le middle end, on convertit ça en couple de string *)

let id_id_to_string_string id = match id with
	| (ID(a), ID(b)) -> (a, b)
;;

(* Une a_list est une liste de couple d'id, il faut la traduire en liste de couple de string pour le middle end *)

let rec a_list_to_string_string_list a_list = match a_list with
	| [] -> []
	| [a] -> [id_id_to_string_string a]
	| head :: tail -> [(id_id_to_string_string head)]@(a_list_to_string_string_list tail)
;;

(* Une attr_list est une liste de a_list, la réduction se fait en une liste de couple de string*)

let rec attr_list_to_string_string_list params = match params with 
	| [] -> []
	| head :: tail -> (a_list_to_string_string_list head)@(attr_list_to_string_string_list tail) 
;;

(* Compare l'identifiant d'un noeud avec une chaîne de caractère *)

let compare_nodes node id = match node with
	| NODE (node_id, node_params) -> if id = node_id then true else false
;;

(* Récupère le nom d'un paramètre, utile pour la comparaison *)
let get_param_name param = match param with
	| (a, b) -> a
;;

(* Dans le middle end, si un paramètre existe déjà, on remplace automatiquement sa valeur par la nouvelle *)
let replace_param_value param1 param2 = match (param1, param2) with
	| ((a,b), (c,d)) -> (a,d)
;;

(* Compare le nom de deux paramètres, true s'ils sont identiques, case sensitive *)
let compare_params param1 param2 = 
	if (get_param_name param1) = (get_param_name param2) 
	then true 
	else false
;;

(* Parcourt la liste des paramètres existants du noeud pour savoir quoi faire du paramètre courant (ajout ou modification) *)

let rec sweep_param_list stmt_params param = match stmt_params with
	| [] -> [param]
	| head :: tail -> if (compare_params head param) then [(replace_param_value head param)]@tail else [head]@(sweep_param_list tail param)
;;

(* Parcourt la liste des paramètres d'un stmt en cours d'analyse. Chaque paramètre est traité pour savoir s'il faut l'ajouter ou modifier un existant *)

let rec sweep_both_param_list stmt_params params = match params with
	| [] -> []
	| head :: tail -> (sweep_param_list stmt_params head)@(sweep_both_param_list stmt_params tail)
;;

(* Récupère la liste des paramètres d'une arête pour la fusionner avec une liste de paramètres *)
let analyze_edge_param_list edge params = match edge with
	| EDGE (start_node, end_node, edge_params) -> EDGE(start_node, end_node, (sweep_both_param_list edge_params params))
;;	

(* Récupère la liste des paramètre d'un noeud pour la fusionner avec une liste de paramètres *)
let analyze_node_param_list node params = match node with
	| NODE (node_id, node_params) -> NODE(node_id, (sweep_both_param_list node_params params))  
;;

(* Ajoute un label valant l'ID s'il n'en pas déjà un lors de la création d'un noeud *)
let rec add_label id params = match params with
	| [] -> [("label", id)]
	| head :: tail -> if (compare_params head ("label", "")) 
		then params
		else [head]@(add_label id tail) 
;;

(* La liste des noeuds est parcourue, si un nom n'est pas présent, on l'ajoute avec ses paramètres, 
	sinon, on ne crée pas de noeud mais on parcourt les paramètres pour voir s'il y en a des nouveaux ou des modifiables *)

let rec sweep_node_list couple id params = match (get_node_list_from_couple couple) with 
	| [] -> ([NODE(id, (add_label id params))], get_edge_list_from_couple couple)
	|	head :: tail -> if (compare_nodes head id) 
		then ([(analyze_node_param_list head params)]@tail, (get_edge_list_from_couple couple))
		else ([head]@(get_node_list_from_couple(sweep_node_list (tail, (get_edge_list_from_couple couple)) id params)), (get_edge_list_from_couple couple)) 
;;

(* Compare une arête avec deux strings *)
let compare_edges edge start_node end_node = match edge with
	| EDGE (sn, en, params) -> if (sn = start_node && en = end_node)
		then true
		else false
;;

(* La liste des arêtes est parcourue, si un nom n'est pas présent, on l'ajoute avec ses paramètres, 
	sinon, on ne crée pas d'arête mais on parcourt les paramètres pour voir s'il y en a des nouveaux ou des modifiables *)
let rec sweep_edge_list couple start_node end_node params = match (get_edge_list_from_couple couple) with
	| [] -> ((get_node_list_from_couple couple), [EDGE(start_node, end_node, params)]) 
	| head :: tail -> if (compare_edges head start_node end_node)
		then ((get_node_list_from_couple couple), [(analyze_edge_param_list head params)]@tail)
		else ((get_node_list_from_couple couple), [head]@(get_edge_list_from_couple(sweep_edge_list ((get_node_list_from_couple couple), tail) start_node end_node params)))
;;

(* Parcourt la liste d'arêtes pour ajouter ou modifier les paramètres de params *)
let rec add_params_to_each_edge edges params = match edges with
	| [] -> []
	| head :: tail -> [(analyze_edge_param_list head params)]@(add_params_to_each_edge tail params)
;; 

(* Parcourt la liste de noeuds pour ajouter ou modifier les paramètres de params *)
let rec add_params_to_each_node nodes params = match nodes with
	| [] -> []
	| head :: tail -> [(analyze_node_param_list head params)]@(add_params_to_each_node tail params)
;; 

(* Analise un attr_stmt pour savoir s'il faut l'appliquer au graph, aux noeuds ou arêtes et agit en fonction *) 
let add_params_from_attr_stmt stmt_type params couple = match stmt_type with
	| "graph" -> couple (* Les attributs de graph ne sont pas transmis à D3.js *)
	| "node" -> ((add_params_to_each_node (get_node_list_from_couple couple) params), (get_edge_list_from_couple couple))
	| "edge" -> ((get_node_list_from_couple couple), (add_params_to_each_edge (get_edge_list_from_couple couple) params)) 
	| _ -> couple
;;

(* Ajoute une arête à un ensemble d'arêtes existant *)
let add_edge_to_edges edges edge = match edge with
	| EDGE(start_node, end_node, params) -> get_edge_list_from_couple (sweep_edge_list ([], edges) start_node end_node params)
;;

(* Parcout  l'ensemble des arêtes récentes pour les ajouter à l'ancien ou modifier celles de l'ancien s'il y en a des communes *)
let rec merge_edges edges1 edges2 = match edges2 with
	| [] -> edges1
	| head :: tail -> merge_edges (add_edge_to_edges edges1 head) tail
;;

(* Ajoute un noeud à un ensemble de noeuds existant *)
let add_node_to_nodes nodes node = match node with
	| NODE (id, params) -> get_node_list_from_couple (sweep_node_list (nodes, []) id params)
;;

(* Parcourt l'ensemble des noeuds récents pour les ajouter à l'ancien ou modifier ceux de l'ancien s'il y en a des communs *) 
let rec merge_nodes nodes1 nodes2 = match nodes2 with
	| [] -> nodes1
	| head :: tail -> merge_nodes (add_node_to_nodes nodes1 head) tail
;;

(* Fusionne deux ensembles de noeuds et arêtes. Couple2 étant l'ensemble le plus récent, ses params identiques à ceux de couple1 modifier ceux du premier *)
let merge_couple couple1 couple2 = match (couple1, couple2) with
	| ((nodes1, edges1), (nodes2, edges2)) -> ((merge_nodes nodes1 nodes2), (merge_edges edges1 edges2))
;;

let create_edge node1 node2 params = match (node1, node2) with
	|	(NODE(id1, params1), NODE(id2, params2)) -> EDGE(id1, id2, params)
;;

let rec create_multiple_edges nodes node params = match nodes with
	| [] -> []
	| head :: tail -> [create_edge node head params]@(create_multiple_edges tail node params)
;;

(* Crée une arête pour chaque noeud d'un premier ensemble avec chaque noeud du second *)
let rec create_edges_between_nodes nodes1 nodes2 params = match nodes1 with
	| [] -> []
	| head :: tail -> (create_multiple_edges nodes2 head params)@(create_edges_between_nodes tail nodes2 params)
;;

(* Point d'entrée du traitement du graph *)
let rec call_create_nodes_edges graph =
(* Analyse le stmt courant parmi une liste pour savoir quoi faire (ajout de noeud, arête, modification ou ajout de paramètre *)
	let rec analyze_stmt stmt couple = match stmt with 
		| NODE_STMT(id, params) -> sweep_node_list couple (id_id_to_string id) (attr_list_to_string_string_list params)
		| ATTR_STMT (stmt_type, params) -> (add_params_from_attr_stmt stmt_type (attr_list_to_string_string_list params) couple)
		| ID_ID param -> couple
		| EDGE_STMT(stmt, edgeRHS, params) -> merge_couple couple (sweep_edge_stmt stmt edgeRHS ([], []) (attr_list_to_string_string_list params)) 
		| SUBGRAPH (id, stmt_list) -> merge_couple couple (call_create_nodes_edges (GRAPH(id, stmt_list))) (* Crée le sous-ensemble de sommets d'arêtes du subgraph et le fusionne avec celui du graph *) and
			(* Crée les arêtes issus d'un edge_stmt, si un noeud composant l'arête n'existe pas, il est aussi ajouté *)
			sweep_edge_stmt stmt edgeRHS couple params = match edgeRHS with
				| EDGERHS_EMPTY -> ([], [])
				| EDGERHS (next_stmt, next_edgeRHS)-> let c1 = (analyze_stmt stmt ([], [])) in
					let c2 = (analyze_stmt next_stmt ([], [])) in 
					let c3 = merge_couple c1 c2 in 
					let c4 = ((get_node_list_from_couple c3), (get_edge_list_from_couple c3)@(create_edges_between_nodes (get_node_list_from_couple c1) (get_node_list_from_couple c2) params)) in
					let c5 = (merge_couple c4 (sweep_edge_stmt next_stmt next_edgeRHS couple params)) in
					(merge_couple couple c5) in
				(* Parcourt tous les stmts d'un graphe, retourne l'ensemble des noeuds ou arêtes *)
				let rec sweep_stmt_list stmt_list couple = match stmt_list with
				| [] -> couple
				| head :: tail -> sweep_stmt_list tail (analyze_stmt head couple) in
					let couple = ([], []) in  
						match graph with
						| GRAPH (id, stmt_list) -> sweep_stmt_list stmt_list couple
;;

(* Programme d'appel, lit le fichier .dot, le transforme en graph, l'analyse et retourne le fichier html *)
let _ =
	if (Array.length Sys.argv) == 2 then begin  
		if check_suffix (Sys.argv.(1))  ".dot" then begin
			let file = open_in Sys.argv.(1) in 
			try
				let lexbuf = Lexing.from_channel file in
					let graph = Parser.graph Lexer.token lexbuf in
						print_endline ("Graph: OK" ^ "\nContenu du graphe: " ^ (graph_to_string graph)); let couple = call_create_nodes_edges graph in	
							print_endline ("Nodes and edges: OK\n" ^ couple_to_string couple); flush stdout
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
