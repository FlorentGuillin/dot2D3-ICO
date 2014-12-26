type id = ID of string

and edgeRHS = EDGERHS_EMPTY
	| EDGERHS of stmt * edgeRHS

and stmt = NODE_STMT of (id * id) * ((id * id) list) list
	| EDGE_STMT of stmt * edgeRHS * ((id * id) list) list
	| ATTR_STMT of string * ((id * id) list) list 
	| ID_ID of (id * id) 
	| SUBGRAPH of id * stmt list

and graph = GRAPH of id * stmt list;;

type node = NODE of string * (string * string) list;;
type edge = EDGE of string * string * (string * string) list;;
