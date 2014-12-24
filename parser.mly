%{
open Syntax;;
%}

%token GRAPH ID
%token STRICT
%token LB
%token LCB
%token RB
%token RCB
%token SUBGRAPH
%token EDGEOP
%token SEMICOLON
%token COLON
%token EDGE
%token NODE
%token COMMA
%token EQUAL
%token EOF
%token <string> ID
%start graph
%type <Syntax.graph> graph
%%

graph:
		STRICT GRAPH ID LCB stmt_list RCB EOF { GRAPH(ID($3), $5) }
	| STRICT GRAPH LCB stmt_list RCB EOF { GRAPH(ID(""), $4) } 
	| GRAPH ID LCB stmt_list RCB EOF { GRAPH(ID($2), $4) }
	| GRAPH LCB stmt_list RCB EOF { GRAPH(ID(""), $3) }
;

stmt_list:
		{[]} 
	|	stmt { [$1] }
	| stmt SEMICOLON stmt_list { $1 :: $3 }
	| stmt stmt_list { $1 :: $2 }
;	

node_id: 
		ID { (ID($1), ID("")) }
	| ID COLON ID { (ID($1), ID($3)) }
;

subgraph: 
		SUBGRAPH ID LCB stmt_list RCB { SUBGRAPH(ID($2), $4) }
	| SUBGRAPH LCB stmt_list RCB { SUBGRAPH(ID(""), $3) }
	| LCB stmt_list RCB { SUBGRAPH(ID(""), $2) } 
;

edgeRHS: 
		EDGEOP subgraph edgeRHS { EDGERHS_S($2, $3)}
	|	EDGEOP subgraph { EDGERHS_S($2, EDGERHS_E) }
	| EDGEOP node_id edgeRHS { EDGERHS_N($2, $3) }
	| EDGEOP node_id { EDGERHS_N($2, EDGERHS_E) }
; 

attr_stmt: 
		GRAPH attr_list { ATTR_STMT("graph", $2) }
	|	NODE attr_list { ATTR_STMT("node", $2) }
	| EDGE attr_list { ATTR_STMT("edge", $2) }
;

attr_list: 
		{[]}
	|	LB a_list RB { [$2] }
	|	LB a_list RB a_list { $2 :: [$4] }
;

a_list:
		{[]} 
	|	ID EQUAL ID { [(ID($1),ID($3))] }
	|	ID EQUAL ID SEMICOLON a_list { (ID($1),ID($3)) :: $5}
	|	ID EQUAL ID COMMA a_list { (ID($1),ID($3)) :: $5}
;

node_stmt: 
	node_id attr_list { NODE_STMT($1, $2) }
;

edge_stmt: 
		subgraph edgeRHS attr_list { EDGE_STMT($1, $2, $3) }
	|	node_id edgeRHS attr_list {EDGE_STMT(NODE_STMT($1, []), $2, $3) }
; 

stmt:
		node_stmt { $1 }
	| edge_stmt { $1 }
	| ID EQUAL ID { ID_ID(ID($1), ID($3)) }
	| attr_stmt { $1 }
	| subgraph { $1 }
;
