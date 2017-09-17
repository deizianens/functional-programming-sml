(*Escreva um tipo de dados suit, cujos valores sejam os quatro naipes de um baralho: 
	Hearts, Clubs, Diamonds e Spades.*)

datatype suit = Spades | Hearts | Diamonds | Clubs;

(*Usando a defini¸c˜ao do exerc´ıcio anterior, escreva a função suitname to tipo suit ->
string que retorne um valor do tipo string descrevendo o nome do naipe.
*)
	
fun suitname(x) = 
	if x = Spades then "Spades"
	else if x = Clubs then "Clubs"
	else if x = Diamonds then "Diamonds"
	else if x = Hearts then "Hearts"
	else "Erro";
	
(* Escreva um tipo de dados number, cujos valores sejam ou n´umeros inteiros ou n´umeros
reais.*)

datatype number = N of int | N2 of real;

(*Usando a defini¸c˜ao do exercıcio anterior, escreva uma função plus de tipo number ->
number -> number que some dois n´umeros. A sua função deve converter inteiros para
reais quando receber um parametro real e um parˆametro inteiro.*)

fun plus (N x) (N y) = (N(x+y))
|	plus (N x) (N2 y) = (N2(real(x)+y))
|	plus (N2 x) (N2 y) = (N2(x+y))
| 	plus (N2 x) (N y) = (N2(x+ real(y))); 

(* Escreva uma função addUp do tipo intnest -> int que some todos os inteiros em um
intnest. Este tipo algébrico ´e definido abaixo:

datatype intnest = INT of int | LIST of intnest list*)

fun addUp (INT x) = x
| addUp(LIST []) = 0
| addUp (LIST(h::t)) = case h of 
	INT h => h + addUp(LIST t) |
	LIST h => addUp(LIST h) + addUp(LIST t);
	
(*Escreva a função prod to tipo int mylist -> int que receba um int mylist x e
retorne o produto de todos os elementos de x. Se a lista for NIL, a sua função dever´a
retornar o n´umero 1. A definiç˜ao de mylist ´e dada abaixo:

datatype ’element mylist = NIL | CONS of ’element * ’element mylist *)
	
fun prod NIL = 1
| prod (CONS (a,b)) = a * prod b;

(* Escreva a função reverse do tipo ’a -> ’a mylist que receba um mylist a e retorne
um mylist de todos os elementos de a, em ordem inversa. Use a defini¸c˜ao de mylist
do exerc´ıcio anterior.

Escreva a função append to tipo ’a mylist -> ’a mylist -> ’a mylist que receba
dois valores do tipo mylist, por exemplo, a e b, e retorne um valor do tipo mylist
contendo todos os elementos de a seguidos de todos os elementos de b. Use a defini¸c˜ao
de mylist do exercıcio anterior. *)
	
fun append NIL ys = ys
  | append (CONS(x, xs)) ys = CONS(x, append xs ys)	
	
fun reverse NIL = NIL
  | reverse (CONS(x, xs)) = append (reverse xs) (CONS(x, NIL))	

(*Podemos representar uma árvore binária em SML usando o tipo algébrico abaixo:
datatype ’data tree = Empty | Node of ’data tree * ’data * ’data tree
Um nodo Empty ´e um sentinela usado para indicar que chegamos ao fim de um caminho
na árvore; j´a um nodo Node contem uma sub-árvore `a direita, um item de dado, e uma
sub-árvore a esquerda. Nesta quest˜ao vocˆe deve implementar uma função revTree,
cujo tipo ´e ’a tree -> ’a tree. Esta função inverte o conte´udo de uma árvore:

-revTree (Node(Node(Empty, 1, Empty), 2, Node(Empty, 3, Empty)));
val it = Node(Node(Empty, 3, Empty), 2, Node(Empty, 1, Empty)) : int tree *)
  
fun revTree (tree)
  = let fun visit(Empty)
  = Empty
    | visit(Node(left,elm,right))
    = Node(visit(right),elm,visit(left))
    in visit (tree)
    end;

(*Escreva a função appendall do tipo ’a list tree -> ’a list que receba uma árvore
de listas e retorne a lista resultante da concatena¸c˜ao de todas as listas na árvore.
Coloque as listas juntas em uma viagem in-order na árvore, isto ´e, primeiro todas as
listas da parte esquerda de um nodo, ent˜ao a lista do pr´oprio nodo, e finalmente todoas
as listas `a direita do nodo. *)
	
fun appendall Empty = []
|   appendall (Node(left, root, right)) =
   let
	fun append (left, root, right) =
		(appendall left)@root@(appendall right) 
   in
	append (left, root, right)
   end;

(*Uma árvore binária ´e chamada completa se cada nodo tem, ou dois filhos, ou nenhum
filho. Em termos de nosso tipo algébrico tree, a árvore ´e completa se cada nodo
tem, ou dois filhos do tipo Empty, ou dois filhos do tpo Node, mas n˜ao um de cada.
Escreva uma função isComplete do tipo ’a tree -> bool que teste se um valor tree
´e completo.*)
   
fun isComplete Empty = true
|	isComplete (Node(Empty, y, Empty)) = true
|	isComplete (Node(Empty, y, z)) = false
|	isComplete (Node(x, y, Empty)) = false
|	isComplete (Node(x,y,z)) =
		if (isComplete(x)) = (isComplete(z)) then
			true	
		else
			false; 

(*Escreva uma função makeBST, do tipo ’a list -> (’a * ’a -> bool) -> ’a tree que organize os itens
da lista em uma árvore binária. Sua árvore n˜ao precisa ser balanceada. Voce pode
assumir que a lista de entrada contem somente elementos diferentes.	*)		
	
fun insert (a,Empty,c) = Node(Empty, a, Empty)
|   insert (a,Node(x,y,z),c) =
		if c(a,y) then 
			Node(insert(a, x, c),y,z)
		else 
			Node(x,y,insert(a, z, c));
fun makeBST L x = foldr (fn (i,c) => insert(i, c, x)) Empty L;			

(*Escreva uma função searchBST do tipo ’’a tree -> (’’a * ’’a -> bool) -> ’’a
-> bool que busque um elemento em uma árvore binária.*)
			
fun searchBST Empty f e = false
|	searchBST (Node(a,b,c)) f e =
		if b = e then
			true 
		else if f(e,b) then
			(searchBST a f e)
		else
			(searchBST c f e);	