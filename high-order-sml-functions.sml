(*feitos ultizando as funções de alta ordem map, foldr, e foldl, além das funções predefinidas da linguagem*)

(*Escreva uma função il3rl, de tipo int list -> real list que receba uma lista
de inteiros e retorne uma lista com cada valor convertido para real. Por exemplo, a
chamada il2rl [1, 2, 3] deveria retornar [1.0, 2.0, 3.0]. *)

fun il3rl L = map(fn x => (Real.fromInt)x) L;

(*Escreva uma função ordList, de tipo char list -> int list que receba uma lista
de caracteres e retorne uma lista com os c´odigos ASCII destes caracteres. Por exemplo,
a chamada ordList [#"A", #"b", #"C"] deveria retornar [65, 98, 67].*)

fun ordList L = map (fn x => (Char.ord) x) L;

(*Escreva uma função squareList, de tipo int list -> int list que receba uma lista
de inteiros e retorne uma lista dos quadrados destes inteiros. Por exemplo, a chamada
squareList [1, 2, 3, 4] deveria retornar [1, 4, 9, 16].*)

fun squareList L = map(fn x => x*x) L;

(*Escreva uma função multPairs, de tipo (int * int) list -> int list que receba
uma lista de pares de inteiros e retorne uma lista com os produtos de cada par. Por
exemplo, a chamada multPairs [(1, 2), (3, 4)] deveria retornar [2, 12].*)

fun multPairs L = map(op * ) L;

(*Escreva uma função incList, de tipo int list -> int -> int list que receba uma
lista de inteiros e um inteiro que ser´a usado como incremento. A função deve retornar
a mesma lista de entrada, exceto que o incremento ser´a somado a cada elemento de
entrada. Por exemplo, a chamada incList [1, 2, 3, 4] 10 deveria retornar [11,
12, 13, 14]. Note que a função ´e currificada.*)

fun incList L x = map(fn n => n+x) L;

(*Escreva uma função sqSum, de tipo int list -> int que receba uma lista de inteiros
e retorne a soma dos quadrados destes inteiros. Por exemplo, a chamada sqSum [1,
2, 3, 4] deveria retornar 30*)

fun sqSum L = foldr(op +) 0 (map(fn x => x*x) L);

(*Escreva uma função bor, de tipo bool list -> bool que receba uma lista de booleanos
e retorne o or lógico de todos eles. Se a lista estiver vazia, ent˜ao false deve ser retornado.*)

fun bor L = foldr (fn (a,b) => a orelse b) false L;

(*Escreva uma função band, de tipo bool list -> bool que receba uma lista de booleanos
e retorne o and l´ogico de todos eles. Se a lista estiver vazia, ent˜ao true deve ser retornado.*)

fun band L = foldl (fn (a,b) => a andalso b) true L;

(* Escreva uma função bxor, de tipo bool list -> bool que receba uma lista de booleanos
e retorne o or exclusivo de todos eles. Isto quer dizer que esta função deveria retornar
true se o n´umero de valores true na lista for ´ımpar, e false caso contr´ario. Se a lista
estiver vazia, ent˜ao false deve ser retornado.*)

fun bxor L = foldl (fn(a,b) => (a orelse b) andalso not(a andalso b)) false L;

(* Escreva uma função dupList, de tipo ’a list -> ’a list, cujo resultado seja a
lista de entrada com cada elemento repetido em sequˆencia. Por exemplo, a chamada
dupList [1, 2 ,3] deveria retornar [1, 1, 2, 2, 3, 3]. Se a lista de entrada ´e
nula, o resultado deve ser a lista nula.*)

fun dupList L = foldr (fn (a, b) => a::a::b) [] L;

(*Escreva uma função myLength, de tipo ’a list -> int que retorne o tamanho da
lista de entrada. Obviamente vocˆe n˜ao pode usar a função predefinida length para
resolver este exercício*)

fun myLenght L = foldr (fn (x,y) => 1+y) 0 L;

(* Escreva uma função is2absrl, de tipo int list -> real list que receba uma lista
de inteiros e retorne uma lista contendo o valor absoluto de cada um destes inteiros,
convertido em um n´umero real.*)

fun is2absrl L = map(fn x => abs(Real.fromInt(x))) L;

(* Escreva uma função trueCount, de tipo bool list -> int que receba uma lista de
valores booleanos e retorne o n´umero de valores verdadeiros nesta lista.*)

fun trueCount L = foldl (fn (x,y) => if x = true then 1+y else y) 0 L;

(*Escreva uma função maxPairs, de tipo (int * int) list -> int list que receba
uma lista de pares de inteiros e retorne a lista formada pelos maiores elementos de
cada par. Por exemplo, a chamada maxPairs [(1, 3), (4, 2), ( 3, 4)] deveria
retornar a lista [3, 4, 3].*)

fun maxPairs L = map (fn (x,y) => if x > y then x else y) L;

(* Escreva uma função myImplode que funcione como a função implode, predefinida em
SML/NJ. Em outras palavras, esta função deve ter o tipo char list -> string. A
função recebe uma lista de caracteres e retorna uma string contendo estes mesmos
caracteres, na mesma ordem.*)

fun myimplode L = foldr (op^) "" (map str L);

(* Escreva uma função lconcat, de tipo ’a list list -> ’a list que receba uma lista
de listas como entrada e retorne uma lista formada pela composi¸c˜ao das listas de entrada,
em ordem. Por exemplo, a chamada lconcat [[1, 2], [3, 4, 5, 6], [7]]
deveria retornar [1, 2, 3, 4, 5, 6, 7]. Note que existe uma função predefinida,
chamada concat, que faz exatamente isto. Obviamente vocˆe n˜ao pode usar esta
função.*)

fun lconcat L = foldr (fn (x,y) => x@y) [] L;

(*Escreva uma função max, de tipo int list -> int que retorne o maior inteiro dentre
os n´umeros da lista de entrada. A sua função pode gerar um erro se acaso a lista de
entrada estiver vazia*)

fun max (h::t) =  foldl Int.max h t;

(*Escreva uma função min, de tipo int list -> int que retorne o menor inteiro dentre
os n´umeros da lista de entrada. A sua função pode gerar um erro se acaso a lista de
entrada estiver vazia.*)

fun min (h::t) =  foldl Int.min h t;

(*Escreva uma função member, de tipo ’’a * ’’a list -> bool, tal que a chamada
member (e, L) seja verdadeira se, e somente se, o elemento e pertence a lista L.*)

fun member (x,L)= foldl (fn (y,b)=>b orelse x=y) false L

(*Escreva uma função append, de tipo ’a list -> ’a list -> ’a list que receba
duas listas, e retorne a lista resultante da concatena¸c˜ao da segunda sobre a primeira.
Por exemplo, a chamada append [1, 2, 3] [4, 5, 6] deveria retornar [1, 2, 3,
4, 5, 6]. N˜ao use a função predefinida concat, tampouco use o operador @.*)

fun append L1 L2 = foldr (op::) L2 L1;

(* Escreva uma função less, de tipo int * int list -> int list tal que less(e, L)
retorne uma lista com todos os elementos de L que sejam menores que e.*)

fun less (e, L) = foldr (fn (a, t) => if a < e then a::t else t) [] L;

(*Escreva uma função evens, de tipo int list -> int list que receba uma lista de
inteiros e retorne a lista de todos os inteiros pares da lista original. Por exemplo, a
chamada evens [1, 2, 3, 4] deveria retornar [2, 4].
*)
fun evens L = foldr (fn (a, t) => if a mod 2 =0 then a::t else t) [] L;

(*Escreva uma função convert, de tipo (’a * ’b) list -> ’a list * ’b list que
converta uma lista de pares em um par de listas. Por exemplo, a chamada convert
[(1, 2), (3, 4), (5, 6)] deveria retornar a lista [1, 3, 5], [2, 4, 6].*)

fun convert l= foldr (fn((x,y),(u,v))=>(x::u,y::v)) (nil,nil) l;

(*Escreva uma função myMap, que possua o mesmo tipo e a mesma semˆantica de map.
Claro que vocˆe n˜ao pode usar a função predefinida map.
*)
fun mymap f L = foldr (fn (x, t) => (f x)::t) [] L;

(*Neste exercício, um polinômio será representado usando uma lista com os seus coefi-
cientes reais, come¸cando com a constante, e tendo como ultimo elemento o coeficiente
de grau mais alto. Por exemplo, 3x^2 + 5x + 1 seria representado como a lista [1.0,5.0, 3.0]. Ja x
3−2x seria representado como [0.0, 2.0, 0.0, 1.0]. Escreva uma função eval, de tipo real list -> real -> real que receba um polinˆomio representado
desta forma, mais um valor de x, e retorne o valor do polinomio para aquele dado
x. Por exemplo, eval([1.0, 5.0, 3.0], 2.0) deveria produzir o resultado 23.0, j´a
que, se x = 2, ent˜ao 3x^2 + 5x + 1 = 23. Este exercicio ja apareceu na lista 6, exceto
que agora o tipo de função é currificado, e a resposta deve ser escrita com uma linha
somente.*)

fun eval (L, x) = foldr (fn (y,a) => y + x*a) 0.0 L;



(*NÃO SÃO DE ALTA ORDEM (RECURSIVAS)*)

fun min3 (a, b, c):real = 
    if a < b andalso a < c then a
        else if b < a andalso b < c then b
        else c;

fun mid3 (a, b, c):real = 
    if (a < b andalso a > c) orelse (a > b andalso a < c) then a
        else if (b < a andalso b > c) orelse (b > a andalso b < c) then b
        else c;

fun max3 (a, b, c):real = 
    if a > b andalso a > c then a
    else if b > a andalso b > c then b
        else c;

fun sort3 (a, b, c) = 
    min3(a, b, c)::mid3(a, b, c)::max3(a, b, c)::[];





