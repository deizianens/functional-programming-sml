(*Escreva uma fun¸c˜ao il3rl, de tipo int list -> real list que receba uma lista
de inteiros e retorne uma lista com cada valor convertido para real. Por exemplo, a
chamada il2rl [1, 2, 3] deveria retornar [1.0, 2.0, 3.0]. *)

fun il3rl L = map(fn x => (Real.fromInt)x) L;

(*Escreva uma fun¸c˜ao ordList, de tipo char list -> int list que receba uma lista
de caracteres e retorne uma lista com os c´odigos ASCII destes caracteres. Por exemplo,
a chamada ordList [#"A", #"b", #"C"] deveria retornar [65, 98, 67].*)

fun ordList L = map (fn x => (Char.ord) x) L;

Escreva uma fun¸c˜ao squareList, de tipo int list -> int list que receba uma lista
de inteiros e retorne uma lista dos quadrados destes inteiros. Por exemplo, a chamada
squareList [1, 2, 3, 4] deveria retornar [1, 4, 9, 16].

fun squareList L = map(fn x => x*x) L;

Escreva uma fun¸c˜ao multPairs, de tipo (int * int) list -> int list que receba
uma lista de pares de inteiros e retorne uma lista com os produtos de cada par. Por
exemplo, a chamada multPairs [(1, 2), (3, 4)] deveria retornar [2, 12].

fun multPairs L = map(op * ) L;

fun incList L x = map(fn n => n+x) L;

fun sqSum L = foldr(op +) 0 (map(fn x => x*x) L);

fun bor L = foldr (fn (a,b) => a orelse b) false L;

fun band L = foldl (fn (a,b) => a andalso b) true L;

fun bxor L = foldl (fn(a,b) => (a orelse b) andalso not(a andalso b)) false L;

fun dupList L = foldr (fn (a, b) => a::a::b) [] L;

fun myLenght L = foldr (fn (x,y) => 1+y) 0 L;

fun is2absrl L = map(fn x => abs(Real.fromInt(x))) L;

fun trueCount L = foldl (fn (x,y) => if x = true then 1+y else y) 0 L;

fun maxPairs L = map (fn (x,y) => if x > y then x else y) L;

fun myimplode L = foldr (op^) "" (map str L);

fun lconcat L = foldr (fn (x,y) => x@y) [] L;

fun max (h::t) =  foldl Int.max h t;

fun min (h::t) =  foldl Int.min h t;

fun member (x,L)= foldl (fn (y,b)=>b orelse x=y) false L

fun append L1 L2 = foldr (op::) L2 L1;

fun less (e, L) = foldr (fn (a, t) => if a < e then a::t else t) [] L;

fun evens L = foldr (fn (a, t) => if a mod 2 =0 then a::t else t) [] L;

fun convert l= foldr (fn((x,y),(u,v))=>(x::u,y::v)) (nil,nil) l;

fun mymap f L = foldr (fn (x, t) => (f x)::t) [] L;

fun eval (L, x) = foldr (fn (y,a) => y + x*a) 0.0 L;



///////NÃO SAO DE ALTA ORDEM (RECURSIVAS)

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





