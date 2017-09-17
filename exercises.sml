(*Defina a fun¸c˜ao mymap, que tenha o mesmo tipo e comportamento de map
VERSÃO SEM USAR FOLDR, FOLDL, OU MAP *)

fun mymap (f, nil) = nil
   |mymap (f, h::t) = (f h)::mymap(f, t);
   
(*Defina a funç˜ao myfoldr, que tenha o mesmo tipo e comportamento de foldr.*)
	  
fun myfoldr f x nil = x
|   myfoldr f x (h::t) = f(h, myfoldr f x t);

(*Defina a funç˜ao myfoldl, que tenha o mesmo tipo e comportamento de foldl*)

fun myfoldl f x nil = x
|  myfoldl f x (h::t) = myFoldl f (f(h, x)) t;

