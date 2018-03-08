(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* <Madison> <Fichtner>
* <madisontfichtner@msn.com>
*
***************************************************************)

(* Define your data type and functions here *)

(* Creates a datatype of two different types. Either Set or Empty *)
datatype 'element set = Empty | Set of 'element * 'element set;

(* warmup function *)
fun f [] = [] (* a *)(* This line creates an empty list as the base case *)
  | f (x::xs) = (x + 1) :: (f xs); (* b *)(* Given a list, take the head and add 1. The xs is sent through f again and the result of the head plus 1 is added to the list *)

(*Checks if element is a member of the Set*)
fun isMember e Empty = false    (* If the set passed is empty, then the element isn't a member *)
  | isMember e (Set (x,xs)) = e = x orelse isMember e xs;     (* if the element being checked is the same as the head element, return true, otherwise call ismember with the tail *)

(* Determines if all members make up set *)
fun isSet Empty = true
  | isSet (Set (x,xs)) = if (isMember x xs) then false else isSet xs;

(* Converting a list to a set *)
fun list2Set[] = Empty        (* If the function is passed an empty list then it returns a empty set *)
  | list2Set (x::xs) =
    let
      fun setBuilder [] = Empty       (* creates a new set with duplicates *)
        | setBuilder (x::xs) = Set(x, setBuilder (xs))
    in
      if isMember x (setBuilder(xs)) then list2Set(xs)      (* If x is found multiple times in a set, don't include x and continue one calling list2set with the tail *)
                      else Set(x, list2Set(xs))
    end;

(* Takes two sets and returns the union of the two entered sets *)
fun union set1 set2 = case set1 of Empty => set2
  | Set(head,tail) =>
    let
      val s = Set(head, set2)
    in
      if isMember head set2 then union tail set2
      else union tail s
    end;

(* Takes two sets and returns the union of the two entered sets *)
fun intersect set1 set2 =  case set1 of Empty => Empty  (* if set1 is empty, return an empty set *)
  | Set(head,tail) =>
      if isMember head set2 then Set(head, (intersect tail set2))
      else intersect tail set2;

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
print "\nQuestion 1: ";
f[3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
