type arity = int
type variable = string
type symbol = string
type signature = (symbol * arity) list
type term = V of variable | Node of symbol * (term list)
type substitution = (variable * term) list

exception NOT_UNIFIABLE

(* This function given a symbol and a list of (symbol * arity) checks if the symbol is in the list of (symbol * arity) *)
let rec check_dup (sym:symbol) (sign:signature) :bool = match sign with
      [] -> true
    | (x1,x2) :: xs -> if (sym = x1) then false else check_dup sym xs ;;
(* This function given a signature, iterates over and checks arity greater than equal to zero and symbol duplicates in the remaing list ahead of the (symbol * arity) pair *)
let rec check_sig (sign:signature) :bool = match sign with
      [] -> true
    | (x1,x2) :: xs -> if (check_dup x1 xs) && (x2>0) then check_sig xs else false ;;
(* # check_sig [("Anuj",-1)] ;;
- : bool = false
# check_sig [("Anuj",2);("Anuj",3)] ;;
- : bool = false
# check_sig [("Anuj",2);("Dipen",2);("Kumar",3)] ;;
- : bool = true *)

(* This function given a symbol, a arity and a valid signature checks if the given arity matches with the arity of the given symbol in the given signature  *)
let rec check_arity (sym:symbol) (a:arity) (sign:signature) :bool = match sign with
      [] -> false
    | (x1,x2) :: xs -> if (sym = x1) then (if a == x2 then true else false) else check_arity sym a xs ;;
(* This function given a signature and a term if it is terminal i.e. V of variable returns true else it is Node of (symbol * term list) returns true if the symbol is in the signatue with arity length of the list
and also each terms in the list should also return true with the given signature when this function is called recursively *)
let rec wfterm (sign:signature) (t:term) :bool = match t with
      V(z) -> true
    | Node(y,ys) -> if (check_arity y (List.length ys) sign) then wftermlist sign ys else false
and wftermlist (sign:signature) (tlist:term list) :bool = match tlist with
      [] -> true
    | x :: xs -> if (wfterm sign x) then wftermlist sign xs else false ;;
(* # wfterm [("Anuj",2);("Kumar",1)] (Node("Anuj",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("X");Node("Kumar",[V("Y")])])])) ;;
- : bool = true
# wfterm [("Anuj",2);("Kumar",1)] (Node("Anuj",[Node("Anuj",[Node("India",[]);Node("India",[])]);Node("Anuj",[Node("India",[]);Node("Kumar",[V"Nepal"])])])) ;;
- : bool = false
# wfterm [("Anuj",2);("Kumar",1);("India",0)] (Node("Anuj",[Node("Anuj",[Node("India",[]);Node("India",[])]);Node("Anuj",[Node("India",[]);Node("Kumar",[V"Nepal"])])])) ;;
- : bool = true *)

(* This function given a term if it is terminal i.e. V of variable returns 0 else it is Node of (symbol * term list) returns one added to max of height of each terms in the list
but if there is no terms in the list then it again returns 0 *)
let rec ht (t:term) :int = match t with
      V(z) -> 0
    | Node(y,ys) -> 1 + (htlist ys (-1))
and htlist (tlist:term list) (max:int) :int = match tlist with
      [] -> max
    | x :: xs -> if (max < ht x) then htlist xs (ht x) else htlist xs max ;;
(* # ht (Node("Anuj",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("X");Node("Kumar",[V("Y")])])])) ;;
- : int = 3
# ht (Node("Anuj",[Node("Anuj",[Node("India",[]);Node("India",[])]);Node("Anuj",[Node("India",[]);Node("Kumar",[V"Nepal"])])])) ;;
- : int = 3 *)

(* This function given a term if it is terminal i.e. V of variable returns 1 else it is Node of (symbol * term list) returns one added to sum of size of each terms in the list *)
let rec size (t:term) :int = match t with
      V(z) -> 1
    | Node(y,ys) -> 1 + (sizelist ys 0)
and sizelist (tlist:term list) (sum:int) :int = match tlist with
      [] -> sum
    | x :: xs -> sizelist xs (sum+(size x)) ;;
(* # size (Node("Anuj",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("X");Node("Kumar",[V("Y")])])])) ;;
- : int = 8
# size (Node("Anuj",[Node("Anuj",[Node("India",[]);Node("India",[])]);Node("Anuj",[Node("India",[]);Node("Kumar",[V"Nepal"])])])) ;;
- : int = 8 *)

(* This function push a variable in a variable list if it was not present in the list earlier *)
let rec pushelement (set0:variable list) (set1:variable list) (v:variable) :variable list = match set1 with
      [] -> v :: set0
    | x :: xs -> if (x = v) then set0 @ set1 else pushelement (x :: set0) xs v ;;
(* This function given two variable list iterates over list2 and push the variable in the list1 if not present earlier by calling pushelement function *)
let rec pushset (set1:variable list) (set2:variable list) :variable list = match set2 with
      [] -> set1
    | x :: xs -> pushset (pushelement [] set1 x) xs ;;
(* This function given a term if it is terminal i.e. V of variable returns a singleton list containg the variable else it is Node of (symbol * term list) returns a list of variable
that is union (with no duplicates) of variable list of each terms in the list achived by calling pushset which further call pushelement *)
let rec vars (t:term) :variable list = match t with
      V(z) -> [z]
    | Node(y,ys) -> varslist ys []
and varslist (tlist:term list) (set:variable list) :variable list = match tlist with
      [] -> set
    | x :: xs -> varslist xs (pushset set (vars x)) ;;
(* # vars (Node("Anuj",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("X");Node("Kumar",[V("Y")])])])) ;;
- : variable list = ["Y"; "X"]
# vars (Node("Anuj",[Node("Anuj",[Node("India",[]);Node("India",[])]);Node("Anuj",[Node("India",[]);Node("Kumar",[V"Nepal"])])])) ;;
- : variable list = ["Nepal"] *)

(* This function given a variable and a (variable * term) list returns that term in the given list whose variable matches with the given variable but if no variable matches with the given variable then returns V(given variable) *)
let rec replace (v:variable) (s:substitution) :term = match s with
      [] -> V(v)
    | (x1,x2) :: xs -> if (v = x1) then x2 else replace v xs ;;
(* This function given a term if it is terminal i.e. V of variable replaces the given according to given substitution else it is Node of (symbol * term list) returns a Node of (same symbol * modified term list)
where modified term list has substituted terms with given substitution and previous terms *)
let rec subst (t:term) (s:substitution) :term = match t with
      V(z) -> replace z s
    | Node(y,ys) -> Node(y,substlist ys [] s)
and substlist (oldtl:term list) (newtl:term list) (s:substitution) :term list = match oldtl with
      [] -> newtl
    | x :: xs -> substlist xs (newtl @ [subst x s]) s ;;
(* # subst (Node("Anuj",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("X");Node("Kumar",[V("Y")])])])) [("X",Node("India",[]));("Y",V("Nepal"))] ;;
- : term = Node("Anuj",[Node("Anuj",[Node("India",[]);Node("India",[])]);Node("Anuj",[Node("India",[]);Node("Kumar",[V"Nepal"])])]) *)

(* This function given a vaiable and a variable list returns true if variable is not in the list else false *)
let rec check_aval (v:variable) (set:variable list) :bool = match set with
      [] -> true
    | x :: xs -> if (v = x) then false else check_aval v xs ;;
(* This function given two substitution such that the input set of the second substitution is same as the output set of the first substitution returns its composition substitution
with the input set same as the input set of first substitution and the output set same as the output set of the second substitution *)
let rec comp (s1:substitution) (s2:substitution) :substitution = match s1 with
      [] -> s2
    | (x1,x2) :: xs -> comp xs ((x1,subst x2 s2) :: s2) ;;
(* This function given two terms if both of them are terminal that is V of variable returns a empty list if both variable are same else not same returns a singleton list with pair (variable1 * V(variable2))
else if one term is terminal and another is non terminal then returns a singleton list with pair (variable of terminal * non terminal term) if nonterminal term doesn't contain variable of terminal in its variable list else
raise exception NOT_UNIFIABLE else that is both terms are non terminals then raise exception NOT_UNIFIABLE if both symbol doesn't matches else return a substitution that is composition of all mgu of all pair of terms
obtained from tainking corresponding terms from both list of terms of these two non termninals *)
let rec mgu (t1:term) (t2:term) :substitution = match t1 with
      V(z1) -> ( match t2 with
                    V(z2) -> if (z1 = z2) then [] else [(z1,V(z2))]
                  | Node(y2,ys2) -> if (check_aval z1 (vars t2)) then [(z1,t2)] else raise NOT_UNIFIABLE )
    | Node(y1,ys1) -> ( match t2 with
                           V(z2) -> if (check_aval z2 (vars t1)) then [(z2,t1)] else raise NOT_UNIFIABLE
                         | Node(y2,ys2) -> if (y1 = y2) then mgulist ys1 ys2 [] else raise NOT_UNIFIABLE )
and mgulist (tlist1:term list) (tlist2:term list) (s:substitution) :substitution = match tlist1 with
      [] -> ( match tlist2 with
                 [] -> s
               | x2 :: xs2 -> raise NOT_UNIFIABLE )
    | x1 :: xs1 -> ( match tlist2 with
                        [] -> raise NOT_UNIFIABLE
                      | x2 :: xs2 -> mgulist xs1 xs2 (comp s (mgu (subst x1 s) (subst x2 s))) ) ;;
(* # mgu (Node("Anuj",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("X");Node("Kumar",[V("Y")])])])) (Node("Anuj",[Node("Anuj",[V("X");V("Y")]);Node("Anuj",[Node("Kumar",[V("Y")]);V("Z")])])) ;;
Exception: NOT_UNIFIABLE.
# mgu (Node("Anuj",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("Z");Node("Kumar",[V("Y")])])])) (Node("Anuj",[Node("Anuj",[V("X");V("Y")]);Node("Anuj",[Node("Kumar",[V("Y")]);V("X")])])) ;;
Exception: NOT_UNIFIABLE.
# mgu (Node("Dipen",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("Z");Node("Kumar",[V("Y")])]);Node("Aniket",[])])) (Node("Dipen",[Node("Anuj",[V("X");V("Y")]);Node("Anuj",[Node("Kumar",[V("Y")]);V("W")]);Node("Abhishek",[])])) ;;
Exception: NOT_UNIFIABLE.
# mgu (Node("Dipen",[Node("Anuj",[V("X");V("X")]);Node("Anuj",[V("Z");Node("Kumar",[V("Y")])]);Node("Nishant",[])])) (Node("Dipen",[Node("Anuj",[V("X");V("Y")]);Node("Anuj",[Node("Kumar",[V("Y")]);V("W")]);Node("Nishant",[])])) ;;
- : substitution = [("W", Node ("Kumar", [V "Y"])); ("Z", Node ("Kumar", [V "Y"])); ("X", V "Y")] *)
