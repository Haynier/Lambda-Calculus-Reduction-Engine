val counter = ref 0;

fun getFresh () = (
  counter := (!counter) + 1; "😱" ^ (Int.toString(!counter)));

datatype lambda = 
    LM of string * lambda
  | AP of lambda * lambda
  | VA of string;

fun toString (l as AP(t1,t2)) = "AP(" ^ (toString t1) ^ "," ^ (toString t2) ^ ")"
  | toString (l as LM(x,t)) = "LM(" ^ x ^ "," ^ (toString t) ^ ")"
  | toString (l as VA(x)) = "VA'" ^ x ^ "'";

fun isR (AP(LM(x,t),s)) = true
  | isR (AP (t1,t2)) = (isR t1) orelse (isR t2)
  | isR (LM (x,t)) = isR t
  | isR (VA(x)) = false;

fun replace (y, r, AP(t1,t2)) = AP(replace (y,r,t1),replace (y,r,t2))
  | replace (y,r,LM(x,t)) = 
    if y = x then LM(x,t) (* <= this case is weird *)
    else (let val z = getFresh() in   
    LM(z, replace (x,VA(z),replace(y,r,t)))(* <= this should be the fresh variable case *)
    end)
  | replace (y,r,VA(x)) = if y = x then r else VA(x);

fun reduce (AP(LM(x,t),s)) = replace (x,s,t)
  | reduce (AP (t1,t2)) = AP(reduce t1, reduce t2)
  | reduce (LM (x,t)) = LM(x,reduce t)
  | reduce (VA (x)) = VA(x);

fun reduceV (AP(LM(x,t),s)) = (print ("[" ^ x ^ "/"^ (toString s)^"]\n") ;replace (x,s,t))
  | reduceV (AP (t1,t2)) = AP(reduce t1, reduce t2)
  | reduceV (LM (x,t)) = LM(x,reduce t)
  | reduceV (VA (x)) = VA(x);

fun norReduce f = ( 
  if isR f then norReduce (reduce f)
  else f);

fun norReduceVerbose f = ( 
  if isR f then norReduceVerbose (reduceV f)
  else f);

fun pretty (LM(x,t)) = "fn " ^ x ^ " => " ^ (pretty t)
  | pretty (AP(t1,t2)) = (pretty t1) ^ "(" ^ (pretty t2) ^ ")"
  | pretty (VA(x)) = x;

