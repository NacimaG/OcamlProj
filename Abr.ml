
(* 
        -->PARTIE 1.1
Construction ABR simple 
*)
(*Question 1.1 
    Extraction Alea
    'a list -> 'a list -> 'a list * 'a list 
    prend 2 liste en paramètre renvoie un couple de liste 
    retourn un couple de liste 
         *)


let extraction_alea l p =
  let rec extract ls ind ranv fl sl=
    match fl with
      |[] -> (fl,sl)
      |hfl::tfl ->
          if (ind==ranv) then
            (ls@tfl,hfl::sl)
          else
          extract (ls@[hfl]) (ind+1) (ranv) tfl sl
  in
    extract [] 0 (Random.int (List.length l)) l p


(*Question 1.2
        Gen_list
        int -> int list 
--> pour faire on a introsuit une fonction gen_list , qui permet de générer une liste aléatoire 
*)
let rec gen_list n =
  match n with
    | 0 -> []
    | _ -> n::(gen_list  (n-1))

(*gen_permutration
    int -> int list 
*)


let gen_permutation n =
  let l=gen_list n in
  let p=[] in
  let rec rec_gen_perm l p n =
    match n with
      |0-> p
      |_->
          let (l,p)=extraction_alea l p in
            rec_gen_perm l p (n-1)
  in
    rec_gen_perm l p n

(*Structure d'une Arbre Binaire de Recherche *)

type 'a abr = Empty_abr
            | Node_abr of ('a * 'a abr * 'a abr)

(*Fonction d'insertion d'une valeur dans un Abr
    'a -> 'a abr -> 'a abr 
 *)

let rec ins v node =
  match node with
    |Empty_abr -> Node_abr (v,Empty_abr,Empty_abr)
    |Node_abr (x,fg,fd)-> if x<v then Node_abr (x,fg,(ins v fd)) else Node_abr (x,(ins v fg),fd)


(*Construction d'un ABR à partir d'unr liste 
    --> appel à chaque insertion la methode ins dans un abr
    'a list -> 'a abr -> 'a abr
*)

let rec list_to_abr l n=
  match l with
    | []->n
    | h::t -> list_to_abr t (ins h n)

    (* Génération d'un abr à aprtir d'un entier n 
    --> à partir de ce n , on génére une liste d'entier avec gen_permutation sur laquelle on appel la methode "list-to-abr" 
    int -> int abr
  *)

let gen_abr n = 
    list_to_abr (gen_permutation n) Empty_abr


(*Fonction d'affichage d'un abr 
    --> introduction de la structure parenthésée
    int abr -> string    

    *)

let rec print_tree a =
  match a with
    |Empty_abr -> ""
    |Node_abr (x,fg,fd)->
        "("^print_tree(fg)^(string_of_int x)^print_tree(fd)^")"

(* Fonction de représentation d'un abr avec les structure de parenthèses 
    --> 'a abr -> string

*)
let rec struct_abr node=
  match node with
    | Empty_abr -> ""
    | Node_abr (x,fg,fd)-> "("^struct_abr fg ^")"^ struct_abr fd

(* Gestion des étiquettes  
    --> Différentes structures permettant de les incrémenter decrémenter remettre à 0
    --> int ref = {contents = 0}
    --> unit -> unit 
    --> unit -> unit
    --> unit -> unit
    --> unit -> int 

*)

let cpt = ref 0
let cpt_incr () = cpt := !cpt+1
let cpt_decr () = cpt := !cpt-1
let cpt_reset () = cpt := 0
let gen_id () =
    cpt_incr(); !cpt



(* position d'un noeud dans l'arbre 
        --> Gauche, Droite ou racine *)        
type pos = G | D | R

(* Comparaison de listes 
    'a list -> 'a list -> int 

*)

let rec comp_ids et_l1 et_l2 =
    let rec c_e et_l1 et_l2 =     
        match (et_l1,et_l2) with
        | (h1::t1,h2::t2) -> if(h1>h2) then 1 else (if (h1<h2) then -1 else c_e t1 t2)
        | ([],h2::t2) -> -1
        | (h1::t1,[]) -> 1               
        | ([],[]) -> 0
        in
            c_e et_l1 et_l2
    
    
   (* Comparaison tuples 
    --> pour la constructon recherche de tuples
    --> 'a * 'b list -> 'a * 'b list -> int 
 *)   
let comp_tups p1 p2 =
        match (p1,p2) with
        | ((v1,el1),(v2,el2)) ->
            match (comp_ids el1 el2) with
            | 0 -> if(v1>v2) then 1 else(if(v1<v2) then -1 else 0)
            | _ -> -2
            
 (*Construction d'une string représentant une liste 
    --> int list -> string

*)           
let intl_to_str l =
    let rec r_its l =
        match l with 
        | [] -> ""
        | h::t -> if(t=([])) then (string_of_int h) else ((string_of_int h)^";"^(r_its t))
    in
    "["^(r_its l)^"]"
            
    (* Fonction de recherche dans un ABR simple
    --> 'a -> 'a abr -> int * 'a
 *)    

let rec search_val_in_abr v abr=
    match abr with 
    | Empty_abr -> assert false
    | Node_abr(nv,fg,fd) -> if (v=nv) then (0,v) else (if (v>nv) then (search_val_in_abr v fd) else (search_val_in_abr v fg))
            
       (* rotation gauche 
    --> 'a abr -> 'a-> abr 
*)      
let rg abr = 
    match abr with 
    | Node_abr(p,u,Node_abr(q,v,w)) ->
        Node_abr(q,Node_abr(p,u,v),w)
    | _ -> assert false

(* rotation droite
    --> 'a abr -> 'a-> abr 
*) 
let rd abr = 
    match abr with 
    | Node_abr(q,Node_abr(p,u,v),w) ->
        Node_abr(p,u,Node_abr(q,v,w))
    | _ -> Empty_abr

(* rotation gauche droite 

    --> 'a abr -> 'a-> abr
*) 
let rgd abr = 
    match abr with 
    | Node_abr(q,Node_abr(p,u,Node_abr(r,v1,v2)),w) ->
        Node_abr(r,Node_abr(p,u,v1),Node_abr(q,v2,w))
    | _ -> Empty_abr
    
(* rotation droite  gauche 

    --> 'a abr -> 'a-> abr

*)     
let rdg abr = 
    match abr with 
    | Node_abr(q,w,Node_abr(p,Node_abr(r,v1,v2),u)) ->
        Node_abr(r,Node_abr(q,w,v1),Node_abr(p,v2,u))
    | _ -> Empty_abr    

(* hauteur arbre 
    --> 'a abr -> int
*)
let rec h abr =
    match abr with 
    | Empty_abr -> 0
    | Node_abr(_,fg,fd) -> max (1+ (h fg)) (1+(h fd))

    (* différence hateur fils gauche et fils droit 
    --> 'a abr -> int

*)
    
let diff_h abr =
    match abr with
    | Empty_abr -> 0
    | Node_abr(_,fg,fd) -> (h fd)-(h fg)
    (* equilibrer l'arbre 
    'a abr -> 'a -> abr
*)
    
let equi abr =
    match ((diff_h abr),abr) with
    | (x,Node_abr(_,Node_abr(_,Node_abr(_,_,_),_),_)) -> if (x < (-1)) then (rd abr) else abr
    | (x,Node_abr(_,_,Node_abr(_,_,Node_abr(_,_,_)))) -> if (x > 1) then (rg abr) else abr
    | (x,Node_abr(_,Node_abr(_,_,Node_abr(_,_,_)),_)) -> if(x < (-1)) then (rgd abr) else abr
    | (x,Node_abr(_,_,Node_abr(_,Node_abr(_,_,_),_))) -> if(x > 1) then (rdg abr) else abr
    | _ -> abr
    
    
    
    
    
    
    
    
    
    
    
