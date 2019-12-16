(* 2. COMPRESSION DES ABR
        --> Construction d'un arbre compressé à partir d'un ABR
*)

open Abr;;


(*Structure utilisée pour la éfinition d'un ABR compressé 
        --> | VIDE 
              | racine, des références vers deux ACL (gauche et droit) 

*)
type acl = Empty_acl
           | Node_acl of (n_acl *  acl ref *  acl ref)
and n_acl ={
    mutable vals: (int * int list) list;
    mutable a_fg: int list;
    mutable a_fd: int list}
    
(*
représentation de la liste de structure de l'arbre , en gardant des référeneces sur ce dernier
*)      
type strs_acl={
    mutable str_list: str_acl list}
and str_acl={
    str:string;
    mutable p: acl ref }

(*
fonction de recherche et verification si on est déjà tombé sur une même structure s dans une liste de structure strs    
    --> return false et une référence vers un arbre vide si c'est la première fois qu'on voit la structure 
    -->return true et ref de l'abr sinon 
    --> string -> str_acl -> acl ref * bool  
*)
let deja_vu_acl s strs =
  let rec rec_deja_vu str l =
    match l with
      | [] -> (ref Empty_acl,false)
      | h::t -> if((compare s h.str)==0) then (h.p,true) else rec_deja_vu s t
  in
    rec_deja_vu s strs.str_list


(*
fonction d'ajout d'une nouvelle structure 
    --> string -> strs_acl -> acl ref -> strs_acl
*)

let add_str_acl s strs r_node =
  strs.str_list <- (strs.str_list)@[{str=s;p=r_node}];strs


(*
fonction de combinaison d'un abr et abr compressé 
    --> int abr -> acl ref -> int list -> acl ref
*)
let rec comb_abr_acl abr racl et_l =
    match (abr,!racl) with 
    | (Node_abr(abrv,abrfg,abrfd),Node_acl(nacl,raclfg,raclfd)) ->
        let et_l_fg = et_l@(nacl.a_fg) in
        let et_l_fd = et_l@(nacl.a_fd) in(
        ignore (comb_abr_acl abrfg raclfg et_l_fg);
        ignore (comb_abr_acl abrfd raclfd et_l_fd);
        nacl.vals <- nacl.vals@[(abrv,et_l)];
        racl)
    | _ -> ref Empty_acl 

(* la fonction de compression 
    --> int abr -> acl 
*)
let comp_abr_acl abr=
  let rec rec_compress abr str_l=
    let str=struct_abr abr in
    let tup=(deja_vu_acl str str_l) in
      match tup with
        | (x,true) ->(
            match (!x) with
            | Node_acl(_,_,_) ->
                let new_id=( gen_id ()) in
                    ((comb_abr_acl abr x [new_id]),[new_id])
            | _ -> assert false)
        | (x,false) ->
            match (!x,abr) with 
            | (Empty_acl,Node_abr (abrv,abrfg,abrfd)) ->(
                let (rfg,idsfg)= (rec_compress abrfg str_l) in
                let (rfd,idsfd)= (rec_compress abrfd str_l) in                
                let content={vals=[(abrv,[])];a_fg=idsfg;a_fd=idsfd} in  
                let ref_nn = (ref (Node_acl(content,
                                rfg,
                                rfd))) in 
                        ignore (add_str_acl str str_l ref_nn);
                        (ref_nn,[]))
            | (Empty_acl,Empty_abr) -> (ref Empty_acl,[])
            | _ -> assert false
    in
        match (rec_compress abr ({str_list=[]})) with (a,l) -> !a;;


(* RECHERCHE *)
(* recherche de tuples 
    --> int * int list -> n-acl -> int * (int * int list)
*)
let search_tup_in_nacl tup n =
    let rec rsc tup cpls n =
        match cpls with
        | [] -> assert false
        | (v,ets)::t ->
                match (comp_tups tup (v,ets),tup) with        
                | (0,(tp1,tp2)) -> (0,(v,ets))
                | (1,(tp1,tp2)) -> (1,(tp1,(tp2@(n.a_fd))))
                | (-1,(tp1,tp2)) -> (-1,(tp1,(tp2@(n.a_fg))))
                | (-2,(tp1,tp2)) -> rsc tup t n
                | _ -> assert false   
    in
        rsc tup (n.vals) n
(* Recherche valeur dans acl
    --> int -> acl -> int * int list
*)

let search_val_in_acl v acl =
    let rec rsv tup acl = 
        match acl with 
        | Empty_acl -> assert false
        | Node_acl (n,rfg,rfd) ->
            let res=(search_tup_in_nacl tup n) in
                match res with
                | (0,x) -> x
                | (1,x) -> rsv x (!rfd)
                | (-1,x) -> rsv x (!rfg)
                | _ -> assert false
    in
        rsv (v,[]) acl

(* rcheche une valeur v dans une liste l
    --> 'a -> 'a list -> bool 
*)
let rec v_in_l v l =
    match l with
    | [] -> false
    | h::t -> if (h=v) then true else v_in_l v t
    
    
    

