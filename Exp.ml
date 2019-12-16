open Abr;;
open Acl;;
open Acm;;
open AcmC;;

(** Tests de validité **) 
    
(* Recherche acl      
    --> int list -> acl -> string 
*)
let rec str_srch_acl vals tr =
    match vals with 
    | h::t ->(
        match (search_val_in_acl h tr) with
        |(v,l) -> 
            "("^(string_of_int v)^","^(intl_to_str l)^")\n"^(str_srch_acl t tr)
        )
    | [] -> "";;    
                           
(* tester la compression et recherche dans des acl 
    --> int list -> int abr -> unit *)                           
let test_acl intl abr= 
        Printf.printf "%s\n" (str_srch_acl intl (comp_abr_acl abr));;

(*Recherche acm 
    --> int list -> acm -> string  *)        
let rec str_srch_acm vals tr =
    match vals with 
    | h::t ->(
        match (search_val_in_acm h tr) with
        |(v,l) -> 
            "("^(string_of_int v)^","^(intl_to_str l)^")\n"^(str_srch_acm t tr)
        )
    | [] -> "";;    
                                   
(* teste la compression et la recherche dans les acm
    --> int list -> int abr -> unit  *)
                           
let test_acm intl abr= 
        Printf.printf "%s\n" (str_srch_acm intl (comp_abr_acm abr));;
        
(*Recherche acmC
    --> int list -> acmCouter -> string  *)      
  
let rec str_srch_acmC vals tr =
    match vals with 
    | h::t ->(
        match (search_val_in_acmC h tr) with
        |(v,l) -> 
            "("^(string_of_int v)^","^(intl_to_str l)^")\n"^(str_srch_acmC t tr)
        )
    | [] -> "";;    
                                   
(* teste la compression et la recherche dans les acm
    --> int list -> int abr -> unit  *)                        
let test_acmC intl abr= 
        Printf.printf "%s\n" (str_srch_acmC intl (comp_abr_acmC abr));;


(* focntion permattant e tester les fonctions ci-dessus 
     --> int -> unit    *)   
let full_test n = 
    let intl=(gen_permutation n) in
    let abr = (list_to_abr intl Empty_abr) in(
        Printf.printf " test acl \n";
        test_acl intl abr;
        cpt_reset();
        Printf.printf " test acm \n";
        test_acm intl abr;
        cpt_reset();
        Printf.printf " test acmC \n";        
        test_acmC intl abr
    );;

Printf.printf " FULL TEST \n";
full_test 18;;

Printf.printf " END FULL TEST \n";;


(* calcul de la taille memoire prise par une structure de données en nombre de mots
    --> 'a -> int * Obj.t list  *)
let sizeof v =
  let rec rec_size d r =
    if List.memq r d then (1, d) else
    if not(Obj.is_block r) then (1, r::d) else
    if (Obj.tag r) = (Obj.double_tag) then (2, r::d) else
    if (Obj.tag r) = (Obj.string_tag) then (Obj.size r, r::d) else
    if (Obj.tag r) = (Obj.object_tag) ||
       (Obj.tag r) = (Obj.closure_tag)
    then invalid_arg "please only provide datas"
    else
      let len = Obj.size r in
      let rec aux d sum i =
        if i >= len then (sum, r::d) else
        let this = Obj.field r i in
        let this_size, d = rec_size d this in
        aux d (sum + this_size) (i+1)
      in
      aux d (1) 0
  in
  rec_size [] (Obj.repr v);;

(* TESTs *)

let intl=[4;2;3;8;1;9;6;7;5] in
let abr=(list_to_abr intl Empty_abr) in
let acm= comp_abr_acm abr in
let acl= comp_abr_acl abr in
let acmC=comp_abr_acmC abr in(  
        cpt_reset();
        Printf.printf " test acl \n";
        test_acl intl abr;
        cpt_reset();
        Printf.printf " test acm \n";
        test_acm intl abr;
        cpt_reset();
        Printf.printf " test acmC \n";
        test_acmC intl abr;
Printf.printf "abr = %d\n" (match (sizeof abr) with | (a,l) -> a);
Printf.printf "acl = %d\n" (match (sizeof acl) with | (a,l) -> a);
Printf.printf "acm = %d\n" (match (sizeof acm) with | (a,l) -> a);
Printf.printf "acmC = %d\n" (match (sizeof acmC) with | (a,l) -> a)
)
;;

(* Espace occupé en  noeuds *)  
(* calcul du nombre de noeuds, de clés, et d'ids créer quand on compresse un abr
    --> 'a -> 'a list -> bool  *)
let rec r_in_l r l =
    match l with
    | [] -> false
    | h::t -> if(r=h) then true else r_in_l r t;;

(* calcul du nombre de noeuds créer dans un acl
    --> acl -> int  *) 
let nbr_noeuds_acl acl = 
    let rec nn acl rl =
        match acl with 
        | Empty_acl -> 0
        | Node_acl(nl,fg,fd) -> if (r_in_l (ref (acl)) (!rl)) then 0
                                else
                                    (rl:=((ref (acl))::(!rl));
                                    (1+(nn !fg rl)+(nn !fd rl)))
    in
        nn acl (ref []);;
(* calcul du nombre de clefs créer dans un acl 
    --> acl -> int   *)
let nbr_clefs_acl acl = 
    let rec nc acl rl=
        match acl with 
        | Empty_acl -> 0
        | Node_acl(nl,fg,fd) -> if (r_in_l (ref (acl)) (!rl)) then 0
                                else
                                    (rl:=((ref (acl))::(!rl));
                                    ((List.length (nl.vals))+(nc !fg rl)+(nc !fd rl)))
    in
        nc acl (ref []);;
(* calcul du nombre d'ids créer dans un acl 
    --> ('a * 'b list ) list -> int  *)
let rec nbid l =
    match l with
    | [] -> 0
    | (v,idl)::t -> (List.length idl)+(nbid t)

(* calcul du nombre de noeuds créer dans un acl
    --> int abr -> int * int * int  *)
let nbr_nci_ac abr =
    let rec rn acl rl =
        match acl with 
        | Empty_acl -> (0,0,0)
        | Node_acl(nl,fg,fd) ->
            if (r_in_l (ref (acl)) (!rl)) then (0,0,0)
            else
                (rl:=((ref (acl))::(!rl));
                let v1,v2,v3 = (rn !fg rl) in
                let v4,v5,v6 = (rn !fd rl) in
                    (v1+v4+1,v2+v5+(List.length nl.vals),v3+v6+(nbid nl.vals)))
    in
        rn (comp_abr_acl abr) (ref []);;
        
(*    --> int list -> string -> int  *)        
let nbr_nci_to_file intl outf=
    let rec build intl l = 
        match intl with 
        | [] -> l
        | h::t -> let v1,v2,v3 =(nbr_nci_ac (list_to_abr (gen_permutation h) Empty_abr))  in
            (h,v1,v2,v3)::(build t l)      
    in 
        let wf= (open_out outf) in
        Printf.fprintf wf "nb_valeurs  nb_noeuds  nb_cles  nb_ids\n";
        let rec write_list l =
            match l with 
            | [] -> close_out wf; 0
            | (h,v1,v2,v3)::t -> 
                Printf.fprintf wf "  %d  %d  %d  %d  \n" h v1 v2 v3; write_list t
        in 
            write_list (build intl []);;

        
(* let nbr_nci_acC abr  nombre de noeuds de clées et d'identifiants dans un acmC pour un abr quelqconque *)        
                  


(***************************************************************************)
(* 3.11 *)

(******************** TEMPS **********************)
(*
    --> int list -> int -> (int * float *float * float * float * float) list
*)

let av_srch_time intl nb_it =
    let abr=(list_to_abr intl Empty_abr) in
    let acl=(comp_abr_acl abr) in
    let acm=(comp_abr_acm abr) in
    let acmC=(comp_abr_acmC abr) in
    let rec srch cpt l=
        if(cpt<=0) then l
        else 
        let v=((Random.int (List.length intl))+1) in
        let t=Sys.time () in
        ignore (search_val_in_abr v abr);
        let vabr=((Sys.time ()) -. t) in
        let t=Sys.time () in
        ignore (search_val_in_acl v acl);
        let vacl=((Sys.time ()) -. t) in
        let t=Sys.time () in
        ignore (search_val_in_acm v acm);
        let vacm=((Sys.time ()) -. t) in
        let t=Sys.time () in
        ignore (search_val_in_acmC v acmC);
        let vacmC=((Sys.time ()) -. t) in(
            match l with 
            | [] -> srch (cpt-1) [(vabr,vacl,vacm,vacmC)] 
            | (v1,v2,v3,v4)::[] -> srch (cpt-1) [(v1 +. vabr),(v2 +. vacl),(v3 +. vacm),(v4 +. vacmC)]
            | _ -> assert false
        )
    in
        match(srch nb_it []) with 
            | (v1,v2,v3,v4)::[] -> [((List.length intl),(v1 /. 4.),(v2 /. 4.),(v3 /. 4.),(v4 /. 4.))]
            | _ -> assert false ;;

(* --> int list -> string -> int -> int  *)
let time_test intl outf nb_it=
    let rec tt intl l nb_it=
        match intl with 
        | [] -> l
        | h::t -> tt t (l@(av_srch_time h nb_it)) nb_it
    in
        let wf= open_out outf in
        Printf.fprintf wf "  nb_noeuds tmps_abr  tmps_acl  tmps_acm  tmps_acmC\n";
        let l=(tt intl [] nb_it) in
            let rec write_list l =
                match l with 
                | [] -> close_out wf; 0
                | (n,v1,v2,v3,v4)::t ->  Printf.fprintf wf "   %d    %f  %f  %f  %f  \n" n v1 v2 v3 v4; write_list t
            in
                write_list l;;
                

let l=List.map gen_permutation [100];;

time_test l  "output.txt" 10;;
(*nbr_nci_to_file [300;350;400;450;500;750;100] [50;100;150;200;250;300;350;400;450;500] "output_nci.txt";;*)

(***************************************************************************)
(* 3.12 *)
    
(******************* CALCULE MEMOIRE  **********************)
(*
    --> int -> out_channel -> unit
*)
let av_comp_mem intl outf =
	Printf.fprintf outf " 	%d "  intl ; 
        Gc.compact();
        let init_all = Gc.allocated_bytes() in
     	let l = (gen_permutation intl) in 
     	Gc.compact();
     	let list_all = Gc.allocated_bytes() in
      	let abr = (list_to_abr  l  Empty_abr ) in 
        Gc.compact();
        let abr_all = Gc.allocated_bytes() in
        let growth_abr = (abr_all -. list_all) in
        Printf.fprintf outf " 	%.0f "  growth_abr ; 
        let acl = (comp_abr_acl abr ) in 
        Gc.compact();            
        let acl_all = Gc.allocated_bytes() in
        let growth_acl = (acl_all -. abr_all)in
        Printf.fprintf outf "	%.0f "  growth_acl ; 
        let acm=(comp_abr_acm abr) in(
        Gc.compact();
        let acm_all = Gc.allocated_bytes() in 
        let growth_acm = (acm_all -. acl_all)in
        Gc.compact();
        Printf.fprintf outf "	%.0f "  growth_acm ; 
        let acmC=(comp_abr_acmC abr )in
        Gc.compact();
        let all_acmC = Gc.allocated_bytes () in
        let acmC_growth = (all_acmC -. acm_all) in            
        Printf.fprintf outf "	%.0f \n"  acmC_growth ;   
)   ;;
(* affectation des valeurs obtenus par nod tests dans un fichier 
    --> int list -> string -> string  *)
let nbr_mem intl outf =
 let wf = open_out outf in 
    Printf.fprintf wf "nb_cles 	mem_abr		mem_acl		mem_acm		mem_acmC \n";
    let rec build l wf =
        match l with 
        | [] -> ""
        | h :: t ->  av_comp_mem h wf;  build t wf
    in        
    build intl wf           
    ;;
           
          
(**  Execution test mémoire en octets   **)
nbr_mem [20;30;40;50;100;150;200] "res_e.txt" ;;


(***************************************************************************)
(* 3.13 *)
(*
    --> string -> int list
*)
let file_str_to_intl filename=
    let st=(input_line (open_in filename)) in
    let strl=(String.split_on_char ',' (String.sub st 1 ((String.length st)-2))) in
    let rec rrf sl il b=
        match sl with 
        | [] -> il
        | h::t -> if(b=true) then  rrf t (il@[(int_of_string h)])  false
                    else rrf t (il@[(int_of_string ( String.sub h 1 ((String.length h)-1)  ))]) false
    in
        rrf strl [] true ;;

(*
    --> string list -> unit 
*)
let rec test_files l =
    
    match l with 
    | [] -> Printf.printf ""
    | h::t -> 
        let l= file_str_to_intl h in
        let acl=comp_abr_acl (list_to_abr l Empty_abr) in
        let nbclefs=(nbr_clefs_acl acl) in
        let nbnoeuds=(nbr_noeuds_acl acl) in(
            Printf.printf "Fichier : %s\n" h;
            Printf.printf "nbr total noeuds : %d\n" nbnoeuds;
            Printf.printf "nbr tatal clefs : %d\n" nbclefs;
            Printf.printf "nbr moyen de clefs par noeuds : %d\n\n" (nbclefs/nbnoeuds)
        ); test_files t;;
        

        
test_files ["Jeu_de_tests/donnee100.txt"];
test_files ["Jeu_de_tests/donnee150.txt"];
test_files ["Jeu_de_tests/donnee500.txt"];
test_files ["Jeu_de_tests/donnee750.txt"];
test_files ["Jeu_de_tests/donnee1000.txt"];
test_files ["Jeu_de_tests/donnee10000.txt"];
        
       
