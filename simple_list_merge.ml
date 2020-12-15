open Lwt.Infix
open Irmin_unix

let rec intlist_to_string acc list =
    (* convert list of ints to string of ints seperated by ';' *)
    match list with
        | [] -> acc
        | [x] -> acc ^ (Int64.to_string x)
        | x::xs -> intlist_to_string (acc ^ (Int64.to_string x) ^ ";") xs;;

let string_to_intlist s =
    (* convert string of ints seperated by ';' to list of ints *)
    let s = String.sub s 1 (String.length s - 2) in
    List.map int_of_string (String.split_on_char ';' s);;

module IntMap = Map.Make(Int64);;

let rec create_map head map =
    (* create a map from list of ints *)
    match head with
        | [] -> map
        | [x] -> IntMap.add x 1 map
        | x::xs -> create_map xs (IntMap.add x 1 map);;

let find_count_in_map map x = 
    (* return count stored as value of key otherwise return 0 if not found *)
    try IntMap.find x map with Not_found -> 0;;

let rec handle_child_elements head child_map root_map = match head with
    (* add elements of head (a list) to child_map if not present in root_map
    otherwise, increment count of val of root_map key by 1 *)
    | [] -> child_map
    | x::xs -> let count = find_count_in_map root_map x in
        match count with
            | 0 -> handle_child_elements xs (IntMap.add x 1 child_map) root_map
            | _ -> handle_child_elements xs child_map root_map;;

let rec modify_root_map_with_child_elements head root_map child_map = 
    (* put elements of head (a list) in root_map(1 or +1) if not present in child_map *)
    match head with
        | [] -> root_map
        | x::xs -> let count = find_count_in_map child_map x in
            if count > 0 then modify_root_map_with_child_elements xs root_map child_map
            else let new_count = find_count_in_map root_map x in
                let new_count = new_count + 1 in
                let new_root_map = IntMap.add x new_count root_map in
                modify_root_map_with_child_elements xs new_root_map child_map;;

let create_list_from_root_map root_map = 
    (* iterate over root_map and accumulate items with count=3 in acc(a list) *)
    IntMap.fold (fun k v acc -> if v = 3 then k::acc else acc) root_map [];;

let create_list_from_map map acc =
    (* iterate over root_map and accumulate all items in acc(a list) *)
    IntMap.fold (fun k v acc -> k::acc) map [];;

let rec combine_map_and_list map list =
    (* if element of list not in map then add element in map *)
    match list with 
    | [] -> map
    | x::xs -> let count = find_count_in_map map x in
        if count = 0 then combine_map_and_list (IntMap.add x 1 map) xs
        else combine_map_and_list map xs;;

let combine_child_maps map_1 map_2 = 
    (* iterate over map_2 and add elements in map_1 if missing *)
    let map_2_list = create_list_from_map map_2 [] in
    combine_map_and_list map_1 map_2_list;;

let rev_list list = 
    let rec rev acc = function
        | [] -> acc
        | h::t -> rev (h::acc) t in
    rev [] list;;

let merge_3_intlist (root:int64 list) (left:int64 list) (right:int64 list) = 
    (* root_map stores map of elements in root node *)
    let root_map = create_map root IntMap.empty in
    (* left_child_map stores map of elements in left_node not present in root_node *)
    let left_child_map = handle_child_elements left IntMap.empty root_map in
    (* root_map stores count of elements encountered in root_node and left_node *)
    let root_map = modify_root_map_with_child_elements left root_map left_child_map in
    (* right_child_map stores map of elements in left_node not present in root_node *)
    let right_child_map = handle_child_elements right IntMap.empty root_map in
    (* root_map stores count of elements encountered in root_node, left_node, right_node *)
    let root_map = modify_root_map_with_child_elements right root_map right_child_map in
    let new_list = create_list_from_root_map root_map in
    let new_list = rev_list new_list in
    let child_map = combine_child_maps left_child_map right_child_map in 
    let new_list = create_list_from_map child_map new_list in
    let new_list = rev_list new_list in
    new_list;;

module NewList = struct
    type t = int64 list
    let t = Irmin.Type.(list (int64))
    let of_string = string_to_intlist
    let to_string = intlist_to_string ""
    let merge ~old a b =
        let open Irmin.Merge.Infix in
        old () >|=* fun old ->
        let old = match old with None -> [] | Some o -> o in
        let merged_list = merge_3_intlist old a b in
        merged_list
    let merge = Irmin.Merge.(option (v t merge))
end;;

module Git_store = Irmin_unix.Git.FS.KV(NewList);;

let git_config = Irmin_git.config ~bare:true "/tmp/irmin_list";;

let info message = Irmin_unix.info ~author:"Example" "%s" message;;

(* create master branch *)
let master = Lwt_main.run begin Git_store.Repo.v git_config >>= Git_store.master end;;

(* commit val in master *)
let commit_in_master val1 = Lwt_main.run begin
Git_store.set_exn master ["path"] val1 ~info:(info "random") end;;

(* create local branch *)
let local = Lwt_main.run begin Git_store.Repo.v git_config >>=
    fun repo -> Git_store.of_branch repo "local" end;;

(* commit val in local *)
let commit_in_local val1 = Lwt_main.run begin
    Git_store.set_exn local ["path"] val1 ~info:(info "random") end;;

(* merge local in master *)
let merge_in_local () = Lwt_main.run begin
    Git_store.merge_into ~into:local master ~info:(info "merging into local") end;;

(* merge master in local *)
let merge_in_master () = Lwt_main.run begin
    Git_store.merge_into ~into:master local ~info:(info "merging into master") end;;

(* print master val *)
let master_val () = Lwt_main.run begin
    Git_store.get master ["path"] >|= fun s -> Printf.printf "%s\n" (intlist_to_string "" s) end;;

(* print local val *)
let local_val () = Lwt_main.run begin
    Git_store.get local ["path"] >|= fun s -> Printf.printf "%s\n" (intlist_to_string "" s) end;;

(* add element to master list *)
let master_element_add val1 = Lwt_main.run begin
    Git_store.get master ["path"] >|= fun s -> let s = rev_list s in
    let new_list = val1::s in
    let new_list = rev_list new_list in
    Git_store.set_exn master ["path"] new_list ~info:(info "random") end;;

(* add element to local list *)
let local_element_add val1 = Lwt_main.run begin
    Git_store.get local ["path"] >|= fun s -> let s = rev_list s in
    let new_list = val1::s in
    let new_list = rev_list new_list in
    Git_store.set_exn local ["path"] new_list ~info:(info "random") end;;

(* remove element from master list *)

(* remove element from local list *)

(* test merge function *)
let test_merge () = Lwt_main.run begin
    (* create master branch *)
    Git_store.Repo.v git_config >>= Git_store.master >>= fun m_b ->
    (* create local branch *)
    Git_store.Repo.v git_config >>= fun repo -> Git_store.of_branch repo "local" >>= fun l_b ->
    (* add 3 elements in master *)
    Git_store.set_exn m_b ["path"] [1L] ~info:(info "creating first element master") >>= fun () ->
    Git_store.get m_b ["path"] >|= fun s -> 
    let s = rev_list s in let new_list = 2L::s in let new_list = rev_list new_list in
    Git_store.set_exn m_b ["path"] new_list ~info:(info "adding second element master") >>= fun () ->
    Git_store.get m_b ["path"] >|= fun s -> 
    let s = rev_list s in let new_list = 3L::s in let new_list = rev_list new_list in
    Git_store.set_exn m_b ["path"] new_list ~info:(info "adding third element master") >>= fun () ->
    (* add 2 elements in local branch *)
    Git_store.set_exn l_b ["path"] [4L] ~info:(info "creating first element local") >>= fun () ->
    Git_store.get l_b ["path"] >|= fun s ->
    let s = rev_list s in let new_list = 5L::s in let new_list = rev_list new_list in
    Git_store.set_exn l_b ["path"] new_list ~info:(info "adding second element local") >>= fun () ->
    (* merge master in local *)
    Git_store.merge_into ~into:l_b m_b ~info:(info "merging into local") >>= fun _ ->
    (* merge local in master *)
    Git_store.merge_into ~into:m_b l_b ~info:(info "merging into master") >>= fun _ ->
    Git_store.get m_b ["path"] >|= fun s -> 
    (* check if local val is equal to what is desired *)
    assert ([1L;2L;1L;4L;5L] = s)
end;;
