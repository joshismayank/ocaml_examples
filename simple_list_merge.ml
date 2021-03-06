open Lwt.Infix
open Irmin_unix

let rec intlist_to_string acc list =
    let _ = Printf.printf "in intlist_to_string func \n" in
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

let find_count_in_map map x = 
    (* return count stored as value of key otherwise return 0 if not found *)
    try IntMap.find x map with Not_found -> 0;;

let rec create_map head map =
    (* create a map from list of ints *)
    match head with
        | [] -> map
        | [x] -> let curr_count = find_count_in_map map x in IntMap.add x (curr_count+1) map
        | x::xs -> let curr_count = find_count_in_map map x in create_map xs (IntMap.add x (curr_count+1) map);;

let create_list_from_map map acc =
    (* iterate over root_map and accumulate all items in acc(a list) *)
    IntMap.fold (fun k v acc -> k::acc) map [];;

let rec add_key_from_list map list = match list with
    | [] -> map
    | x::xs -> let curr_count = find_count_in_map map x in 
        if curr_count = 0 then add_key_from_list (IntMap.add x 0 map) xs 
        else add_key_from_list map xs;;

let add_missing_key map_1 map_2 =
    let map_2_list = create_list_from_map map_2 [] in
    add_key_from_list map_1 map_2_list;;

let rev_list list = 
    let rec rev acc = function
        | [] -> acc
        | h::t -> rev (h::acc) t in
    rev [] list;;

let rec add_element_in_list count element list =
    match count with
        | 0 -> list
        | x -> add_element_in_list (x-1) element (element::list);;

let rec combine_lists list_1 list_2 =
    match list_2 with
        | [] -> list_1
        | x::xs -> combine_lists (x::list_1) xs;;

let rec delete_element_from_list element list new_list =
    match list with
        | [] -> new_list
        | x::xs -> if x = element then combine_lists new_list xs
            else delete_element_from_list element xs (x::new_list);;

let rec create_new_list root_map right_map left_map root_list new_list =
    match root_list with 
        | [] -> new_list
        | x::xs -> let root_count = find_count_in_map root_map x in
            let left_count = find_count_in_map left_map x in
            let right_count = find_count_in_map right_map x in
            let new_count = right_count + left_count - root_count in
            if new_count > 0 then
                create_new_list root_map right_map left_map xs (add_element_in_list new_count x new_list)
            else create_new_list root_map right_map left_map xs new_list;;

let merge_3_intlist (root:int64 list) (left:int64 list) (right:int64 list) = 
    let root_map = create_map root IntMap.empty in
    let left_child_map = create_map left IntMap.empty in
    let right_child_map = create_map right IntMap.empty in
    let updated_root_map = add_missing_key root_map left_child_map in
    let updated_root_map = add_missing_key updated_root_map right_child_map in
    let root_list = create_list_from_map updated_root_map [] in
    let new_list = create_new_list root_map right_child_map left_child_map root_list [] in
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

(* create local branch *)
let local = Lwt_main.run begin Git_store.Repo.v git_config >>=
    fun repo -> Git_store.of_branch repo "local" end;;

(* commit val in master *)
let commit_in_master val1 = Lwt_main.run begin
    Git_store.set_exn master ["path"] val1 ~info:(info "random") end;;

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
    let _ = Printf.printf "in master_val func \n" in
    Git_store.get master ["path"] >|= fun s -> Printf.printf "%s\n" (intlist_to_string "" s) end;;

(* print local val *)
let local_val () = Lwt_main.run begin
    Git_store.get local ["path"] >|= fun s -> Printf.printf "%s\n" (intlist_to_string "" s) end;;

(* add element to master list *)
let master_element_add val1 = Lwt_main.run begin
    Git_store.get master ["path"] >|= fun s -> let s = (val1::s) in
    Git_store.set_exn master ["path"] s ~info:(info "random") end;;

(* add element to local list *)
let local_element_add val1 = Lwt_main.run begin
    Git_store.get local ["path"] >|= fun s -> let s = (val1::s) in
    Git_store.set_exn local ["path"] s ~info:(info "random") end;;

(* remove element from master list *)
let master_element_remove val1 = Lwt_main.run begin
    Git_store.get master ["path"] >|= fun s -> let s = delete_element_from_list val1 s [] in
    Git_store.set_exn master ["path"] s ~info:(info "random") end;;

(* remove element from local list *)
let local_element_remove val1 = Lwt_main.run begin
    Git_store.get local ["path"] >|= fun s -> let s = delete_element_from_list val1 s [] in
    Git_store.set_exn local ["path"] s ~info:(info "random") end;;

(* test merge function *)
let test_merge () = Lwt_main.run begin
    (* create master branch *)
    let _ = Printf.printf "in test_merge func \n" in
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
    (* let _ = Git_store.get m_b ["path"] >|= fun s -> Printf.printf "master_branch: %s\n" (intlist_to_string "" s) in
    let _ = Git_store.get l_b ["path"] >|= fun s -> Printf.printf "local_branch: %s\n" (intlist_to_string "" s) in *)
    Git_store.merge_into ~into:m_b l_b ~info:(info "merging into master") >>= fun res -> match res with
    (* merge local in master *)
    (* Git_store.Repo.v git_config >>= Git_store.master >>= fun m_b_n -> *)
    (* create local branch *)
    (* Git_store.Repo.v git_config >>= fun repo -> Git_store.of_branch repo "local" >>= fun l_b_n -> *)
    | Ok () -> Git_store.merge_into ~into:l_b m_b ~info:(info "merging into local")  >>= fun res -> 
        Git_store.get m_b ["path"] >|= fun s -> assert ([1L;2L;1L;4L;5L] = s)
    | Error conflict -> Git_store.merge_into ~into:l_b m_b ~info:(info "merging into local") >>= fun res -> 
        Git_store.get m_b ["path"] >|= fun s -> assert ([1L;2L;1L;4L;5L] = s)
    (* Git_store.get m_b_n ["path"] >|= fun s -> Printf.printf "final: %s\n" (intlist_to_string "" s) *)
    (* check if local val is equal to what is desired *)
    (* assert ([1L;2L;1L;4L;5L] = s) *)
end;;

let n_rounds = 10;;

let n_ops_per_commit_start = 10;;

let n_ops_per_commit_end = 100;;

let n_ops_per_commit_gap = 10;;

let rec delete_at_pos list pos new_list =
    match list with 
        | [] -> new_list
        | x::xs -> if pos = 0 then delete_at_pos xs (pos-1) new_list
            else delete_at_pos xs (pos-1) (x::new_list)

let do_one_op curr_list =
    let curr_rand_int = Random.int 9 in
    if curr_rand_int < 8 then 
        let curr_rand_int64 = Random.int64 Int64.max_int in 
        (curr_rand_int64::curr_list)
    else let curr_rand_pos = Random.int (List.length curr_list) in 
        delete_at_pos curr_list curr_rand_pos [];;

let rec do_total_ops total curr list = 
    if curr < total then let new_list = do_one_op list in
        do_total_ops total (curr+1) new_list
    else list;;

let get_list_from_master = Lwt_main.run begin 
    Git_store.Repo.v git_config >>= Git_store.master >>= fun master -> 
    Git_store.get master ["path"] >|= fun s -> s end;;

let get_list_from_local = Lwt_main.run begin 
    Git_store.Repo.v git_config >>= fun repo -> 
    Git_store.of_branch repo "local" >>= fun local -> 
    Git_store.get local ["path"] >|= fun s -> s end;;

let commit_in_master list = Lwt_main.run begin
    Git_store.Repo.v git_config >>= Git_store.master >>= fun master -> 
    Git_store.set_exn master ["path"] list ~info:(info "commiting in master") end;;

(* commit val in local *)
let commit_in_local list = Lwt_main.run begin
    Git_store.Repo.v git_config >>= fun repo -> 
    Git_store.of_branch repo "local" >>= fun local -> 
    Git_store.set_exn local ["path"] list ~info:(info "commiting in local") end;;

let run_experiment_for_one_commit n_ops_per_commit = 
    (* get list from master *)
    let master_list = get_list_from_master in
    (* get list from local *)
    let local_list = get_list_from_local in
    (* do n_ops_per_commit no of ops on master list *)
    let new_master_list = do_total_ops n_ops_per_commit 0 master_list in
    (* do n_ops_per_commit no of ops on local list *)
    let new_local_list = do_total_ops n_ops_per_commit 0 local_list in
    (* commit master list in master *)
    let _ = commit_in_master new_master_list in
    (* commit local list in local *)
    commit_in_local new_local_list;;
    (* merge into local ?? *)
    (* merge into master ?? *)

let rec iter terminate gap curr param = 
    if curr < terminate then
        let _ = run_experiment_for_one_commit param in 
        iter terminate gap (curr+gap) param;;

let rec perform_varying_ops_per_commit terminate gap curr =
    if curr < terminate then
        let _ = iter n_rounds 1 1 curr in
        perform_varying_ops_per_commit terminate gap (curr+gap);;

let main () =
    begin
    perform_varying_ops_per_commit n_ops_per_commit_end n_ops_per_commit_gap n_ops_per_commit_start
    end;;

main ();;
