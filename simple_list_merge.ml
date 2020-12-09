open Lwt.Infix
open Irmin_unix

let rec intlist_to_string acc list =
    (* convert list of ints to string of ints seperated by ';' *)
    match list with
        | [] -> acc
        | [x] -> acc ^ (Int64.to_string x)
        | x::xs -> intlist_to_string (acc ^ (Int64.to_string x) ^ ";") xs

let string_to_intlist s =
    (* convert string of ints seperated by ';' to list of ints *)
    let s = String.sub s 1 (String.length s - 2) in
    List.map int_of_string (String.split_on_char ';' s)

let string_to_intlist_alt s =
    (* convert string of ints seperated by ';' to list of ints *)
    let s = String.sub s 0 (String.length s - 1) in
    List.map Int64.of_string (String.split_on_char ';' s)

module IntMap = Map.Make(Int64)

let fold_method m = 
    (* get string of ints from keys of a map *)
    IntMap.fold (fun k v acc -> acc ^ Int64.to_string k ^ ";") m "";;

let rec create_map head map =
    (* create a map from list of ints *)
    match head with
        | [] -> map
        | [x] -> IntMap.add x 1 map
        | x::xs -> create_map xs (IntMap.add x 1 map)

let find_count_in_map map x = 
    (* return count stored as value of key otherwise return 0 if not found *)
    try IntMap.find x map with Not_found -> 0

let rec handle_child_elements head child_map root_map = match head with
    (* add elements of head (a list) to child_map if not present in root_map
    otherwise, increment count of val of root_map key by 1 *)
    | [] -> child_map
    | x::xs -> let count = find_count_in_map root_map x in
        match count with
            | 0 -> handle_child_elements xs (IntMap.add x 1 child_map) root_map
            | _ -> handle_child_elements xs child_map root_map

let rec modify_root_map_with_child_elements head root_map child_map = 
    (* put elements of head (a list) in root_map(1 or +1) if not present in child_map *)
    match head with
        | [] -> root_map
        | x::xs -> let count = find_count_in_map child_map x in
            if count > 0 then modify_root_map_with_child_elements xs root_map child_map
            else let new_count = find_count_in_map root_map x in
                let new_count = new_count + 1 in
                let new_root_map = IntMap.add x new_count root_map in
                modify_root_map_with_child_elements xs new_root_map child_map

let rec get_new_list curr_list map new_list = 
    (* append items from curr_list in new_list if count of element in map is 3 *)
    match curr_list with 
        | [] -> new_list
        | x::xs -> let count = find_count_in_map map x in
            if count < 3 then get_new_list curr_list map new_list
            else get_new_list curr_list map (x::new_list)

let create_list_from_root_map root_map = 
    (* iterate over root_map and accumulate items with count=3 in acc(a list) *)
    let string_of_map = fold_method root_map in
    let curr_list = string_to_intlist_alt string_of_map in
    get_new_list curr_list root_map []

let create_list_from_map map acc =
    (* iterate over root_map and accumulate all items in acc(a list) *)
    let string_of_map = fold_method map in
    let curr_list = string_to_intlist_alt string_of_map in
    curr_list

let rec combine_map_and_list map list =
    (* if element of list not in map then add element in map *)
    match list with 
    | [] -> map
    | x::xs -> let count = find_count_in_map map x in
        if count = 0 then combine_map_and_list (IntMap.add x 1 map) xs
        else combine_map_and_list map xs

let combine_child_maps map_1 map_2 = 
    (* iterate over map_2 and add elements in map_1 if missing *)
    let string_of_map = fold_method map_2 in
    let map_2_list = string_to_intlist_alt string_of_map in
    combine_map_and_list map_1 map_2_list

let merge_3_intlist (root:int64 list) (left:int64 list) (right:int64 list) = 
    (* root_map stores map of elements in root node *)
    let root_map = IntMap.empty in
    let root_map = create_map root root_map in
    (* left_child_map stores map of elements in left_node not present in root_node *)
    let left_child_map = IntMap.empty in
    let left_child_map = handle_child_elements left left_child_map root_map in
    (* root_map stores count of elements encountered in root_node and left_node *)
    let root_map = modify_root_map_with_child_elements left root_map left_child_map in
    (* right_child_map stores map of elements in left_node not present in root_node *)
    let right_child_map = IntMap.empty in
    let right_child_map = handle_child_elements right right_child_map root_map in
    (* root_map stores count of elements encountered in root_node, left_node, right_node *)
    let root_map = modify_root_map_with_child_elements right root_map right_child_map in
    let new_list = create_list_from_root_map root_map in
    let child_map = combine_child_maps left_child_map right_child_map in 
    let new_list = create_list_from_map child_map new_list in
    new_list

module NewList = struct
    type t = int64 list
    let t = Irmin.Type.(list (int64))
    let of_string = string_to_intlist
    let to_string = intlist_to_string ""
    let merge ~old a b =
        let open Irmin.Merge.Infix in
		old >|=? fun old ->
        let merged_list = merge_3_intlist old a b in
        Irmin.Merge.ok merged_list
    let merge = Irmin.Merge.(option (v t merge))
end

module Git_store = Irmin_unix.Git.FS.KV(NewList)
