open Lwt.Infix
open Irmin_unix

(* Counter module *)
module Newcounter: Irmin.Contents.S with type t = int64 = struct
	type t = int64
	let t = Irmin.Type.int64
    (* let pp = Irmin.Type.dump t *)
    let of_string s =
        match Int64.of_string_opt s with
        | Some i -> Ok i
        | None -> Error (`Msg "invalid counter value")
    let merge ~old a b =
    (* let rec merge ~(old:t Irmin.Merge.promise) a b = *)
	    let open Irmin.Merge.Infix in
		old () >|=* fun old ->
        let old = match old with None -> 0L | Some o -> o in
        let (+) = Int64.add and (-) = Int64.sub in
        a + b - old
    let merge = Irmin.Merge.(option (v t merge))
end

(* create git store and associated configs *)
module Git_store = Irmin_unix.Git.FS.KV(Newcounter)

let git_config = Irmin_git.config ~bare:true "/tmp/irmin"

let info message = Irmin_unix.info ~author:"Example" "%s" message

(* create master branch *)
let master = Lwt_main.run begin Git_store.Repo.v git_config >>= Git_store.master end

(* commit val in master *)
let commit_in_master val1 = Lwt_main.run begin
Git_store.set_exn master ["path"] val1 ~info:(info "random") end

(* create local branch *)
let local = Lwt_main.run begin Git_store.Repo.v git_config >>= fun repo -> Git_store.of_branch repo "local" end

(* commit val in local *)
let commit_in_local val1 = Lwt_main.run begin
Git_store.set_exn local ["path"] val1 ~info:(info "random") end

(* merge local in master *)
let merge_in_local () = Lwt_main.run begin
Git_store.merge_into ~into:local master ~info:(info "merging into local") end

(* merge master in local *)
let merge_in_master () = Lwt_main.run begin
Git_store.merge_into ~into:master local ~info:(info "merging into master") end

(* print master val *)
let master_val () = Lwt_main.run begin
Git_store.get master ["path"] >|= fun s -> Printf.printf "%Ld\n" s end

(* print local val *)
let local_val () = Lwt_main.run begin
Git_store.get local ["path"] >|= fun s -> Printf.printf "%Ld\n" s end

(* handle remote master scenario *)