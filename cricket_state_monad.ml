(* Cricket Monad: takes a state and computes operations
  State -> list of (player_number,team_number,player_points,is_player_active)
  Operations -> 
    add_player : add a new player
    update_player_points : change points of player
    retire_player : set player as inactive but keep the data of player
    transfer_player : update team of player to a different team
    get_top_10_players : get 10 players with highest points
    get_players_of_team : get all players of a team
 *)
(* let rec best_players player_count given_state generated_list = match given_state with
| [] -> generated_list
| (a,b,c,d)::t ->  *)
module CricketMond: sig
    type 'a t
    val add_player: int -> int -> int -> unit t
    val update_player_points: int -> int -> unit t
    val retire_player: int -> bool t
    val transfer_player: int -> int -> bool t
    (* val get_top_10_players: list t *)
    (* val get_players_of_team: int -> list t *)
    val bind: 'a t -> ('a -> 'b t) -> 'b t
end = struct
    type st = (int*int*int*bool) list
    type 'a t = st -> (st*'a)
    let add_player player team points = fun st -> 
        let st' = (player,team,points,true)::st in
        (st', ())
    let update_player_points player points = fun st -> 
        let st' = List.map (fun (a,b,c,d) -> if a=player then (a,b,points,d) else (a,b,c,d)) st in
        (st',())
    let retire_player player = fun st -> 
        let st' = List.map (fun (a,b,c,d) -> if a=player then (a,b,c,false) else (a,b,c,d)) st in
        (st',true)
    let transfer_player player team = fun st -> 
        let st' = List.map (fun (a,b,c,d) -> if a=player then (a,team,c,d) else (a,b,c,d)) st in
        (st',true)
    let bind m f = fun st -> 
        let (st',res1) = m st in
        let m1 = f res1 in
        m1 st'
end