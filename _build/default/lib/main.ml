open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let print_game (game : Game.t) =
    let list1 = List.init 3 ~f:(fun a -> a) in
    List.iter list1 ~f:(fun y ->
      if y > 0 then printf "\n---------\n";
      List.iter list1 ~f:(fun x ->
        if List.exists (Map.keys game.board) ~f:(fun key ->
             x = key.column && y = key.row)
        then
          Map.iteri game.board ~f:(fun ~key ~data:value ->
            if x = key.column && y = key.row
            then printf !"%{Game.Piece}" value)
        else printf " ";
        if x < 2 then printf " | "))
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let moves =
      List.concat
        (List.init (Game.Game_kind.board_length game.game_kind) ~f:(fun y ->
           List.init
             (Game.Game_kind.board_length game.game_kind)
             ~f:(fun x -> { Game.Position.row = y; column = x })))
    in
    List.filter moves ~f:(fun move ->
      not
        (List.exists (Map.keys game.board) ~f:(fun a ->
           Game.Position.equal move a)))
  ;;

  let player_moves (player : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    Map.keys
      (Map.filteri game.board ~f:(fun ~key:_ ~data:value ->
         if Game.Piece.equal value player then true else false))
  ;;

  let check_for_win (player : Game.Piece.t) (game : Game.t) =
    let _gomoku_iter_list = List.init 11 ~f:(fun a -> a) in
    let add_list = List.tl_exn (List.init 6 ~f:(fun a -> a)) in
    (* print_s [%message (gomoku_win_possibilities : Game.Position.t list
       list)]; *)
    let iter_list = List.init 3 ~f:(fun a -> a) in
    let win_possibilities =
      match game.game_kind with
      | Tic_tac_toe ->
        List.map iter_list ~f:(fun y ->
          List.map iter_list ~f:(fun x ->
            { Game.Position.row = y; column = x }))
        @ List.map iter_list ~f:(fun x ->
          List.map iter_list ~f:(fun y ->
            { Game.Position.row = y; column = x }))
        @ [ List.map iter_list ~f:(fun a ->
              { Game.Position.row = a; column = a })
          ]
        @ [ List.map iter_list ~f:(fun a ->
              { Game.Position.row = a; column = 2 - a })
          ]
      | Omok ->
        List.concat
          (List.map _gomoku_iter_list ~f:(fun y ->
             List.map _gomoku_iter_list ~f:(fun x ->
               List.map add_list ~f:(fun add ->
                 { Game.Position.row = y; column = x + add })))
           @ List.map _gomoku_iter_list ~f:(fun y ->
             List.map _gomoku_iter_list ~f:(fun x ->
               List.map add_list ~f:(fun add ->
                 { Game.Position.row = x + add; column = y })))
           @ List.map _gomoku_iter_list ~f:(fun y ->
             List.map _gomoku_iter_list ~f:(fun x ->
               List.map add_list ~f:(fun add ->
                 { Game.Position.row = x + add; column = y + add })))
           @ List.map _gomoku_iter_list ~f:(fun y ->
             List.map _gomoku_iter_list ~f:(fun x ->
               List.map add_list ~f:(fun add ->
                 { Game.Position.row = x + add; column = 15 - y + add }))))
    in
    let used_moves = player_moves player game in
    if List.exists win_possibilities ~f:(fun list ->
         List.for_all list ~f:(fun move ->
           List.exists used_moves ~f:(fun my_move ->
             Game.Position.equal my_move move)))
    then Some player
    else None
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    if List.exists (Map.keys game.board) ~f:(fun key ->
         key.row > Game.Game_kind.board_length game.game_kind
         && key.column > Game.Game_kind.board_length game.game_kind)
    then Illegal_move
    else (
      match check_for_win X game with
      | Some player -> Game.Evaluation.Game_over { winner = Some player }
      | None ->
        (match check_for_win O game with
         | Some player2 ->
           Game.Evaluation.Game_over { winner = Some player2 }
         | None ->
           if List.is_empty (available_moves game)
           then Game.Evaluation.Game_over { winner = None }
           else Game_continues))
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    List.filter (available_moves game) ~f:(fun a ->
      let board = Map.add_exn game.board ~key:a ~data:me in
      match check_for_win me { Game.game_kind = game.game_kind; board } with
      | Some _player -> true
      | None -> false)
  ;;

  (* match List.find (available_moves game) ~f:(fun a -> let board =
     Map.add_exn game.board ~key:a ~data:me in match check_for_win me {
     Game.game_kind = game.game_kind; board } with | Some _player -> true |
     None -> false) with | Some move -> [ move ] | None -> [] *)

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    if Game.Piece.equal me X
    then
      if not (List.is_empty (winning_moves ~me:O game))
      then
        List.filter (available_moves game) ~f:(fun a ->
          not
            (List.exists (winning_moves ~me:O game) ~f:(fun b ->
               Game.Position.equal a b)))
      else []
    else if not (List.is_empty (winning_moves ~me:X game))
    then
      List.filter (available_moves game) ~f:(fun a ->
        not
          (List.exists (winning_moves ~me:X game) ~f:(fun b ->
             Game.Position.equal a b)))
    else []
  ;;

  let available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    =
    List.filter (available_moves game) ~f:(fun a ->
      not
        (List.exists (losing_moves ~me game) ~f:(fun b ->
           Game.Position.equal a b)))
  ;;

  let rec minimax depth maximizing_player (game : Game.t) ~me =
    (* Need to implement heuristic *)
    match check_for_win me game with
    | Some _player ->
      if maximizing_player then Int.max_value else Int.min_value
    | None ->
      if depth = 0 || List.length (available_moves game) = 0
      then
        List.length (winning_moves game ~me)
        - List.length (losing_moves game ~me)
      else if maximizing_player
      then (
        let value = ref Int.min_value in
        List.iter (available_moves game) ~f:(fun a ->
          let new_board = Map.add_exn game.board ~key:a ~data:me in
          value
          := max
               !value
               (minimax
                  (depth - 1)
                  false
                  { Game.game_kind = game.game_kind; board = new_board }
                  ~me));
        !value)
      else (
        let value = ref Int.max_value in
        List.iter (available_moves game) ~f:(fun a ->
          let new_board =
            Map.add_exn
              game.board
              ~key:a
              ~data:(if Game.Piece.equal me X then O else X)
          in
          value
          := min
               !value
               (minimax
                  (depth - 1)
                  true
                  { Game.game_kind = game.game_kind; board = new_board }
                  ~me));
        !value)
  ;;

  let get_move (game : Game.t) ~me =
    (* First or second move go middle *)
    (* if List.length (Map.keys game.board) < 1 then { Game.Position.row = 1;
       column = 1 } else if List.length (Map.keys game.board) = 1 then if
       List.exists (available_moves game) ~f:(fun a -> Game.Position.equal a
       { Game.Position.row = 1; column = 1 }) then { Game.Position.row = 1;
       column = 1 } else { Game.Position.row = 2; column = 2 } (* Win game if
       possible *) *)
    if not (List.is_empty (winning_moves game ~me))
    then List.hd_exn (winning_moves game ~me) (* No winning positions *)
    else (
      (* Only look at moves where we wont lose unless loss is inevitable *)
      let moves_to_look =
        if List.is_empty
             (available_moves_that_do_not_immediately_lose ~me game)
        then available_moves game
        else available_moves_that_do_not_immediately_lose ~me game
      in
      (* Establish default values for move and highest score *)
      let highest_score = ref Int.min_value in
      let move = ref (List.hd_exn moves_to_look) in
      (* Iter over selected moves *)
      List.iter moves_to_look ~f:(fun a ->
        let minimax_call =
          minimax
            2
            false
            { Game.game_kind = game.game_kind
            ; board = Map.add_exn game.board ~key:a ~data:me
            }
            ~me
        in
        printf "%d\n" minimax_call;
        if minimax_call > !highest_score
        then (
          move := a;
          highest_score := minimax_call));
      !move)
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ]
  ;;
end

let handle (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  print_s [%message "Received query"];
  Core.printf "Finished waiting\n%!";
  let response =
    { Rpcs.Take_turn.Response.piece = query.you_play
    ; position = Exercises.get_move query.game ~me:query.you_play
    }
  in
  return response
;;

let command_play =
  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:[ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle ]
  in
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (* and _controller = flag "-controller" (required host_and_port) ~doc:"_
        host_and_port of controller" *)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

(* return () *)

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
