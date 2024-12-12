open Cards

type t =
  | RoyalFlush
  | StraightFlush of rank
  | FourOfAKind of rank * rank
  | FullHouse of rank * rank
  | Flush of rank list
  | Straight of rank
  | ThreeOfAKind of rank * rank list
  | TwoPair of rank * rank * rank
  | OnePair of rank * rank list
  | HighCard of rank list

let rank_value = function
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14

let compare_ranks r1 r2 = compare (rank_value r1) (rank_value r2)

(** [sort_by_rank cards] sorts [cards] by ascending rank. *)
let sort_by_rank cards =
  List.sort (fun c1 c2 -> compare_ranks (get_rank c1) (get_rank c2)) cards

(** [is_flush cards] checks if [cards] all share the same suit (flush). *)
let is_flush cards =
  match cards with
  | [] -> false
  | c :: cs ->
      let s = get_suit c in
      List.for_all (fun x -> get_suit x = s) cs

(** [is_straight ranks] checks if the [ranks] forms a regular straight *)
let is_straight ranks =
  match ranks with
  | [ r1; r2; r3; r4; r5 ] ->
      let v1, v2, v3, v4, v5 =
        ( rank_value r1,
          rank_value r2,
          rank_value r3,
          rank_value r4,
          rank_value r5 )
      in
      v2 = v1 + 1 && v3 = v2 + 1 && v4 = v3 + 1 && v5 = v4 + 1
  | _ -> false

(** [is_wheel_straight ranks] checks if [ranks] is a specific corner case *)
let is_wheel_straight ranks =
  match ranks with
  | [ Ace; Five; Four; Three; Two ] -> true
  | _ -> false

(** [rank_frequencies cards] counts the frequency of each in the [cards]. *)
let rank_frequencies cards =
  let tbl = Hashtbl.create 7 in
  List.iter
    (fun c ->
      let r = get_rank c in
      Hashtbl.replace tbl r
        (1 + (Hashtbl.find_opt tbl r |> Option.value ~default:0)))
    cards;
  let freq_list = Hashtbl.fold (fun r count acc -> (r, count) :: acc) tbl [] in
  List.sort
    (fun (r1, c1) (r2, c2) ->
      let c = compare c2 c1 in
      if c <> 0 then c else compare_ranks r2 r1)
    freq_list

(* extract ranks in descending order *)
let extract_ranks_desc cards =
  let ranks = List.map get_rank cards in
  List.sort (fun a b -> compare_ranks b a) ranks

(* main hand evaluation function *)
let evaluate_hand cards =
  if List.length cards <> 5 then
    invalid_arg "evaluate_hand: must provide exactly 5 cards";
  let sorted_cards = sort_by_rank cards in
  let ranks_asc = List.map get_rank sorted_cards in
  let flush_flag = is_flush sorted_cards in
  let straight_flag = is_straight ranks_asc || is_wheel_straight ranks_asc in
  let freq = rank_frequencies sorted_cards in

  match freq with
  | [ (r1, 4); (r2, 1) ] -> FourOfAKind (r1, r2)
  | [ (r1, 3); (r2, 2) ] -> FullHouse (r1, r2)
  | _ when flush_flag && straight_flag ->
      let ranks_desc = List.rev ranks_asc in
      let rv = List.map rank_value ranks_desc in
      if rv = [ 10; 11; 12; 13; 14 ] then RoyalFlush
      else
        let high =
          if is_wheel_straight ranks_asc then Five else List.nth ranks_asc 4
        in
        StraightFlush high
  | _ when flush_flag -> Flush (extract_ranks_desc sorted_cards)
  | _ when straight_flag ->
      let high =
        if is_wheel_straight ranks_asc then Five else List.nth ranks_asc 4
      in
      Straight high
  | [ (r1, 3); (r2, 1); (r3, 1) ] ->
      let kickers = List.sort (fun a b -> compare_ranks b a) [ r2; r3 ] in
      ThreeOfAKind (r1, kickers)
  | [ (r1, 2); (r2, 2); (r3, 1) ] ->
      let pair_ranks = [ r1; r2 ] |> List.sort (fun a b -> compare_ranks b a) in
      TwoPair (List.nth pair_ranks 0, List.nth pair_ranks 1, r3)
  | [ (r1, 2); (r2, 1); (r3, 1); (r4, 1) ] ->
      let kickers =
        [ r2; r3; r4 ] |> List.sort (fun a b -> compare_ranks b a)
      in
      OnePair (r1, kickers)
  | _ -> HighCard (extract_ranks_desc sorted_cards)

let compare_hand_rank h1 h2 =
  let rank_category = function
    | RoyalFlush -> 9
    | StraightFlush _ -> 8
    | FourOfAKind _ -> 7
    | FullHouse _ -> 6
    | Flush _ -> 5
    | Straight _ -> 4
    | ThreeOfAKind _ -> 3
    | TwoPair _ -> 2
    | OnePair _ -> 1
    | HighCard _ -> 0
  in

  let c1 = rank_category h1 in
  let c2 = rank_category h2 in
  if c1 <> c2 then compare c1 c2
  else
    let compare_descending_ranks l1 l2 =
      let rec aux = function
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | x :: xs, y :: ys ->
            let cmp = compare_ranks x y in
            if cmp <> 0 then cmp else aux (xs, ys)
      in
      aux (l1, l2)
    in
    match (h1, h2) with
    | RoyalFlush, RoyalFlush -> 0
    | StraightFlush r1, StraightFlush r2 -> compare_ranks r1 r2
    | FourOfAKind (r1, k1), FourOfAKind (r2, k2) ->
        let cmp = compare_ranks r1 r2 in
        if cmp = 0 then compare_ranks k1 k2 else cmp
    | FullHouse (t1, p1), FullHouse (t2, p2) ->
        let cmp = compare_ranks t1 t2 in
        if cmp = 0 then compare_ranks p1 p2 else cmp
    | Flush l1, Flush l2 -> compare_descending_ranks l1 l2
    | Straight r1, Straight r2 -> compare_ranks r1 r2
    | ThreeOfAKind (t1, ks1), ThreeOfAKind (t2, ks2) ->
        let cmp = compare_ranks t1 t2 in
        if cmp = 0 then compare_descending_ranks ks1 ks2 else cmp
    | TwoPair (h1a, h1b, k1), TwoPair (h2a, h2b, k2) ->
        let cmp = compare_ranks h1a h2a in
        if cmp <> 0 then cmp
        else
          let cmp2 = compare_ranks h1b h2b in
          if cmp2 <> 0 then cmp2 else compare_ranks k1 k2
    | OnePair (p1, ks1), OnePair (p2, ks2) ->
        let cmp = compare_ranks p1 p2 in
        if cmp = 0 then compare_descending_ranks ks1 ks2 else cmp
    | HighCard ks1, HighCard ks2 -> compare_descending_ranks ks1 ks2
    | _ -> 0

(** [combinations_7_choose_5] generates every possible combination of 5 cards in
    the 7 total [cards]. *)
let combinations_7_choose_5 cards =
  let rec choose k lst =
    if k = 0 then [ [] ]
    else
      match lst with
      | [] -> []
      | x :: xs ->
          let with_x = List.map (fun c -> x :: c) (choose (k - 1) xs) in
          let without_x = choose k xs in
          with_x @ without_x
  in
  choose 5 cards

(** [best_hand_of_seven cards7] takes the best 5-card hand of [cards7]*)
let best_hand_of_seven cards7 =
  let all_5_combos = combinations_7_choose_5 cards7 in
  let best =
    List.fold_left
      (fun best_opt combo ->
        let hand = evaluate_hand combo in
        match best_opt with
        | None -> Some hand
        | Some current_best ->
            if compare_hand_rank hand current_best > 0 then Some hand
            else best_opt)
      None all_5_combos
  in
  match best with
  | Some h -> h
  | None -> failwith "No best hand found."
