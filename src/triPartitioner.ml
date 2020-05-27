type t =
    | Low
    | Medium
    | High

module type Signature = sig

    type elem
    
    val partition: elem list -> elem -> t

end

module Make (Ord: Map.OrderedType) : Signature with type elem = Ord.t = struct

    type elem = Ord.t

    let partition values  =
        let sorted = List.sort Ord.compare values in
        let all_high _ = High in
        match sorted with
        | [] -> all_high
        | [_] -> all_high
        | [low; _] -> 
            fun v ->
                if (Ord.compare v low) = 0
                then Low
                else High
        | _ ->
            let len = List.length sorted in
            let low = List.nth sorted ((len / 3) - 1) in
            let med = List.nth sorted (((2 * len) / 3) - 1) in
            fun v ->
                if (Ord.compare v low) <= 0
                then Low
                else if (Ord.compare v med) <= 0
                then Medium
                else High

end

