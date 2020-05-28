open Printf

module FuncEx = struct
    let swap f a b = f b a
end

module Either = struct
    type ('a, 'b) t =
        | Left of 'a
        | Right of 'b   
end

module ListEx = struct
    let choosei trans list =
        list 
        |> List.mapi (fun index item ->
            match (trans index item) with 
            | None -> []
            | Some new_item -> [new_item]
        )
        |> List.flatten

    let map_while_ok trans list = 
        let rec map acc = function
            | [] -> Tea.Result.Ok (List.rev acc)
            | head::rest ->
                match trans head with
                | Tea.Result.Ok result ->
                    map (result::acc) rest
                | Tea.Result.Error err ->
                    Tea.Result.Error err in
        (map [] list) 

    let compare_fst (left, _) (right, _) = compare left right

    let move ~from_index ~to_before_index list =
        if from_index = to_before_index
        then list
        else
            list
            |> List.mapi (fun idx item -> 
                if idx = from_index
                then ((to_before_index * 2) - 1, item)
                else (idx * 2, item)
            )
            |> List.sort compare_fst
            |> List.map snd

    let find_opt pred list =
        try
            Some (List.find pred list)
        with
            _ -> None

    let remove_index index list = Belt.List.keepWithIndex list (fun _ idx -> idx <> index) 
end

module StringEx = struct
    let case_insensitive_equal left right =
        let clean s = s |> String.trim |> String.lowercase_ascii in
        (clean left) = (clean right)
end

module Names = struct
    let verify_new_name name name_list =
        let cleaned_name = name |> String.trim in 
        if String.length cleaned_name < 1 
        then Tea.Result.Error "Required"
        else if List.exists (StringEx.case_insensitive_equal cleaned_name) name_list
        then Tea.Result.Error (sprintf "'%s' already in list. Try a different name." name)
        else Tea.Result.Ok cleaned_name

    let verify_existing_name name name_list =
        if List.exists ((=) name) name_list
        then Tea.Result.Ok(()) 
        else Tea.Result.Error (sprintf "'%s' cannot be found in the list" name)    

    let find_name_index name_to_find name_list = 
        match name_list |> ListEx.choosei (fun idx name -> if (StringEx.case_insensitive_equal name_to_find name) then Some idx else None) with
        | [] -> Tea.Result.Error (sprintf "Unable to find '%s'" name_to_find)
        | [index] -> Tea.Result.Ok(index)
        | _ -> Tea.Result.Error (sprintf "Multiple with name %s" name_to_find)
end

module ResultEx = struct

    let map (fn: 'original -> 'transformed) = function
        | Tea.Result.Ok original -> Tea.Result.Ok (fn original)
        | Tea.Result.Error err -> Tea.Result.Error err

    let map_error (fn: ('b -> 'b)) = function 
        | Tea.Result.Ok v -> Tea.Result.Ok v
        | Tea.Result.Error err -> Tea.Result.Error (fn err)

    let return value = Tea.Result.Ok value

    let flatten = function
        | Tea.Result.Ok original_result -> original_result
        | Tea.Result.Error err -> Tea.Result.Error err

    let join = flatten

    let flatMap (fn: 'original -> 'transformed_result) = function
        | Tea.Result.Ok original -> fn original
        | Tea.Result.Error err -> Tea.Result.Error err

    let map2 (fn: 'left -> 'right -> 'result) left_result right_result =
        left_result
        |> flatMap (fun left_value ->
            right_result
            |> map (fun right_value -> fn left_value right_value)
        )

    (** return an unwrapped value from the result, if the result is an error string then fail with. *)
    let getExnFailWith = function
        | Tea.Result.Ok value -> value
        | Tea.Result.Error err -> failwith err

    (** return an unwrapped value from the result, raising the exception if in error*)
    let getExn = function
        | Tea.Result.Ok value -> value
        | Tea.Result.Error err -> raise err
end
