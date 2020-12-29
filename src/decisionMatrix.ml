open Printf

module IntCellValue : Table.CellValue with type t = int = struct
    type t = int

    let default () = 0    
    let of_string s = 
        try 
            let v = s |> String.trim |> int_of_string in
            if v >= 0 && v <= 10 
            then Tea.Result.Ok v
            else Tea.Result.Error "Must be between 0 and 10."
        with 
            _ -> Tea.Result.Error (Printf.sprintf "Trouble converting '%s' to a number" s)

    let to_string = string_of_int
end

module type DecisionMatrix = sig
    type t
    type factor_name
    type alternative_name

    type dimension_name = 
        | FactorName of factor_name
        | AlternativeName of alternative_name

    type factor_value = factor_name * IntCellValue.t

    type alternative = {
        name: alternative_name;
        values: factor_value list;
        score: int;
    }

    type decision_matrix_factory = unit -> t
    type decision_matrix_factory_result = (decision_matrix_factory, string) Tea.Result.t     

    type dimension_item =
        | Alternative of alternative_name
        | Factor of factor_name

    type 'dimension_name dimension_editing = {
        label: string;
        add_instructions: string;
        add: t -> with_name: string -> decision_matrix_factory_result;
        remove: t -> with_name: 'dimension_name -> decision_matrix_factory_result;
        change_name: t -> old_name: 'dimension_name -> new_name: string -> decision_matrix_factory_result;
        move: t -> moving: 'dimension_name -> to_after: 'dimension_name -> decision_matrix_factory_result;            
    }    
    type alternative_editing = alternative_name dimension_editing
    type factor_editing = factor_name dimension_editing

    val dimension_name_string: dimension_name -> string
    val string_of_factor_name: factor_name -> string 
    val string_of_alternative_name: alternative_name -> string
    val factor_names: t -> factor_name list
    val weight_cells: t -> (alternative_name * (factor_value list))
    val alternatives: t -> alternative list

    val empty: unit -> t
    val factor_editing: factor_name dimension_editing
    val alternative_editing: alternative_name dimension_editing
    val update_cell: t -> factor_name -> alternative_name -> string -> decision_matrix_factory_result         

    val decode: string -> (t, string) Tea.Result.t
    val encode: t -> string
end

module DecisionMatrix : DecisionMatrix = struct

    module DecisionMatrixTable = Table.MakeTable(IntCellValue)
    type t = DecisionMatrixTable.t
    type factor_name = DecisionMatrixTable.column_name 
    type alternative_name = DecisionMatrixTable.row_name
    type dimension_name = 
        | FactorName of factor_name
        | AlternativeName of alternative_name
    type factor_value = factor_name * IntCellValue.t
    type alternative = {
        name: alternative_name;
        values: factor_value list;
        score: int;
    }
    type decision_matrix_factory = unit -> t
    type decision_matrix_factory_result = (decision_matrix_factory, string) Tea.Result.t     
    type dimension_item =
        | Alternative of alternative_name
        | Factor of factor_name

    type 'dimension_name dimension_editing = {
        label: string;
        add_instructions: string;
        add: t -> with_name: string -> decision_matrix_factory_result;
        remove: t -> with_name: 'dimension_name -> decision_matrix_factory_result;
        change_name: t -> old_name: 'dimension_name -> new_name: string -> decision_matrix_factory_result;
        move: t -> moving: 'dimension_name -> to_after: 'dimension_name -> decision_matrix_factory_result;            
    }    
    type alternative_editing = alternative_name dimension_editing
    type factor_editing = factor_name dimension_editing

    let dimension_name_string = function 
        | FactorName name -> DecisionMatrixTable.string_of_column_name name
        | AlternativeName name -> DecisionMatrixTable.string_of_row_name name
        
    let string_of_factor_name = DecisionMatrixTable.string_of_column_name
    let string_of_alternative_name = DecisionMatrixTable.string_of_row_name
    let weights_name_string = "Weight"
    let weights_error fmt = Tea.Result.Error (sprintf fmt weights_name_string)
    let factor_names t : factor_name list = DecisionMatrixTable.column_names t
        
    let empty =
        DecisionMatrixTable.row_editing.add 
            ~with_name: weights_name_string
            (DecisionMatrixTable.empty ())        
        |> Utils.ResultEx.getExnFailWith

    let weights_row table = 
        table
        |> DecisionMatrixTable.rows 
        |> List.find (fun row -> (DecisionMatrixTable.row_name_string row) = weights_name_string)

    let weight_cells table = 
        let weights_row = weights_row table in
        let cells = Belt.List.zip (table |> factor_names) (weights_row |> DecisionMatrixTable.row_cells) in
        (DecisionMatrixTable.row_name weights_row, cells)

    let alternatives table = 
        let weight_cells = table |> weight_cells |> snd in
        let score_of row = 
            Belt.List.zip (DecisionMatrixTable.row_cells row) weight_cells
            |> List.map (fun (alternative_score, (_, weighting)) -> alternative_score * weighting)
            |> List.fold_left (+) 0 in
        table 
        |> DecisionMatrixTable.rows 
        |> List.filter (fun row -> (DecisionMatrixTable.row_name_string row) <> weights_name_string)
        |> List.map (fun row -> 
            {
                name = DecisionMatrixTable.row_name row;
                values = Belt.List.zip (table |> factor_names) (DecisionMatrixTable.row_cells row); 
                score = score_of row;
            }            
        )

    let factor_editing : factor_editing = 
        let decorated: DecisionMatrixTable.column_name DecisionMatrixTable.dimension_editing = DecisionMatrixTable.column_editing in        
        { label = "Criterion"
        ; add_instructions = "Add a new criterion which you will use to judge your choices."
        ; add = decorated.add
        ; remove = decorated.remove
        ; change_name = decorated.change_name
        ; move = decorated.move 
        }        
    
    let alternative_editing : alternative_editing = 
        let decorated: DecisionMatrixTable.row_name DecisionMatrixTable.dimension_editing = DecisionMatrixTable.row_editing in 
        let add = decorated.add in
        let remove table ~with_name = 
            let weights_row = weights_row table in
            if with_name = (DecisionMatrixTable.row_name weights_row) 
            then weights_error "Cannot remove the %s."
            else decorated.remove table ~with_name: with_name 
        in
        let change_name table ~old_name ~new_name =
            let weights_row = weights_row table in
            if old_name = (DecisionMatrixTable.row_name weights_row) 
            then weights_error "Cannot rename %s."
            else decorated.change_name table ~old_name: old_name ~new_name: new_name
        in
        let move table ~moving ~to_after =
            let weights_row = weights_row table in
            if moving = (DecisionMatrixTable.row_name weights_row) 
            then weights_error "Cannot move %s."
            else if to_after = (DecisionMatrixTable.row_name weights_row) 
            then weights_error "Cannot move to before %s."
            else decorated.move table ~moving: moving ~to_after: to_after
        in
            { label = "Choice"
            ; add_instructions = "Enter the name of the next 'Choice'."
            ; add; remove; change_name; move } 

    let update_cell (matrix: t) factor_name alternative_name new_value : decision_matrix_factory_result =
        DecisionMatrixTable.update_cell_from_string 
            matrix
            ~column_name: factor_name
            ~row_name: alternative_name
            ~new_value: new_value

    let decode s = DecisionMatrixTable.decode s
    let encode matrix = DecisionMatrixTable.encode matrix 
end