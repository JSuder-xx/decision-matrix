open Printf

module type CellValue = sig
    type t  
    val default: unit -> t
    val of_string: string -> t option
    val to_string: t -> string
end

module type Table = sig    
    (** the type of the value of a single cell of the table *)
    type cell_value
    (** the table type itself *)
    type t
    (** row name (existential, opaque) *)
    type row_name    
    (** column name (existential, opaque) *)
    type column_name
    (** either a specific row or column by name *)
    type point_on_axis =
        | Row of row_name
        | Column of column_name
    (** a factory which returns a new table *)
    type table_factory = unit -> t
    (** a result for either a table factory or an error string describing why the factory could not be produced *)
    type table_factory_result = (table_factory, string) Tea.Result.t 
    (** a single row of table data *)
    type row

    type 'dimension_name dimension_editing = {
        add: t -> with_name: string -> table_factory_result;
        remove: t -> with_name: 'dimension_name -> table_factory_result;
        change_name: t -> old_name: 'dimension_name -> new_name: string -> table_factory_result;
        move: t -> moving: 'dimension_name -> to_after: 'dimension_name -> table_factory_result;
    }

    (** Create a new empty table. *)
    val empty: table_factory

    val string_of_row_name: row_name -> string
    val string_of_column_name: column_name -> string

    val column_names: t -> column_name list    
    val rows: t -> row list                        
    val row_name: row -> row_name
    val row_name_string: row -> string
    val row_cells: row -> cell_value list    

    val column_editing: column_name dimension_editing
    val row_editing: row_name dimension_editing
    val update_cell: t -> column_name: column_name -> row_name: row_name -> new_value: cell_value -> table_factory_result 
    val update_cell_from_string: t -> column_name: column_name -> row_name: row_name -> new_value: string -> table_factory_result 

    val decode: string -> (t, string) Tea.Result.t
    val encode: t -> string
end

module MakeTable (CellValue: CellValue) : (Table with type cell_value := CellValue.t) = struct
    type row = { 
        name: string;
        cell_values: CellValue.t list
    }

    type t = { 
        columns: string list;
        rows: row list;
    }
    
    type table_factory = unit -> t       
    
    type table_factory_result = (table_factory, string) Tea.Result.t 

    type row_name = string
    let string_of_row_name s = s
    type column_name = string
    let string_of_column_name s = s

    type point_on_axis =
        | Row of row_name
        | Column of column_name

    type 'dimension_name dimension_editing = {
        add: t -> with_name: string -> table_factory_result;
        remove: t -> with_name: 'dimension_name -> table_factory_result;
        change_name: t -> old_name: 'dimension_name -> new_name: string -> table_factory_result;
        move: t -> moving: 'dimension_name -> to_after: 'dimension_name -> table_factory_result;
    }

    let empty () = { columns = []; rows = []; }
    let column_names { columns } = columns |> List.rev
    let rows { rows } = rows |> List.rev
    let row_name { name } = name
    let row_name_string { name } = name
    let row_cells { cell_values } = cell_values |> List.rev
    let row_name_strings table = table.rows |> List.map (fun {name} -> name)            

    let make_dimension_editing 
        ~string_of_name:(string_of_name:('dimension_name -> string)) 
        ~get_names 
        ~add_to_table 
        ~remove_index_from_table 
        ~move_by_index 
        ~change_name_at_index =        
        let add table ~with_name =
            Utils.ResultEx.map                
                (fun verified_name -> fun () -> (add_to_table table verified_name))
                (Utils.Names.verify_new_name with_name (get_names table)) in
        let remove table ~with_name:(with_name:'dimension_name) =
            Utils.ResultEx.map
                (fun index_found -> fun () -> (remove_index_from_table table index_found))
                (Utils.Names.find_name_index (string_of_name with_name) (get_names table)) in
        let change_name table ~old_name:(old_name: 'dimension_name) ~new_name:(new_name: string) = 
            if (string_of_name old_name) = new_name
            then Tea.Result.Ok(fun () -> table)
            else 
                Utils.ResultEx.map2 
                    (fun verified_name index -> fun () -> (change_name_at_index ~index: index ~new_name: verified_name table))
                    (Utils.Names.verify_new_name new_name (get_names table))
                    (Utils.Names.find_name_index (string_of_name old_name) (get_names table)) in
        let move table ~moving:(moving:'dimension_name) ~to_after:(to_after:'dimension_name) =
            Utils.ResultEx.map2
                (fun moving_index before_index -> fun () -> move_by_index ~from_index: moving_index ~to_before_index: before_index table)
                (Utils.Names.find_name_index (string_of_name moving) (get_names table))
                (Utils.Names.find_name_index (string_of_name to_after) (get_names table)) in 
        (
            { add; remove; change_name; move }
        )

    let column_editing : column_name dimension_editing = 
        let add_default_cell row = { row with cell_values = (CellValue.default ())::row.cell_values } in 
        let remove_cell_index index_to_remove row = 
            { row with cell_values = Utils.ListEx.remove_index index_to_remove row.cell_values } in
        let move_cell ~from_index ~to_before_index row =
            { row with cell_values = row.cell_values |> Utils.ListEx.move ~from_index: from_index ~to_before_index: to_before_index } in
        (make_dimension_editing
            ~string_of_name: string_of_column_name
            ~get_names: (fun { columns } -> columns)
            ~add_to_table: (fun table verified_name -> 
                {
                    columns = verified_name::table.columns;
                    rows = table.rows |> List.map add_default_cell 
                }
            )        
            ~remove_index_from_table: (fun table index_found -> 
                {
                    columns = table.columns |> Utils.ListEx.remove_index index_found;
                    rows = table.rows |> List.map (remove_cell_index index_found) 
                }
            ) 
            ~change_name_at_index: (fun ~index ~new_name table ->
                { 
                    table with columns = table.columns |> List.mapi (fun cur_index column_name -> if cur_index = index then new_name else column_name)
                }            
            )
            ~move_by_index: (fun ~from_index ~to_before_index table ->
                {
                    columns = table.columns |> Utils.ListEx.move ~from_index: from_index ~to_before_index: to_before_index;
                    rows = table.rows |> List.map (move_cell ~from_index: from_index ~to_before_index: to_before_index)
                }
            )             
        )

    let row_editing : row_name dimension_editing =
        make_dimension_editing
            ~string_of_name: string_of_row_name
            ~get_names: row_name_strings
            ~add_to_table: (fun table verified_name -> 
                let new_row = 
                    {
                        name = verified_name;
                        cell_values = table.columns |> List.map (fun _ -> CellValue.default ())
                    } in
                { table with rows = new_row::table.rows }
            )
            ~remove_index_from_table: (fun table index_found -> 
                { table with rows = Utils.ListEx.remove_index index_found table.rows }                            
            )
            ~change_name_at_index: (fun ~index ~new_name table ->            
                { table with rows = 
                    table.rows 
                    |> List.mapi (fun idx row -> 
                        if idx = index 
                        then { row with name = new_name }
                        else row
                    )
                }
            )
            ~move_by_index: (fun ~from_index ~to_before_index table ->            
                { table with rows =
                    table.rows |> Utils.ListEx.move ~from_index: from_index  ~to_before_index: to_before_index
                }
            )

    let update_cell table ~column_name ~row_name ~new_value =
        Utils.ResultEx.map2
            (fun column_index row_index -> fun () ->
                { table with rows =
                    table.rows 
                    |> List.mapi (fun index row ->
                        if index <> row_index
                        then row
                        else 
                            { row with cell_values =
                                row.cell_values
                                |> List.mapi (fun index cell_value ->
                                    if index <> column_index
                                    then cell_value
                                    else new_value
                                )
                            }
                    )                    
                }                
            )
            (Utils.Names.find_name_index (string_of_column_name column_name) table.columns)
            (Utils.Names.find_name_index (string_of_row_name row_name) (table |> row_name_strings))

    let decode s = 
        let open Tea.Json in
            let string_list_decoder = Decoder.map List.rev (Decoder.list Decoder.string) in
            let values_decoder = Decoder.map List.rev (Decoder.list string_list_decoder) in
            let record_decoder = Decoder.map3 
                (fun columns rows values -> (columns, rows, values))
                (Decoder.field "columns" string_list_decoder)
                (Decoder.field "rows" string_list_decoder) 
                (Decoder.field "values" values_decoder) in                
            let cleaned_string = String.trim s in
            if (String.length cleaned_string) = 0
            then Tea.Result.Error "Empty string; unable to deserialize"
            else 
                (Decoder.decodeString record_decoder cleaned_string)
                |> Utils.ResultEx.flatMap (fun (columns, row_names, values) ->
                    Belt.List.zip row_names values
                    |> Utils.ListEx.map_while_ok (fun (name, cell_value_strings) -> 
                        cell_value_strings 
                        |> List.map (fun cell_value_string -> 
                            match CellValue.of_string cell_value_string with
                            | Some v -> Tea.Result.Ok v
                            | None -> Tea.Result.Error (sprintf "could not parse '%s' for row '%s'" cell_value_string name)
                        ) 
                        |> Tea.Result.accumulate 
                        |> Utils.ResultEx.map (fun cell_values -> { name; cell_values })
                    )
                    |> Utils.ResultEx.map (fun rows -> { columns; rows })
                ) 
                       

    let encode { columns; rows } = 
        [
            (
                "columns", 
                columns |> List.rev |> Array.of_list |> Js.Json.stringArray
            )
            ; (
                "rows",
                rows |> List.rev |> Array.of_list |> Array.map (fun {name} -> name) |> Js.Json.stringArray
            )
            ; (
                "values",
                rows 
                |> List.rev
                |> List.map (fun {cell_values} -> 
                    cell_values
                    |> List.rev
                    |> List.map CellValue.to_string
                    |> Array.of_list
                    |> Js.Json.stringArray
                )
                |> Array.of_list 
                |> Js.Json.array
            )
        ] 
        |> Js.Dict.fromList
        |> Js.Json.object_ 
        |> Js.Json.stringify
            
    let update_cell_from_string table ~column_name ~row_name ~new_value =
        match (CellValue.of_string new_value) with
        | None -> Tea.Result.Error (sprintf "failed to update column %s, row %s - '%s' is not a valid value" (string_of_column_name column_name) (string_of_row_name row_name) new_value)
        | Some cell -> update_cell table ~column_name: column_name ~row_name: row_name ~new_value: cell
end