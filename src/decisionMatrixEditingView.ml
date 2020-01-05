open Tea.Html
open Utils
open Printf
open DecisionMatrix
open DecisionMatrixEditing

let button_view ?enabled:(enabled = true) title msg  =
    button
        [ onClick msg; Attributes.disabled (not enabled)]
        [ text title ]

let button_link txt message additional_class = 
    a
        [onClick message; classList [(additional_class, true); (Styles.button_link_class, true)]]
        [text txt]

let container_view = div [] 

let decision_matrix_view 
    ?change_cell_value_opt:(change_cell_value_opt=None) 
    ?header_message_for_dimension_name_opt:(header_message_for_dimension_name_opt=None)  
    ?should_highlight_cell_opt:(should_highlight_cell_opt=None)
    matrix =
    let highlight_class b = classList [(Styles.highlight_class, b)] in
    let editable_cell_styling = match should_highlight_cell_opt with
        | Some should_highlight_cell -> (fun alternative factor ->
            highlight_class (should_highlight_cell alternative factor)
        )
        | None -> (fun _ _ -> noProp) in
    let readonly_dimension_name_header name = th [] [span [] [name |> DecisionMatrix.dimension_name_string |> text]] in
    let link_header_cell = match header_message_for_dimension_name_opt with
        | Some header_message_for_dimension_name ->
            (fun (name: DecisionMatrix.dimension_name) ->
                match header_message_for_dimension_name name with
                | Some message ->
                    th [] 
                        [ 
                            a [onClick message] 
                                [text (DecisionMatrix.dimension_name_string name)]
                        ]
                | None ->
                    readonly_dimension_name_header name
            )
        | None -> readonly_dimension_name_header in
    let header_cell s = th [] [text s] in
    let readonly_span v = v |> string_of_int |> text |> (Belt.List.make 1) |> (span [class' Styles.cell_value_class]) in
    let editable_cell_value ?additional_class_opt:(additional_class_opt = None) =
        match change_cell_value_opt with
            | Some change_cell_value -> 
                (fun alternative_name (factor_name, v) ->                    
                    td 
                        [
                            editable_cell_styling alternative_name factor_name
                            ; match additional_class_opt with
                                | None -> Tea.Html.noProp
                                | Some additional -> class' additional
                        ] 
                        [
                        input'
                            [
                                onInput (change_cell_value alternative_name factor_name)
                                ; value (string_of_int v)                            
                                ; type' "number"
                                ; class' Styles.cell_value_class
                            ]
                            []                        
                        ]
                )
            | None -> 
                (fun alternative_name (factor_name, v) -> 
                    td [editable_cell_styling alternative_name factor_name] [readonly_span v]                    
                ) in 
    let header_row factor_names = 
        let factor_view factor_name = link_header_cell (DecisionMatrix.FactorName factor_name) in
        tr [] 
            (List.concat 
                [
                    [(header_cell "Factors")]
                    ; (
                        factor_names
                        |> List.map factor_view
                    )
                    ; [(header_cell "")]
                ]
            ) in
    let weights_row = 
        tr []
            (List.concat 
                [
                    [(header_cell "Weight")]
                    ; (
                        let (weights_name, values) = DecisionMatrix.weight_cells matrix in
                        values |> List.map (editable_cell_value ~additional_class_opt: (Some Styles.weight_cell_class) weights_name)
                    )
                    ; [th [class' Styles.score_class] [text "Score"]]
                ]
            ) in      
    let alternative_row ({name; values; score}: DecisionMatrix.alternative) =        
        tr [] 
            (List.concat 
                [
                    [link_header_cell (DecisionMatrix.AlternativeName name)]
                    ; values  |> List.map (editable_cell_value name)
                    ; [td [class' Styles.score_class] [readonly_span score]]
                ]
            ) in
    let factor_names = matrix |> DecisionMatrix.factor_names in
    let alternatives = matrix |> DecisionMatrix.alternatives in                                      
    let almost_there_messsage_text required = text (sprintf "Almost there! Add at least one %s to start filling in the the Decision Matrix." required) in
    match factor_names, alternatives with
    | [], [] -> p [] [text (sprintf "Add %ss and %ss using the buttons above to start filling in the Decision Matrix." DecisionMatrix.alternative_editing.label DecisionMatrix.factor_editing.label)]
    | _, [] -> p [] [almost_there_messsage_text DecisionMatrix.alternative_editing.label]
    | [], _ -> p [] [almost_there_messsage_text DecisionMatrix.factor_editing.label]
    | _ ->
        table
            []
            ((header_row factor_names)
            ::(weights_row::(alternatives |> List.map alternative_row))
            )
let should_display_matrix_view matrix =
    let factors = matrix |> DecisionMatrix.factor_names in
    let alternatives = matrix |> DecisionMatrix.alternatives in                                      
    let missing_pieces = (factors = []) || (alternatives = []) in
    not missing_pieces

let error_view msg_opt = 
    span [Tea.Html.class' Styles.error_class] [text (Belt.Option.getWithDefault msg_opt "")]

let update_name_view ~caption ~current_value ~error_message = 
    container_view
        [
            container_view
                [
                    label [] [text caption]
                    ; input' 
                        [
                            value current_value
                            ; onInput (fun name -> DecisionMatrixEditing.Message.UpdateName name)     
                            ; (autofocus true)
                            ; classList [(Styles.error_class, Belt.Option.isSome error_message)]
                        ] 
                        []
                    ; button_view "Commit" DecisionMatrixEditing.Message.Commit ~enabled: (Belt.Option.isNone error_message)
                    ; button_view "Cancel" DecisionMatrixEditing.Message.Cancel 
                ]
            ; error_view error_message
        ]

let adding_view (adding_info: AddingInfo.t) ~error_message = 
    update_name_view
        ~caption: (sprintf "New %s" (DimensionToEdit.label adding_info.dimension_editing))
        ~current_value: adding_info.new_name
        ~error_message: error_message

let changing_name_view ({dimension_item_to_edit; new_name}:ChangingNameInfo.t) ~error_message = 
    update_name_view
        ~caption: (sprintf "Re-name %s from '%s' to " 
            (DimensionItemToEdit.label dimension_item_to_edit) 
            (DimensionItemToEdit.name dimension_item_to_edit) 
        )
        ~current_value: new_name
        ~error_message: error_message

let view ({ decision_matrix; interaction_state; error_message }: Model.t) =
    let (factor_label, alternative_label) = (DecisionMatrix.factor_editing.label, DecisionMatrix.alternative_editing.label) in
    let update_cell_value (alternative_name:DecisionMatrix.alternative_name) (factor_name:DecisionMatrix.factor_name) (value: string) : Message.t =
        Message.UpdateCellValue (alternative_name, factor_name, value) in
    let instructions s = p [] [text s] in
    let (top_controls, main_content) =
        (match interaction_state with
            | EditingCellValues ->
                let display_matrix_view = should_display_matrix_view decision_matrix in
                let add_factor_button = button_view (sprintf "+%s" factor_label) (Message.StartAdding (Either.Left DecisionMatrix.factor_editing)) in
                let add_alternative_button = button_view (sprintf "+%s" alternative_label) (Message.StartAdding (Either.Right DecisionMatrix.alternative_editing)) in
                let matrix_instructions = instructions 
                    ({j|
A Decision Matrix is an analytical tool for choosing between different Alternatives (aka Choices or Options) by rating how each Alternative 
scores for different Factors (aka Criteria). The analyst (you) determines the Weight (significance) of each Factor for the
current problem or situation. The Matrix displays the score for each alternative based upon the combination of its own ratings and the 
situational weights. The Alternative with the highest score is the best choice based upon the given weights. 
|j}
                    ) in
                (
                    (
                        if not display_matrix_view
                        then 
                            [add_factor_button; add_alternative_button; matrix_instructions]
                        else
                            [
                                add_factor_button
                                ; add_alternative_button
                                ; button_view "Re-Name" (Message.SelectItemTo ChangeName)
                                ; button_view "Remove" (Message.SelectItemTo Delete)
                                ; button_view "Move" (Message.SelectItemTo Move)
                                ; button_view "Share to Url" (Message.SaveToUrl)
                                ; button_view "Reset" Message.RequestReset
                                ; matrix_instructions
                            ]
                    )
                    , container_view 
                        [
                            decision_matrix_view 
                                decision_matrix 
                                ~change_cell_value_opt: (Some update_cell_value)                                  
                            ; error_view error_message
                        ]
                )
            | SelectingItemTo action ->
                (
                    [
                        button_view "Cancel" Message.Cancel
                        ; instructions (
                            let message action = 
                                sprintf 
                                    "Click the header link for the %s or %s below to %s. Click Cancel if you would rather not." 
                                    factor_label alternative_label action in                                
                            match action with
                            | Move -> message "begin MOVING the element"
                            | ChangeName -> message "RE-NAME the element"
                            | Delete -> message "DELETE the element from the Decision Matrix"
                        )
                    ]
                    , decision_matrix_view 
                        decision_matrix 
                        ~header_message_for_dimension_name_opt: (Some 
                            (fun dimension_name -> 
                                let item_to_edit = match dimension_name with
                                    | DecisionMatrix.FactorName name -> DimensionItemToEdit.make_factor name
                                    | DecisionMatrix.AlternativeName name -> DimensionItemToEdit.make_alternative name in
                                Some (
                                    match action with
                                    | Move -> Message.StartMoving item_to_edit
                                    | ChangeName -> Message.StartChangingName item_to_edit
                                    | Delete -> Message.RequestRemove item_to_edit                             
                                )
                            )
                        )                    
                )
            | MovingItem dimension_item_to_edit ->
                let highlight_predicate = match dimension_item_to_edit with
                    | Left (_, name) -> (fun _alternative factor -> factor = name)
                    | Right (_, name) -> (fun alternative _factor -> alternative = name) in
                let can_move_to = match dimension_item_to_edit with
                    | Left (_, name) -> (fun dimension_name -> 
                        match dimension_name with
                        | DecisionMatrix.AlternativeName _ -> false
                        | DecisionMatrix.FactorName factor -> factor <> name
                    )
                    | Right (_, name) -> (fun dimension_name -> 
                        match dimension_name with
                        | DecisionMatrix.AlternativeName alternative -> alternative <> name
                        | DecisionMatrix.FactorName _ -> false
                    ) in
                (
                    [
                        button_view "Done" Message.Cancel
                        ; instructions "The original item will be placed after the next item you choose. Choose Done when finished moving items."
                    ]
                    , decision_matrix_view 
                        decision_matrix 
                        ~header_message_for_dimension_name_opt: (Some 
                            (fun dimension_name -> 
                                if can_move_to dimension_name
                                then (Some (Message.MoveToAfter dimension_name))
                                else None
                            )
                        )       
                        ~should_highlight_cell_opt: (Some highlight_predicate)                        
                )
            | Adding adding ->
                (
                    [adding_view ~error_message: error_message adding]
                    , decision_matrix_view decision_matrix
                )
            | ChangingName changing ->
                (
                    [changing_name_view ~error_message: error_message changing]
                    , decision_matrix_view decision_matrix
                )
            | ShowingDialog dialog ->
                (
                    [Dialog.view dialog]
                    , decision_matrix_view decision_matrix
                )            
        ) in
    container_view 
        [
            div [class' Styles.top_controls_class] top_controls
            ; hr [] []
            ; main_content
        ]
