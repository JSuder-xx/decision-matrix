open Tea.Html
open Utils
open Printf
open DecisionMatrix
open DecisionMatrixEditing

module TriIntPartitioner = TriPartitioner.Make(
    struct
        type t = int
        let compare = compare
    end
)

module TeaHtmlEx = struct

    let on_keyed ~eventName ~key ~decoder = 
        Tea.Html.onCB eventName key (fun event ->
            if Tea.Html.defaultOptions.stopPropagation then event##stopPropagation () |> ignore;
            if Tea.Html.defaultOptions.preventDefault then event##preventDefault () |> ignore;
            event
            |> Tea_json.Decoder.decodeEvent decoder
            |> Tea_result.result_to_option
        )

    module Keydown = struct
        type keydown_info = 
            { ctrlKey: bool
            ; keyCode: int
            }
        
        let keydown ~key ~msg ~keydown_predicate = 
            let decode_keydown_info = 
                let ctrlKey = Tea_json.Decoder.field "ctrlKey" Tea_json.Decoder.bool in
                Tea.Json.Decoder.map2 (fun keyCode ctrlKey -> { keyCode; ctrlKey }) Tea.Html.keyCode ctrlKey in
            let keydown_to_message info = 
                if keydown_predicate info
                then Tea.Json.Decoder.succeed msg                
                else Tea.Json.Decoder.fail "Incorrect keys" in
            on_keyed 
                ~eventName: "keydown" 
                ~key: key
                ~decoder: (Tea.Json.Decoder.andThen keydown_to_message decode_keydown_info)
    end

end

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
    let editable_cell_value ?show_percentage_out_of_opt:(show_percentage_out_of_opt = None) ?additional_class_opt:(additional_class_opt = None) =
        match change_cell_value_opt with
            | Some change_cell_value -> 
                (fun alternative_name (factor_name, v) ->                    
                    td 
                        [ editable_cell_styling alternative_name factor_name
                        ; match additional_class_opt with
                            | None -> Tea.Html.noProp
                            | Some additional -> class' additional
                        ] 
                        [ input'
                            [ onInput (change_cell_value alternative_name factor_name)
                            ; value (string_of_int v)                            
                            ; type' "number"
                            ; class' Styles.cell_value_class
                            ]
                            []                        
                        ; match show_percentage_out_of_opt with
                            | None -> 
                                Tea.Html.noNode
                            | Some show_percentage_out_of ->
                                let display = 
                                    if show_percentage_out_of = 0 
                                    then ""
                                    else 
                                        let percentage = (100 * v) / show_percentage_out_of in
                                        Printf.sprintf " %d%%" percentage in
                                span [class' Styles.percentage_class] [text display]
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
                [ [(header_cell "Criteria")]
                ; factor_names |> List.map factor_view                
                ; [(header_cell "")]
                ]
            ) in
    let weights_row = 
        tr []
            (List.concat 
                [
                    [(header_cell "Significance")]
                    ; (
                        let (weights_name, values) = DecisionMatrix.weight_cells matrix in
                        let total = values |> List.map snd |> List.fold_left (+) 0 in 
                        values |> List.map (editable_cell_value ~show_percentage_out_of_opt: (Some total) ~additional_class_opt: (Some Styles.weight_cell_class) weights_name)
                    )
                    ; [th [class' Styles.score_header_class] [text "Score"]]
                ]
            ) in      
    let alternative_row (({name; values; score}: DecisionMatrix.alternative), state) =        
        tr [] 
            (List.concat 
                [ [link_header_cell (DecisionMatrix.AlternativeName name)]
                ; values  |> List.map (editable_cell_value name)
                ; [ td 
                    [state |> Styles.score_class |> class'] 
                    [readonly_span score]
                  ]
                ]
            ) in
    let factor_names = matrix |> DecisionMatrix.factor_names in
    let alternatives = matrix |> DecisionMatrix.alternatives in                                      
    let almost_there_messsage_text required = text (sprintf "Add at least one %s to start filling in the the Decision Matrix cells." required) in
    match factor_names, alternatives with
    | [], [] -> p [] [text (sprintf "Use the Add buttons above to start filling in the Decision Matrix.")]
    | _, [] -> p [] [almost_there_messsage_text DecisionMatrix.alternative_editing.label]
    | [], _ -> p [] [almost_there_messsage_text DecisionMatrix.factor_editing.label]
    | _ ->
        let partition = alternatives 
            |> List.map (fun ({score}: DecisionMatrix.alternative) -> score) 
            |> TriIntPartitioner.partition in
        table
            []
            ((header_row factor_names)
            ::(weights_row::(alternatives |> List.map (fun (alt: DecisionMatrix.alternative) -> (alt, partition alt.score)) |> List.map alternative_row))
            )
let should_display_matrix_view matrix =
    let factors = matrix |> DecisionMatrix.factor_names in
    let alternatives = matrix |> DecisionMatrix.alternatives in                                      
    let missing_pieces = (factors = []) || (alternatives = []) in
    not missing_pieces

let error_view msg_opt = 
    span [Tea.Html.class' Styles.error_class] [text (Belt.Option.getWithDefault msg_opt "")]

let update_name_view ~instructions ~caption ~current_value ~error_message = 
    let is_commit_enabled = Belt.Option.isNone error_message in
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
                            ; TeaHtmlEx.Keydown.keydown 
                                ~key: (Printf.sprintf "%s%b" current_value is_commit_enabled)
                                ~msg: DecisionMatrixEditing.Message.Commit
                                ~keydown_predicate: (fun keydown_info  -> keydown_info.keyCode = 13 && is_commit_enabled)
                        ] 
                        []
                    ; button_view "OK" DecisionMatrixEditing.Message.Commit ~enabled: is_commit_enabled
                    ; button_view "Cancel" DecisionMatrixEditing.Message.Cancel 
                ]
            ; error_view error_message
            ; (
                match instructions with 
                | None -> Tea.Html.noNode
                | Some instruction_text -> p [] [text instruction_text]
            )
        ]

let adding_view (adding_info: AddingInfo.t) ~error_message = 
    (* let x = adding_info.dimension_editing *)
    update_name_view
        ~caption: (sprintf "New %s" (DimensionToEdit.label adding_info.dimension_editing))
        ~instructions: (Some (DecisionMatrixEditing.DimensionToEdit.add_instructions adding_info.dimension_editing))
        ~current_value: adding_info.new_name
        ~error_message: error_message

let changing_name_view ({dimension_item_to_edit; new_name}:ChangingNameInfo.t) ~error_message = 
    update_name_view
        ~caption: (sprintf "Re-name %s from '%s' to " 
            (DimensionItemToEdit.label dimension_item_to_edit) 
            (DimensionItemToEdit.name dimension_item_to_edit) 
        )
        ~instructions: None
        ~current_value: new_name
        ~error_message: error_message

let share_caption = "Share to Url"

module Instructions = struct

    let general = 
        [ {j|
A Decision Matrix is an analytical tool for choosing between competing Choices in a particular context. Each Choice 
is scored with a number from 0 to 10 for relevant Criteria. Each Criterion is given a Significance score which indicates the significant of that Criterion for the decision at hand. 
The Choice with the highest weighted score is the best choice for that situation. 
|j}     ; {j|
A Decision Matrix can be adapted to different situations by changing the Significance of the Criteria. This allows peers to share a common assessment of the Choices and yet 
adapt that assessment to each individuals situational requirements. 
|j}
        ; {j|
Click <$share_caption> and copy the url to share the decision matrix with a collaborator. 
|j}
        ]

end

let hyper_links = 
    [ ("https://jsuder-xx.github.io", "My Home Page")
    ; ("https://github.com/JSuder-xx/decision-matrix", "On GitHub")
    ]

let view ({ decision_matrix; interaction_state; error_message }: Model.t) =
    let (factor_label, alternative_label) = (DecisionMatrix.factor_editing.label, DecisionMatrix.alternative_editing.label) in
    let update_cell_value (alternative_name:DecisionMatrix.alternative_name) (factor_name:DecisionMatrix.factor_name) (value: string) : Message.t =
        Message.UpdateCellValue (alternative_name, factor_name, value) in
    let paragraph s = p [] [text s] in 
    let instructions paragraphs = div [class' Styles.instructions_container_class] (paragraphs |> List.map paragraph) in
    let (top_controls, main_content) =
        (match interaction_state with
            | EditingCellValues ->
                let display_matrix_view = should_display_matrix_view decision_matrix in
                let add_factor_button = button_view (sprintf "+%s" factor_label) (Message.StartAdding (Either.Left DecisionMatrix.factor_editing)) in
                let add_alternative_button = button_view (sprintf "+%s" alternative_label) (Message.StartAdding (Either.Right DecisionMatrix.alternative_editing)) in
                let button_divider () = span [] [text " | "] in
                let matrix_instructions = instructions Instructions.general in
                (
                    (
                        if not display_matrix_view
                        then 
                            [add_factor_button; add_alternative_button; matrix_instructions]
                        else
                            [
                                add_factor_button
                                ; add_alternative_button
                                ; button_divider ()
                                ; button_view "Re-Name" (Message.SelectItemTo ChangeName)
                                ; button_view "Remove" (Message.SelectItemTo Delete)
                                ; button_view "Move" (Message.SelectItemTo Move)
                                ; button_divider ()
                                ; button_view share_caption (Message.SaveToUrl)
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
                        ; instructions [
                            let message action = 
                                sprintf 
                                    "Click the header link for the %s or %s below to %s. Click Cancel if you would rather not." 
                                    factor_label alternative_label action in                                
                            match action with
                            | Move -> message "begin MOVING the element"
                            | ChangeName -> message "RE-NAME the element"
                            | Delete -> message "DELETE the element from the Decision Matrix"
                        ]
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
                        ; instructions ["The original item will be placed after the next item you choose. Choose Done when finished moving items."]
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
            div [ class' Styles.menu_class ]                
                (
                    hyper_links
                    |> List.map (fun (msg, txt) ->
                        a [ Tea.Html.href msg; Tea.Html.target "_blank" ] [text txt] 
                    )
                )
            ; div [ class' Styles.body_class ]
                [ div [class' Styles.top_controls_class] top_controls
                ; hr [] []
                ; main_content
                ]        
        ]
