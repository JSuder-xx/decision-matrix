open DecisionMatrix
open Utils

let error_message = function
    | Tea.Result.Ok _ -> None
    | Tea.Result.Error error_message -> Some error_message

type element_action =
    | Move
    | ChangeName
    | Delete

module DimensionToEdit = struct
    type t = (DecisionMatrix.factor_editing, DecisionMatrix.alternative_editing) Either.t 
    let factor : t = Either.Left DecisionMatrix.factor_editing
    let alternative : t = Either.Right DecisionMatrix.alternative_editing
    let label : t -> string = function
        | Left editing -> editing.label
        | Right editing -> editing.label
end

let decision_matrix_key = "decision_matrix"

module DimensionItemToEdit = struct
    type t = (DecisionMatrix.factor_editing * DecisionMatrix.factor_name, DecisionMatrix.alternative_editing * DecisionMatrix.alternative_name) Either.t 

    let label : t -> string = function
        | Left (editing, _) -> editing.label
        | Right (editing, _) -> editing.label
    
    let name : t -> string = function
        | Left (_, factor_name) -> DecisionMatrix.string_of_factor_name factor_name
        | Right (_, alternative_name) -> DecisionMatrix.string_of_alternative_name alternative_name

    let make_factor name : t =
        Either.Left (DecisionMatrix.factor_editing, name)

    let make_alternative name : t =
        Either.Right (DecisionMatrix.alternative_editing, name)        

    let remove (edit:t) = 
        match edit with
        | Left (editing, name) -> editing.remove ~with_name: name 
        | Right (editing, name) -> editing.remove ~with_name: name 
end

module Message = struct
    type t = 
        | Cancel

        | SelectItemTo of element_action

        | StartAdding of DimensionToEdit.t
        | StartChangingName of DimensionItemToEdit.t
        | UpdateName of string
        | Commit

        | StartMoving of DimensionItemToEdit.t
        | MoveToAfter of DecisionMatrix.dimension_name

        | RequestRemove of DimensionItemToEdit.t
        | Remove of DimensionItemToEdit.t

        | RequestReset 
        | Reset

        | UpdateCellValue of DecisionMatrix.alternative_name * DecisionMatrix.factor_name * string        

        | SaveToUrl 
        | LoadedFromLocalStorage of string
        | ErrorLoadingFromLocalStorage of string
        | LocationChanged of Web.Location.location
end

module AddingInfo = struct
    type t = {
        dimension_editing: DimensionToEdit.t;
        new_name: string;
    }

    let make dimension_editing = { dimension_editing; new_name = "" }

    let commit decision_matrix {dimension_editing; new_name} = 
        match dimension_editing with
        | Left a -> a.add decision_matrix ~with_name: new_name 
        | Right a -> a.add decision_matrix ~with_name: new_name 

    let validate decision_matrix adding_info = error_message (commit decision_matrix adding_info) 
end

module ChangingNameInfo = struct
    type t = {
        dimension_item_to_edit: DimensionItemToEdit.t;
        new_name: string;
    }

    let make dimension_item_to_edit = { dimension_item_to_edit; new_name = "" }

    let commit decision_matrix {dimension_item_to_edit; new_name} = 
        match dimension_item_to_edit with
        | Left (editing, item) -> editing.change_name decision_matrix ~old_name: item ~new_name: new_name
        | Right (editing, item) -> editing.change_name decision_matrix ~old_name: item ~new_name: new_name

    let validate decision_matrix changing_name = error_message (commit decision_matrix changing_name)
end

module MovingInfo = struct
    type t = DimensionItemToEdit.t

    let make dimension_item_to_edit = dimension_item_to_edit

    let commit_alternative decision_matrix (dimension_item_to_edit: DimensionItemToEdit.t) (to_after: DecisionMatrix.alternative_name) = 
        match dimension_item_to_edit with
        | Left (_, _) -> Tea.Result.Error "Moving a factor but attempting to move after an alternative"
        | Right (editing, item) -> editing.move decision_matrix ~moving: item ~to_after: to_after

    let commit_factor decision_matrix (dimension_item_to_edit: DimensionItemToEdit.t) (to_after: DecisionMatrix.factor_name) = 
        match dimension_item_to_edit with
        | Left (editing, item) -> editing.move decision_matrix ~moving: item ~to_after: to_after
        | Right (_, _) -> Tea.Result.Error "Moving an alternative but attempting to move after a factor"

    let validate_alternative decision_matrix moving to_after =  error_message (commit_alternative decision_matrix moving to_after)
    let validate_factor decision_matrix moving to_after =  error_message (commit_factor decision_matrix moving to_after)
end

module Model = struct

    type interaction_state =
        | EditingCellValues 
        | SelectingItemTo of element_action 
        | MovingItem of MovingInfo.t 
        | Adding of AddingInfo.t
        | ChangingName of ChangingNameInfo.t
        | ShowingDialog of Message.t Dialog.t

    type t = {
        decision_matrix: DecisionMatrix.t;        
        interaction_state: interaction_state;    
        error_message: string option;
    }

    let default_interaction_state = EditingCellValues

    let url_of_string = JsInterop.btoa

    let string_of_url = JsInterop.atob

    let url_string_of_decision_matrix decision_matrix =
        Printf.sprintf "#/matrix/%s" (url_of_string (DecisionMatrix.encode decision_matrix))

    let decision_matrix_from_location location =
        match Js.String.split "/" location.Web.Location.hash with
        | [|"#"; "matrix"; matrix_url|] -> 
            DecisionMatrix.decode (string_of_url matrix_url)
        | _ -> Tea.Result.Error "Unable to match pattern"

    let init location =
        match decision_matrix_from_location location with
        | Tea.Result.Ok decision_matrix ->
            Js.Console.log "loaded decision matrix from location";
            ({ decision_matrix; interaction_state = default_interaction_state; error_message = None }, Tea.Cmd.NoCmd)
        | Tea.Result.Error error_message ->
            Js.Console.log (Printf.sprintf "Failed to load from location: %s. Issuing command to load from storage" error_message);
            (
                { decision_matrix = (DecisionMatrix.empty ()); interaction_state = default_interaction_state; error_message = None }, 
                Tea_task.attempt 
                    (fun load_result -> 
                        match load_result with 
                        | Ok loaded -> 
                            if (Js.eqNull loaded Js.null)
                            then Message.Reset
                            else Message.LoadedFromLocalStorage loaded
                        
                        | Error msg ->
                            Message.ErrorLoadingFromLocalStorage msg
                    ) 
                    (Tea.Ex.LocalStorage.getItem decision_matrix_key) 
            )

    let commit decision_matrix ~initial_interaction ~successful_interaction = function
        | Tea.Result.Ok factory -> 
            { interaction_state = successful_interaction; error_message = None; decision_matrix = factory () }
        | Tea.Result.Error error_message ->
            { decision_matrix; interaction_state = initial_interaction; error_message = Some error_message; }
end

let save_when_no_error (model: Model.t) = 
    match model.error_message with
    | Some _ ->
        (model, Tea.Cmd.NoCmd)
    | None ->
        let serialized = DecisionMatrix.encode model.decision_matrix in
        (model, Tea.Ex.LocalStorage.setItemCmd decision_matrix_key serialized)

let update ({ interaction_state; decision_matrix; error_message }: Model.t) = let open Model in function 
    | Message.LocationChanged location -> 
        Model.init location
    | Message.SaveToUrl -> 
        (match error_message with
        | None -> 
            (
                { interaction_state; decision_matrix; error_message }
                , Tea.Navigation.newUrl (url_string_of_decision_matrix decision_matrix)
            )
        | Some err -> 
            save_when_no_error 
                { interaction_state
                ; decision_matrix
                ; error_message = Some (Printf.sprintf "Cannot save to url when there is an error: %s" err)
                }
        )
    | Message.Cancel -> 
        save_when_no_error { decision_matrix; error_message = None; interaction_state = Model.default_interaction_state }
    | Message.SelectItemTo element_action ->
        save_when_no_error { decision_matrix; error_message = None; interaction_state = Model.SelectingItemTo element_action }
    | Message.StartAdding dimension_editing ->
        let new_adding = AddingInfo.make dimension_editing in
        save_when_no_error
            { decision_matrix
            ; interaction_state = Model.Adding new_adding
            ; error_message = AddingInfo.validate decision_matrix new_adding
            }  
    | StartChangingName dimension_item_to_edit ->
        let changing_name_info = ChangingNameInfo.make dimension_item_to_edit in
        save_when_no_error
            { decision_matrix
            ; interaction_state = Model.ChangingName changing_name_info
            ; error_message = ChangingNameInfo.validate decision_matrix changing_name_info
            }
    | StartMoving dimension_item_to_edit ->
        save_when_no_error
            { decision_matrix
            ; interaction_state = Model.MovingItem dimension_item_to_edit
            ; error_message = None
            }
    | MoveToAfter to_after -> 
        save_when_no_error
            (match interaction_state with
            | MovingItem moving ->
                (match to_after with
                | FactorName after ->         
                    Model.commit 
                        decision_matrix 
                        ~initial_interaction: interaction_state 
                        ~successful_interaction: (Model.SelectingItemTo Move)
                        (MovingInfo.commit_factor decision_matrix moving after)
                | AlternativeName after ->         
                    Model.commit 
                        decision_matrix 
                        ~initial_interaction: interaction_state 
                        ~successful_interaction: (Model.SelectingItemTo Move)
                        (MovingInfo.commit_alternative decision_matrix moving after)
                )
            | _ -> { interaction_state; decision_matrix; error_message = Some "Expecting to be moving."}
            )                   
    | UpdateName new_name -> 
        save_when_no_error
            (match interaction_state with
            | Model.Adding adding -> 
                let new_adding = { adding with new_name } in
                { 
                    decision_matrix;
                    interaction_state = Model.Adding new_adding;
                    error_message = AddingInfo.validate decision_matrix new_adding;
                }                
            | Model.ChangingName changing -> 
                let new_changing_name = { changing with new_name } in
                { 
                    decision_matrix;
                    interaction_state = Model.ChangingName new_changing_name;
                    error_message = ChangingNameInfo.validate decision_matrix new_changing_name
                }                
            | _ -> { interaction_state; decision_matrix; error_message = Some "Expecting to be in some mode that supports updating names."}
            )
    | Commit -> 
        save_when_no_error
            (match interaction_state with
            | Model.Adding adding -> (
                Model.commit 
                    decision_matrix 
                    ~initial_interaction: interaction_state 
                    ~successful_interaction: Model.EditingCellValues 
                    (AddingInfo.commit decision_matrix adding)
                
            )
            | Model.ChangingName changing -> (
                Model.commit 
                    decision_matrix 
                    ~initial_interaction: interaction_state 
                    ~successful_interaction: Model.EditingCellValues 
                    (ChangingNameInfo.commit decision_matrix changing)
                
            )
            | _ -> { interaction_state; decision_matrix; error_message = Some "Cannot commit adding an alternative unless in Adding Alternative mode."}
            )
    | Reset ->
        save_when_no_error
            {
                decision_matrix = (DecisionMatrix.empty ()); 
                error_message = None;
                interaction_state = default_interaction_state
            }
    | RequestReset ->
        save_when_no_error
            {
                decision_matrix; 
                error_message = None;
                interaction_state = Model.ShowingDialog 
                    {
                        Dialog.title = "Confirm Reset Matrix"
                        ; message = "Are you sure you wish to reset back to the empty decision matrix?" 
                        ; choices = 
                            [
                                ("Yes", Message.Reset)
                                ; ("No", Message.Cancel)
                            ]
                    }
            }    | RequestRemove dimension_item_to_edit ->
        save_when_no_error
            { 
                decision_matrix; 
                error_message = None;
                interaction_state = Model.ShowingDialog 
                    {
                        Dialog.title = "Confirm Delete"
                        ; message = (Printf.sprintf "Are you sure you wish to delete %s '%s'?" 
                            (DimensionItemToEdit.label dimension_item_to_edit) 
                            (DimensionItemToEdit.name dimension_item_to_edit)
                        )
                        ; choices = 
                            [
                                ("Yes", Message.Remove dimension_item_to_edit)
                                ; ("No", Message.Cancel)
                            ]
                    }
            }
    | Remove dimension_item_to_edit ->
        save_when_no_error (
            Model.commit
                decision_matrix
                ~initial_interaction: interaction_state 
                ~successful_interaction: Model.EditingCellValues 
                (DimensionItemToEdit.remove dimension_item_to_edit decision_matrix)
        )          
    | UpdateCellValue (alternative, factor, value) ->
        save_when_no_error (
            Model.commit
                decision_matrix
                ~initial_interaction: interaction_state
                ~successful_interaction: interaction_state
                (DecisionMatrix.update_cell decision_matrix factor alternative value)
        )
    | ErrorLoadingFromLocalStorage err ->
        save_when_no_error 
            { interaction_state
            ; decision_matrix
            ; error_message = Some (Printf.sprintf "Error loading from local storage: %s" err)
            }

    | LoadedFromLocalStorage loaded ->
        Js.Console.log "H";
        Js.Console.log loaded;
        Js.Console.log "H";
        match (DecisionMatrix.decode loaded) with
        | Ok decision_matrix ->
            save_when_no_error 
                { interaction_state = default_interaction_state
                ; decision_matrix
                ; error_message 
                }
        | Error err ->
            save_when_no_error 
                { interaction_state
                ; decision_matrix
                ; error_message = Some (Printf.sprintf "Error loading from local storage: %s" err)
                }            
    
let init () location = Model.init location