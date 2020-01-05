open DecisionMatrixEditing
open DecisionMatrixEditingView

let main =
    let program = Tea.Navigation.navigationProgram
        (fun location -> DecisionMatrixEditing.Message.LocationChanged location)
        {
            init
            ; update
            ; view
            ; shutdown = (fun _ -> Tea.Cmd.none)
            ; subscriptions = (fun _ -> Tea.Sub.none)
        } in
    (fun web_node unit ->
        let style = JsInterop.document##createElement("style") in
        (
            JsInterop.document##head##appendChild(style) |> ignore;
            style##innerHTML #= Styles.style
        ); 
        program web_node unit
    )
