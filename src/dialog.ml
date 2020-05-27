open Tea.Html

type 'message t = {
    title: string;
    message: string;
    choices: (string * 'message) list;
}

let view dialog = 
    let view_choice (display, message) =
        button 
            [onClick message]
            [text display] in
    div [class' Styles.dialog_class]
        [ h3 [] [text dialog.title]
        ; p [] [text dialog.message]                 
        ; div [] (dialog.choices |> List.map view_choice)
        ]
