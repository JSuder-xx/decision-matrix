let blue_color = "#48a9dc"

let gray_color = "#666"

let red_color = "#d44"

let error_class = "error"

let top_controls_class = "top-controls"

let cell_value_class = "cell-value"

let disabled_class = "disabled"

let delete_class = "delete"

let button_link_class = "button-link"

let move_link_class = "move-link"

let dialog_class = "dialog"

let highlight_class = "highlight"

let score_class = "score"

let weight_cell_class = "weight-cell"

let style = {j|
    body {
        background-color: #eee;
        display: flex;
        flex-direction: column;
        font-family: sans-serif;
    }

    p {
        font-size: 14px;
    }

    table { 
        border-collapse: collapse; 
        border: solid 1px $gray_color;
    }   

    td {
        padding: 8px;
        border: solid 1px $gray_color;
    }

    th {
        font-size: 14px;
        padding-right: 10px;
        padding-top: 6px;
        padding-bottom: 6px;
    }

    .$highlight_class { 
        background-color: yellow;
    }

    .$score_class {
        background-color: white;
    }

    th > div {
        margin-right: 6px;
    }

    .$cell_value_class {
        width: 40px;      
        display: inline-block;
        margin-left: 0px;  
        font-size: 14px;
    }

    .$weight_cell_class {
        background-color: #ddf;
    }

    span.$cell_value_class {
        padding: 3px;
    }

    input {
        margin-left: 6px;
    }          

    button {
        background-color: white;
        color: $blue_color;
        box-shadow: 0 0 0 1px $blue_color;
        border: none;
        padding: 6px;
        margin-left: 4px;
        font-size: 14px;
    }

    button:active {
        background-color: $blue_color;
        color: white;
    }

    button:disabled {
        background-color: #ddd;
        color: #90adbd;
    }

    a, a:active, a:visited {
        display: inline-block;
        color: $blue_color
    }

    a.$button_link_class {
        font-size: 12px;
        padding-left: 6px;
        padding-right: 6px;
        padding-top: 4px;
        padding-bottom: 4px;
        margin-left: 4px;
        text-decoration: none;
    }

    a.$delete_class, a.$delete_class:visited {
        background-color: #844;
        color: white;
    }
    a.$delete_class:active {
        background-color: #faa;
        color: black;
    }

    a.$move_link_class, a.$move_link_class:visited {
        background-color: #bbb;
        color: black;
    }
    a.$move_link_class:active {
        background-color: #444;
        color: white;
    }

    div.$top_controls_class {
        height: 140px;
        padding: 8px;
        border: solid 1px #999;
        margin: 6px;
        background-color: #ddd;
        min-width: 580px;
    }

    span.$error_class {
        color: $red_color;
    }

    input.$error_class {
        border-color: $red_color;
    }

    div.$dialog_class {        
    }

    .$dialog_class h3 {
        margin-top: 0px;
        margin-bottom: 0px;
        font-size: 16px;
    }

    .$dialog_class p {
        margin-top: 8px;
        margin-bottom: 8px;
    }
|j}
