// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Printf from "bs-platform/lib/es6/printf.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";
import * as DecisionMatrix from "./decisionMatrix.bs.js";
import * as Tea_navigation from "bucklescript-tea/src-ocaml/tea_navigation.js";

function error_message(param) {
  if (param.tag) {
    return Caml_option.some(param[0]);
  }
  
}

var factor = /* Left */Block.__(0, [DecisionMatrix.DecisionMatrix.factor_editing]);

var alternative = /* Right */Block.__(1, [DecisionMatrix.DecisionMatrix.alternative_editing]);

function label(param) {
  return param[0][/* label */0];
}

var DimensionToEdit = {
  factor: factor,
  alternative: alternative,
  label: label
};

function label$1(param) {
  return param[0][0][/* label */0];
}

function name(param) {
  if (param.tag) {
    return Curry._1(DecisionMatrix.DecisionMatrix.string_of_alternative_name, param[0][1]);
  } else {
    return Curry._1(DecisionMatrix.DecisionMatrix.string_of_factor_name, param[0][1]);
  }
}

function make_factor(name) {
  return /* Left */Block.__(0, [/* tuple */[
              DecisionMatrix.DecisionMatrix.factor_editing,
              name
            ]]);
}

function make_alternative(name) {
  return /* Right */Block.__(1, [/* tuple */[
              DecisionMatrix.DecisionMatrix.alternative_editing,
              name
            ]]);
}

function remove(edit) {
  if (edit.tag) {
    var match = edit[0];
    var name = match[1];
    var func = match[0][/* remove */2];
    return (function (param) {
        return Curry._2(func, param, name);
      });
  } else {
    var match$1 = edit[0];
    var name$1 = match$1[1];
    var func$1 = match$1[0][/* remove */2];
    return (function (param) {
        return Curry._2(func$1, param, name$1);
      });
  }
}

var DimensionItemToEdit = {
  label: label$1,
  name: name,
  make_factor: make_factor,
  make_alternative: make_alternative,
  remove: remove
};

var Message = { };

function make(dimension_editing) {
  return /* record */[
          /* dimension_editing */dimension_editing,
          /* new_name */""
        ];
}

function commit(decision_matrix, param) {
  return Curry._2(param[/* dimension_editing */0][0][/* add */1], decision_matrix, param[/* new_name */1]);
}

function validate(decision_matrix, adding_info) {
  return error_message(commit(decision_matrix, adding_info));
}

var AddingInfo = {
  make: make,
  commit: commit,
  validate: validate
};

function make$1(dimension_item_to_edit) {
  return /* record */[
          /* dimension_item_to_edit */dimension_item_to_edit,
          /* new_name */""
        ];
}

function commit$1(decision_matrix, param) {
  var match = param[/* dimension_item_to_edit */0][0];
  return Curry._3(match[0][/* change_name */3], decision_matrix, match[1], param[/* new_name */1]);
}

function validate$1(decision_matrix, changing_name) {
  return error_message(commit$1(decision_matrix, changing_name));
}

var ChangingNameInfo = {
  make: make$1,
  commit: commit$1,
  validate: validate$1
};

function make$2(dimension_item_to_edit) {
  return dimension_item_to_edit;
}

function commit_alternative(decision_matrix, dimension_item_to_edit, to_after) {
  if (dimension_item_to_edit.tag) {
    var match = dimension_item_to_edit[0];
    return Curry._3(match[0][/* move */4], decision_matrix, match[1], to_after);
  } else {
    return /* Error */Block.__(1, ["Moving a factor but attempting to move after an alternative"]);
  }
}

function commit_factor(decision_matrix, dimension_item_to_edit, to_after) {
  if (dimension_item_to_edit.tag) {
    return /* Error */Block.__(1, ["Moving an alternative but attempting to move after a factor"]);
  } else {
    var match = dimension_item_to_edit[0];
    return Curry._3(match[0][/* move */4], decision_matrix, match[1], to_after);
  }
}

function validate_alternative(decision_matrix, moving, to_after) {
  return error_message(commit_alternative(decision_matrix, moving, to_after));
}

function validate_factor(decision_matrix, moving, to_after) {
  return error_message(commit_factor(decision_matrix, moving, to_after));
}

var MovingInfo = {
  make: make$2,
  commit_alternative: commit_alternative,
  commit_factor: commit_factor,
  validate_alternative: validate_alternative,
  validate_factor: validate_factor
};

function url_of_string(prim) {
  return btoa(prim);
}

function string_of_url(prim) {
  return atob(prim);
}

function url_string_of_decision_matrix(decision_matrix) {
  return Curry._1(Printf.sprintf(/* Format */[
                  /* String_literal */Block.__(11, [
                      "#/matrix/",
                      /* String */Block.__(2, [
                          /* No_padding */0,
                          /* End_of_format */0
                        ])
                    ]),
                  "#/matrix/%s"
                ]), btoa(DecisionMatrix.DecisionMatrix.encode(decision_matrix)));
}

function decision_matrix_from_location($$location) {
  console.log($$location[/* hash */7]);
  var match = $$location[/* hash */7].split("/");
  if (match.length !== 3) {
    return /* Ok */Block.__(0, [Curry._1(DecisionMatrix.DecisionMatrix.empty, /* () */0)]);
  } else {
    var match$1 = match[0];
    if (match$1 === "#") {
      var match$2 = match[1];
      if (match$2 === "matrix") {
        var matrix_url = match[2];
        console.log(atob(matrix_url));
        return DecisionMatrix.DecisionMatrix.decode(atob(matrix_url));
      } else {
        return /* Ok */Block.__(0, [Curry._1(DecisionMatrix.DecisionMatrix.empty, /* () */0)]);
      }
    } else {
      return /* Ok */Block.__(0, [Curry._1(DecisionMatrix.DecisionMatrix.empty, /* () */0)]);
    }
  }
}

function init($$location) {
  var match = decision_matrix_from_location($$location);
  if (match.tag) {
    return /* record */[
            /* decision_matrix */Curry._1(DecisionMatrix.DecisionMatrix.empty, /* () */0),
            /* interaction_state : EditingCellValues */0,
            /* error_message */match[0]
          ];
  } else {
    return /* record */[
            /* decision_matrix */match[0],
            /* interaction_state : EditingCellValues */0,
            /* error_message */undefined
          ];
  }
}

function commit$2(decision_matrix, initial_interaction, successful_interaction, param) {
  if (param.tag) {
    return /* record */[
            /* decision_matrix */decision_matrix,
            /* interaction_state */initial_interaction,
            /* error_message */param[0]
          ];
  } else {
    return /* record */[
            /* decision_matrix */Curry._1(param[0], /* () */0),
            /* interaction_state */successful_interaction,
            /* error_message */undefined
          ];
  }
}

var Model = {
  default_interaction_state: /* EditingCellValues */0,
  url_of_string: url_of_string,
  string_of_url: string_of_url,
  url_string_of_decision_matrix: url_string_of_decision_matrix,
  decision_matrix_from_location: decision_matrix_from_location,
  init: init,
  commit: commit$2
};

function no_command(model) {
  return /* tuple */[
          model,
          /* NoCmd */0
        ];
}

function update(param, param$1) {
  var error_message$1 = param[/* error_message */2];
  var interaction_state = param[/* interaction_state */1];
  var decision_matrix = param[/* decision_matrix */0];
  if (typeof param$1 === "number") {
    switch (param$1) {
      case /* Cancel */0 :
          return /* tuple */[
                  /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : EditingCellValues */0,
                    /* error_message */undefined
                  ],
                  /* NoCmd */0
                ];
      case /* Commit */1 :
          var tmp;
          if (typeof interaction_state === "number") {
            tmp = /* record */[
              /* decision_matrix */decision_matrix,
              /* interaction_state */interaction_state,
              /* error_message */"Cannot commit adding an alternative unless in Adding Alternative mode."
            ];
          } else {
            switch (interaction_state.tag | 0) {
              case /* Adding */2 :
                  tmp = commit$2(decision_matrix, interaction_state, /* EditingCellValues */0, commit(decision_matrix, interaction_state[0]));
                  break;
              case /* ChangingName */3 :
                  tmp = commit$2(decision_matrix, interaction_state, /* EditingCellValues */0, commit$1(decision_matrix, interaction_state[0]));
                  break;
              default:
                tmp = /* record */[
                  /* decision_matrix */decision_matrix,
                  /* interaction_state */interaction_state,
                  /* error_message */"Cannot commit adding an alternative unless in Adding Alternative mode."
                ];
            }
          }
          return /* tuple */[
                  tmp,
                  /* NoCmd */0
                ];
      case /* RequestReset */2 :
          return /* tuple */[
                  /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : ShowingDialog */Block.__(4, [/* record */[
                          /* title */"Confirm Reset Matrix",
                          /* message */"Are you sure you wish to reset back to the empty decision matrix?",
                          /* choices : :: */[
                            /* tuple */[
                              "Yes",
                              /* Reset */3
                            ],
                            /* :: */[
                              /* tuple */[
                                "No",
                                /* Cancel */0
                              ],
                              /* [] */0
                            ]
                          ]
                        ]]),
                    /* error_message */undefined
                  ],
                  /* NoCmd */0
                ];
      case /* Reset */3 :
          return /* tuple */[
                  /* record */[
                    /* decision_matrix */Curry._1(DecisionMatrix.DecisionMatrix.empty, /* () */0),
                    /* interaction_state : EditingCellValues */0,
                    /* error_message */undefined
                  ],
                  /* NoCmd */0
                ];
      case /* SaveToUrl */4 :
          if (error_message$1 !== undefined) {
            return /* tuple */[
                    /* record */[
                      /* decision_matrix */decision_matrix,
                      /* interaction_state */interaction_state,
                      /* error_message */Curry._1(Printf.sprintf(/* Format */[
                                /* String_literal */Block.__(11, [
                                    "Cannot save to url when there is an error: ",
                                    /* String */Block.__(2, [
                                        /* No_padding */0,
                                        /* End_of_format */0
                                      ])
                                  ]),
                                "Cannot save to url when there is an error: %s"
                              ]), error_message$1)
                    ],
                    /* NoCmd */0
                  ];
          } else {
            return /* tuple */[
                    /* record */[
                      /* decision_matrix */decision_matrix,
                      /* interaction_state */interaction_state,
                      /* error_message */error_message$1
                    ],
                    Tea_navigation.newUrl(url_string_of_decision_matrix(decision_matrix))
                  ];
          }
      
    }
  } else {
    switch (param$1.tag | 0) {
      case /* SelectItemTo */0 :
          return /* tuple */[
                  /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : SelectingItemTo */Block.__(0, [param$1[0]]),
                    /* error_message */undefined
                  ],
                  /* NoCmd */0
                ];
      case /* StartAdding */1 :
          var new_adding_000 = /* dimension_editing */param$1[0];
          var new_adding = /* record */[
            new_adding_000,
            /* new_name */""
          ];
          return /* tuple */[
                  /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : Adding */Block.__(2, [new_adding]),
                    /* error_message */error_message(commit(decision_matrix, new_adding))
                  ],
                  /* NoCmd */0
                ];
      case /* StartChangingName */2 :
          var changing_name_info_000 = /* dimension_item_to_edit */param$1[0];
          var changing_name_info = /* record */[
            changing_name_info_000,
            /* new_name */""
          ];
          return /* tuple */[
                  /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : ChangingName */Block.__(3, [changing_name_info]),
                    /* error_message */error_message(commit$1(decision_matrix, changing_name_info))
                  ],
                  /* NoCmd */0
                ];
      case /* UpdateName */3 :
          var new_name = param$1[0];
          var tmp$1;
          if (typeof interaction_state === "number") {
            tmp$1 = /* record */[
              /* decision_matrix */decision_matrix,
              /* interaction_state */interaction_state,
              /* error_message */"Expecting to be in some mode that supports updating names."
            ];
          } else {
            switch (interaction_state.tag | 0) {
              case /* Adding */2 :
                  var new_adding_000$1 = /* dimension_editing */interaction_state[0][/* dimension_editing */0];
                  var new_adding$1 = /* record */[
                    new_adding_000$1,
                    /* new_name */new_name
                  ];
                  tmp$1 = /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : Adding */Block.__(2, [new_adding$1]),
                    /* error_message */error_message(commit(decision_matrix, new_adding$1))
                  ];
                  break;
              case /* ChangingName */3 :
                  var new_changing_name_000 = /* dimension_item_to_edit */interaction_state[0][/* dimension_item_to_edit */0];
                  var new_changing_name = /* record */[
                    new_changing_name_000,
                    /* new_name */new_name
                  ];
                  tmp$1 = /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : ChangingName */Block.__(3, [new_changing_name]),
                    /* error_message */error_message(commit$1(decision_matrix, new_changing_name))
                  ];
                  break;
              default:
                tmp$1 = /* record */[
                  /* decision_matrix */decision_matrix,
                  /* interaction_state */interaction_state,
                  /* error_message */"Expecting to be in some mode that supports updating names."
                ];
            }
          }
          return /* tuple */[
                  tmp$1,
                  /* NoCmd */0
                ];
      case /* StartMoving */4 :
          return /* tuple */[
                  /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : MovingItem */Block.__(1, [param$1[0]]),
                    /* error_message */undefined
                  ],
                  /* NoCmd */0
                ];
      case /* MoveToAfter */5 :
          var to_after = param$1[0];
          var tmp$2;
          if (typeof interaction_state === "number" || interaction_state.tag !== /* MovingItem */1) {
            tmp$2 = /* record */[
              /* decision_matrix */decision_matrix,
              /* interaction_state */interaction_state,
              /* error_message */"Expecting to be moving."
            ];
          } else {
            var moving = interaction_state[0];
            tmp$2 = to_after.tag ? commit$2(decision_matrix, interaction_state, /* SelectingItemTo */Block.__(0, [/* Move */0]), commit_alternative(decision_matrix, moving, to_after[0])) : commit$2(decision_matrix, interaction_state, /* SelectingItemTo */Block.__(0, [/* Move */0]), commit_factor(decision_matrix, moving, to_after[0]));
          }
          return /* tuple */[
                  tmp$2,
                  /* NoCmd */0
                ];
      case /* RequestRemove */6 :
          var dimension_item_to_edit = param$1[0];
          return /* tuple */[
                  /* record */[
                    /* decision_matrix */decision_matrix,
                    /* interaction_state : ShowingDialog */Block.__(4, [/* record */[
                          /* title */"Confirm Delete",
                          /* message */Curry._2(Printf.sprintf(/* Format */[
                                    /* String_literal */Block.__(11, [
                                        "Are you sure you wish to delete ",
                                        /* String */Block.__(2, [
                                            /* No_padding */0,
                                            /* String_literal */Block.__(11, [
                                                " '",
                                                /* String */Block.__(2, [
                                                    /* No_padding */0,
                                                    /* String_literal */Block.__(11, [
                                                        "'?",
                                                        /* End_of_format */0
                                                      ])
                                                  ])
                                              ])
                                          ])
                                      ]),
                                    "Are you sure you wish to delete %s '%s'?"
                                  ]), label$1(dimension_item_to_edit), name(dimension_item_to_edit)),
                          /* choices : :: */[
                            /* tuple */[
                              "Yes",
                              /* Remove */Block.__(7, [dimension_item_to_edit])
                            ],
                            /* :: */[
                              /* tuple */[
                                "No",
                                /* Cancel */0
                              ],
                              /* [] */0
                            ]
                          ]
                        ]]),
                    /* error_message */undefined
                  ],
                  /* NoCmd */0
                ];
      case /* Remove */7 :
          return /* tuple */[
                  commit$2(decision_matrix, interaction_state, /* EditingCellValues */0, remove(param$1[0])(decision_matrix)),
                  /* NoCmd */0
                ];
      case /* UpdateCellValue */8 :
          return /* tuple */[
                  commit$2(decision_matrix, interaction_state, interaction_state, DecisionMatrix.DecisionMatrix.update_cell(decision_matrix, param$1[1], param$1[0], param$1[2])),
                  /* NoCmd */0
                ];
      case /* LocationChanged */9 :
          return /* tuple */[
                  init(param$1[0]),
                  /* NoCmd */0
                ];
      
    }
  }
}

function init$1(param, $$location) {
  return /* tuple */[
          init($$location),
          /* NoCmd */0
        ];
}

export {
  error_message ,
  DimensionToEdit ,
  DimensionItemToEdit ,
  Message ,
  AddingInfo ,
  ChangingNameInfo ,
  MovingInfo ,
  Model ,
  no_command ,
  update ,
  init$1 as init,
  
}
/* DecisionMatrix Not a pure module */