// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Utils from "./utils.bs.js";
import * as Printf from "bs-platform/lib/es6/printf.js";
import * as Js_dict from "bs-platform/lib/es6/js_dict.js";
import * as Tea_json from "bucklescript-tea/src-ocaml/tea_json.js";
import * as Belt_List from "bs-platform/lib/es6/belt_List.js";
import * as Tea_result from "bucklescript-tea/src-ocaml/tea_result.js";
import * as Caml_option from "bs-platform/lib/es6/caml_option.js";

function MakeTable(CellValue) {
  var string_of_row_name = function (s) {
    return s;
  };
  var string_of_column_name = function (s) {
    return s;
  };
  var empty = function (param) {
    return /* record */[
            /* columns : [] */0,
            /* rows : [] */0
          ];
  };
  var column_names = function (param) {
    return List.rev(param[/* columns */0]);
  };
  var rows = function (param) {
    return List.rev(param[/* rows */1]);
  };
  var row_name = function (param) {
    return param[/* name */0];
  };
  var row_name_string = function (param) {
    return param[/* name */0];
  };
  var row_cells = function (param) {
    return List.rev(param[/* cell_values */1]);
  };
  var row_name_strings = function (table) {
    return List.map((function (param) {
                  return param[/* name */0];
                }), table[/* rows */1]);
  };
  var make_dimension_editing = function (string_of_name, get_names, add_to_table, remove_index_from_table, move_by_index, change_name_at_index) {
    var add = function (table, with_name) {
      return Utils.ResultEx.map((function (verified_name, param) {
                    return Curry._2(add_to_table, table, verified_name);
                  }), Utils.Names.verify_new_name(with_name, Curry._1(get_names, table)));
    };
    var remove = function (table, with_name) {
      return Utils.ResultEx.map((function (index_found, param) {
                    return Curry._2(remove_index_from_table, table, index_found);
                  }), Utils.Names.find_name_index(Curry._1(string_of_name, with_name), Curry._1(get_names, table)));
    };
    var change_name = function (table, old_name, new_name) {
      if (Curry._1(string_of_name, old_name) === new_name) {
        return /* Ok */Block.__(0, [(function (param) {
                      return table;
                    })]);
      } else {
        return Utils.ResultEx.map2((function (verified_name, index, param) {
                      return Curry._3(change_name_at_index, index, verified_name, table);
                    }), Utils.Names.verify_new_name(new_name, Curry._1(get_names, table)), Utils.Names.find_name_index(Curry._1(string_of_name, old_name), Curry._1(get_names, table)));
      }
    };
    var move = function (table, moving, to_after) {
      return Utils.ResultEx.map2((function (moving_index, before_index, param) {
                    return Curry._3(move_by_index, moving_index, before_index, table);
                  }), Utils.Names.find_name_index(Curry._1(string_of_name, moving), Curry._1(get_names, table)), Utils.Names.find_name_index(Curry._1(string_of_name, to_after), Curry._1(get_names, table)));
    };
    return /* record */[
            /* add */add,
            /* remove */remove,
            /* change_name */change_name,
            /* move */move
          ];
  };
  var add_default_cell = function (row) {
    return /* record */[
            /* name */row[/* name */0],
            /* cell_values : :: */[
              Curry._1(CellValue.$$default, /* () */0),
              row[/* cell_values */1]
            ]
          ];
  };
  var column_editing = make_dimension_editing(string_of_column_name, (function (param) {
          return param[/* columns */0];
        }), (function (table, verified_name) {
          return /* record */[
                  /* columns : :: */[
                    verified_name,
                    table[/* columns */0]
                  ],
                  /* rows */List.map(add_default_cell, table[/* rows */1])
                ];
        }), (function (table, index_found) {
          return /* record */[
                  /* columns */Utils.ListEx.remove_index(index_found, table[/* columns */0]),
                  /* rows */List.map((function (param) {
                          var index_to_remove = index_found;
                          var row = param;
                          return /* record */[
                                  /* name */row[/* name */0],
                                  /* cell_values */Utils.ListEx.remove_index(index_to_remove, row[/* cell_values */1])
                                ];
                        }), table[/* rows */1])
                ];
        }), (function (from_index, to_before_index, table) {
          return /* record */[
                  /* columns */Utils.ListEx.move(from_index, to_before_index, table[/* columns */0]),
                  /* rows */List.map((function (param) {
                          var from_index$1 = from_index;
                          var to_before_index$1 = to_before_index;
                          var row = param;
                          return /* record */[
                                  /* name */row[/* name */0],
                                  /* cell_values */Utils.ListEx.move(from_index$1, to_before_index$1, row[/* cell_values */1])
                                ];
                        }), table[/* rows */1])
                ];
        }), (function (index, new_name, table) {
          return /* record */[
                  /* columns */List.mapi((function (cur_index, column_name) {
                          if (cur_index === index) {
                            return new_name;
                          } else {
                            return column_name;
                          }
                        }), table[/* columns */0]),
                  /* rows */table[/* rows */1]
                ];
        }));
  var row_editing = make_dimension_editing(string_of_row_name, row_name_strings, (function (table, verified_name) {
          var new_row_001 = /* cell_values */List.map((function (param) {
                  return Curry._1(CellValue.$$default, /* () */0);
                }), table[/* columns */0]);
          var new_row = /* record */[
            /* name */verified_name,
            new_row_001
          ];
          return /* record */[
                  /* columns */table[/* columns */0],
                  /* rows : :: */[
                    new_row,
                    table[/* rows */1]
                  ]
                ];
        }), (function (table, index_found) {
          return /* record */[
                  /* columns */table[/* columns */0],
                  /* rows */Utils.ListEx.remove_index(index_found, table[/* rows */1])
                ];
        }), (function (from_index, to_before_index, table) {
          return /* record */[
                  /* columns */table[/* columns */0],
                  /* rows */Utils.ListEx.move(from_index, to_before_index, table[/* rows */1])
                ];
        }), (function (index, new_name, table) {
          return /* record */[
                  /* columns */table[/* columns */0],
                  /* rows */List.mapi((function (idx, row) {
                          if (idx === index) {
                            return /* record */[
                                    /* name */new_name,
                                    /* cell_values */row[/* cell_values */1]
                                  ];
                          } else {
                            return row;
                          }
                        }), table[/* rows */1])
                ];
        }));
  var update_cell = function (table, column_name, row_name, new_value) {
    return Utils.ResultEx.map2((function (column_index, row_index, param) {
                  return /* record */[
                          /* columns */table[/* columns */0],
                          /* rows */List.mapi((function (index, row) {
                                  if (index !== row_index) {
                                    return row;
                                  } else {
                                    return /* record */[
                                            /* name */row[/* name */0],
                                            /* cell_values */List.mapi((function (index, cell_value) {
                                                    if (index !== column_index) {
                                                      return cell_value;
                                                    } else {
                                                      return new_value;
                                                    }
                                                  }), row[/* cell_values */1])
                                          ];
                                  }
                                }), table[/* rows */1])
                        ];
                }), Utils.Names.find_name_index(column_name, table[/* columns */0]), Utils.Names.find_name_index(row_name, row_name_strings(table)));
  };
  var decode = function (s) {
    var string_list_decoder = Tea_json.Decoder.map(List.rev, Tea_json.Decoder.list(Tea_json.Decoder.string));
    var values_decoder = Tea_json.Decoder.map(List.rev, Tea_json.Decoder.list(string_list_decoder));
    var record_decoder = Tea_json.Decoder.map3((function (columns, rows, values) {
            return /* tuple */[
                    columns,
                    rows,
                    values
                  ];
          }), Tea_json.Decoder.field("columns", string_list_decoder), Tea_json.Decoder.field("rows", string_list_decoder), Tea_json.Decoder.field("values", values_decoder));
    return Utils.ResultEx.flatMap((function (param) {
                  var columns = param[0];
                  return Utils.ResultEx.map((function (rows) {
                                return /* record */[
                                        /* columns */columns,
                                        /* rows */rows
                                      ];
                              }), Utils.ListEx.map_while_ok((function (param) {
                                    var name = param[0];
                                    return Utils.ResultEx.map((function (cell_values) {
                                                  return /* record */[
                                                          /* name */name,
                                                          /* cell_values */cell_values
                                                        ];
                                                }), Tea_result.accumulate(List.map((function (cell_value_string) {
                                                          var match = Curry._1(CellValue.of_string, cell_value_string);
                                                          if (match !== undefined) {
                                                            return /* Ok */Block.__(0, [Caml_option.valFromOption(match)]);
                                                          } else {
                                                            return /* Error */Block.__(1, [Curry._2(Printf.sprintf(/* Format */[
                                                                                /* String_literal */Block.__(11, [
                                                                                    "could not parse '",
                                                                                    /* String */Block.__(2, [
                                                                                        /* No_padding */0,
                                                                                        /* String_literal */Block.__(11, [
                                                                                            "' for row '",
                                                                                            /* String */Block.__(2, [
                                                                                                /* No_padding */0,
                                                                                                /* Char_literal */Block.__(12, [
                                                                                                    /* "'" */39,
                                                                                                    /* End_of_format */0
                                                                                                  ])
                                                                                              ])
                                                                                          ])
                                                                                      ])
                                                                                  ]),
                                                                                "could not parse '%s' for row '%s'"
                                                                              ]), cell_value_string, name)]);
                                                          }
                                                        }), param[1])));
                                  }), Belt_List.zip(param[1], param[2])));
                }), Tea_json.Decoder.decodeString(record_decoder, s));
  };
  var encode = function (param) {
    var rows = param[/* rows */1];
    return JSON.stringify(Js_dict.fromList(/* :: */[
                    /* tuple */[
                      "columns",
                      $$Array.of_list(List.rev(param[/* columns */0]))
                    ],
                    /* :: */[
                      /* tuple */[
                        "rows",
                        $$Array.map((function (param) {
                                return param[/* name */0];
                              }), $$Array.of_list(List.rev(rows)))
                      ],
                      /* :: */[
                        /* tuple */[
                          "values",
                          $$Array.of_list(List.map((function (param) {
                                      return $$Array.of_list(List.map(CellValue.to_string, List.rev(param[/* cell_values */1])));
                                    }), List.rev(rows)))
                        ],
                        /* [] */0
                      ]
                    ]
                  ]));
  };
  var update_cell_from_string = function (table, column_name, row_name, new_value) {
    var match = Curry._1(CellValue.of_string, new_value);
    if (match !== undefined) {
      return update_cell(table, column_name, row_name, Caml_option.valFromOption(match));
    } else {
      return /* Error */Block.__(1, [Curry._3(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "failed to update column ",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* String_literal */Block.__(11, [
                                      ", row ",
                                      /* String */Block.__(2, [
                                          /* No_padding */0,
                                          /* String_literal */Block.__(11, [
                                              " - '",
                                              /* String */Block.__(2, [
                                                  /* No_padding */0,
                                                  /* String_literal */Block.__(11, [
                                                      "' is not a valid value",
                                                      /* End_of_format */0
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ]),
                          "failed to update column %s, row %s - '%s' is not a valid value"
                        ]), column_name, row_name, new_value)]);
    }
  };
  return {
          empty: empty,
          string_of_row_name: string_of_row_name,
          string_of_column_name: string_of_column_name,
          column_names: column_names,
          rows: rows,
          row_name: row_name,
          row_name_string: row_name_string,
          row_cells: row_cells,
          column_editing: column_editing,
          row_editing: row_editing,
          update_cell: update_cell,
          update_cell_from_string: update_cell_from_string,
          decode: decode,
          encode: encode
        };
}

export {
  MakeTable ,
  
}
/* No side effect */