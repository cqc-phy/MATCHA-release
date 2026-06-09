(* ::Package:: *)

ClearAll[
  MakeRedefinitionObject, ExtractCouplingSolutionsObject,
  ExtractDerivativeSolutionsObject, CleanSolutionRules, CloseSolutionRules,
  SymbolInContext, SymbolContext, MatchingVariableContext,
  redefinitionRulesFromProcess, derivativeRuleQ, couplingRuleQ
];

Options[MakeRedefinitionObject] = {
  SimplifyResult -> True
};



CleanSolutionRules[rules_] :=
  DeleteDuplicatesBy[
    DeleteCases[Flatten[{rules}], {} | {{}} | Rule[x_, x_] | Missing[__]],
    First
  ];

CloseSolutionRules[rules_] := Module[{clean},
  clean = CleanSolutionRules[rules];
  CleanSolutionRules[
    clean /. Rule[l_, r_] :> Rule[l, Simplify[FixedPoint[# /. clean &, r, 20]]]
  ]
];

derivativeRuleQ[Rule[lhs_, _]] := Module[{name},
  name = SymbolName[Unevaluated[lhs]];
  StringMatchQ[name, "p" ~~ DigitCharacter.. ~~ "0"]
];
derivativeRuleQ[_] := False;

couplingRuleQ[rule_Rule] := !derivativeRuleQ[rule];
couplingRuleQ[_] := False;

redefinitionRulesFromProcess[entry_Association, key_String] := Module[{direct, matching},
  direct = Lookup[entry, key, Missing["NotAvailable"]];

  If[direct =!= Missing["NotAvailable"],
    Return[direct]
  ];

  matching = CleanSolutionRules[Lookup[entry, "MatchingSolution", {}]];

  Switch[
    key,
    "CouplingSolution", Select[matching, couplingRuleQ],
    "DerivativeSolution", Select[matching, derivativeRuleQ],
    _, {}
  ]
];

ExtractCouplingSolutionsObject[obj_Association] := Module[{processes},
  processes = Values[Lookup[obj, "AllProcesses", <||>]];
  CleanSolutionRules[
    redefinitionRulesFromProcess[#, "CouplingSolution"] & /@ processes
  ]
];

ExtractDerivativeSolutionsObject[obj_Association] := Module[{processes},
  processes = Values[Lookup[obj, "AllProcesses", <||>]];
  CleanSolutionRules[
    (redefinitionRulesFromProcess[#, "DerivativeSolution"] & /@ processes) /.
      Rule[l_, r_] :> Rule[l, Simplify[r]]
  ]
];


SymbolInContext[ctx_String, name_String] := ToExpression[ctx <> name];

SymbolContext[s_Symbol] := Context[s];

MatchingVariableContext[obj_Association] := Module[{processes, vars, syms},
  processes = Values[Lookup[obj, "AllProcesses", <||>]];
  vars = Flatten[
    Lookup[#, "Variables", Lookup[#, "MatchingVariables", {}]] & /@ processes
  ];
  syms = Cases[vars, s_Symbol :> s, Infinity];

  If[syms === {},
    "Global`",
    SymbolContext[First[syms]]
  ]
];



MakeRedefinitionObject[obj_Association, opts:OptionsPattern[]] := Module[
  {
    simplifyResult, higgsOrderMax, couplingSolutions,
    derivativeSolutions, allInputSolutions, ctx, higgsf, v, mh, Mw, Mt,
    der, mu, hfield, derh, drules, xvars, pvars, dvars, avars, cvars,
    dfinalvars, afinalvars, cfinalvars, htrafo, Lnoncanonic,
    LnoncanonicRedefined, kineticEquations, xSolutions, xRules,
    VpotentialList, VpotentialRedefinedList, VpotentialOriginalList,
    dFinalSolutionsRaw, dFinalSolutions, Wbil, LWHiggsList,
    LWHiggsRedefinedList, LWHiggsOriginalList, aFinalSolutionsRaw,
    aFinalSolutions, Ybil, LYukawaList, LYukawaRedefinedList,
    LYukawaOriginalList, cFinalSolutionsRaw, cFinalSolutions,
    finalSolutions
  },


  simplifyResult = OptionValue[SimplifyResult];



  higgsOrderMax = obj["HiggsOrder"];

  couplingSolutions = ExtractCouplingSolutionsObject[obj];
  derivativeSolutions = ExtractDerivativeSolutionsObject[obj];

  allInputSolutions =
    CloseSolutionRules[
      Join[couplingSolutions, derivativeSolutions]
    ];



  ctx = MatchingVariableContext[obj];



  higgsf = SymbolInContext[ctx, "higgsf"];
  v  = Lookup[$SMParams, "SMvacuum", SymbolInContext[ctx, "v"]];
  mh = Lookup[$SMParams, "HiggsMass", SymbolInContext[ctx, "mh"]];
  Mw = Lookup[$SMParams, "WMass", SymbolInContext[ctx, "Mw"]];
  Mt = Lookup[$SMParams, "TopMass", SymbolInContext[ctx, "Mt"]];


  der = Unique["myder$"];
  mu = Unique["mu$"];
  hfield = Unique["hfield$"];
  derh = Unique["myderh$"];


  drules = {der[a_ + b_, mu] :> der[a, mu] + der[b, mu], 
  der[a_ b_, mu] :> der[a, mu] b + a der[b, mu], 
  der[c_ /; FreeQ[c, higgsf], mu] :> 0, 
  der[higgsf^n_., mu] :> n higgsf^(n - 1) der[higgsf, mu]};

  xvars[nhomax_Integer?Positive] :=
    Table[
      SymbolInContext[ctx, "x" <> ToString[vark]],
      {vark, 2, nhomax}
    ];

  pvars[nhomax_Integer?Positive] :=
    Table[
      SymbolInContext[ctx, "p" <> ToString[vark]],
      {vark, 2, nhomax}
    ];

  dvars[nhomax_Integer?Positive] :=
    Table[
      SymbolInContext[ctx, "d" <> ToString[n]],
      {n, 3, nhomax + 2}
    ];

  avars[nhomax_Integer?Positive] :=
    Table[
      SymbolInContext[ctx, "a" <> ToString[n]],
      {n, 1, nhomax}
    ];

  cvars[nhomax_Integer?Positive] :=
    Table[
      SymbolInContext[ctx, "c" <> ToString[n]],
      {n, 1, nhomax}
    ];

  dfinalvars[nhomax_Integer?Positive] :=
    Table[
      SymbolInContext[ctx, "d" <> ToString[n] <> "final"],
      {n, 3, nhomax + 2}
    ];

  afinalvars[nhomax_Integer?Positive] :=
    Table[
      SymbolInContext[ctx, "a" <> ToString[n] <> "final"],
      {n, 1, nhomax}
    ];

  cfinalvars[nhomax_Integer?Positive] :=
    Table[
      SymbolInContext[ctx, "c" <> ToString[n] <> "final"],
      {n, 1, nhomax}
    ];

  (* =================================================== *)
  (* Field redefinition                               *)
  (* =================================================== *)

  htrafo =
    higgsf -> higgsf + Sum[
      xvars[higgsOrderMax][[vark - 1]] v (higgsf/v)^(vark + 1),
      {vark, 2, higgsOrderMax}
    ];

  (* =================================================== *)
  (* F. Kinetic/non-canonical sector                     *)
  (* =================================================== *)

  Lnoncanonic =
    Expand[
      1/2 der[higgsf, mu]^2 (
        1 + 2 Sum[
          pvars[higgsOrderMax][[vark - 1]] (higgsf/v)^vark,
          {vark, 2, higgsOrderMax}
        ]
      )
    ];

  LnoncanonicRedefined =
    Normal @ Series[
      Lnoncanonic /. htrafo //. drules /.
        der[_, _] -> derh hfield /. higgsf -> higgsf hfield,
      {hfield, 0, higgsOrderMax + 2}
    ];

  LnoncanonicRedefined =
    Expand[LnoncanonicRedefined //. allInputSolutions];

  kineticEquations =
    Table[
      Coefficient[LnoncanonicRedefined, hfield, vark] == 0,
      {vark, 4, higgsOrderMax + 2}
    ];


  xSolutions =
    Solve[
      kineticEquations,
      xvars[higgsOrderMax]
    ];

  xRules =
    If[
      xSolutions === {} || xSolutions === {{}},
      {},
      First[xSolutions]
    ];

  (* =================================================== *)
  (* Original interaction lagrangian                       *)
  (* =================================================== *)

  VpotentialList[nhomax_Integer?Positive] :=
    Join[
      {
        1/2 mh^2 higgsf^2,
        1/2 mh^2 dvars[nhomax][[1]] higgsf^3/v
      },
      If[
        nhomax >= 2,
        {1/8 mh^2 dvars[nhomax][[2]] higgsf^4/v^2},
        {}
      ],
      Table[
        mh^2 dvars[nhomax][[n]] higgsf^(n + 2)/v^n,
        {n, 3, nhomax}
      ]
    ];

  VpotentialRedefinedList[nhomax_Integer?Positive] := Module[{expr},
    expr = Total[VpotentialList[nhomax] /. htrafo /. higgsf -> higgsf hfield];

    expr =
      Normal @ Series[
        expr,
        {hfield, 0, nhomax + 2}
      ];

    Table[
      Coefficient[expr, hfield, n] hfield^n,
      {n, 2, nhomax + 2}
    ] /. {hfield -> 1, higgsf -> 1}
  ];

  VpotentialOriginalList[nhomax_Integer?Positive] :=
    VpotentialRedefinedList[nhomax] /.
      Thread[xvars[nhomax] -> 0] /.
      Thread[dvars[nhomax] -> dfinalvars[nhomax]];

  dFinalSolutionsRaw =
    Solve[
      Thread[
        VpotentialOriginalList[higgsOrderMax] ==
          VpotentialRedefinedList[higgsOrderMax]
      ],
      dfinalvars[higgsOrderMax]
    ];

  dFinalSolutions =
    Simplify[
      dFinalSolutionsRaw /. xRules //. allInputSolutions
    ];



  Wbil = Unique["Wbil$"];

  LWHiggsList[nhomax_Integer?Positive] :=
    Table[
      Mw^2 Wbil avars[nhomax][[n]] higgsf^n/v^n,
      {n, 1, nhomax}
    ];

  LWHiggsRedefinedList[nhomax_Integer?Positive] := Module[{expr},
    expr = Total[LWHiggsList[nhomax] /. htrafo /. higgsf -> higgsf hfield];

    expr =
      Normal @ Series[
        expr,
        {hfield, 0, nhomax}
      ];

    Table[
      Coefficient[expr, hfield, n] hfield^n,
      {n, 1, nhomax}
    ] /. {hfield -> 1, higgsf -> 1}
  ];

  LWHiggsOriginalList[nhomax_Integer?Positive] :=
    LWHiggsRedefinedList[nhomax] /.
      Thread[xvars[nhomax] -> 0] /.
      Thread[avars[nhomax] -> afinalvars[nhomax]];

  aFinalSolutionsRaw =
    Solve[
      Thread[
        LWHiggsOriginalList[higgsOrderMax] ==
          LWHiggsRedefinedList[higgsOrderMax]
      ],
      afinalvars[higgsOrderMax]
    ];

  aFinalSolutions =
    Simplify[
      aFinalSolutionsRaw /. xRules //. allInputSolutions
    ];



  Ybil = Unique["Ybil$"];

  LYukawaList[nhomax_Integer?Positive] :=
    Table[
      Mt Ybil cvars[nhomax][[n]] higgsf^n/v^n,
      {n, 1, nhomax}
    ];

  LYukawaRedefinedList[nhomax_Integer?Positive] := Module[{expr},
    expr = Total[LYukawaList[nhomax] /. htrafo /. higgsf -> higgsf hfield];

    expr =
      Normal @ Series[
        expr,
        {hfield, 0, nhomax}
      ];

    Table[
      Coefficient[expr, hfield, n] hfield^n,
      {n, 1, nhomax}
    ] /. {hfield -> 1, higgsf -> 1}
  ];

  LYukawaOriginalList[nhomax_Integer?Positive] :=
    LYukawaRedefinedList[nhomax] /.
      Thread[xvars[nhomax] -> 0] /.
      Thread[cvars[nhomax] -> cfinalvars[nhomax]];

  cFinalSolutionsRaw =
    Solve[
      Thread[
        LYukawaOriginalList[higgsOrderMax] ==
          LYukawaRedefinedList[higgsOrderMax]
      ],
      cfinalvars[higgsOrderMax]
    ];

  cFinalSolutions =
    Simplify[
      cFinalSolutionsRaw /. xRules //. allInputSolutions
    ];

  (* =================================================== *)
  (* Final solutions         *)
  (* =================================================== *)

  finalSolutions =
    CleanSolutionRules[
      Flatten[
        {
          dFinalSolutions,
          aFinalSolutions,
          cFinalSolutions
        }
      ]
    ];

  finalSolutions =
    Simplify[
      finalSolutions //. allInputSolutions
    ];

  If[TrueQ[simplifyResult],
    finalSolutions = Simplify[finalSolutions];
  ];



  Join[
    obj,
    <|
      "RedefinitionInputSolutions" -> allInputSolutions,
      "FieldRedefinition" -> htrafo,
      "Lnoncanonic" -> Lnoncanonic,
      "LnoncanonicRedefined" -> LnoncanonicRedefined,
      "KineticEquations" -> kineticEquations,
      "FieldRedefinitionSolutions" -> xSolutions,
      "FieldRedefinitionRules" -> xRules,
      "DFinalSolutionsRaw" -> dFinalSolutionsRaw,
      "AFinalSolutionsRaw" -> aFinalSolutionsRaw,
      "CFinalSolutionsRaw" -> cFinalSolutionsRaw,
      "DFinalSolutions" -> dFinalSolutions,
      "AFinalSolutions" -> aFinalSolutions,
      "CFinalSolutions" -> cFinalSolutions,
      "FinalSolutions" -> finalSolutions
    |>
  ]
];