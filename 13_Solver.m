(* ::Package:: *)


ClearAll[
  TestDefinition, ToLongMandelstamRules, MandelstamVariables,
  ZeroMandelstamRules, StripAmpWrapper, MainMatchingVariable,
  DerivativeMatchingVariables, ZeroMomentumPairRules,
  KinematicVariablesForSolver, MatchingEquationsFromExpression,
  SolveMatchingProcess, SolveMatchingObject
];

SolveMatchingProcess::missingExpression =
  "The process entry does not contain \"MatchingExpression\".";
SolveMatchingObject::noProcesses =
  "The input object does not contain a valid \"AllProcesses\" association.";

Options[SolveMatchingProcess] = {
  SimplifyResult -> True
};

Options[SolveMatchingObject] = Options[SolveMatchingProcess];

TestDefinition[label_String, expr_] := Module[{res},
  Print[""];
  Print["--- TEST ", label, " ---"];
  res = Quiet @ Check[expr, $Failed];
  Print["Result: ", res];
  res
];

FCPrivateSymbol[name_String] := Symbol["FormCalc`" <> name];

FCK[i_Integer] := FCPrivateSymbol["k"][i];

FCPair[i_Integer, j_Integer] := 
  FCPrivateSymbol["Pair"][FCK[i], FCK[j]];

FCPair[x_, j_Integer] := FCPrivateSymbol["Pair"][x, FCK[j]];

FCPair[i_Integer, x_] := FCPrivateSymbol["Pair"][FCK[i], x];

FCPair[x_, y_] := FCPrivateSymbol["Pair"][x, y];


MandelstamSymbol["S", i_Integer, j_Integer] := 
  FCPrivateSymbol["S" <> ToString[i] <> ToString[j]];

MandelstamSymbol["T", i_Integer, j_Integer] := 
  FCPrivateSymbol["T" <> ToString[i] <> ToString[j]];


ToLongMandelstamRules[n_Integer?Positive] := {FA["S"] -> MandelstamSymbol["S", 1, 2], 
  FA["T"] -> MandelstamSymbol["T", 1, 3], 
  FA["U"] -> MandelstamSymbol["T", 2, 3]}



MandelstamVariables[n_Integer?Positive] := Module[{last}, last = n + 2;
  DeleteDuplicates[
   Join[{MandelstamSymbol["S", 1, 2], MandelstamSymbol["T", 1, 3], 
     MandelstamSymbol["T", 2, 3]}, 
    Flatten[Table[
      MandelstamSymbol["T", i, j], {i, 1, 2}, {j, 3, last}]], 
    Flatten[Table[
      MandelstamSymbol["S", i, j], {i, 3, last - 1}, {j, i + 1, 
       last}]]]]];




ZeroMandelstamRules[n_Integer?Positive] := 
  Join[ToLongMandelstamRules[n], Thread[MandelstamVariables[n] -> 0]];

StripAmpWrapper[expr_] := expr /. {
  HoldPattern[Amp[x_]] :> x,
  HoldPattern[MATCHA`Private`Amp[x_]] :> x,
  HoldPattern[Global`Amp[x_]] :> x
};

MainMatchingVariable[entry_Association] := Module[{vars},
  vars = Lookup[entry, "MatchingVariables", Lookup[entry, "Variables", {}]];
  If[ListQ[vars] && Length[vars] > 0, First[vars], Missing["NoMatchingVariable"]]
];

DerivativeMatchingVariables[entry_Association] := Module[{vars},
  vars = Lookup[entry, "MatchingVariables", Lookup[entry, "Variables", {}]];
  If[ListQ[vars] && Length[vars] > 1, Rest[vars], {}]
];



ZeroMomentumPairRules[] := 
 With[{pair = FC["Pair"], 
   kk = FC["k"]}, {HoldPattern[pair[kk[_], kk[_]]] :> 0, 
   HoldPattern[
     p_[m1_[_], m2_[_]] /; 
      SymbolName[Unevaluated[p]] === "Pair" && 
       SymbolName[Unevaluated[m1]] === "k" && 
       SymbolName[Unevaluated[m2]] === "k"] :> 0}]

ZeroMandelstamRules[n_Integer?Positive] := 
 Join[ToLongMandelstamRules[n], Thread[MandelstamVariables[n] -> 0]]

KinematicVariablesForSolver[expr_] := Module[{mandelstam},
  If[expr === $Failed, Return[{}]];
  mandelstam = Cases[expr, Alternatives @@ MandelstamVariables[], Infinity];
  DeleteDuplicates[mandelstam]
];

MatchingEquationsFromExpression[expr_,entry_Association,solveVars_List:{}] := Module[
  {clean, kinVars, coeffs},
  n = entry["HiggsOrder"];
  vars = Lookup[entry, "MatchingVariables", Lookup[entry, "Variables", {}]];
  If[expr === $Failed, Return[{$Failed == 0}]];
  fulleq = StripAmpWrapper[expr]; 
  nonder = fulleq //. ZeroMomentumPairRules[] //.ZeroMandelstamRules[n];
  clean = Numerator[clean];
  kinVars = KinematicVariablesForSolver[clean];
  eqnonder={nonder==0};
  
  sol1 = Solve[eqnonder, vars[[1]]];

 
  sol2 = If[
    Length[vars] >= 2 && sol1 =!= {},
    Solve[
      (fulleq /. ToLongMandelstamRules[n] /. sol1[[1]]) == 0,
      vars[[2]]
    ],
    {}
  ];

  solutionsboth = Flatten[{sol1, sol2}];

  solutionsboth=Flatten[{sol1,sol2}];Return[solutionsboth];
  (*
  coeffs = If[
    kinVars === {},
    {clean},
    Flatten[CoefficientList[Expand[clean], kinVars]]
  ];

  coeffs = DeleteDuplicates[DeleteCases[Expand /@ coeffs, 0]];
  If[coeffs === {}, {0 == 0}, Thread[coeffs == 0]]*)
];

SolveMatchingProcess[entry_Association, opts:OptionsPattern[]] := Module[
  {expr, vars, equations, solution},



  If[!KeyExistsQ[entry, "MatchingExpression"],
    Message[SolveMatchingProcess::missingExpression];
    Return[Join[entry, <|"MatchingEquations" -> {}, "MatchingSolution" -> $Failed|>]]
  ];

  expr = entry["MatchingExpression"];
  vars = Lookup[entry, "MatchingVariables", Lookup[entry, "Variables", {}]];
  If[!ListQ[vars], vars = {}];



  solution=MatchingEquationsFromExpression[expr, entry,vars];

  If[TrueQ[OptionValue[SimplifyResult]] && solution =!= $Failed,
    solution = Simplify[solution]
  ];


  Join[entry, <|"MatchingEquations" -> equations, "MatchingSolution" -> solution|>]
];

SolveMatchingObject[matchingObj_Association, opts:OptionsPattern[]] := Module[
  {allProcesses, newProcesses},


  allProcesses = Lookup[matchingObj, "AllProcesses", $Failed];

  If[!AssociationQ[allProcesses],
    Message[SolveMatchingObject::noProcesses];
    Return[$Failed]
  ];





  newProcesses = Association @ KeyValueMap[
    #1 -> SolveMatchingProcess[#2, opts]&,
    allProcesses
  ];



  Join[matchingObj, <|"AllProcesses" -> newProcesses|>]
];
