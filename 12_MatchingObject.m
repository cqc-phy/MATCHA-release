(* ::Package:: *)


ClearAll[BuildMatchingProcess, BuildMatchingObject];

BuildMatchingProcess::missingUV =
  "The process entry does not contain \"UVAmplitudeInvariant\".";
BuildMatchingProcess::missingHEFT =
  "The process entry does not contain \"HEFTAmplitude\".";
BuildMatchingObject::noProcesses =
  "The input object does not contain a valid \"AllProcesses\" association.";

Options[BuildMatchingProcess] = {
  SimplifyResult -> True
};

Options[BuildMatchingObject] = Options[BuildMatchingProcess];

BuildMatchingProcess[entry_Association, opts:OptionsPattern[]] := Module[
  {uv, heft, expr, vars},



  If[!KeyExistsQ[entry, "UVAmplitudeInvariant"],
    Message[BuildMatchingProcess::missingUV];
    Return[Join[entry, <|"MatchingExpression" -> $Failed|>]]
  ];

  If[!KeyExistsQ[entry, "HEFTAmplitude"],
    Message[BuildMatchingProcess::missingHEFT];
    Return[Join[entry, <|"MatchingExpression" -> $Failed|>]]
  ];

  uv = entry["UVAmplitudeInvariant"];
  heft = entry["HEFTAmplitude"];
  vars = Lookup[entry, "Variables", {}];



  expr = If[uv === $Failed || heft === $Failed, $Failed, Expand[uv - heft]];
  If[TrueQ[OptionValue[SimplifyResult]] && expr =!= $Failed, expr = Simplify[expr]];



  Join[entry, <|"MatchingExpression" -> expr, "MatchingVariables" -> vars|>]
];

BuildMatchingObject[ampObj_Association, opts:OptionsPattern[]] := Module[
  {allProcesses, newProcesses},


  allProcesses = Lookup[ampObj, "AllProcesses", $Failed];

  If[!AssociationQ[allProcesses],
    Message[BuildMatchingObject::noProcesses];
    Return[$Failed]
  ];



  newProcesses = Association @ KeyValueMap[
    #1 -> BuildMatchingProcess[#2, opts]&,
    allProcesses
  ];



  Join[ampObj, <|"AllProcesses" -> newProcesses|>]
];
