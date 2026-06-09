(* ::Package:: *)

ClearAll[ExpandAmplitude, ExpandUVAmplitudeForProcess, ExpandUVAmplitudes];

ExpandUVAmplitudeForProcess::missingAmplitude =
  "The process entry does not contain \"UVAmplitudeProcessed\".";

ExpandUVAmplitudes::noProcesses =
  "The input object does not contain a valid \"AllProcesses\" association.";

Options[ExpandAmplitude] = {
  HeavyMassList -> {},
  EFTorder -> 2,
  SimplifyResult -> True
};

Options[ExpandUVAmplitudeForProcess] = {
  HeavyMassList -> {},
  EFTorder -> 2,
  SimplifyResult -> True
};

Options[ExpandUVAmplitudes] = Options[ExpandUVAmplitudeForProcess];

ExpandAmplitude[inputExpr_, opts:OptionsPattern[]] := Module[
  {expr, heavyMasses, eftOrder, simplifyResult, expanded},

  expr = inputExpr;

  heavyMasses = OptionValue[HeavyMassList];
  eftOrder = OptionValue[EFTorder];
  simplifyResult = OptionValue[SimplifyResult];
  
  expr = expr /. Global`M$FACouplings;


  If[expr === $Failed, Return[$Failed]];



  expanded = Fold[
    Function[{acc, mass},
      Quiet @ Check[Normal @ Series[acc, {mass, Infinity, 0}], acc]
    ],
    expr,
    heavyMasses
  ];

  expanded = Expand[expanded];
  If[TrueQ[simplifyResult], expanded = Simplify[expanded]];

  expanded
];

ExpandUVAmplitudeForProcess[entry_Association, opts:OptionsPattern[]] := Module[
  {raw, expanded, heavyMasses, eftOrder},


  heavyMasses = OptionValue[HeavyMassList];
  eftOrder = OptionValue[EFTorder];

  If[!KeyExistsQ[entry, "UVAmplitudeProcessed"],
    Message[ExpandUVAmplitudeForProcess::missingAmplitude];
    Return[Join[entry, <|"UVAmplitudeExpanded" -> $Failed|>]]
  ];

  raw = entry["UVAmplitudeProcessed"];



  expanded = ExpandAmplitude[
    raw,
    HeavyMassList -> heavyMasses,
    EFTorder -> eftOrder,
    SimplifyResult -> OptionValue[SimplifyResult]
  ];



  Join[entry, <|"UVAmplitudeExpanded" -> expanded|>]
];

ExpandUVAmplitudes[ampObj_Association, opts:OptionsPattern[]] := Module[
  {allProcesses, newProcesse},


  allProcesses = Lookup[ampObj, "AllProcesses", $Failed];

  If[!AssociationQ[allProcesses],
    Message[ExpandUVAmplitudes::noProcesses];
    Return[$Failed]
  ];



  newProcesses = Association @ KeyValueMap[
    #1 -> ExpandUVAmplitudeForProcess[#2, opts]&,
    allProcesses
  ];



  Join[ampObj, <|"AllProcesses" -> newProcesses|>]
];
