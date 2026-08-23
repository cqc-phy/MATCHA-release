(* ::Package:: *)

ClearAll[ExtractDiracStructureForProcess, ExtractDiracStructures, ExtractDirac];

ExtractDiracStructureForProcess::missingAmplitude =
  "The process entry does not contain \"UVAmplitudeExpanded\".";

ExtractDiracStructures::noProcesses =
  "The input object does not contain a valid \"AllProcesses\" association.";

Options[ExtractDiracStructureForProcess] = {
  SimplifyResult -> True
};

Options[ExtractDiracStructures] = Options[ExtractDiracStructureForProcess];

ExtractDirac[expr_, name_String] := Module[
  {WC6, WC7, tmp},

  If[!StringMatchQ[name, "c" ~~ DigitCharacter ..],
    Return[expr]
  ];

  tmp = Expand[
    expr /. {
      HoldPattern[
        FormCalc`WeylChain[
          FormCalc`Spinor[FormCalc`k[2], m_, -1, 2, 0],
          6,
          FormCalc`Spinor[FormCalc`k[1], m_, 1, 1, 0]
        ]
      ] :> WC6,

      HoldPattern[
        FormCalc`WeylChain[
          FormCalc`Spinor[FormCalc`k[2], m_, -1, 2, 0],
          7,
          FormCalc`Spinor[FormCalc`k[1], m_, 1, 1, 0]
        ]
      ] :> WC7
    }
  ];

  If[!FreeQ[tmp, FormCalc`Spinor],
    Return[expr]
  ];

  Simplify[(tmp /. {WC6 -> 1, WC7 -> 1})/2]
];

ExtractDiracStructureForProcess[entry_Association, opts:OptionsPattern[]] := Module[
  {amp, family, extracted, vars, name},



  If[!KeyExistsQ[entry, "UVAmplitudeExpanded"],
    Message[ExtractDiracStructureForProcess::missingAmplitude];
    Return[Join[entry, <|"UVAmplitudeDiracExtracted" -> $Failed|>]]
  ];

  amp = entry["UVAmplitudeExpanded"];
  family = Lookup[entry, "Family", ""];

  vars = Lookup[entry, "MatchingVariables", Lookup[entry, "Variables", {}]];

  name = If[
    ListQ[vars] && Length[vars] > 0 && Head[First[vars]] === Symbol,
    SymbolName[First[vars]],
    ""
  ];

  extracted = If[
    amp === $Failed,
    $Failed,
    Switch[family,
      "FermionHiggs",
        ExtractDirac[amp, name],
      _,
        amp
    ]
  ];

  If[TrueQ[OptionValue[SimplifyResult]] && extracted =!= $Failed,
    extracted = Simplify[extracted]
  ];



  Join[entry, <|"UVAmplitudeDiracExtracted" -> extracted|>]
];

ExtractDiracStructures[ampObj_Association, opts:OptionsPattern[]] := Module[
  {allProcesses, newProcesses},

  allProcesses = Lookup[ampObj, "AllProcesses", $Failed];

  If[!AssociationQ[allProcesses],
    Message[ExtractDiracStructures::noProcesses];
    Return[$Failed]
  ];



  newProcesses = Association @ KeyValueMap[
    #1 -> ExtractDiracStructureForProcess[#2, opts]&,
    allProcesses
  ];


  Join[ampObj, <|"AllProcesses" -> newProcesses|>]
];