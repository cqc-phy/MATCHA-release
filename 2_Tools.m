(* ::Package:: *)

ClearAll[LoadTools, MATCHANullDevice, withPackageOutputControl];

Options[LoadTools] = {Config -> Automatic, VerboseMode -> True};

LoadTools::noConfig = "No valid configuration found. Define $MATCHARoutesConfig or pass Config -> <|...|>.";
LoadTools::badRoutes = "FeynArtsRoute, FormCalcRoute and ModelPath must be non-empty strings.";

LoadTools[opts:OptionsPattern[]] := Module[
  {cfg, faRoute, fcRoute, modelPath, verbose},

  cfg = Replace[OptionValue[Config], Automatic :> $MATCHARoutesConfig];
  verbose = OptionValue[VerboseMode];

  If[!AssociationQ[cfg], Message[LoadTools::noConfig]; Return[$Failed]];

  faRoute = cfg["FeynArtsRoute"];
  fcRoute = cfg["FormCalcRoute"];
  modelPath = cfg["ModelPath"];

  If[!StringQ[faRoute] || faRoute === "" ||
     !StringQ[fcRoute] || fcRoute === "" ||
     !StringQ[modelPath] || modelPath === "",
    Message[LoadTools::badRoutes];
    Return[$Failed]
  ];

  Do[
    If[StringQ[p] && DirectoryQ[p] && !MemberQ[$Path, p], AppendTo[$Path, p]],
    {p, {faRoute, fcRoute, modelPath}}
  ];

  If[TrueQ[verbose], Print["Loading FeynArts..."]];
  Get["FeynArts.m"];

  If[TrueQ[verbose], Print["Loading FormCalc..."]];
  Get["FormCalc.m"];

  If[ValueQ[FeynArts`$ModelPath],
    FeynArts`$ModelPath = DeleteDuplicates[Append[FeynArts`$ModelPath, modelPath]]
  ];

  If[TrueQ[verbose], Print["Tools loaded successfully."]];

  <|
    "FeynArtsLoaded" -> True,
    "FormCalcLoaded" -> True,
    "FeynArtsRoute" -> faRoute,
    "FormCalcRoute" -> fcRoute,
    "ModelPath" -> modelPath
  |>
];

MATCHANullDevice[] := If[$OperatingSystem === "Windows", "NUL", "/dev/null"];

SetAttributes[withPackageOutputControl, HoldFirst];
withPackageOutputControl[expr_, debugPackage_] := If[TrueQ[debugPackage],
  expr,
  Module[{nullStream},
    Internal`WithLocalSettings[
      nullStream = OpenWrite[MATCHANullDevice[]],
      Quiet[Block[{Print = Function[Null], $Messages = {}, $Output = {nullStream}}, expr]],
      Close[nullStream]
    ]
  ]
];

ClearAll[ClearMATCHARunState];

ClearMATCHARunState[] := Module[{},
  Quiet @ Check[
    If[NameQ["FormCalc`ClearProcess"], FormCalc`ClearProcess[]],
    Null
  ];

  $MatchConfig = <||>;
  $HEFTExpressions = <||>;
  $GeneratedDiagrams = <||>;
  $ExportedDiagrams = <||>;
  $UVAmplitudes = <||>;
  $ProcessedUVAmplitudes = <||>;
  $ExpandedUVAmplitudes = <||>;
  $DiracExtractedAmplitudes = <||>;
  $InvariantRewrittenAmplitudes = <||>;
  $MatchingObject = <||>;
  $SolvedMatchingObject = <||>;
  $RedefinedMatchingObject = <||>;
  $FinalMatchingResults = {};

  Clear["Global`MATCHA*"];

  ClearSystemCache["Symbolic"];
  Share[];

  Null
];
