(* ::Package:: *)


ClearAll[MatchToHEFT, MatchToHEFTFunctionDefinedQ];

(*Options*)

MatchToHEFTFunctionDefinedQ[f_Symbol] := DownValues[f] =!= {};

MatchToHEFT::badHiggsOrderMax = "HiggsOrderMax must be a positive integer.";
MatchToHEFT::badMassList = "The heavy-mass input must be a list.";
MatchToHEFT::missingFunction = "The internal function `1` has no definition. Load the corresponding section first.";
MatchToHEFT::failedStep = "MatchToHEFT failed at step `1`.";
MatchToHEFT::badEFTorder = "Only EFTorder -> 2 is available.";

Options[MatchToHEFT] = {
  EFTorder -> 2,
  SimplifyResult -> True,
  Verbose -> True,
  ExportDiagrams -> False,
  OutputDirectory -> Automatic,
  AlternateBasis -> True,
  OnlyRelevantDiagrams -> False
};

(*Function*)

MatchToHEFT[modelName_String, higgsOrderMax_Integer?Positive, heavyMasses_List, opts:OptionsPattern[]] :=
  MatchToHEFT[modelName, modelName, higgsOrderMax, heavyMasses, opts];

MatchToHEFT[
  modelModName_String,
  modelGenName_String,
  higgsOrderMax_Integer?Positive,
  heavyMasses_List,
  opts:OptionsPattern[]
] := Module[
  {
    eftOrder, simplifyResult, verbose,
    exportDiagrams, outputDirectory, requiredFunctions,
    ampLO, ampDiags, ampDiagsForNext, ampUV, ampProcessed, onlyRelevantDiagrams, 
    ampExpanded, ampDirac, ampInvariant, matchingObj, solvedObj, redefinedObj,finalRules, localResults,alternateBasis, coeffDir, coeffBase, alternateRules, toJSONRules
  },

  

  eftOrder = OptionValue[EFTorder];

  If[eftOrder =!= 2,
    Message[MatchToHEFT::badEFTorder];
    Return[$Failed];
  ];
  simplifyResult = OptionValue[SimplifyResult];
  verbose = OptionValue[Verbose];
  exportDiagrams = OptionValue[ExportDiagrams];
  outputDirectory = OptionValue[OutputDirectory];
  alternateBasis = OptionValue[AlternateBasis];
  onlyRelevantDiagrams = OptionValue[OnlyRelevantDiagrams];

  requiredFunctions = {
    AmpMatrixMatcha,
    GenerateDiags,
    GenerateUVAmplitudes,
    ProcessUVAmplitudes,
    ExpandUVAmplitudes,
    ExtractDiracStructures,
    RewriteInvariantsObject,
    BuildMatchingObject,
    SolveMatchingObject,
    MakeRedefinitionObject
  };

  If[TrueQ[exportDiagrams],
    requiredFunctions = Append[requiredFunctions, ExportDiagrams]
  ];

  Do[
    If[!MatchToHEFTFunctionDefinedQ[f],
      Message[MatchToHEFT::missingFunction, HoldForm[f]];
      Return[$Failed];
    ],
    {f, requiredFunctions}
  ];

  $MatchConfig = <|
    "UVModel" -> modelModName,
    "GenericModel" -> modelGenName,
    "HiggsOrderMax" -> higgsOrderMax,
    "HeavyMasses" -> heavyMasses,
    "EFTorder" -> eftOrder,
    "SimplifyResult" -> simplifyResult,
    "Verbose" -> verbose,
    "ExportDiagrams" -> exportDiagrams,
    "OutputDirectory" -> outputDirectory,
    "SMFields" -> $SMFieldMap,
    "BSMFields" -> $BSMFields,
    "SMParams" -> $SMParams
  |>;

 If[TrueQ[verbose],
  Print[
    Style[
      Row[
        {
          "MATCHA: Matching ",
          Style[
            "\"" <> modelModName <> "\"",
            Darker[Green]
          ],
          " model to HEFT"
        }
      ],
      Bold,
      25,
      RGBColor[0.1, 0.2, 0.5]
    ]
  ];
  Print[""];
 ];




  ampLO = AmpMatrixMatcha[
    higgsOrderMax,
    EFTorder -> eftOrder,
    Verbose -> verbose
  ];

  If[ampLO === $Failed || !AssociationQ[ampLO],
    Message[MatchToHEFT::failedStep, "AmpMatrixMatcha"];
    Return[$Failed];
  ];

  $HEFTExpressions = ampLO;
  Global`MATCHAHEFTExpressions = ampLO;
  Global`MATCHAHEFTCatalog = ampLO["AllProcesses"];


  MATCHAStatus[i_Integer, total_Integer, msg_String] :=
  Print[
    Style[
      Row[{"[", i, "/", total, "]  ", msg, " ..."}],
      20,
      Bold,
      RGBColor[0.15, 0.25, 0.55]
    ]
  ];


  MATCHAStatus[1, 7, "Generating UV diagrams"];


  ampDiags = GenerateDiags[
    ampLO,
    UVModel -> modelModName,
    FA["GenericModel"] -> modelGenName,
    SMFields -> $SMFieldMap,
    BSMFields -> $BSMFields,
    OnlyRelevantDiagrams -> onlyRelevantDiagrams
  ];

  If[ampDiags === $Failed || !AssociationQ[ampDiags],
    Message[MatchToHEFT::failedStep, "GenerateDiags"];
    Return[$Failed];
  ];

  $GeneratedDiagrams = ampDiags;
  Global`MATCHAGeneratedDiagrams = ampDiags;
  Global`MATCHADiagrams =
  Join[
    ampDiags["AllProcesses"],
    Association @ Flatten @ KeyValueMap[
      Function[{name, entry},
        Thread[
          DeleteCases[
            SymbolName /@ Lookup[entry, "Variables", {}],
            _?(StringStartsQ[#, "p"] &)
          ] -> entry
        ]
      ],
      ampDiags["AllProcesses"]
    ]
  ];

  ampDiagsForNext = ampDiags;

  If[TrueQ[exportDiagrams],


    If[outputDirectory === Automatic,
      outputDirectory = $MATCHARoutesConfig["OutputDirectory"];
    ];

    ampDiagsForNext = ExportDiagrams[
      ampDiags,
      UVModel -> modelModName,
      OutputDirectory -> outputDirectory,
      OnlyRelevantDiagrams -> onlyRelevantDiagrams
    ];

    If[ampDiagsForNext === $Failed || !AssociationQ[ampDiagsForNext],
      Message[MatchToHEFT::failedStep, "ExportDiagrams"];
      Return[$Failed];
    ];

    $ExportedDiagrams = ampDiagsForNext;
    Global`MATCHAExportedDiagrams = ampDiagsForNext;
    Global`MATCHAExportedDiagramProcesses = ampDiagsForNext["AllProcesses"];
  ];

  MATCHAStatus[2, 7, "Generating UV amplitudes"];

  ampUV = GenerateUVAmplitudes[
    ampDiagsForNext
  ];

  If[ampUV === $Failed || !AssociationQ[ampUV],
    Message[MatchToHEFT::failedStep, "GenerateUVAmplitudes"];
    Return[$Failed];
  ];


  MATCHAStatus[3, 7, "Processing UV amplitudes"];
  $UVAmplitudes = ampUV;
  Global`MATCHAUVAmplitudes = ampUV;
  Global`MATCHAUVAmplitudeProcesses = ampUV["AllProcesses"];


  ampProcessed = ProcessUVAmplitudes[
    ampUV
  ];

 

  If[ampProcessed === $Failed || !AssociationQ[ampProcessed],
    Message[MatchToHEFT::failedStep, "ProcessUVAmplitudes"];
    Return[$Failed];
  ];

  $ProcessedUVAmplitudes = ampProcessed;
  Global`MATCHAProcessedUVAmplitudes = ampProcessed;
  Global`MATCHAProcessedUVAmplitudeProcesses = ampProcessed["AllProcesses"];


  MATCHAStatus[4, 7, "Expanding UV amplitudes"];

  LoadModel[
  FileNameJoin[
    {
      $MATCHARoutesConfig["FeynArtsRoute"],
      "Models",
      modelModName
    }
  ]
  ];

  If[!ValueQ[Global`M$FACouplings] || !ListQ[Global`M$FACouplings],
  Global`M$FACouplings = {};
  ];


  ampExpanded = ExpandUVAmplitudes[
    ampProcessed,
    HeavyMassList -> heavyMasses,
    EFTorder -> eftOrder,
    SimplifyResult -> simplifyResult
  ];

  If[ampExpanded === $Failed || !AssociationQ[ampExpanded],
    Message[MatchToHEFT::failedStep, "ExpandUVAmplitudes"];
    Return[$Failed];
  ];

  $ExpandedUVAmplitudes = ampExpanded;
  Global`MATCHAExpandedUVAmplitudes = ampExpanded;
  Global`MATCHAExpandedUVAmplitudeProcesses = ampExpanded["AllProcesses"];



  ampDirac = ExtractDiracStructures[
    ampExpanded,
    SimplifyResult -> simplifyResult
  ];

  If[ampDirac === $Failed || !AssociationQ[ampDirac],
    Message[MatchToHEFT::failedStep, "ExtractDiracStructures"];
    Return[$Failed];
  ];

  $DiracExtractedAmplitudes = ampDirac;
  Global`MATCHADiracExtractedAmplitudes = ampDirac;
  Global`MATCHADiracExtractedAmplitudeProcesses = ampDirac["AllProcesses"];


  MATCHAStatus[5, 7, "Rewriting kinematics"];


  ampInvariant = RewriteInvariantsObject[
    ampDirac,
    SimplifyResult -> simplifyResult
  ];

  If[ampInvariant === $Failed || !AssociationQ[ampInvariant],
    Message[MatchToHEFT::failedStep, "RewriteInvariantsObject"];
    Return[$Failed];
  ];

  $InvariantRewrittenAmplitudes = ampInvariant;
  Global`MATCHAInvariantRewrittenAmplitudes = ampInvariant;
  Global`MATCHAInvariantRewrittenAmplitudeProcesses = ampInvariant["AllProcesses"];



  matchingObj = BuildMatchingObject[
    ampInvariant,
    SimplifyResult -> simplifyResult
  ];

  MATCHAStatus[6, 7, "Solving"];
  If[matchingObj === $Failed || !AssociationQ[matchingObj],
    Message[MatchToHEFT::failedStep, "BuildMatchingObject"];
    Return[$Failed];
  ];

  $MatchingObject = matchingObj;
  Global`MATCHAMatchingObject = matchingObj;
  Global`MATCHAMatchingProcesses = matchingObj["AllProcesses"];


  solvedObj = SolveMatchingObject[
    matchingObj,
    SimplifyResult -> simplifyResult
  ];

  If[solvedObj === $Failed || !AssociationQ[solvedObj],
    Message[MatchToHEFT::failedStep, "SolveMatchingObject"];
    Return[$Failed];
  ];

  $SolvedMatchingObject = solvedObj;
  Global`MATCHASolvedMatchingObject = solvedObj;
  Global`MATCHASolvedMatchingProcesses = solvedObj["AllProcesses"];

  MATCHAStatus[7, 7, "Obtaining HEFT couplings"];


  redefinedObj = MakeRedefinitionObject[
    solvedObj,
    SimplifyResult -> simplifyResult
  ];

  If[redefinedObj === $Failed || !AssociationQ[redefinedObj],
    Message[MatchToHEFT::failedStep, "MakeRedefinitionObject"];
    Return[$Failed];
  ];

  (* Export matching coefficients in the same directory used by ExportDiagrams *)

  If[outputDirectory === Automatic,
    outputDirectory = $MATCHARoutesConfig["OutputDirectory"];
  ];

  coeffDir = DiagramExportDirectory[modelModName, outputDirectory];

  If[!DirectoryQ[coeffDir],
    CreateDirectory[coeffDir, CreateIntermediateDirectories -> True]
  ];

  coeffBase =
    FileNameJoin[
      {
        coeffDir,
        "MATCHA_Coefficients_" <> SafeFileName[modelModName]
      }
    ];

  toJSONRules[rules_] :=
    rules /. (lhs_ -> rhs_) :>
      <|
        "Coefficient" -> ToString[Unevaluated[lhs], InputForm],
        "Expression" -> ToString[Unevaluated[rhs], InputForm],
        "Rule" -> ToString[Unevaluated[lhs -> rhs], InputForm]
      |>;

  finalRules = CleanSolutionRules[redefinedObj["FinalSolutions"]];

  Put[finalRules, coeffBase <> "_FinalBasis.m"];

  Export[
    coeffBase <> "_FinalBasis.json",
    toJSONRules[finalRules],
    "JSON"
  ];

  If[TrueQ[alternateBasis],

    alternateRules =
      CleanSolutionRules[redefinedObj["RedefinitionInputSolutions"]];

    Put[alternateRules, coeffBase <> "_AlternateBasis.m"];

    Export[
      coeffBase <> "_AlternateBasis.json",
      toJSONRules[alternateRules],
      "JSON"
    ];
  ];

  $RedefinedMatchingObject = redefinedObj;

  finalRules = redefinedObj["FinalSolutions"];

  localResults = Association @ Replace[
  finalRules,
  (lhs_Symbol -> rhs_) :>
    (
      StringReplace[
        SymbolName[Unevaluated[lhs]],
        "final" ~~ EndOfString -> ""
      ] -> rhs
    ),
  {1}
  ];


  $FinalMatchingResults = localResults;

  Global`MATCHARedefinedMatchingObject = redefinedObj;
  Global`MATCHAFinalMatchingResults = localResults;
  Global`Results = localResults;

  Grid[
    Prepend[
      Table[{k, localResults[k]}, {k, Keys[localResults]}],
      {"HEFT coefficient", "Matching Expression"}
    ],
    Frame -> All
  ]
];

MatchToHEFT[modelName_String, higgsOrderMax_, heavyMasses_List, opts:OptionsPattern[]] /;
    !MatchQ[higgsOrderMax, _Integer?Positive] :=
  (
    Message[MatchToHEFT::badHiggsOrderMax];
    $Failed
  );

MatchToHEFT[modelName_String, higgsOrderMax_Integer?Positive, heavyMasses_, opts:OptionsPattern[]] /;
    !ListQ[heavyMasses] :=
  (
    Message[MatchToHEFT::badMassList];
    $Failed
  );