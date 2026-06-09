(* ::Package:: *)


ClearAll[SafeFileName, DiagramExportDirectory, PaintDiagramSet, ExportDiagramsForProcess, ExportDiagrams];

ExportDiagrams::noModel = "You must specify the UV model with UVModel -> \"ModelName\".";
ExportDiagrams::noOutputDirectory = "You must specify the output directory with OutputDirectory -> \"path\".";
ExportDiagrams::noProcesses = "The input object does not contain a valid \"AllProcesses\" association.";
ExportDiagramsForProcess::paintHeld = "FeynArts Paint did not evaluate for process `1`. Check contexts, model loading, and diagram generation.";

SafeFileName[x_] := StringReplace[
  ToString[x, InputForm],
  {
    " " -> "",
    "\"" -> "",
    "[" -> "",
    "]" -> "",
    "{" -> "",
    "}" -> "",
    "," -> "_",
    "->" -> "to",
    "-" -> "m",
    "/" -> "_",
    "`" -> ""
  }
];

DiagramExportDirectory[uvModel_, outputDirectory_String] :=
  FileNameJoin[{outputDirectory, "Diagrams_" <> SafeFileName[uvModel]}];

PaintDiagramSet[diagrams_, debugPackage_: False] := withPackageOutputControl[
  FA["Paint"][
    diagrams,
    FA["PaintLevel"] -> {FA["Particles"]},
    ImageSize -> 500,
    FA["AutoEdit"] -> False,
    DisplayFunction -> Identity
  ],
  False
];

Options[ExportDiagramsForProcess] = {
  OnlyRelevantDiagrams -> False
};

ExportDiagramsForProcess[
  processID_,
  entry_Association,
  exportDir_String,
  opts:OptionsPattern[]
] := Module[
  {
    family, familyDir, processDir, label, safeLabel,
    allPainted, relevantPainted, ignoredPainted,
    allFile, relevantFile, ignoredFile, onlyRelevantDiagrams
  },

  onlyRelevantDiagrams = OptionValue[OnlyRelevantDiagrams];

  family = ToString[entry["Family"]];
  label = ToString[processID];
  safeLabel = SafeFileName[label];

  familyDir = FileNameJoin[{exportDir, SafeFileName[family]}];
  processDir = FileNameJoin[{familyDir, safeLabel}];

  If[!DirectoryQ[processDir],
    CreateDirectory[processDir, CreateIntermediateDirectories -> True]
  ];

  allFile = FileNameJoin[{processDir, safeLabel <> "_AllDiagrams.pdf"}];
  relevantFile = FileNameJoin[{processDir, safeLabel <> "_RelevantDiagrams.pdf"}];
  ignoredFile = FileNameJoin[{processDir, safeLabel <> "_IgnoredDiagrams.pdf"}];


  relevantPainted = PaintDiagramSet[entry["RelevantDiagrams"], False];

  If[!FreeQ[HoldComplete[relevantPainted], FA["Paint"]],
    Message[ExportDiagramsForProcess::paintHeld, processID];
    Return[$Failed]
  ];

  withPackageOutputControl[
    Export[relevantFile, relevantPainted],
    False
  ];

  If[TrueQ[onlyRelevantDiagrams],
    Return[
      Join[
        entry,
        <|
          "RelevantPaintedDiags" -> relevantPainted,
          "FamilyExportDirectory" -> familyDir,
          "DiagramExportDirectory" -> processDir,
          "RelevantDiagramsPDF" -> relevantFile
        |>
      ]
    ];
  ];

  allPainted = PaintDiagramSet[entry["AllDiagrams"], False];
  ignoredPainted = PaintDiagramSet[entry["IgnoredDiagrams"], False];

  If[!FreeQ[HoldComplete[{allPainted, ignoredPainted}], FA["Paint"]],
    Message[ExportDiagramsForProcess::paintHeld, processID];
    Return[$Failed]
  ];

  withPackageOutputControl[
    Export[allFile, allPainted],
    False
  ];

  withPackageOutputControl[
    Export[ignoredFile, ignoredPainted],
    False
  ];



  Join[
    entry,
    <|
      "AllPaintedDiags" -> allPainted,
      "RelevantPaintedDiags" -> relevantPainted,
      "IgnoredPaintedDiags" -> ignoredPainted,
      "FamilyExportDirectory" -> familyDir,
      "DiagramExportDirectory" -> processDir,
      "AllDiagramsPDF" -> allFile,
      "RelevantDiagramsPDF" -> relevantFile,
      "IgnoredDiagramsPDF" -> ignoredFile
    |>
  ]
];

Options[ExportDiagrams] = {
  UVModel -> Automatic,
  OutputDirectory -> Automatic,
  OnlyRelevantDiagrams -> False
};

ExportDiagrams[diagObj_Association, opts:OptionsPattern[]] := Module[
  {uvModel, outputDirectory, exportDir, allProcesses, newProcesses,onlyRelevantDiagrams},

  uvModel = OptionValue[UVModel];
  outputDirectory = OptionValue[OutputDirectory];
  onlyRelevantDiagrams = OptionValue[OnlyRelevantDiagrams];

  If[uvModel === Automatic,
    Message[ExportDiagrams::noModel];
    Return[$Failed]
  ];

  If[outputDirectory === Automatic,
    Message[ExportDiagrams::noOutputDirectory];
    Return[$Failed]
  ];

  allProcesses = Lookup[diagObj, "AllProcesses", $Failed];
  If[!AssociationQ[allProcesses],
    Message[ExportDiagrams::noProcesses];
    Return[$Failed]
  ];

  exportDir = DiagramExportDirectory[uvModel, outputDirectory];
  If[!DirectoryQ[exportDir], CreateDirectory[exportDir, CreateIntermediateDirectories -> True]];


  newProcesses = Association @ KeyValueMap[
    (#1 -> ExportDiagramsForProcess[
      #1,
      #2,
      exportDir,
      OnlyRelevantDiagrams -> onlyRelevantDiagrams
    ]) &,
    allProcesses
  ];

  If[MemberQ[Values[newProcesses], $Failed], Return[$Failed]];


  Join[
    diagObj,
    <|
      "UVModel" -> uvModel,
      "OutputDirectory" -> outputDirectory,
      "DiagramExportDirectory" -> exportDir,
      "AllProcesses" -> newProcesses
    |>
  ]
];
