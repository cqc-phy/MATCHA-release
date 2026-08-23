(* ::Package:: *)



ClearAll[GenerateUVAmplitudeForProcess, GenerateUVAmplitudes];

GenerateUVAmplitudeForProcess::missingDiagrams =
  "The required diagram key \"RelevantDiagrams\" is not present in the process entry.";

GenerateUVAmplitudes::noProcesses =
  "The input object does not contain a valid \"AllProcesses\" association.";


Options[GenerateUVAmplitudeForProcess] = {};

Options[GenerateUVAmplitudes] = Options[GenerateUVAmplitudeForProcess];



GenerateUVAmplitudeForProcess[entry_Association, opts:OptionsPattern[]] := Module[
  {diagramKey, diagrams, uvAmplitudeRaw},

  diagramKey = "RelevantDiagrams";


  If[!KeyExistsQ[entry, diagramKey],
    Message[GenerateUVAmplitudeForProcess::missingDiagrams];
    Return[Join[entry, <|"UVAmplitudeRaw" -> $Failed|>]]
  ];

  diagrams = entry[diagramKey];



  uvAmplitudeRaw = If[
    diagrams === $Failed,
    $Failed,
    withPackageOutputControl[
      FA["CreateFeynAmp"][diagrams],
      False
    ]
  ];



  Join[entry, <|"UVAmplitudeRaw" -> uvAmplitudeRaw|>]
];



GenerateUVAmplitudes[diagObj_Association, opts:OptionsPattern[]] := Module[
  {allProcesses, newProcesses},



  allProcesses = Lookup[diagObj, "AllProcesses", $Failed];

  If[!AssociationQ[allProcesses],
    Message[GenerateUVAmplitudes::noProcesses];
    Return[$Failed]
  ];


  newProcesses = Association @ KeyValueMap[
    #1 -> GenerateUVAmplitudeForProcess[
      #2
    ]&,
    allProcesses
  ];



  Join[diagObj, <|"AllProcesses" -> newProcesses|>]
];
