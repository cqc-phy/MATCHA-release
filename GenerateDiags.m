(* ::Package:: *)


ClearAll[GenerateDiagsForProcess, GenerateDiags];

GenerateDiags::noModel = "You must specify the UV model with UVModel -> \"ModelName\".";
GenerateDiags::noProcesses = "The input object does not contain a valid \"AllProcesses\" association.";
GenerateDiagsForProcess::badEntry = "The process entry does not contain a valid \"Process\" rule.";

Options[GenerateDiagsForProcess] = {
  UVModel -> Automatic,
  FA["GenericModel"] -> Automatic,
  SMFields -> Automatic,
  BSMFields -> Automatic,
  OnlyRelevantDiagrams-> False
};

GenerateDiagsForProcess[entry_Association, opts:OptionsPattern[]] := Module[
  {
    process, inFields, outFields, faProcess, totalParticles,
    uvModel, genericModel, smFields, bsmFields,
    topologies, ignoredTopologies, relevantDiagrams, allDiagrams, ignoredDiagrams, onlyRelevantDiagrams
  },

  process = Lookup[entry, "Process", $Failed];
  If[!MatchQ[process, _Rule],
    Message[GenerateDiagsForProcess::badEntry];
    Return[$Failed]
  ];

  inFields = First[process];
  outFields = Last[process];
  faProcess = inFields -> outFields;
  totalParticles = Length[inFields] + Length[outFields];

  uvModel = OptionValue[UVModel];
  genericModel = Replace[OptionValue[FA["GenericModel"]], Automatic :> uvModel];
  smFields = Replace[OptionValue[SMFields], Automatic :> $SMFieldMap];
  bsmFields = Replace[OptionValue[BSMFields], Automatic :> $BSMFields];
  onlyRelevantDiagrams = OptionValue[OnlyRelevantDiagrams];

 

  {topologies, ignoredTopologies, relevantDiagrams, allDiagrams, ignoredDiagrams} =
    withPackageOutputControl[
      Module[{tops, ignoredTops, relevant, all, ignored},
        tops = FA["CreateTopologies"][
          0,
          Length[inFields] -> Length[outFields],
          FA["Adjacencies"] -> Range[3, totalParticles]
        ];


        relevant = FA["InsertFields"][
          tops,
          faProcess,
          FA["Model"] -> uvModel,
          FA["GenericModel"] -> genericModel,
          FA["InsertionLevel"] -> {FA["Particles"]},
          FA["ExcludeParticles"] -> Values[smFields]
        ];

        If[TrueQ[onlyRelevantDiagrams],

          ignoredTops = Missing["NotGenerated"];
          all = Missing["NotGenerated"];
          ignored = Missing["NotGenerated"],

          ignoredTops = FA["DiagramSelect"][
            tops,
            Count[#, FA["Propagator"][FA["Internal"]][__], Infinity] > 0 &
          ];

          all = FA["InsertFields"][
            tops,
            faProcess,
            FA["Model"] -> uvModel,
            FA["GenericModel"] -> genericModel,
            FA["InsertionLevel"] -> {FA["Particles"]}
          ];

          ignored = FA["InsertFields"][
            ignoredTops,
            faProcess,
            FA["Model"] -> uvModel,
            FA["GenericModel"] -> genericModel,
            FA["InsertionLevel"] -> {FA["Particles"]},
            FA["ExcludeParticles"] -> bsmFields
          ];
        ];

        {tops, ignoredTops, relevant, all, ignored}
      ],
      False
    ];


  If[TrueQ[onlyRelevantDiagrams],

    Join[
      entry,
      <|
        "Topologies" -> topologies,
        "RelevantDiagrams" -> relevantDiagrams,
        "AmplitudeGD" -> relevantDiagrams
      |>
    ],

    Join[
      entry,
      <|
        "Topologies" -> topologies,
        "IgnoredTopologies" -> ignoredTopologies,
        "RelevantDiagrams" -> relevantDiagrams,
        "AllDiagrams" -> allDiagrams,
        "IgnoredDiagrams" -> ignoredDiagrams,
        "AmplitudeGD" -> relevantDiagrams
      |>
    ]
  ]
];

Options[GenerateDiags] = Options[GenerateDiagsForProcess];

GenerateDiags[amp_Association, opts:OptionsPattern[]] := Module[
  {uvModel, genericModel, smFields, bsmFields, allProcesses, newProcesses,onlyRelevantDiagrams},

  uvModel = OptionValue[UVModel];
  genericModel = Replace[OptionValue[FA["GenericModel"]], Automatic :> uvModel];
  smFields = Replace[OptionValue[SMFields], Automatic :> $SMFieldMap];
  bsmFields = Replace[OptionValue[BSMFields], Automatic :> $BSMFields];
  onlyRelevantDiagrams = OptionValue[OnlyRelevantDiagrams];

  If[uvModel === Automatic,
    Message[GenerateDiags::noModel];
    Return[$Failed]
  ];

  allProcesses = Lookup[amp, "AllProcesses", $Failed];
  If[!AssociationQ[allProcesses],
    Message[GenerateDiags::noProcesses];
    Return[$Failed]
  ];



  newProcesses = Association @ KeyValueMap[
    (#1 -> GenerateDiagsForProcess[
      #2,
      UVModel -> uvModel,
      FA["GenericModel"] -> genericModel,
      SMFields -> smFields,
      BSMFields -> bsmFields,
      OnlyRelevantDiagrams -> onlyRelevantDiagrams
    ]) &,
    allProcesses
  ];

  If[MemberQ[Values[newProcesses], $Failed], Return[$Failed]];


  Join[
    amp,
    <|
      "UVModel" -> uvModel,
      "GenericModel" -> genericModel,
      "SMFields" -> smFields,
      "BSMFields" -> bsmFields,
      "AllProcesses" -> newProcesses
    |>
  ]
];
