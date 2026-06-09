(* ::Package:: *)



ClearAll[ProcessUVAmplitudeForProcess, ProcessUVAmplitudes];

ProcessUVAmplitudeForProcess::missingRawAmplitude =
  "The process entry does not contain \"UVAmplitudeRaw\".";

ProcessUVAmplitudes::noProcesses =
  "The input object does not contain a valid \"AllProcesses\" association.";



Options[ProcessUVAmplitudeForProcess] = {};

Options[ProcessUVAmplitudes] = Options[ProcessUVAmplitudeForProcess];


ProcessUVAmplitudeForProcess[entry_Association, opts:OptionsPattern[]] := Module[
  {raw, processed, tmp, family},


  If[!KeyExistsQ[entry, "UVAmplitudeRaw"],
    Message[ProcessUVAmplitudeForProcess::missingRawAmplitude];
    Return[Join[entry, <|"UVAmplitudeProcessed" -> $Failed|>]]
  ];

  raw = entry["UVAmplitudeRaw"];
  family = entry["Family"];



  

  processed = If[
    raw === $Failed,
    $Failed,
    withPackageOutputControl[
        FC["ClearProcess"][];
        tmp = FC["CalcFeynAmp"][
          raw,
          FC["OnShell"] -> False,
          FC["Invariants"] -> False
        ];
      If[
        tmp === $Failed,
        $Failed,
        If[
          family === "GaugeHiggs",
          tmp = FC["UnAbbr"][tmp] /. {
            FC["Pair"][Except[FC["e"][1]], Except[FC["e"][2]]] -> 0
          }
        ];

        If[
          family === "PureHiggs",
          tmp = FC["UnAbbr"][tmp] /. {
            FC["Den"][x_, y_] :> (-1/y) (1 + x/y)
          }
        ];

        tmp = FC["UnAbbr"][tmp[[1]]] /. {
          FC["Den"][x_, y_] :> 1/(x - y)
        };

        tmp
      ],
      False
    ]
  ];

  



  Join[entry, <|"UVAmplitudeProcessed" -> processed|>]
];


ProcessUVAmplitudes[ampObj_Association, opts:OptionsPattern[]] := Module[
  {allProcesses, newProcesses},



  allProcesses = Lookup[ampObj, "AllProcesses", $Failed];

  If[!AssociationQ[allProcesses],
    Message[ProcessUVAmplitudes::noProcesses];
    Return[$Failed]
  ];



  newProcesses = Association @ KeyValueMap[
    #1 -> ProcessUVAmplitudeForProcess[
      #2
    ]&,
    allProcesses
  ];


  Join[ampObj, <|"AllProcesses" -> newProcesses|>]
];
