(* ::Package:: *)


If[NameQ["MATCHA`GenericModel"], Quiet[Remove["MATCHA`GenericModel"]]];

BeginPackage["MATCHA`"];

LoadTools::usage = "LoadTools[] loads FeynArts and FormCalc using the routes configured in Config.m.";
SetSMFields::usage = "SetSMFields[assoc] sets the SM field map. The keys must be Higgs, GaugeCharged, GaugeNeutral, GoldstoneCharged and Top.";
SetBSMFields::usage = "SetBSMFields[list] sets the list of BSM fields.";
SetSMParams::usage = "SetSMParams[assoc] sets the SM parameter map. The keys must be HiggsMass, SMvacuum, WMass and TopMass.";
MatchToHEFT::usage = "MatchToHEFT[modelName, higgsOrderMax, heavyMasses] runs MATCHA through matching, redefinitions and returns the final matching object. Use ExportDiagrams -> True to export diagram PDFs.";
MATCHAConfig::usage = "MATCHAConfig[] returns the current MATCHA route/configuration association.";
GetOutputDirectory::usage = "GetOutputDirectory[] returns the configured output directory.";

Config::usage = "Config is an option for LoadTools.";
EFTorder::usage = "EFTorder is the HEFT counting order.";
SMFields::usage = "SMFields is an option carrying the SM field association.";
SMParams::usage = "SMParams is an option carrying the SM parameter association.";
BSMFields::usage = "BSMFields is an option carrying the BSM field list.";
UVModel::usage = "UVModel is an option carrying the FeynArts .mod model name as a string.";
OutputDirectory::usage = "OutputDirectory is an option controlling where exported files are written.";
SimplifyResult::usage = "SimplifyResult is an option used by later stages.";
Verbose::usage = "Verbose controls standard MATCHA status messages.";
VerboseMode::usage = "VerboseMode controls standard package-loading messages.";
ExportDiagrams::usage = "ExportDiagrams is both the diagram-exporting stage and the MatchToHEFT option controlling whether diagrams are exported.";
HeavyMassList::usage = "HeavyMassList is an internal option carrying the heavy masses used in the expansion stage.";
AlternateBasis::usage ="AlternateBasis is an option of MatchToHEFT. If True, MatchToHEFT also exports the coefficient solutions before the final field redefinition, including the p-coefficients.";
OnlyRelevantDiagrams::usage ="OnlyRelevantDiagrams is an option of MatchToHEFT. If True, only relevant diagrams are generated and exported; AllDiagrams and IgnoredDiagrams are skipped.";


$MATCHARoutesConfig::usage = "$MATCHARoutesConfig stores FeynArts, FormCalc, model and output routes.";
$SMFieldMap::usage = "$SMFieldMap stores the SM field map set by SetSMFields.";
$BSMFields::usage = "$BSMFields stores the BSM field list set by SetBSMFields.";
$SMParams::usage = "$SMParams stores SM parameters set by SetSMParams.";
$MatchConfig::usage = "$MatchConfig stores the latest MatchToHEFT configuration.";
$HEFTExpressions::usage = "$HEFTExpressions stores the latest HEFT expression object.";
$GeneratedDiagrams::usage = "$GeneratedDiagrams stores the latest diagram object.";
$ExportedDiagrams::usage = "$ExportedDiagrams stores the latest exported diagram object.";
$UVAmplitudes::usage = "$UVAmplitudes stores the latest raw UV amplitude object.";
$ProcessedUVAmplitudes::usage = "$ProcessedUVAmplitudes stores the latest processed UV amplitude object.";
$ExpandedUVAmplitudes::usage = "$ExpandedUVAmplitudes stores the latest heavy-mass-expanded amplitude object.";
$DiracExtractedAmplitudes::usage = "$DiracExtractedAmplitudes stores the latest Dirac-extracted amplitude object.";
$InvariantRewrittenAmplitudes::usage = "$InvariantRewrittenAmplitudes stores the latest invariant-rewritten amplitude object.";
$MatchingObject::usage = "$MatchingObject stores the latest matching object.";
$SolvedMatchingObject::usage = "$SolvedMatchingObject stores the latest solved matching object.";
$RedefinedMatchingObject::usage = "$RedefinedMatchingObject stores the latest object after field/coupling redefinitions.";
$FinalMatchingResults::usage = "$FinalMatchingResults stores the latest final matching rules after redefinitions.";


Begin["`Private`"];

$MATCHAPackageDirectory = DirectoryName[$InputFileName];

Get[FileNameJoin[{$MATCHAPackageDirectory, "1_Config.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "2_Tools.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "3_SetUp.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "4_HEFTExpressions.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "5_GenerateDiags.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "6_Export.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "7_UVAmplitudes.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "8_UVProcessing.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "9_Expansion.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "10_DiracExtraction.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "11_InvariantRewrite.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "12_MatchingObject.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "13_Solver.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "14_Redefinitions.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "MatchToHEFT.m"}]];

Print["=============================================="];
Print[Style["MATCHA", Bold]];
Print[Style["MATChing H(EFT) Amplitudes", Italic]];
Print["by Raquel Gómez-Ambrosio and Carlos Quezada Calonge"];
Print["Version: MATCHA 1.0"];
Print["============================================="];

Quiet[
  Check[
    Print[
      ImageResize[
        Import[
          FileNameJoin[{$MATCHAPackageDirectory, "logomatcha.png"}]
        ],
        {1000, 400}
      ]
    ],
    Print["Logo image could not be loaded."]
  ]
];

End[];
EndPackage[];