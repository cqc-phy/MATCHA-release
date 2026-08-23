(* ::Package:: *)


If[NameQ["MATCHA`GenericModel"], Quiet[Remove["MATCHA`GenericModel"]]];

BeginPackage["MATCHA`"];

LoadTools::usage = "LoadTools[] loads FeynArts and FormCalc using the routes configured in Config.m.";
SetSMFields::usage ="SetSMFields[assoc] sets the SM field map. The input may be an Association or a list of rules. The allowed keys are \"Higgs\", \"GaugeCharged\", \"GaugeNeutral\", \"GoldstoneCharged\", \"GoldstoneNeutral\" and \"Quark\". The value assigned to each key must be of the form {\"S\", n}, {\"V\", m}, {\"F\", k}, or {\"F\", k, {gen}}, specifying the corresponding FeynArts field type and index defined by the user.";
SetBSMFields::usage ="SetBSMFields[fieldList] defines the mapping of the heavy BSM fields to be integrated out. The input fieldList must be a list containing entries of the form {\"S\", n}, {\"V\", m}, {\"F\", k}, {\"F\", k, {gen}}, or {\"U\", r}, where the first element specifies the field type and the second one specifies the corresponding FeynArts index defined by the user.";
SetSMParams::usage ="SetSMParams[assoc] sets the SM parameter map. The input may be an Association or a list of rules, and may contain any subset of the keys \"HiggsMass\", \"SMvacuum\", \"WMass\" and \"QuarkMass\". For Yukawa matching, \"QuarkMass\" should be set to the mass of the selected quark. Otherwise, the generic Mq may appear in the result.";
MatchToHEFT::usage ="MatchToHEFT[modelModName, modelGenName, higgsOrderMax, massList] runs the matching of the model modelModName onto HEFT up to Higgs order higgsOrderMax. The argument modelGenName specifies the corresponding .gen file, and massList contains the masses associated with the heavy fields specified in SetBSMFields. If the .gen file has the same name as the .mod file, the short form MatchToHEFT[modelModName, higgsOrderMax, massList] can be used.";
MATCHAConfig::usage = "MATCHAConfig[] returns the current MATCHA route/configuration association.";
GetOutputDirectory::usage = "GetOutputDirectory[] returns the configured output directory.";

Global`Results::usage ="Results[\"coupling\"] returns the final matching expression for the HEFT coupling specified by \"coupling\" in the canonical HEFT basis.";

Global`MATCHADiagrams::usage ="MATCHADiagrams[\"coupling\"] returns the diagram information associated with the HEFT coupling specified by \"coupling\". The entries \"RelevantDiagrams\", \"AllDiagrams\" and \"IgnoredDiagrams\" access the corresponding diagram categories.";

Config::usage = "Config is an option for LoadTools.";
EFTorder::usage = "EFTorder is the HEFT counting order.";
SMFields::usage = "SMFields is an option carrying the SM field association.";
SMParams::usage = "SMParams is an option carrying the SM parameter association.";
BSMFields::usage = "BSMFields is an option carrying the BSM field list.";
UVModel::usage = "UVModel is an option carrying the FeynArts .mod model name as a string.";
OutputDirectory::usage = "OutputDirectory is an option controlling where exported files are written.";
SimplifyResult::usage = "SimplifyResult is an option used by later stages.";
VerboseMode::usage = "VerboseMode controls standard package-loading messages.";
ExportDiagrams::usage = "ExportDiagrams is both the diagram-exporting stage and the MatchToHEFT option controlling whether diagrams are exported.";
HeavyMassList::usage = "HeavyMassList is an internal option carrying the heavy masses used in the expansion stage.";
AlternativeBasis::usage ="AlternativeBasis is an option of MatchToHEFT. If True, MatchToHEFT also exports the coefficient solutions before the final field redefinition, including the p-coefficients.";
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

Get[FileNameJoin[{$MATCHAPackageDirectory, "Config.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "Tools.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "SetUp.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "HEFTExpressions.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "GenerateDiags.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "Export.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "UVAmplitudes.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "UVProcessing.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "Expansion.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "DiracExtraction.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "InvariantRewrite.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "MatchingObject.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "Solver.m"}]];
Get[FileNameJoin[{$MATCHAPackageDirectory, "Redefinitions.m"}]];
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
        {600, 250}
      ]
    ],
    Print["Logo image could not be loaded."]
  ]
];

End[];
EndPackage[];