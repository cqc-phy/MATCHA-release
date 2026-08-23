(* ::Package:: *)


ClearAll[FA, FC, MATCHAConfig, GetOutputDirectory, MATCHADebugPrint, MATCHADefaultOutputDirectory];

(* Context creation of FeynArts and FormCalc *)

FA[name_String] := Symbol["FeynArts`" <> name];
FC[name_String] := Symbol["FormCalc`" <> name];

MATCHADefaultOutputDirectory[] := Quiet @ Check[NotebookDirectory[], Directory[]];

$MATCHARoutesConfig = <|
  "FeynArtsRoute" -> "/home/carlos/PROGS/FeynArts-3.11",
  "FormCalcRoute" -> "/home/carlos/PROGS/FormCalc-9.8",
  "ModelPath" -> "/home/carlos/MATCHA-release-main/models",
  "OutputDirectory" -> MATCHADefaultOutputDirectory[]
|>;

$SMFieldMap = <||>;
$BSMFields = {};
$SMParams = <|
  "HiggsMass" -> mh,
  "SMvacuum" -> v,
  "WMass" -> Mw,
  "QuarkMass" -> Mq
|>;
$MatchConfig = <||>;
$HEFTExpressions = <||>;
$GeneratedDiagrams = <||>;
$ExportedDiagrams = <||>;

MATCHAConfig[] := $MATCHARoutesConfig;
GetOutputDirectory[] := $MATCHARoutesConfig["OutputDirectory"];
MATCHADebugPrint[file_String, msg_, debug_] := If[TrueQ[debug], Print["[", file, "] ", msg]];
