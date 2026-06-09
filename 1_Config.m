(* ::Package:: *)


ClearAll[FA, FC, MATCHAConfig, GetOutputDirectory, MATCHADebugPrint, MATCHADefaultOutputDirectory];

(* Context creation of FeynArts and FormCalc *)

FA[name_String] := Symbol["FeynArts`" <> name];
FC[name_String] := Symbol["FormCalc`" <> name];

MATCHADefaultOutputDirectory[] := Quiet @ Check[NotebookDirectory[], Directory[]];

$MATCHARoutesConfig = <|
  "FeynArtsRoute" -> "/home/carlos/FEYNTUTOR/PROGS/FeynArts-3.11",
  "FormCalcRoute" -> "/home/carlos/FEYNTUTOR/PROGS/FormCalc-9.8",
  "ModelPath" -> FileNameJoin[{"/home/carlos/FEYNTUTOR/PROGS/FeynArts-3.11", "Models"}],
  "OutputDirectory" -> MATCHADefaultOutputDirectory[]
|>;

$SMFieldMap = <||>;
$BSMFields = {};
$SMParams = <|
  "HiggsMass" -> mh,
  "SMvacuum" -> v,
  "WMass" -> Mw,
  "TopMass" -> Mt
|>;
$MatchConfig = <||>;
$HEFTExpressions = <||>;
$GeneratedDiagrams = <||>;
$ExportedDiagrams = <||>;

MATCHAConfig[] := $MATCHARoutesConfig;
GetOutputDirectory[] := $MATCHARoutesConfig["OutputDirectory"];
MATCHADebugPrint[file_String, msg_, debug_] := If[TrueQ[debug], Print["[", file, "] ", msg]];
