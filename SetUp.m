(* ::Package:: *)

ClearAll[toFeynArtsField, convertFieldRules];

SetSMFields::invalidKeys =
  "Allowed SetSMFields keys are: Higgs, GaugeCharged, GaugeNeutral, GoldstoneCharged, GoldstoneNeutral and Quark.";
SetSMParams::invalidKeys ="Unknown SetSMParams keys: `1`. Allowed keys are: HiggsMass, SMvacuum, WMass and QuarkMass.";
SetSMFields::invalidField = "Invalid field. Use {\"S\", n}, {\"V\", n}, {\"F\", n} or {\"F\", n, {gen}}.";
SetBSMFields::invalidInput = "SetBSMFields must receive a list of fields of the form {\"S\", n}, {\"V\", n}, {\"F\", n}, {\"U\", n}, or {\"F\", n, {gen}}.";


toFeynArtsField[{head_String, n_Integer}] := Switch[
  head,
  "S", FA["S"][n],
  "V", FA["V"][n],
  "F", FA["F"][n],
  "U", FA["U"][n],
  _, Message[SetSMFields::invalidField, {head, n}]; $Failed
];


toFeynArtsField[{head_String, n_Integer, gen_List}] := Switch[
  head,
  "F", FA["F"][n, gen],
  _, Message[SetSMFields::invalidField, {head, n, gen}]; $Failed
];

toFeynArtsField[x_] := (Message[SetSMFields::invalidField, x]; $Failed);

convertFieldRules[assoc_Association] := Association @ KeyValueMap[#1 -> toFeynArtsField[#2] &, assoc];

SetSMFields[assoc_Association] := Module[{allowedKeys, converted},
   allowedKeys = {"Higgs", "GaugeCharged", "GaugeNeutral","GoldstoneCharged", "GoldstoneNeutral", "Quark"};

  If[!SubsetQ[allowedKeys, Keys[assoc]],
    Message[SetSMFields::invalidKeys];
    Return[$Failed]
  ];

  converted = convertFieldRules[assoc];
  If[MemberQ[Values[converted], $Failed], Return[$Failed]];

  If[!AssociationQ[$SMFieldMap], $SMFieldMap = <||>];
  $SMFieldMap = converted;

  If[AssociationQ[$MatchConfig], $MatchConfig["SMFields"] = $SMFieldMap];

  Print["SM fields set to: ", $SMFieldMap];
  $SMFieldMap
];

SetSMFields[rules_List] := SetSMFields[Association[rules]];

SetBSMFields[fields_List] := Module[{converted},
  converted = toFeynArtsField /@ fields;
  If[MemberQ[converted, $Failed], Message[SetBSMFields::invalidInput]; Return[$Failed]];

  $BSMFields = converted;
  If[AssociationQ[$MatchConfig], $MatchConfig["BSMFields"] = $BSMFields];

  Print["BSM fields set to: ", $BSMFields];
  $BSMFields
];

SetSMParams[assoc_Association] := Module[
  {allowedKeys, unknownKeys},

  allowedKeys = {"HiggsMass", "SMvacuum", "WMass", "QuarkMass"};
  unknownKeys = Complement[Keys[assoc], allowedKeys];

  If[unknownKeys =!= {},
    Message[SetSMParams::invalidKeys, unknownKeys];
    Return[$Failed]
  ];

  $SMParams = assoc;

  If[AssociationQ[$MatchConfig],
    $MatchConfig["SMParams"] = $SMParams
  ];

  Print["SM parameters set to: ", $SMParams];
  $SMParams
];

SetSMParams[rules_List] := SetSMParams[Association[rules]];

(* Default SM parameters if the user does not call SetSMParams *)
$SMParams = <|
  "HiggsMass" -> Global`m,
  "SMvacuum" -> Global`v,
  "WMass" -> Global`Mw,
  "QuarkMass" -> Global`Mq
|>;

If[AssociationQ[$MatchConfig],
  $MatchConfig["SMParams"] = $SMParams
];
