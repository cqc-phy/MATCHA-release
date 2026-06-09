(* ::Package:: *)

ClearAll[
  validEFTOrderQ, HEFTSymbol, HEFTPVariable, SelectHEFTOrders,
  BuildHEFTProcessEntry, DummyNLOGaugeAmplitude, DummyNLOScalarAmplitude,
  DummyNLOFermionAmplitude, hhContactFullAmp,
  GaugeHiggsChargedDefinition, GaugeHiggsNeutralDefinition,
  PureHiggsDefinition, FermionHiggsDefinition,
  BuildGaugeHiggsDefinitions, BuildPureHiggsDefinitions,
  BuildFermionHiggsDefinitions, BuildHEFTProcessDefinitions,
  BuildHEFTCatalog, AmpMatrixMatcha
];

BuildHEFTProcessEntry::badOrder = "EFTorder must be 2 for LO.";
BuildHEFTProcessDefinitions::missingSetup = "SM fields and SM parameters must be set before building HEFT process definitions. Use SetSMFields[...].";

validEFTOrderQ[x_] := MemberQ[{2, 4}, x];


HEFTSymbol[prefix_String, n_Integer] := ToExpression["MATCHA`" <> prefix <> ToString[n]];
HEFTPVariable["PureHiggs", n_Integer] := ToExpression["MATCHA`p" <> ToString[n]];
HEFTPVariable[_, _] := Nothing;

SelectHEFTOrders[orderData_Association, eftOrder_Integer] :=
  Sort @ Select[Keys[orderData], # <= eftOrder &];

BuildHEFTProcessEntry[family_, process_, orderData_Association, eftOrder_Integer] := Module[
  {orders, vars, amp, higgsOrder},
  If[!validEFTOrderQ[eftOrder],
    Message[BuildHEFTProcessEntry::badOrder, eftOrder];
    Return[$Failed]
  ];

  orders = SelectHEFTOrders[orderData, eftOrder];
  If[orders === {}, Return[$Failed]];

  vars = DeleteDuplicates @ Flatten[(orderData[#]["Variables"] &) /@ orders];
  amp = Total[(orderData[#]["HEFTAmplitude"] &) /@ orders];
  higgsOrder = Length[process[[2]]];

  <|
    "Family" -> family,
    "Process" -> process,
    "HiggsOrder" -> higgsOrder,
    "Variables" -> vars,
    "HEFTAmplitude" -> amp,
    "EFTorder" -> eftOrder,
    "IncludedOrders" -> orders
  |>
];

DummyNLOGaugeAmplitude[process_] :=
  FC["Pair"][FC["e"][1], FC["e"][2]] FC["Pair"][FC["k"][1], FC["k"][2]];

DummyNLOScalarAmplitude[process_] := FC["Pair"][FC["k"][1], FC["k"][2]];
DummyNLOFermionAmplitude[process_] := FC["Pair"][FC["k"][1], FC["k"][2]];

hhContactFullAmp[n_Integer?Positive, mh_, v_] /; n >= 2 := Module[
  {p, d, invs, potFactor, kineticTerm, potentialTerm},

  p = HEFTPVariable["PureHiggs", n];
  d = HEFTSymbol["d", n + 2];
  potFactor = If[n === 2, 1/8, 1];

  invs = FC["S12"];
  invs += FC["T13"];
  invs += FC["T23"];
  invs += Sum[FC["T1" <> ToString[j]] + FC["T2" <> ToString[j]], {j, 4, n + 1}];
  invs += Sum[FC["S" <> ToString[i] <> ToString[j]], {i, 3, n}, {j, i + 1, n + 1}];

  kineticTerm = p/v^n (invs - (n - 2) Sum[FC["Pair"][FC["k"][i], FC["k"][i]], {i, 1, n + 1}]);
  potentialTerm =  potFactor ((n + 1) (n + 2) d mh^2 )/v^n;

  Factorial[n] (kineticTerm - potentialTerm)
];

GaugeHiggsChargedDefinition[n_Integer] := Module[
  {H, W, v, Mw, an, anNLO, process},

  H = $SMFieldMap["Higgs"];
  W = $SMFieldMap["GaugeCharged"];
  v = $SMParams["SMvacuum"];
  Mw = $SMParams["WMass"];
  an = HEFTSymbol["a", n];
  anNLO = HEFTSymbol["aNLO", n];
  process = {W, -W} -> ConstantArray[H, n];

  <|
    "Family" -> "GaugeHiggsCharged",
    "Process" -> process,
    "HiggsOrder" -> n,
    "Orders" -> <|
      2 -> <|"Variables" -> {an}, "HEFTAmplitude" -> Factorial[n] an Mw^2/v^n FC["Pair"][FC["e"][1], FC["e"][2]]|>,
      4 -> <|"Variables" -> {anNLO}, "HEFTAmplitude" -> anNLO/v^(n + 2) DummyNLOGaugeAmplitude[process]|>
    |>
  |>
];

GaugeHiggsNeutralDefinition[n_Integer] := Module[
  {H, Z, v, Mw, an, zNLO, process},

  H = $SMFieldMap["Higgs"];
  Z = $SMFieldMap["GaugeNeutral"];
  v = $SMParams["SMvacuum"];
  Mw = $SMParams["WMass"];
  an = HEFTSymbol["aZ", n];
  zNLO = HEFTSymbol["zNLO", n];
  process = {Z, Z} -> ConstantArray[H, n];

  <|
    "Family" -> "GaugeHiggsNeutral",
    "Process" -> process,
    "HiggsOrder" -> n,
    "Orders" -> <|
      2 -> <|"Variables" -> {an}, "HEFTAmplitude" -> factorZ Factorial[n] an Mw^2/v^n FC["Pair"][FC["e"][1], FC["e"][2]]|>,
      4 -> <|"Variables" -> {zNLO}, "HEFTAmplitude" -> zNLO/v^(n + 2) DummyNLOGaugeAmplitude[process]|>
    |>
  |>
];

PureHiggsDefinition[n_Integer] := Module[
  {H, v, mh, dn, pn, dnNLO, process, ampLO, varsLO},

  H = $SMFieldMap["Higgs"];
  v = $SMParams["SMvacuum"];
  mh = $SMParams["HiggsMass"];
  dn = HEFTSymbol["d", n + 2];
  pn = HEFTPVariable["PureHiggs", n];
  dnNLO = HEFTSymbol["dNLO", n + 2];
  process = {H, H} -> ConstantArray[H, n];

  varsLO = If[n === 1, {dn}, {dn, pn}];
  ampLO = If[n === 1,
    -1/2 Factorial[n + 2] dn mh^2/v^n,
    hhContactFullAmp[n, mh, v]
  ];

  <|
    "Family" -> "PureHiggs",
    "Process" -> process,
    "HiggsOrder" -> n,
    "Orders" -> <|
      2 -> <|"Variables" -> varsLO, "HEFTAmplitude" -> ampLO|>,
      4 -> <|"Variables" -> {dnNLO}, "HEFTAmplitude" -> dnNLO/v^(n + 2) DummyNLOScalarAmplitude[process]|>
    |>
  |>
];

FermionHiggsDefinition[n_Integer] := Module[
  {H, t, v, Mt, cn, cnNLO, process},

  H = $SMFieldMap["Higgs"];
  t = $SMFieldMap["Top"];
  v = $SMParams["SMvacuum"];
  Mt = $SMParams["TopMass"];
  cn = HEFTSymbol["c", n];
  cnNLO = HEFTSymbol["cNLO", n];
  process = {t, -t} -> ConstantArray[H, n];

  <|
    "Family" -> "FermionHiggs",
    "Process" -> process,
    "HiggsOrder" -> n,
    "Orders" -> <|
      2 -> <|"Variables" -> {cn}, "HEFTAmplitude" -> -Factorial[n] cn Mt/v^n|>,
      4 -> <|"Variables" -> {cnNLO}, "HEFTAmplitude" -> cnNLO/v^(n + 2) DummyNLOFermionAmplitude[process]|>
    |>
  |>
];

BuildPureHiggsDefinitions[nho_Integer] := Table[PureHiggsDefinition[n], {n, 1, nho}];

BuildGaugeHiggsDefinitions[nho_Integer] := Join[
  If[KeyExistsQ[$SMFieldMap, "GaugeCharged"], Table[GaugeHiggsChargedDefinition[n], {n, 1, nho}], {}],
  If[KeyExistsQ[$SMFieldMap, "GaugeNeutral"], Table[GaugeHiggsNeutralDefinition[n], {n, 1, nho}], {}]
];

BuildFermionHiggsDefinitions[nho_Integer] :=
  If[KeyExistsQ[$SMFieldMap, "Top"], Table[FermionHiggsDefinition[n], {n, 1, nho}], {}];

BuildHEFTProcessDefinitions[nho_Integer?Positive] := Module[{},
  If[!AssociationQ[$SMFieldMap] || !AssociationQ[$SMParams] || $SMFieldMap === <||> || $SMParams === <||>,
    Message[BuildHEFTProcessDefinitions::missingSetup];
    Return[$Failed]
  ];

  Join[
    BuildPureHiggsDefinitions[nho],
    BuildGaugeHiggsDefinitions[nho],
    BuildFermionHiggsDefinitions[nho]
  ]
];

Options[BuildHEFTCatalog] = {EFTorder -> 2};

BuildHEFTCatalog[defs_List, opts:OptionsPattern[]] := Module[
  {eftOrder, entries},

  eftOrder = OptionValue[EFTorder];
  entries = (BuildHEFTProcessEntry[# ["Family"], # ["Process"], # ["Orders"], eftOrder] &) /@ defs;
  entries = DeleteCases[entries, $Failed];

  Association @ MapIndexed[
    "Process" <> ToString[First[#2]] <> "_" <> #1["Family"] <> "_H" <> ToString[#1["HiggsOrder"]] -> #1 &,
    entries
  ]
];

Options[AmpMatrixMatcha] = {
  EFTorder -> 2,
  SMFields -> Automatic,
  SMParams -> Automatic,
  Verbose -> True
};

AmpMatrixMatcha[nho_Integer, opts:OptionsPattern[]] := Module[
  {eftOrder, smFields, smParams,, verbose, defs, heftCatalog},

  eftOrder = OptionValue[EFTorder];
  smFields = Replace[OptionValue[SMFields], Automatic :> $SMFieldMap];
  smParams = Replace[OptionValue[SMParams], Automatic :> $SMParams];
  verbose = OptionValue[Verbose];

  

  defs = BuildHEFTProcessDefinitions[nho];
  If[defs === $Failed, Return[$Failed]];

  heftCatalog = BuildHEFTCatalog[defs, EFTorder -> eftOrder];

  <|
    "HiggsOrder" -> nho,
    "EFTorder" -> eftOrder,
    "SMFields" -> smFields,
    "SMParams" -> smParams,
    "AllProcesses" -> heftCatalog
  |>
];
