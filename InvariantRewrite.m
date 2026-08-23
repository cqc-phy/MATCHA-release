(* ::Package:: *)


ClearAll[
  MandelstamSymbol, NormalizeInvariantHeads, MomentumEliminationRules,
  RewriteInvariants, RewriteInvariantsForProcess, RewriteInvariantsObject,
  momentumIndex, pairToInvariant
];

RewriteInvariantsForProcess::missingAmplitude =
  "The process entry does not contain \"UVAmplitudeDiracExtracted\".";

RewriteInvariantsObject::noProcesses =
  "The input object does not contain a valid \"AllProcesses\" association.";

Options[RewriteInvariants] = {
  SimplifyResult -> True
};

Options[RewriteInvariantsForProcess] = {
  SimplifyResult -> True
};

Options[RewriteInvariantsObject] = Options[RewriteInvariantsForProcess];



momentumIndex[x_] := Module[{s, digits},
  s = ToString[Unevaluated[x], InputForm];
  digits = StringCases[s, DigitCharacter..];
  If[digits === {}, Missing["NoMomentumIndex"], ToExpression[Last[digits]]]
];

pairToInvariant[a_, b_] := Module[{ia, ib, inv},
  ia = momentumIndex[a];
  ib = momentumIndex[b];
  inv = MandelstamSymbol[ia, ib];
  If[MissingQ[inv], FC["Pair"][a, b], inv/2]
];


NormalizeInvariantHeads[expr_] := expr /. {
  HoldPattern[MATCHA`Private`S12] :> MandelstamSymbol["S12"],
  HoldPattern[MATCHA`Private`T13] :> MandelstamSymbol["T13"],
  HoldPattern[MATCHA`Private`T23] :> MandelstamSymbol["T23"]
};
(* ===================================================*)

FCPrivateSymbol[name_String] := Symbol["FormCalc`" <> name];

FCK[i_Integer] := FCPrivateSymbol["k"][i];

FCPair[i_Integer, j_Integer] := 
  FCPrivateSymbol["Pair"][FCK[i], FCK[j]];

FCPair[x_, j_Integer] := FCPrivateSymbol["Pair"][x, FCK[j]];

FCPair[i_Integer, x_] := FCPrivateSymbol["Pair"][FCK[i], x];

FCPair[x_, y_] := FCPrivateSymbol["Pair"][x, y];

MandelstamSymbol["S", i_Integer, j_Integer] := 
  FCPrivateSymbol["S" <> ToString[i] <> ToString[j]];

MandelstamSymbol["T", i_Integer, j_Integer] := 
  FCPrivateSymbol["T" <> ToString[i] <> ToString[j]];


NormalizeInvariantHeads[expr_] := 
  expr //. {HoldPattern[
       pairHead_Symbol[kHead1_Symbol[i_Integer], 
        kHead2_Symbol[j_Integer]]] /; 
      SymbolName[Unevaluated[pairHead]] === "Pair" && 
       SymbolName[Unevaluated[kHead1]] === "k" && 
       SymbolName[Unevaluated[kHead2]] === "k" :> FCPair[i, j], 
    HoldPattern[pairHead_Symbol[x_, kHead_Symbol[j_Integer]]] /; 
      SymbolName[Unevaluated[pairHead]] === "Pair" && 
       SymbolName[Unevaluated[kHead]] === "k" :> FCPair[x, j], 
    HoldPattern[pairHead_Symbol[kHead_Symbol[i_Integer], x_]] /; 
      SymbolName[Unevaluated[pairHead]] === "Pair" && 
       SymbolName[Unevaluated[kHead]] === "k" :> FCPair[i, x]};

(* ===================================================*)

MomentumEliminationRules[n_Integer?Positive] := 
  Module[{lastMomentumIndex, previousOutgoingIndices}, 
   lastMomentumIndex = n + 2;
   previousOutgoingIndices = Range[3, n + 1];
   With[{last = lastMomentumIndex, prevOut = previousOutgoingIndices, 
     pair = FCPrivateSymbol["Pair"], 
     kk = FCPrivateSymbol["k"]}, {HoldPattern[
       pair[kk[last], kk[last]]] :> 
      pair[kk[1], kk[last]] + pair[kk[2], kk[last]] - 
       Sum[pair[kk[outIndex], kk[last]], {outIndex, prevOut}], 
     HoldPattern[pair[x_, kk[last]]] :> 
      pair[x, kk[1]] + pair[x, kk[2]] - 
       Sum[pair[x, kk[outIndex]], {outIndex, prevOut}], 
     HoldPattern[pair[kk[last], x_]] :> 
      pair[kk[1], x] + pair[kk[2], x] - 
       Sum[pair[kk[outIndex], x], {outIndex, prevOut}]}]];

(* ===================================================*)
PairToMandelRules[n_Integer?Positive] := 
  Module[{maxMomentumIndex}, maxMomentumIndex = n + 1;
   Join[{With[{pair = FCPrivateSymbol["Pair"], k1 = FCK[1], 
       k2 = FCK[2]}, 
      HoldPattern[pair[k1, k2]] :> 
       1/2 (MandelstamSymbol["S", 1, 2] - FCPair[1, 1] - 
          FCPair[2, 2])]}, 
    Table[With[{pair = FCPrivateSymbol["Pair"], k1 = FCK[1], 
       kj = FCK[j], jIndex = j}, 
      HoldPattern[pair[k1, kj]] :> 
       1/2 (FCPair[1, 1] + FCPair[jIndex, jIndex] - 
          MandelstamSymbol["T", 1, jIndex])], {j, 3, 
      maxMomentumIndex}], 
    Table[With[{pair = FCPrivateSymbol["Pair"], k2 = FCK[2], 
       kj = FCK[j], jIndex = j}, 
      HoldPattern[pair[k2, kj]] :> 
       1/2 (FCPair[2, 2] + FCPair[jIndex, jIndex] - 
          MandelstamSymbol["T", 2, jIndex])], {j, 3, 
      maxMomentumIndex}], 
    Flatten[Table[
      With[{pair = FCPrivateSymbol["Pair"], ki = FCK[i], kj = FCK[j], 
        iIndex = i, jIndex = j}, 
       HoldPattern[pair[ki, kj]] :> 
        1/2 (MandelstamSymbol["S", iIndex, jIndex] - 
           FCPair[iIndex, iIndex] - FCPair[jIndex, jIndex])], {i, 3, 
       maxMomentumIndex - 1}, {j, i + 1, maxMomentumIndex}]]]];

RewriteInvariants[expr_, n_Integer?Positive,opts:OptionsPattern[]] := Module[
  {rewritten},

  If[expr === $Failed, Return[$Failed]];
  orderRule = {HoldPattern[
      pairHead_Symbol[kHead_Symbol[i_Integer], 
       kHead_Symbol[j_Integer]]] /; 
     SymbolName[Unevaluated[pairHead]] === "Pair" && 
      SymbolName[Unevaluated[kHead]] === "k" && i > j :> FCPair[j, i]};
momRules = MomentumEliminationRules[n];
mandelRules = PairToMandelRules[n];
  rewritten = NormalizeInvariantHeads[expr] //. orderRule //. momRules //. orderRule //. mandelRules //. mandelRules ;
  rewritten = Expand[rewritten];

  If[TrueQ[OptionValue[SimplifyResult]], rewritten = Simplify[rewritten]];

  rewritten
];

RewriteInvariantsForProcess[entry_Association, opts:OptionsPattern[]] := Module[
  {amp, rewritten},



  If[!KeyExistsQ[entry, "UVAmplitudeDiracExtracted"],
    Message[RewriteInvariantsForProcess::missingAmplitude];
    Return[Join[entry, <|"UVAmplitudeInvariant" -> $Failed|>]]
  ];

  amp = entry["UVAmplitudeDiracExtracted"];


  higgsOrder = Lookup[entry, "HiggsOrder", $Failed];
  rewritten = RewriteInvariants[
    amp,higgsOrder,
    SimplifyResult -> OptionValue[SimplifyResult]
  ];



  Join[entry, <|"UVAmplitudeInvariant" -> rewritten|>]
];

RewriteInvariantsObject[ampObj_Association, opts:OptionsPattern[]] := Module[
  {allProcesses, newProcesses},


  allProcesses = Lookup[ampObj, "AllProcesses", $Failed];

  If[!AssociationQ[allProcesses],
    Message[RewriteInvariantsObject::noProcesses];
    Return[$Failed]
  ];

  newProcesses = Association @ KeyValueMap[
    #1 -> RewriteInvariantsForProcess[#2, opts]&,
    allProcesses
  ];



  Join[ampObj, <|"AllProcesses" -> newProcesses|>]
];
