(* ::Package:: *)


BeginPackage["RandomData`"]

RandomData::usage="RandomData[] to get random numbers between 0 and 1 with homogeneous distribution";

RandomExp::usage="RandomExp[rate] to get random numbers of specific rate with exponencial distribution";
Begin["`private`"]

Module[{RndNumber = 4406,a=314159269,c=453806245,m=2^31},
RandomData[]:=N[(RndNumber =Mod[(a RndNumber+c),m])/(m-1)];
RandomExp[rate_]:=(-Log[RandomData[]]/rate);
GetRndNumber[]:=RndNumber
]
End[]
EndPackage[]
