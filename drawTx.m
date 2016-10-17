(* ::Package:: *)

BeginPackage["drawTx`"]
SetIniParDraw::usage="SetIniParDraw[propagation_time,ack_time] to configure parameters of transmission";

GetIniParDraw::usage="GetIniParDraw[] to get configure parameters of transmission {propagation_time,ack_time}";

DrawWin::usage="DrawWin[time_ini,width,module] to configure Window for drawing";

SelectPacketInWin::usage="SelectPacketInWin[arr_] to select packets within a Win";

DrawPacketTx::usage="DrawPacketTx[arr] to draw one packet transmission";

Begin["`private`"]

Module[{tack=0.05,speedline=9600,l=480,c=9600,tp=0.01,tw=0.0,ww=10,winMod=0,k=1/4,HiW=10/4 ,hiP=0.1,HiP=1/4,nSect=5},
SetIniParDraw[tP_,tAck_,speed_:9600]:=(tp=tP;tack=tAck;speedline=speed;);
GetIniParDraw[]:={tp,tack,speedline};
DrawLineInWin[u_,v_]:=
If[u[[1]]<=tw,
If[v[[1]]> tw,
Graphics[{Black,Dashed,Line[{{tw,u[[2]]+(v[[2]]-u[[2]])/(v[[1]]-u[[1]]) (tw-u[[1]])},{v[[1]],v[[2]]}}]}],Unevaluated[Sequence[]]]
,If[u[[1]]<=tw+ww,
If[v[[1]]>tw+ww,
Graphics[{Black,Dashed,Line[{{u[[1]],u[[2]]},{tw+ww,u[[2]]+(v[[2]]-u[[2]])/(v[[1]]-u[[1]]) (tw+ww-u[[1]])}}]}]
,Graphics[{Black,Dashed,Line[{{u[[1]],u[[2]]},{v[[1]],v[[2]]}}]}]
],Unevaluated[Sequence[]]
],Unevaluated[Sequence[]]
];

(*GetTextPack[arr_]:="N="<>ToString[arr[[3]]]<>If[arr[[4]]\[Equal]1,"\nError",""]<>"\nNrep="<>ToString[arr[[5]]];*)
GetTextPack[arr_]:=ToString[If[winMod==0,arr[[3]],Mod[arr[[3]],winMod]]];

DrawRectInWin[u_,v_,color_]:=
If[u[[1]]<=tw,
If[v[[1]]> tw+ww,
Graphics[{EdgeForm[Thin],color,Rectangle[{tw,u[[2]]},{tw+ww,v[[2]]}]}]
,If[v[[1]]> tw,
Graphics[{EdgeForm[Thin],color,Rectangle[{tw,u[[2]]},{v[[1]],v[[2]]}]}],Unevaluated[Sequence[]]],Unevaluated[Sequence[]]]
,If[u[[1]]<=tw+ww,
If[v[[1]]> tw+ww,
Graphics[{EdgeForm[Thin],color,Rectangle[{u[[1]],u[[2]]},{tw+ww,v[[2]]}]}]
,Graphics[{EdgeForm[Thin],color,Rectangle[{u[[1]],u[[2]]},{v[[1]],v[[2]]}]}]
],Unevaluated[Sequence[]]
],Unevaluated[Sequence[]]
];

DrawWin[t_,w_,WinMod_]:=(ww=w; HiW=ww k; HiP=HiW hiP; winMod=WinMod;Graphics[{Black,Line[{{tw=t,0},{tw+ww,0}}],Line[{{tw,HiW},{tw+ww,HiW}}]},AspectRatio->Automatic,Axes->{True,False},AxesOrigin->Automatic]);
Gettw[]:=Print[tw];
Getww:=Print[ww];
XInWin[x_]:=If[x>=tw && x<=tw+ww,True,False];
DrawTextInWin[arr_,nSEC_]:=
If[XInWin[a=(arr[[1]]+(arr[[2]]/speedline+tp)/nSEC)],
Graphics[Text[GetTextPack[arr],{a,HiW/nSEC}]]
,Unevaluated[Sequence[]]
];

DrawManually[arr_]:=
Module[{xt=arr[[1]],xf=arr[[1]]+arr[[2]]/speedline},
If[arr[[4]]>=2,
{DrawRectInWin[{xt,-HiP},{xf,0},If[BitAnd[arr[[4]],1]==1,Black,Gray]],DrawLineInWin[{xt,0},{xt+tp/nSect,HiW/nSect}],DrawLineInWin[{xf,0},{xf+tp/nSect,HiW/nSect}],DrawTextInWin[arr,nSect]
},{DrawRectInWin[{xt,-HiP},{xf,0},If[BitAnd[arr[[4]],1]==1,Red,Pink]],DrawLineInWin[{xt,0},{xt+tp,HiW}],DrawLineInWin[{xf,0},{xf+tp,HiW}],DrawRectInWin[{xf+tp,HiW+HiP},{xf+tp+tack,HiW},Pink],DrawLineInWin[{xf+tp,HiW},{xf+2 tp,0}],DrawLineInWin[{xf+tp+tack,HiW},{xf+2 tp+tack,0}],
DrawTextInWin[arr,2]
}
]
];
DrawPacketTx[arr_]:=
Module[{xt=arr[[1]],tI=arr[[2]]/speedline,x=arr[[1]]-tw},
If[x >= 0 ,
If[(x+tI+If[arr[[4]]>=2,tp/nSect,tack+2 tp] )<= ww,
If[arr[[4]]>=2,
Graphics[{EdgeForm[Thin],If[BitAnd[arr[[4]],1]==1,Black,Gray],Rectangle[{xt,0},{xt+tI,-HiP}],Dashed,Line[{{xt,0},{xt+tp/nSect,HiW/nSect}}],Line[{{xt+tI,0},{xt+tp/nSect+tI,HiW/nSect}}],
Text[GetTextPack[arr],{xt+tp/nSect+tI/2,HiW/nSect}]}]
,Graphics[{EdgeForm[Thin],If[BitAnd[arr[[4]],1]==1,Red,Pink],Rectangle[{xt,0},{xt+tI,-HiP}],Dashed,Line[{{xt,0},{xt+tp,HiW}}],Line[{{xt+tI,0},{xt+tp+tI,HiW}}],EdgeForm[Thin],Pink,Rectangle[{xt+tp+tI,HiW},{xt+tp+tI+tack,HiW+HiP}],Black,Dashed,Line[{{xt+tp+tI,HiW},{xt+2 tp+tI,0}}],Line[{{xt+tp+tI+tack,HiW},{xt+2 tp+tI+tack,0}}],
Text[GetTextPack[arr],{xt+(tp+tI)/2,HiW/2}]}
]
],DrawManually[arr]
],DrawManually[arr]
]
];

SelectPacketInWin[arr_]:=
Map[(If[((tw-#[[1]]-#[[2]]/speedline)>=If[#[[4]]>=2,tp/nSect,2 tp +tack ]) || (#[[1]]>tw+ww),Unevaluated[Sequence[]],#])&,arr];
]
End[]
EndPackage[]



