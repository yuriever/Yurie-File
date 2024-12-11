(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`File`batchOpenSaveNotebook`"];


Needs["Yurie`File`"];


(* ::Section:: *)
(*Public*)


batchOpenSaveNotebook::usage =
    "batch open and save notebooks.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


batchOpenSaveNotebook[nbList_List,pause_:0.5] :=
    Module[ {nb,nbobj,counter = 0},
        Monitor[
            Do[
                counter = counter+1;
                nbobj = NotebookOpen[nb];
                Pause[pause];
                NotebookSave[nbobj];
                Pause[pause];
                NotebookClose[nbobj],
                {nb,nbList}
            ],
            ProgressIndicator[counter,{0,Length@nbList}]
        ]
    ];


(* ::Subsection:: *)
(*Helper*)



(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
