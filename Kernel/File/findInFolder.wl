(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`File`findInFolder`"];


Needs["Yurie`File`"];


(* ::Section:: *)
(*Public*)


findInFolder::usage =
    "find files in folder.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Main*)


findInFolder//Options =
    Options@FileNames;

findInFolder//Attributes =
    {HoldFirst};

findInFolder[stringPattern_:All,depth_:{1},opts:OptionsPattern[]][dir_?DirectoryQ] :=
    FileNames[stringPattern,dir,depth,FilterRules[{opts,Options@findInFolder},Options@FileNames]]//
        Map[<|
            "FileName"->FileBaseName[#],
            "File"->File[#],
            "Directory"->File@DirectoryName[#]
        |>&];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
