(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`File`exportImage`"];


Needs["Yurie`File`"];


(* ::Section:: *)
(*Public*)


exportImage::usage =
    "fix SVG issues.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Variable*)


$SVGStylesheet = CurrentValue[DefaultStyleDefinitions];


(* ::Subsection:: *)
(*Option*)


exportSVG//Options = {
    Splice@Options@ExportString,
    "SVGStylesheet"->"Default.nb",
    "SVGSizeInfo"->True
};

exportImage//Options = {
    Splice@Options@exportSVG,
    Splice@Options@Export
}


(* ::Subsection:: *)
(*Main*)


exportImage[imagePath_,opts:OptionsPattern[]][image_] :=
    Module[ {format},
        format =
            First@FileNameToFormatList@imagePath;
        Switch[format,
            "SVG",
                exportSVG["SVG",imagePath,image,FilterRules[{opts,Options@exportImage},Options@exportSVG]],
            _,
                Export[imagePath,image,format,FilterRules[{opts,Options@exportImage},Options@Export]]
        ]
    ];


(* ::Subsection:: *)
(*Helper*)


exportSVG["SVG",imagePath_,image_,opts:OptionsPattern[]] :=
    Module[ {imageString},
        If[ OptionValue["SVGStylesheet"]=!=$SVGStylesheet,
            setStylesheetOfSVG[OptionValue["SVGStylesheet"]]
        ];
        imageString =
            ExportString[image,"SVG",FilterRules[{opts,Options@exportSVG},Options@ExportString]]//
                ifKeepSizeInfo[OptionValue["SVGSizeInfo"]];
        If[ !FileExistsQ@imagePath,
            CreateFile@imagePath
        ];
        (*OpenWrite will overwrite the existing data.*)
        OpenWrite@imagePath;
        WriteString[imagePath,imageString];
        Close@imagePath;
        imagePath
    ];


ifKeepSizeInfo[True][string_] :=
    string;

ifKeepSizeInfo[False][string_] :=
    string//StringSplit[#,"\n",3]&//
        MapAt[removeSizeInfo,2]//
            StringRiffle[#,"\n"]&;


removeSizeInfo[string_] :=
    StringReplace[
        string,
        "width="~~Shortest[__]~~" viewBox":>"viewBox"
    ];


(*set stylesheet when exporting SVG files.*)

setStylesheetOfSVG[stylesheet_] :=
    (
        $SVGStylesheet = stylesheet;
        System`ConvertersDump`createVectorExportPacketExpr[ConvertersDump`expr_,ConvertersDump`opts___] :=
            Module[ {feObj},
                System`ConvertersDump`Utilities`VerbosePrint[System`ConvertersDump`createVectorExportPacketExpr,"Generating VectorExportPacketExpr."];
                If[ SameQ[Head@Unevaluated@ConvertersDump`expr,NotebookObject],
                    feObj = Evaluate@NotebookGet@ConvertersDump`expr,
                    feObj = BoxForm`FrontEndObject@Unevaluated@ConvertersDump`expr;
                ];
                If[ SameQ[feObj,$Failed],
                    Notebook[
                        {
                            Cell[
                                BoxData@MakeBoxes[ConvertersDump`expr,StandardForm],
                                "Output",
                                ShowCellBracket->False,
                                CellMargins->{{0,0},{0,0}}
                            ]
                        },
                        (*the original code does not specify the stylesheet.*)
                        StyleDefinitions->stylesheet
                    ],
                    feObj
                ]
            ];
        Print["test"]
    );


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
