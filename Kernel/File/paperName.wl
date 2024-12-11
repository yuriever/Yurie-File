(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["Yurie`File`paperName`"];


Needs["Yurie`File`"];

Needs["Yurie`File`Info`"];


(* ::Section:: *)
(*Public*)


paperNameQ::usage =
    "check whether a string matches the paper-name format.";

paperName::usage =
    "convert a string to the paper-name format.";

paperNameRename::usage =
    "rename paper files into the paper-name format in the directory.";


(* ::Section:: *)
(*Private*)


(* ::Subsection:: *)
(*Begin*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Constant*)


$paperNamePattern = <|
    "TagTitleAuthor"->RegularExpression["(?s)^(\\d{4}\\.\\d{4,5}|\\d{7}|y\\d{4}|Review|Thesis|Slides|Notes|Book|Lectures)(.*),(.*?)$"],
    "TagTitleAuthorOnlyPreprint"->RegularExpression["(?s)^(\\d{4}\\.\\d{4,5}|\\d{7})(.*),(.*?)$"],
    "TagTitleAuthorWithoutPreprint"->RegularExpression["(?s)^(y\\d{4}|Review|Thesis|Slides|Notes|Book|Lectures)(.*),(.*?)$"],
    "TitleAuthor"->RegularExpression["(?s)^(.*),(.*?)$"]
|>


$paperNameRule =
    $paperNamePattern//ReplaceAll[{
        RegularExpression["(?s)^(.*),(.*?)$"]->RuleDelayed[RegularExpression["(?s)^(.*),(.*?)$"],{"","$1","$2"}],
        regex_RegularExpression:>RuleCondition@RuleDelayed[regex,{"$1","$2","$3"}]
    }];


$specialWordRuleList =
    FileNameJoin@{$thisSourceDir,"paperNameSpecialWordList.json"}//Import[#,"Data"]&//
        Map[{" "<>#<>" "->" "<>#<>" "," "<>#<>","->" "<>#<>","}&]//Flatten;


(* ::Subsection:: *)
(*Main*)


paperName//Attributes =
    {Listable};

paperName//Options =
    {"IncludePreprint"->True};

paperName[str_String,OptionsPattern[]] :=
    Module[ {name},
        name =
            str//StringTrim//replaceSpecialCharacter//regulateTitleAndAuthor[OptionValue["IncludePreprint"]]//
                correctSpecialWord//RemoveDiacritics//StringTrim;
        name//CopyToClipboard;
        name
    ];


paperNameQ//Options =
    {"IncludePreprint"->True};

paperNameQ[str_String,OptionsPattern[]] :=
    Switch[ OptionValue["IncludePreprint"],
        True,
            StringMatchQ[str,$paperNamePattern["TagTitleAuthor"]],
        False,
            StringMatchQ[str,$paperNamePattern["TagTitleAuthorWithoutPreprint"]],
        "Only",
            StringMatchQ[str,$paperNamePattern["TagTitleAuthorOnlyPreprint"]]
    ];


paperNameRename//Options =
    {"OnlyShowRenamedPaper"->True};

paperNameRename[dir_?DirectoryQ,OptionsPattern[]] :=
    Module[ {paperData,paperWithoutAuthor,res},
        paperData =
            findInFolder[file__/;DirectoryQ[file]||StringMatchQ[file,__~~".pdf"|".djvu"]]@dir;
        paperWithoutAuthor =
            paperData//Query[Select[
                !paperNameQ[#FileName,"IncludePreprint"->False]&&
                    !paperNameQ[#FileName,"IncludePreprint"->True]&&
                        !StringContainsQ[#FileName,","]&
            ]];
        res =
            paperData//Query[Select[paperNameQ[#FileName,"IncludePreprint"->False]&]]//
                Query[All,<|"NewName"->paperName[#FileName],#|>&]//
                    Query[All,(
                        If[ #FileName=!=#NewName,
                            RenameFile[#File,FileNameJoin@{#Directory[[1]],#NewName<>fileExtensionWithDot[#File]}]
                        ];
                        <|"IsRenamed"->(#FileName=!=#NewName),#|>
                    )&]//
                        Query[ReverseSortBy[#IsRenamed&]];
        If[ OptionValue["OnlyShowRenamedPaper"]===True,
            res = res//Query[Select[#IsRenamed===True&]]
        ];
        CellPrint@{
            If[ paperWithoutAuthor=!={},
                ExpressionCell[paperWithoutAuthor//Dataset,"Output"],
                (*Else*)
                Nothing
            ],
            If[ res=!={},
                ExpressionCell[res//Dataset,"Output"],
                (*Else*)
                Nothing
            ]
        }
    ];


(* ::Subsection:: *)
(*Helper*)


replaceSpecialCharacter[str_] :=
    StringReplace[
        str,
        {
            ":"->" -",
            "/"->"_",
            "\n"|"\r"->" ",
            "\[Dash]"->"-",
            "\[CloseCurlyQuote]"->"'"
        }
    ];


regulateTitleAndAuthor[dealPreprint_][str_] :=
    Module[ {tagTitleAuthor,title,author},
        tagTitleAuthor =
            str//trySplitPaperName[dealPreprint];
        title =
            tagTitleAuthor[[2]]//StringSplit//regulateWordInTitle//StringRiffle;
        author =
            tagTitleAuthor[[3]]//StringSplit//regulateWordInAuthor//StringRiffle;
        tagTitleAuthor[[1]]<>" "<>title<>", "<>author
    ];


trySplitPaperName[dealPreprint_][str_] :=
    Module[ {rule,cache},
        rule =
            Switch[ dealPreprint,
                False,
                    $paperNameRule["TagTitleAuthorWithoutPreprint"],
                True,
                    $paperNameRule["TagTitleAuthor"]
            ];
        Which[
            str=!=(cache = StringReplace[str,rule]),
                cache[[1]],
            str=!=(cache = StringReplace[str,$paperNameRule["TitleAuthor"]]),
                cache[[1]],
            True,
                {"",str,""}
        ]//Map[StringTrim]
    ];


regulateWordInTitle//Attributes =
    {Listable};

regulateWordInTitle[str_]/;protectedWordQ[str] :=
    str;

regulateWordInTitle[str_]/;str!=DeleteStopwords[str] :=
    (*do not capitalize stopwords.*)
    str//ToLowerCase;

regulateWordInTitle[str_] :=
    str//ToLowerCase//Capitalize;


regulateWordInAuthor//Attributes =
    {Listable};

regulateWordInAuthor[str_]/;StringContainsQ[str,"-"] :=
    (*deal with hyphenated names.*)
    StringSplit[str,"-"]//ToLowerCase//Capitalize//StringRiffle[#,"-"]&;

regulateWordInAuthor[str_] :=
    str//ToLowerCase//Capitalize;


protectedWordQ[str_] :=
    Or[
        StringMatchQ[
            str,
            "I"|"II"|"III"|"cn"
        ],
        StringContainsQ[
            str,
            "("|")"
        ],
        StringContainsQ[
            str,
            "PLACEHOLDER",
            IgnoreCase->True
        ]
    ];


correctSpecialWord[str_] :=
    StringReplace[str,$specialWordRuleList,IgnoreCase->True];


fileExtensionWithDot[file_] :=
    If[ DirectoryQ[file],
        "",
        (*Else*)
        "."<>FileExtension[file]
    ];


(* ::Subsection:: *)
(*End*)


End[];


(* ::Section:: *)
(*End*)


EndPackage[];
