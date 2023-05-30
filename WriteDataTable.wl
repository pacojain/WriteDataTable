(* ::Package:: *)

(* ::Chapter::Closed:: *)
(*Begin*)


BeginPackage["WriteDataTable`"];

ClearAll[WriteRulesToDataTable]
ClearAll[WriteMapDataTable]

WriteRulesToDataTable::usage= "WriteRulesToDataTable[filename, rules] writes rules to filename.";
WriteMapDataTable::usage="WriteMapDataTable[filename, func, items] maps func over items and writes results on the fly to filename.";

Begin["`Private`"];


(* ::Chapter:: *)
(*Code*)


(* ::Section::Closed:: *)
(*ReadTableFile*)


(* here will be code for reading in a file that contains multiple data tables, possibly separated by whitespace or comments *)


(* ::Section::Closed:: *)
(*WriteTableItem*)


ClearAll[WriteTableItem]
WriteTableItem[fileid_, item_, indentationLevel_:1]:=Module[
{
tableItemPattern=HoldPattern[(Rule|RuleDelayed)[(_String | _Symbol?(SymbolName[#]==="Spellings"&)), Except[{}, _List]]]
},
  If[Not[MatchQ[item, tableItemPattern]]
	, 
	WriteString[fileid, StringJoin[Table["\t",indentationLevel]]];
	WriteString[fileid, InputForm[item, NumberMarks->False]]
	,
	Write[fileid, StandardForm[StringJoin[Table["\t",indentationLevel]]], item[[1]], StandardForm[" \[Rule] {"]];
	Do[
		WriteTableItem[fileid, item[[2,i]], indentationLevel+1];
		If[i==Length[item[[2]]], WriteString[fileid, "\n"], WriteString[fileid, ",\n"]]
		,
		{i, Length[item[[2]]]}
	];
	WriteString[fileid,StringJoin[Table["\t",indentationLevel]]<>"}"]
  ]
]


(* ::Section::Closed:: *)
(*WriteRulesToDataTable*)


Options[WriteRulesToDataTable]={
	"Paclet"->"XXXData", (*< Helpful when writing DataTables. Inserts DataTable(Append)[Domain,"Entity"]= on first line of file. *)
	"Type"->"Entity", (*< Used with "Paclet" option. Determines second argument to DataTable/DataTableAppend at first line of file. *)
	"Append"->False,(*< If true, first line calls DataTableAppend. If false, first line calls DataTable *)
	"Preamble" -> "",
	"TableHeaderString" -> None
};
WriteRulesToDataTable[filename_, rules_, OptionsPattern[]]:=Module[
	{
		fileid, rulesLength
	},
	Quiet[Close[filename]];
	fileid=OpenWrite[filename, NumberMarks -> False];
	SetOptions[fileid,PageWidth->Infinity];
	WriteString[fileid, OptionValue["Preamble"]];
	If[
		OptionValue["Paclet"]=!="XXXData" && OptionValue["TableHeaderString"]===None,
		If[
			OptionValue["Append"]===True,
			WriteString[fileid,"DataTableAppend["<>OptionValue["Paclet"]<>", \""<>OptionValue["Type"]<>"\"] = "],
			WriteString[fileid,"DataTable["<>OptionValue["Paclet"]<>", \""<>OptionValue["Type"]<>"\"] = "]
		]
	];
	If[OptionValue["TableHeaderString"] =!= None, WriteString[fileid, OptionValue["TableHeaderString"] <> " = "]];
	WriteString[fileid, "{\n"];
	rulesLength = Length[rules];
	Do[
		WriteTableItem[fileid, rules[[i]]];
		If[i==rulesLength, WriteString[fileid, "\n"], WriteString[fileid, ",\n"]]
		,
		{i, rulesLength}
	];
	WriteString[fileid,"}"];
	Close[fileid];
]


(* ::Section::Closed:: *)
(*WriteMapDataTable*)


Options[WriteMapDataTable]={
	"Paclet"->"XXXData", (*< Helpful when writing DataTables. Inserts DataTable(Append)[Domain,"Entity"]= on first line of file. *)
	"Type"->"Entity", (*< Used with "Paclet" option. Determines second argument to DataTable/DataTableAppend at first line of file. *)
	"Append"->False,(*< If true, first line calls DataTableAppend. If false, first line calls DataTable *)
	"Preamble" -> "",
	"TableHeaderString" -> None
};
WriteMapDataTable[filename_, function_, items_List, OptionsPattern[]]:=Module[
	{
		fileid, numItems
	},
	Quiet[Close[filename]];
	fileid=OpenWrite[filename, NumberMarks -> False];
	SetOptions[fileid,PageWidth->Infinity];
	WriteString[fileid, OptionValue["Preamble"]];
	If[
		OptionValue["Paclet"]=!="XXXData" && OptionValue["TableHeaderString"]===None,
		If[
			OptionValue["Append"]===True,
			WriteString[fileid,"DataTableAppend["<>OptionValue["Paclet"]<>", \""<>OptionValue["Type"]<>"\"]"],
			WriteString[fileid,"DataTable["<>OptionValue["Paclet"]<>", \""<>OptionValue["Type"]<>"\"]"]
		]
	];
	If[OptionValue["TableHeaderString"] =!= None, WriteString[fileid, OptionValue["TableHeaderString"] <> " = "]];
	WriteString[fileid, "{\n"];
	numItems= Length[items];
	Do[
		WriteTableItem[fileid, function[items[[i]]]];
		If[i==numItems, WriteString[fileid, "\n"], WriteString[fileid, ",\n"]]
		,
		{i, numItems}
	];
	WriteString[fileid,"}"];
	Close[fileid];
]


(* ::Chapter::Closed:: *)
(*Examples*)


(* ::Input:: *)
(*preamble="(*::Package::*)*)
(**)
(*(* This file is a placeholder for storing data concerning everyday objects.  *)*)
(*(* The primary scanning will be done via a custom scanner, but basic entity  *)*)
(*(* and property tables exist here to make it easy to query for the data from *)*)
(*(* the custom scanner. Primary motivation is to support things like:         *)*)
(*(* how many mini coopers will fit inside jupiter                             *)*)
(**)
(*";*)


(* ::Input:: *)
(*WriteDataTable[AlphaSourceEntityFilename,dataTableOut,"Type"->"Entity","Paclet"->"\"EverydayObject\"","Append"-> True, "Preamble" -> preamble]//AbsoluteTiming*)


(* ::Chapter::Closed:: *)
(*End*)


End[];
EndPackage[]
