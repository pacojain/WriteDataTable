(* ::Package:: *)

Get["Z:\\GitHub\\MapMonitored\\MapMonitored.m"]


ClearAll[WriteTableItem]
WriteTableItem[fileid_, item_, indentationLevel_:1]:=Module[
{
tableItemPattern=HoldPattern[(Rule|RuleDelayed)[(_String | Spellings), Except[{}, _List]]]
},
  If[Not[MatchQ[item, tableItemPattern]]
	, 
	WriteString[fileid, StringJoin[Table["\t",indentationLevel]]];
	WriteString[fileid, ToString[item, InputForm, NumberMarks->False]]
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


ClearAll[WriteRulesToDataTable]
Options[WriteRulesToDataTable]={
	"Paclet"->"XXXData", (*< Helpful when writing DataTables. Inserts DataTable(Append)[Domain,"Entity"]= on first line of file. *)
	"Type"->"Entity", (*< Used with "Paclet" option. Determines second argument to DataTable/DataTableAppend at first line of file. *)
	"Append"->False,(*< If true, first line calls DataTableAppend. If false, first line calls DataTable *)
	"Preamble" -> "",
	"Monitored" -> True
};
WriteRulesToDataTable[filename_, rules_, OptionsPattern[]]:=Module[
	{
		rulesLength,fileid
	},
	fileid=OpenWrite[filename, NumberMarks -> False];
	SetOptions[fileid,PageWidth->Infinity];
	WriteString[fileid, OptionValue["Preamble"]];
	If[
		OptionValue["Paclet"]=!="XXXData",
		If[
			OptionValue["Append"]===True,
			WriteString[fileid,"DataTableAppend["<>OptionValue["Paclet"]<>", \""<>OptionValue["Type"]<>"\"] = {\n"],
			WriteString[fileid,"DataTable["<>OptionValue["Paclet"]<>", \""<>OptionValue["Type"]<>"\"] = {\n"]
		],
		WriteString[fileid,"{\n"]
	];
	rulesLength=Length[rules];
	If[TrueQ[OptionValue["Monitored"]], DoMonitored, Do][
		WriteTableItem[fileid, rules[[i]]];
		If[i==rulesLength, WriteString[fileid, "\n"], WriteString[fileid, ",\n"]]
		,
		{i,rulesLength}
	];
	WriteString[fileid,"}"];
	Close[fileid];
]


ClearAll[WriteMapDataTable]
Options[WriteMapDataTable]={
	"Paclet"->"XXXData", (*< Helpful when writing DataTables. Inserts DataTable(Append)[Domain,"Entity"]= on first line of file. *)
	"Type"->"Entity", (*< Used with "Paclet" option. Determines second argument to DataTable/DataTableAppend at first line of file. *)
	"Append"->False,(*< If true, first line calls DataTableAppend. If false, first line calls DataTable *)
	"Preamble" -> "",
	"Monitored" -> True
};
WriteMapDataTable[filename_, function_, items_List, OptionsPattern[]]:=Module[
	{
		fileid, numItems
	},
	fileid=OpenWrite[filename, NumberMarks -> False];
	SetOptions[fileid,PageWidth->Infinity];
	WriteString[fileid, OptionValue["Preamble"]];
	If[
		OptionValue["Paclet"]=!="XXXData",
		If[
			OptionValue["Append"]===True,
			WriteString[fileid,"DataTableAppend["<>OptionValue["Paclet"]<>", \""<>OptionValue["Type"]<>"\"] = {\n"],
			WriteString[fileid,"DataTable["<>OptionValue["Paclet"]<>", \""<>OptionValue["Type"]<>"\"] = {\n"]
		],
		WriteString[fileid,"{\n"]
	];
	numItems= Length[items];
	If[TrueQ[OptionValue["Monitored"]], DoMonitored, Do][
		WriteTableItem[fileid, function[items[[i]]]];
		If[i==numItems, WriteString[fileid, "\n"], WriteString[fileid, ",\n"]]
		,
		{i,numItems}
	];
	WriteString[fileid,"}"];
	Close[fileid];
]



(* ::Chapter:: *)
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
