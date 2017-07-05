(* ::Package:: *)

dir = NotebookDirectory[];


json = Import[dir <>"instances_train2014.json"];


instance = Association/@ json[[1,2, All]];
imgIdList = #["image_id"] &/@ instance;


convertToOutputFormat[mask_Image] :=
	ImageData[ ImageResize[ mask, {256, 256}], "Bit"] + 1
 
padZero[num_]:=Module[
	{len},
	len = Length[IntegerDigits[num]];
	StringJoin @ Table["0", 12 - len]<>ToString[num]
]


imageId = imgIdList[[1]];
mask = Import[dir <> "myTrain/mask/mask_"<> padZero @ imageId <> ".jpg"];


imgIdListNoDuplicates = Union @ imgIdList;


Length[imgIdListNoDuplicates]


progress = 0;
SetSharedVariable[progress];

AbsoluteTiming[ParallelDo[
	(*1. Import image i*)
	imageId = imgIdListNoDuplicates[[i]];
	mask = Import[dir <> "myTrain/mask/mask_"<> padZero @ imageId <> ".jpg"];
	outputTensor = convertToOutputFormat[mask];
	Export[dir <> "myTrain/mask_data/mask_"<> padZero@imageId<> ".dat", outputTensor];
	Print[++progress, " (", progress / Length[imgIdListNoDuplicates] * 100., " %)"],
{i, Length[imgIdListNoDuplicates]}]]
