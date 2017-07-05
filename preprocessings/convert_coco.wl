(* ::Package:: *)

(* ::Title:: *)
(*Convert Coco Into Bounding Boxes And Masks*)


(* ::Subtitle:: *)
(*Wenzhen Zhu*)


dir = ParentDirectory[NotebookDirectory[]]


(* ::Section:: *)
(*To Do*)


(* ::ItemNumbered:: *)
(*Some of the mask are not encoded in "Polygon" format, add an if condition to not parse in this case / throw it out in the beginning.*)


(* ::ItemNumbered:: *)
(*Brain storm how to optimize this. *)


(* ::Section:: *)
(*Helper*)


padZero[num_]:=Module[
	{len},
	len = Length[IntegerDigits[num]];
	StringJoin @ Table["0", 12 - len]<>ToString[num]
]


convertAnnotationMask[vtx_List, h_]:= 
	Map[Transpose[{0, h} + {1, -1} Transpose[Partition[#, 2]]]&, vtx]


getMask[pts_List, w_, h_]:= Module[
	{vtx},
	vtx = convertAnnotationMask[pts, h];
	Binarize @ Rasterize[Graphics[{White, Polygon@@vtx}, PlotRange->{{0, w}, {0, h}}, 
	Background->Black],"Image", ImageSize->{w, h}]
]


convertAnnotationBBoxIntoImageCoord[bbox_List, h_]:= Module[
	{x1, y1, dw, dh},
	{x1, y1, dw, dh} = bbox;
	Transpose[{0, h} + {1, -1} Transpose[Partition[{x1, y1, x1 + dw, y1 + dh}, 2]]]
]


extractBBoxAndMask[img_, bbox_, segPts_List]:= Module[
	{bboxLocal, w, h, boxRegion, maskRegion},
	{w, h} = ImageDimensions[img];
	bboxLocal = convertAnnotationBBoxIntoImageCoord[bbox, h];
	boxRegion = ImageTrim[img, bboxLocal];
	maskRegion = ImageTrim[getMask[segPts, w, h], bboxLocal];
	{boxRegion, maskRegion}
]


(* ::Subsection:: *)
(*Yolo-extractBBox*)


extractBBox[img_, bbox_]:= ImageTrim[img, bbox]


(* ::Section:: *)
(*Set up*)


json = Import[dir <> "/coco/annotations/instances_train2014.json"];


imgIdList = Import[dir <> "/dataset/imgIdAll_train2014.mx"];


data = Association/@json[[1,2, All]];


dataDictionary = Association[#["image_id"] -> # & /@ data];


getNData[startInd_, endInd_]:= dataDictionary[#] &/@ imgIdList[[startInd;; endInd]]


(*start = 11;
end = 50;*)
start = 51;
end = 100;
len = end - start;


nJson = getNData[start, end];


bboxList = #["bbox"]&/@ nJson;
maskCoordList = #["segmentation"] &/@ nJson;
imgIds = #["image_id"] &/@ nJson;
outputDir = dir <> "/dataset/sampledDataset/";


(* ::Section:: *)
(*Convert*)


Do[
	(*1. Import image i*)
	imageId = imgIds[[i]];
	image = Import[dir <>"/coco/train2014/COCO_train2014_"<> padZero @ imageId<>".jpg"];
	
	bbox = bboxList[[i]];
	maskCoord = maskCoordList[[i]];
	
	{imgTrim, maskTrim} = extractBBoxAndMask[image, bbox, maskCoord];
	
	Export[outputDir <> "bbox/" <> padZero@imageId <> ".png",imgTrim];
	Export[outputDir <> "mask/" <> padZero@imageId<> ".png",maskTrim];
	
	Echo[i],
	{i, 1, len + 1}
]
