(* ::Package:: *)

(* ::Title:: *)
(*Utility*)


(* ::Subsection:: *)
(*ReadOutput*)


readOutput[outputTensor_]:= outputTensor[[All, All, 1]]


(* ::Subsection:: *)
(*padRegionMask*)


padRegionMask[trimmedMask_, bboxLocal_, w_, h_]:= Block[
	{x1,y1,x2,y2},
	{{x1,y1},{x2,y2}} = bboxLocal;
	ImagePad[trimmedMask, 
		{{x1, w - x2},
		 {y1, h - y2}}]
]


(* ::Subsection:: *)
(*Yolo Result Get*)


ConvertYoloResult[yoloRes_]:= Transpose[yoloRes]/.Rectangle -> List


(* ::Subsection:: *)
(*Crop Regions*)


cropRegion[img_, bbox_]:= Module[
	{w, h, regions},
	{w, h} = ImageDimensions[img];
	regions = extractBBox[img, #] &/@ bbox
]


(* ::Subsection:: *)
(*produceMask*)


(* ::Text:: *)
(*out: output tensor of network*)
(*bbox: a list of bounding boxes*)
(*w = width of image*)
(*h = height of image*)


produceMask[net_, img_, bbox_]:= Module[
	{w, h, regions, outputTensor, masks, regionMasks, resizedRegionMask},
	{w, h} = ImageDimensions[img];
	regions = extractBBox[img, #] &/@ bbox;
	
	outputTensor = readOutput/@ ( net /@ regions);
	masks = Binarize/@( Image /@ outputTensor);
	regionMasks = 1 - (Erosion[#, IdentityMatrix[3]]& /@ masks);
	resizedRegionMask = MapThread[ImageResize[#1, #2]&, {regionMasks, ImageDimensions/@regions}];
	MapThread[padRegionMask[#1, #2, w, h]&, {resizedRegionMask, bbox}]	
]
