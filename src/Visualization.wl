(* ::Package:: *)

(* ::Title:: *)
(*Visualization*)


(* ::Section:: *)
(*Helper*)


padZero[num_]:=Module[
	{len},
	len = Length[IntegerDigits[num]];
	StringJoin @ Table["0", 12 - len]<>ToString[num]
]


(* ::Section:: *)
(*Mask *)


(* ::Text:: *)
(*input: segmentation -> {{Subscript[x, 11], Subscript[y, 11], Subscript[x, 12], Subscript[y, 12], ..., Subscript[x, 1n], Subscript[y, 1n]}, {Subscript[x, 21], Subscript[y, 21], Subscript[x, 22], Subscript[y, 22], ..., Subscript[x, 2n], Subscript[y, 2n]}};*)
(*output: Graphics mask *)


convertAnnotationMask[vtx_List, h_]:= 
	Map[Transpose[{0, h} + {1, -1} Transpose[Partition[#, 2]]]&, vtx]


getMask[pts_List, w_, h_]:= Module[
	{vtx},
	vtx = convertAnnotationMask[pts, h];
	Binarize @ Rasterize[Graphics[{White, Polygon@@vtx}, PlotRange->{{0, w}, {0, h}}, 
	Background->Black], "Image", ImageSize->{w, h}]
]


(* ::Section:: *)
(*BBox*)


convertAnnotationBBoxIntoImageCoord[bbox_List, h_]:= Module[
	{x1, y1, dw, dh},
	{x1, y1, dw, dh} = bbox;
	Transpose[{0, h} + {1, -1} Transpose[Partition[{x1, y1, x1 + dw, y1 + dh}, 2]]]
]


(* ::Section:: *)
(*Extract Mask And BBox*)


(* ::Text:: *)
(*Make the extraction of b-box mask into a pipeline*)


extractBBoxAndMask[img_, bbox_, segPts_List]:= Module[
	{bboxLocal, w, h, boxRegion, maskRegion},
	{w, h} = ImageDimensions[img];
	bboxLocal = convertAnnotationBBoxIntoImageCoord[bbox, h];
	boxRegion = ImageTrim[img, bboxLocal];
	maskRegion = ImageTrim[getMask[segPts, w, h], bboxLocal];
	{boxRegion, maskRegion}
]


(* ::Section:: *)
(*InstanceSegmentation*)


(*instanceSegmentation[img_, ennet_, detectionThreshold_, overlapThreshold_]:= Module[
	{labels, bboxes, probs, masks, coloredMasks, yoloVis, yoloRes, centers},
	yoloRes = detection[img, detectionThreshold, overlapThreshold];
	{labels, bboxes, probs} = convertYoloResult[yoloRes];
	centers = Mean/@ bboxes;
	masks = produceMask[ennet,img, bboxes];
	coloredMasks = Flatten[{RandomColor[], Opacity[.4], #}&/@ masks];
	yoloVis = Transpose @ MapThread[
		{
			{Green,Opacity[0],#2},
			Style[
				Inset[#1<>" ("<>ToString@Round[#3, .01]<>")", #2[[1]], {Left, Top} ],
				FontSize -> Scaled[.02], GrayLevel[0,1], Background->GrayLevel[1,0]
			]
		}&, Transpose[yoloRes]];
	HighlightImage[img, Join[{coloredMasks,yoloVis}], ImagePadding -> Scaled[.02]]	
	(*Flatten[{coloredMasks, visLabels, visBoxes}]*)
]*)


convertYoloResult[yoloRes_]:= Transpose[yoloRes]/.Rectangle -> List


instanceSegmentation[img_, ennet_, detectionThreshold_, overlapThreshold_]:= Module[
	{labels, bboxes, probs, masks, coloredMasks, yoloVis, yoloRes, rectangles, centers},
	
	yoloRes = detection[img, detectionThreshold, overlapThreshold];
	rectangles =  Transpose[yoloRes][[2]];
	{labels, bboxes, probs} = Transpose[yoloRes]/.Rectangle -> List;
	centers = Mean/@ bboxes;
	masks = produceMask[ennet,img, bboxes];
	coloredMasks = Flatten[{RandomColor[], Opacity[.45], #}&/@ masks];
	yoloVis = Transpose @ MapThread[
		{
			{Darker @ Green,Opacity[0],#2},
			Style[
				Inset[#1<>" \n("<>ToString@Round[#3, .01]<>")", #4, {Center, Center} ],
				FontSize -> Scaled[.03], FontColor -> White, GrayLevel[0,1], Background -> GrayLevel[1,0]
			]
		}&, {labels, rectangles, probs, centers}];
	HighlightImage[img, Join[{coloredMasks,yoloVis}], ImagePadding -> Scaled[.02],ImageSize->Large]	
	(*Flatten[{coloredMasks, visLabels, visBoxes}]*)
]


(* ::Section::Closed:: *)
(*Bounding Box*)



labelBox[class_ -> box_]:= Module[{coord,textCoord},(*convert class\[Rule]boxes to labeled boxes*)
	coord=List@@box;
	textCoord={(coord[[1,1]]+coord[[2,1]])/2.,coord[[1,2]]-0.04};
	{{GeometricTransformation[Text[Style[labels[[class]], 20, Darker@Blue],textCoord], 
	ReflectionTransform[{0,1},textCoord]]},EdgeForm[Directive[Red,Thick]],Transparent,box}]

coordToBox[center_, boxCord_, scaling_: 1]:=Module[
	{bx,by,w,h},
	(*conver from {centerx,centery,width,height} to Rectangle object*)
	bx=(center[[1]]+boxCord[[1]])/7.;
	by=(center[[2]]+boxCord[[2]])/7.;
	w=boxCord[[3]]*scaling;
	h=boxCord[[4]]*scaling;
	Rectangle[{bx-w/2,by-h/2},{bx+w/2,by+h/2}]
]

nonMaxSuppression[boxes_, overlapThreshold_, confidThreshold_]:=Module[
	{lth=Length@boxes,boxesSorted,boxi,boxj},
	(*non-max suppresion to eliminate overlapping boxes*)boxesSorted=GroupBy[boxes,#class&][All,SortBy[#prob&]/*Reverse];
	Do[
		Do[
			boxi=boxesSorted[[c,n]];
			If[boxi["prob"]!=0,
				Do[boxj=boxesSorted[[c,m]];
			(*if two boxes overlap largely,kill the box with low confidence*)
					If[RegionMeasure[RegionIntersection[boxi["coord"],boxj["coord"]]]/RegionMeasure[RegionUnion[boxi["coord"],boxj["coord"]]]>=overlapThreshold,
					boxesSorted = ReplacePart[boxesSorted, {c,m,"prob"} -> 0]
					];
					,{m,n+1,Length[boxesSorted[[c]]]}
				]
			],
			{n,1,Length[boxesSorted[[c]]]}
		],
		{c,1,Length@boxesSorted}
	];
	boxesSorted[All, Select[#prob>0&]]]

labelBox[class_ -> box_]:= Module[{coord,textCoord},(*convert class\[Rule]boxes to labeled boxes*)
	coord=List@@box;
	textCoord={(coord[[1,1]]+coord[[2,1]])/2.,coord[[1,2]]-0.04};
	{{GeometricTransformation[Text[Style[labels[[class]], 20, Darker@Blue],textCoord], 
	ReflectionTransform[{0,1},textCoord]]},EdgeForm[Directive[Red,Thick]],Transparent,box}]

drawBoxes[img_,boxes_]:=Module[{labeledBoxes},(*draw boxes with labels*)labeledBoxes=labelBox/@Flatten[Thread/@Normal@Normal@boxes[All,All,"coord"]];
Graphics[GeometricTransformation[{Raster[ImageData[img],{{0,0},{1,1}}],labeledBoxes},ReflectionTransform[{0,1},{0,1/2}]]]]

postProcess[img_,vec_,boxScaling_: 0.7,confidentThreshold_: 0.15,overlapThreshold_: 0.4]:=Module[{grid,prob,confid,boxCoord,boxes,boxNonMax},grid=Flatten[Table[{i,j},{j,0,6},{i,0,6}],1];
prob=Partition[vec[[1;;980]],20];
confid=Partition[vec[[980+1;;980+98]],2];
boxCoord=ArrayReshape[vec[[980+98+1;;-1]],{49,2,4}];
boxes=Dataset@Select[Flatten@Table[<|"coord"->coordToBox[grid[[i]],boxCoord[[i,b]],boxScaling],"class"->c,"prob"->If[#<=confidentThreshold,0,#]&@(prob[[i,c]]*confid[[i,b]])|>,{c,1,20},{b,1,2},{i,1,49}],#prob>=confidentThreshold&];
boxNonMax=nonMaxSuppression[boxes,overlapThreshold,confidentThreshold];
drawBoxes[Image[img],boxNonMax]]


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
	regions = ImageTrim[img, #] &/@ bbox
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
	regions = ImageTrim[img, #] &/@ bbox;
	
	outputTensor = readOutput/@ ( net /@ regions);
	masks = Binarize/@( Image /@ outputTensor);
	regionMasks = 1 - (Erosion[#, IdentityMatrix[3]]& /@ masks);
	resizedRegionMask = MapThread[ImageResize[#1, #2]&, {regionMasks, ImageDimensions/@regions}];
	MapThread[padRegionMask[#1, #2, w, h]&, {resizedRegionMask, bbox}]	
]
