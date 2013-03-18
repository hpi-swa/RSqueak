'From Squeak4.4 of 15 December 2012 [latest update: #12303] on 17 March 2013 at 4:20:53 pm'!
Object subclass: #BitBlt
	instanceVariableNames: 'destForm sourceForm halftoneForm combinationRule destX destY width height sourceX sourceY clipX clipY clipWidth clipHeight colorMap simW simH simSx simSy simDx simDy simDestBits simDestRaster simSourceBits simSourceRaster simHalftoneBits simSkew simMask1 simMask2 simSkewMask simNWords simHDir simVDir simPreload simSourceIndex simDestIndex simSourceDelta simDestDelta simInDebug '
	classVariableNames: 'AllOnes RightMasks WordSize0 WordSize '
	poolDictionaries: ''
	category: 'Graphics-Support'!
TestCase subclass: #BitBltSimTest
	instanceVariableNames: 'path '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GraphicsTests-Primitives'!

!BitBlt methodsFor: 'private'!
clipRange
	destX >= clipX
		ifTrue: 
			[simSx _ sourceX.
			simDx _ destX.
			simW _ width]
		ifFalse: 
			[simSx _ sourceX + (clipX - destX).
			simW _ width - (clipX - destX).
			simDx _ clipX].
	simDx + simW > (clipX + clipWidth) ifTrue: [simW _ simW - (simDx + simW - (clipX + clipWidth))].
	destY >= clipY
		ifTrue: 
			[simSy _ sourceY.
			simDy _ destY.
			simH _ height]
		ifFalse: 
			[simSy _ sourceY + clipY - destY.
			simH _ height - (clipY - destY).
			simDy _ clipY].
	simDy + simH > (clipY + clipHeight) ifTrue: [simH _ simH - (simDy + simH - (clipY + clipHeight))].
	simSx < 0
		ifTrue: 
			[simDx _ simDx - simSx.
			simW _ simW + simSx.
			simSx _ 0].
	simSx + simW > sourceForm width ifTrue: [simW _ simW - (simSx + simW - sourceForm width)].
	simSy < 0
		ifTrue: 
			[simDy _ simDy - simSy.
			simH _ simH + simSy.
			simSy _ 0].
	simSy + simH > sourceForm height ifTrue: [simH _ simH - (simSy + simH - sourceForm height)]! !

!BitBlt methodsFor: 'private' stamp: 'tfel 3/13/2013 12:02'!
copyBitsAgain
	<primitive: 96>
	self simulateCopyBits.! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/15/2013 10:31'!
calculateOffsets
	"check if we need to preload buffer
	(i.e., two words of source needed for first word of destination)"
	simPreload _ (sourceForm notNil) and:
						[simSkew ~= 0 and: [simSkew <= (simSx bitAnd: WordSize0)]].
	simHDir < 0 ifTrue: [simPreload _ simPreload == false].
	"calculate starting offsets"
	simSourceIndex _ simSy * simSourceRaster + (simSx // WordSize).
	simDestIndex _ simDy * simDestRaster + (simDx // WordSize).
	"calculate increments from end of 1 line to start of next"
	simSourceDelta _
		(simSourceRaster * simVDir) -
			(simNWords + (simPreload ifTrue: [1] ifFalse: [0]) * simHDir).
	simDestDelta _ (simDestRaster * simVDir) - (simNWords * simHDir)! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/17/2013 16:16'!
checkOverlap
	| t |
	"check for possible overlap of source and destination"
	simHDir _ simVDir _ 1. "defaults for no overlap"
	(sourceForm == destForm and: [simDy >= simSy])
		ifTrue:
			[simDy > simSy "have to start at bottom"
				ifTrue: [simVDir _ -1. simSy _ simSy + simH - 1. simDy _ simDy + simH - 1]
				ifFalse: [simDx > simSx "y's are equal, but x's are backward"
							ifTrue: [simHDir _ -1.
									simSx _ simSx + simW - 1.
									"start at right"
									simDx _ simDx + simW - 1.
									"and fix up masks"
									simSkewMask _ simSkewMask bitInvert32.
									t _ simMask1.
									simMask1 _ simMask2.
									simMask2 _ t]]]! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/17/2013 16:16'!
computeMasks
	| startBits endBits |
	"calculate skeq and edge masks"
	simDestBits _ destForm bits.
	simDestRaster _ destForm width - 1 // WordSize + 1.
	sourceForm notNil
		ifTrue: [simSourceBits _ sourceForm bits.
				simSourceRaster _ sourceForm width - 1 // WordSize + 1].
	halftoneForm notNil
		ifTrue: [simHalftoneBits _ halftoneForm bits].
	simSkew _ (simSx - simDx) bitAnd: WordSize0.
	"how many bits source gets skewed to right"
	startBits _ WordSize - (simDx bitAnd: WordSize0).
	"how many bits in first word"
	simMask1 _ RightMasks at: startBits + 1.
	endBits _ WordSize0 - ((simDx + simW - 1) bitAnd: WordSize0).
	"how many bits in last word"
	simMask2 _ (RightMasks at: endBits + 1) bitInvert32.
	simSkewMask _
		(simSkew = 0
			ifTrue: [0]
			ifFalse: [RightMasks at: WordSize - simSkew + 1]).
	"determine number of words stored per line; merge masks if necessary"
	simW < startBits
		ifTrue: [simMask1 _ simMask1 bitAnd: simMask2.
				simMask2 _ 0.
				simNWords _ 1]
		ifFalse: [simNWords _ (simW - startBits - 1) // WordSize + 2].! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/17/2013 16:17'!
copyLoop
	| prevWord thisWord skewWord mergeMask
	  halftoneWord mergeWord |
	1 to: simH do: "here is the vertical loop"
		[:i | 
		(halftoneForm notNil)
			ifTrue:
				"XXX Accessing simHalftoneBits with wrap-around ... different from BlueBook"
				[halftoneWord _ simHalftoneBits at: (1 + (simDy \\ simHalftoneBits size)).
				simDy _ simDy + simVDir]
			ifFalse: [halftoneWord _ AllOnes].
		skewWord _ halftoneWord.
		simPreload
			ifTrue: [prevWord _ simSourceBits at: simSourceIndex + 1.
					"load the 32bit shifter. TODO: check if this is WordSize dependent"
					simSourceIndex _ simSourceIndex + simHDir]
			ifFalse: [prevWord _ 0].
		mergeMask _ simMask1.
		1 to: simNWords do: "here is the inner horizontal loop"
			[:word |
			sourceForm notNil "if source is used"
				ifTrue:
					[prevWord _ prevWord bitAnd: simSkewMask.
						    "XXX: Hack to work around out-of-bounds access"
					thisWord := simSourceBits at: (simSourceIndex \\ simSourceBits size) + 1.
										      	 "pick up next word"
					skewWord _
						prevWord bitOr: (thisWord bitAnd: simSkewMask bitInvert32).
					prevWord _ thisWord.
					"Change from BB: bitAnd: AllOnes to stay in word bounds"
					skewWord _ ((skewWord bitShift: simSkew) bitAnd: AllOnes) bitOr:
											(skewWord bitShift: simSkew - WordSize)].
															"WordSize-bit rotate"
			mergeWord _ self merge: (skewWord bitAnd: halftoneWord)
								with: (simDestBits at: simDestIndex + 1).
			simDestBits
				at: simDestIndex + 1
				put: ((mergeMask bitAnd: mergeWord)
								bitOr: (mergeMask bitInvert32
									bitAnd: (simDestBits at: simDestIndex + 1))).
			simSourceIndex _ simSourceIndex + simHDir.
			simDestIndex _ simDestIndex + simHDir.
			word = (simNWords - 1)
				ifTrue: [mergeMask _ simMask2]
				ifFalse: [mergeMask _ AllOnes]].
		simSourceIndex _ simSourceIndex + simSourceDelta.
		simDestIndex _ simDestIndex + simDestDelta]! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/17/2013 16:17'!
merge: srcWord with: dstWord
	"These are the 16 combination rules."
	combinationRule = 0 ifTrue: [^ 0].
	combinationRule = 1 ifTrue: [^ srcWord bitAnd: dstWord].
	combinationRule = 2 ifTrue: [^ srcWord bitAnd: dstWord bitInvert32].
	combinationRule = 3 ifTrue: [^ srcWord].
	combinationRule = 4 ifTrue: [^ srcWord bitInvert32 bitAnd: dstWord].
	combinationRule = 5 ifTrue: [^ dstWord].
	combinationRule = 6 ifTrue: [^ srcWord bitXor: dstWord].
	combinationRule = 7 ifTrue: [^ srcWord bitOr: dstWord].
	combinationRule = 8 ifTrue: [^ srcWord bitInvert32 bitAnd: dstWord bitInvert32].
	combinationRule = 9 ifTrue: [^ srcWord bitInvert32 bitXor: dstWord].
	combinationRule = 10 ifTrue: [^ dstWord bitInvert32].
	combinationRule = 11 ifTrue: [^ srcWord bitOr: dstWord bitInvert32].
	combinationRule = 12 ifTrue: [^ srcWord bitInvert32].
	combinationRule = 13 ifTrue: [^ srcWord bitInvert32 bitOr: dstWord].
	combinationRule = 14 ifTrue: [^ srcWord bitInvert32 bitOr: dstWord bitInvert32].
	combinationRule = 15 ifTrue: [^ dstWord]! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/15/2013 14:49'!
sanitizeInput

	destForm unhibernate.
	sourceForm
		ifNil: [sourceForm := destForm]
		ifNotNil: [sourceForm unhibernate].
	halftoneForm ifNotNil: [
		(halftoneForm isKindOf: Form)
			ifFalse: [halftoneForm := Form new
										bits: halftoneForm;
										yourself].
		halftoneForm unhibernate].
	width ifNil: [width := sourceForm width].
	height ifNil: [height := sourceForm height].! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/15/2013 13:04'!
simClipRange
	"clip and adjust source origin and extent appropriately"
	"first in x"
	destX >= clipX
		ifTrue: [simSx _ sourceX. simDx _ destX. simW _ width]
		ifFalse: [simSx _ sourceX + (clipX - destX).
				simW _ width - (clipX - destX).
				simDx _ clipX].
	simDx + simW > (clipX + clipWidth)
		ifTrue: [simW _ simW - ((simDx + simW) - (clipX + clipWidth))].
	"then in y"
	destY >= clipY
		ifTrue: [simSy _ sourceY. simDy _ destY. simH _ height]
		ifFalse: [simSy _ sourceY + clipY - destY.
				simH _ height - clipY - destY.
				simDy _ clipY].
	simDy + simH > (clipY + clipHeight)
		ifTrue: [simH _ simH - ((simDy + simH) - (clipY + clipHeight))].
	simSx < 0
		ifTrue: [simDx _ simDx - simSx. simW _ simW + simSx. simSx _ 0].
	simSx + simW > sourceForm width
		ifTrue: [simW _ simW - (simSx + simW - sourceForm width)].
	simSy < 0
		ifTrue: [simDy _ simDy - simSy. simH _ simH + simSy. simSy _ 0].
	simSy + simH > sourceForm height
		ifTrue: [simH _ simH - (simSy + simH - sourceForm height)].
! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/16/2013 17:31'!
simDebug: aString

	simInDebug == true
		ifTrue: [1 to: 400 by: 20
			   do: [:word | Display bits at: word put: 0].
			simInDebug _ false]
		ifFalse: [1 to: 400 by: 20
			    do: [:word | Display bits at: word put: 4294967295].
			simInDebug _ true]
! !

!BitBlt methodsFor: 'simulation' stamp: 'tfel 3/16/2013 17:30'!
simulateCopyBits

	"self simDebug: Time now asString."
	self sanitizeInput.
	self simClipRange.
	(simW <= 0 or: [simH <= 0])
		ifTrue: [^ self].
	self computeMasks.
	self checkOverlap.
	self calculateOffsets.
	self copyLoop.! !


!BitBlt class methodsFor: 'examples' stamp: 'tfel 3/13/2013 13:40'!
exampleOne
	"This tests BitBlt by displaying the result of all sixteen combination rules that BitBlt is capable of using. (Please see the comment in BitBlt for the meaning of the combination rules). This only works at Display depth of 1. (Rule 15 does not work?)"
	| pathClass path displayDepth |

	displayDepth := Display depth.
	Display newDepth: 1.

	(Smalltalk hasClassNamed: #Path)
		ifTrue: [pathClass := Smalltalk at: #Path.
				path := pathClass new.
				0 to: 3 do: [:i | 0 to: 3 do: [:j | path add: j * 100 @ (i * 75)]].
				path := path translateBy: 60 @ 40.]
		ifFalse: ["For mini image, where Path isn't available"
				path := OrderedCollection new: 16.
				#(40 115 190 265) do: [:y |
					#(60 160 260 360) do: [:x |
						path add: x@y]]].
	Display fillWhite.
	1 to: 16 do: [:index | BitBlt
			exampleAt: (path at: index)
			rule: index - 1
			fillColor: nil].

	[Sensor anyButtonPressed] whileFalse: [].
	Display newDepth: displayDepth.

	"BitBlt exampleOne"! !

!BitBlt class methodsFor: 'examples' stamp: 'tfel 3/13/2013 13:12'!
exampleTwo
	"This is to test painting with a gray tone. It also tests that the seaming with gray patterns is correct in the microcode. Lets you paint for a while and then automatically stops. This only works at Depth of 1."
	| f aBitBlt displayDepth |
	"create a small black Form source as a brush. "
	displayDepth := Display depth.
	Display newDepth: 1.
	f := Form extent: 20 @ 20.
	f fillBlack.
	"create a BitBlt which will OR gray into the display. "
	aBitBlt := BitBlt
		destForm: Display
		sourceForm: f
		fillColor: Color gray
		combinationRule: Form over
		destOrigin: Sensor cursorPoint
		sourceOrigin: 0 @ 0
		extent: f extent
		clipRect: Display computeBoundingBox.
	"paint the gray Form on the screen for a while. "
	[Sensor anyButtonPressed] whileFalse: 
		[aBitBlt destOrigin: Sensor cursorPoint.
		aBitBlt simulateCopyBits].
	Display newDepth: displayDepth.
	"BitBlt exampleTwo"! !

!BitBlt class methodsFor: 'private' stamp: 'tfel 3/15/2013 14:32'!
exampleAt: originPoint rule: rule fillColor: mask 
	"This builds a source and destination form and copies the source to the
	destination using the specifed rule and mask. It is called from the method
	named exampleOne. Only works with Display depth of 1"

	| s d border aBitBlt | 
	border:=Form extent: 32@32.
	border fillBlack.
	border fill: (1@1 extent: 30@30) fillColor: Color white.
	s := Form extent: 32@32.
	s fillWhite.
	s fillBlack: (7@7 corner: 25@25).
	d := Form extent: 32@32.
	d fillWhite.
	d fillBlack: (0@0 corner: 32@16).

	s displayOn: Display at: originPoint.
	border displayOn: Display at: originPoint rule: Form under.
	d displayOn: Display at: originPoint + (s width @0).
	border displayOn: Display at: originPoint + (s width @0) rule: Form under.

	d displayOn: Display at: originPoint + (s extent // (2 @ 1)). 
	aBitBlt := BitBlt
		destForm: Display
		sourceForm: s
		fillColor: mask
		combinationRule: rule
		destOrigin: originPoint + (s extent // (2 @ 1))
		sourceOrigin: 0 @ 0
		extent: s extent
		clipRect: Display computeBoundingBox.
	aBitBlt simulateCopyBits.
	border 
		displayOn: Display at: originPoint + (s extent // (2 @ 1))
		rule: Form under.

	"BitBlt exampleAt: 100@100 rule: 0 fillColor: nil"  ! !

!BitBlt class methodsFor: 'class initialization' stamp: 'tfel 3/15/2013 10:23'!
initialize
	"self initialize"
	super initialize.
	WordSize := 32.
	WordSize0 := WordSize - 1.
	RightMasks _ #(0), (1 to: WordSize) collect: [:m | (2 raisedTo: m) - 1].
	AllOnes _ (2 raisedTo: WordSize) - 1.
! !


!BitBltSimTest methodsFor: 'sourceForm' stamp: 'tfel 3/15/2013 14:24'!
destForm
	"black top half, white bottom half"
	| bitmap |
	bitmap := Bitmap new: 32.
	 #(4294967295 4294967295 4294967295 4294967295
		4294967295 4294967295 4294967295 4294967295
		4294967295 4294967295 4294967295 4294967295
		4294967295 4294967295 4294967295 4294967295
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) withIndexDo: [:word :idx |
			bitmap at: idx put: word].
	^ (Form extent: 32@32 depth: 1)
		bits: bitmap;
		yourself! !

!BitBltSimTest methodsFor: 'sourceForm' stamp: 'tfel 3/15/2013 14:24'!
sourceForm
	"white form with black rect in the middle"
	| bitmap |
	bitmap := Bitmap new: 32.
	#(0 0 0 0 0 0 0 33554304 33554304 33554304 33554304
	33554304 33554304 33554304 33554304 33554304
	33554304 33554304 33554304 33554304 33554304
	33554304 33554304 33554304 33554304 0 0 0 0 0 0 0) withIndexDo: [:word :idx |
		bitmap at: idx put: word].
	^ (Form extent: 32@32 depth: 1)
		bits: bitmap;
		yourself! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test1

	self runTest: 1.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:41'!
test10

	self runTest: 10.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:39'!
test11

	self runTest: 11.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:28'!
test12

	self runTest: 12.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:28'!
test13

	self runTest: 13.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:29'!
test14

	self runTest: 14.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:29'!
test15

	self runTest: 15.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:29'!
test16

	self runTest: 16.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test2

	self runTest: 2.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test3

	self runTest: 3.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test4

	self runTest: 4.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test5

	self runTest: 5.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test6

	self runTest: 6.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test7

	self runTest: 7.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test8

	self runTest: 8.! !

!BitBltSimTest methodsFor: 'testing' stamp: 'tfel 3/15/2013 11:24'!
test9

	self runTest: 9.! !

!BitBltSimTest methodsFor: 'test data' stamp: 'tfel 3/15/2013 14:43'!
bitsForTest: index
	| results |
	results := #(
		#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		
		#(0 0 0 0 0 0 0 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

		#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 0 0 0 0 0 0 0)

		#(0 0 0 0 0 0 0 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 0 0 0 0 0 0 0)

		#(4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

		#(4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

		#(4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 0 0 0 0 0 0 0)

		#(4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 0 0 0 0 0 0 0)

		#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295)

		#(0 0 0 0 0 0 0 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295)

		#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295)

		#(0 0 0 0 0 0 0 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 33554304 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295)

		#(4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295)

		#(4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295)

		#(4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4261412991 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295)

		#(4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	).

	^ results at: index! !

!BitBltSimTest methodsFor: 'running-modes' stamp: 'tfel 3/17/2013 16:18'!
runTest: index
	"self runTestVisual: index" "to show something"
	self runTestLarge: index "to test non-aligned stuff"
	"self runTestDark: index" "to run without blitting primitive, against saved data"
! !

!BitBltSimTest methodsFor: 'running-modes' stamp: 'tfel 3/15/2013 14:37'!
runTestDark: index
	| s d rule | 
	rule := index - 1.
	s := self sourceForm.
	d := self destForm.
	(BitBlt
		destForm: d
		sourceForm: s
		fillColor: nil
		combinationRule: rule
		destOrigin: 0@0
		sourceOrigin: 0@0
		extent: s extent
		clipRect: (0@0 extent: d extent))
		simulateCopyBits.
	self assert: d bits asArray = (self bitsForTest: index).
! !

!BitBltSimTest methodsFor: 'running-modes' stamp: 'tfel 3/15/2013 14:30'!
runTestLarge: index
	| s d aBitBlt mask rule simD originPoint destOrigin | 
	originPoint := path at: index.
	rule := index - 1.
	mask := nil.
	s := Form extent: 32@32.
	s fillWhite.
	s fillBlack: (7@7 corner: 25@25).
	d := Form extent: 500@500.
	d fillWhite.
	d fillBlack: (0@0 corner: 32@16).
	destOrigin := originPoint + (s extent // (2 @ 1)).

	simD := d deepCopy.
	aBitBlt := BitBlt
		destForm: simD
		sourceForm: s
		fillColor: mask
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: 0 @ 0
		extent: s extent
		clipRect: simD computeBoundingBox.
	aBitBlt simulateCopyBits.
	
	aBitBlt := BitBlt
		destForm: d
		sourceForm: s
		fillColor: mask
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: 0 @ 0
		extent: s extent
		clipRect: d computeBoundingBox.
	aBitBlt copyBits.
	self assert: [d bits = simD bits].
! !

!BitBltSimTest methodsFor: 'running-modes' stamp: 'tfel 3/15/2013 14:43'!
runTestVisual: index
	| s d aBitBlt mask rule simD originPoint destOrigin | 
	originPoint := path at: index.
	rule := index - 1.
	mask := nil.
	s := Form extent: 32@32.
	s fillWhite.
	s fillBlack: (7@7 corner: 25@25).
	d := Form extent: 32@32.
	d fillWhite.
	d fillBlack: (0@0 corner: 32@16).
	destOrigin := 0 @ 0.

	simD := d deepCopy.
	aBitBlt := BitBlt
		destForm: simD
		sourceForm: s
		fillColor: mask
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: 0 @ 0
		extent: s extent
		clipRect: simD computeBoundingBox.
	aBitBlt simulateCopyBits.
	
	aBitBlt := BitBlt
		destForm: d
		sourceForm: s
		fillColor: mask
		combinationRule: rule
		destOrigin: destOrigin
		sourceOrigin: 0 @ 0
		extent: s extent
		clipRect: d computeBoundingBox.
	aBitBlt copyBits.

	simD displayOn: Display at: originPoint + (s width @ 0) rule: Form over.
	d displayOn: Display at: originPoint - (10@0) rule: Form over.
	
	d bits = simD bits
		ifTrue: [index asString displayAt: originPoint - 20]
		ifFalse: [(index asString, ' failed') displayAt: originPoint - 20. self assert: false].! !

!BitBltSimTest methodsFor: 'initialize-release' stamp: 'tfel 3/15/2013 14:38'!
initialize

	super initialize.
	"World restoreDisplay."
	"(Form extent: 500@300 depth: 32)
		fill: (0@0 extent: 500@300) fillColor: Color green muchDarker;
		displayOn: Display at: 0@0 rule: Form over."! !

!BitBltSimTest methodsFor: 'running' stamp: 'tfel 3/15/2013 11:38'!
setUp
	| pathClass |
	(Smalltalk hasClassNamed: #Path)
		ifTrue: [pathClass := Smalltalk at: #Path.
				path := pathClass new.
				0 to: 3 do: [:i | 0 to: 3 do: [:j | path add: j * 100 @ (i * 75)]].
				path := path translateBy: 60 @ 40.]
		ifFalse: ["For mini image, where Path isn't available"
				path := OrderedCollection new: 16.
				#(40 115 190 265) do: [:y |
					#(60 160 260 360) do: [:x |
						path add: x@y]]].
		! !


!BitBltSimTest class methodsFor: 'as yet unclassified' stamp: 'tfel 3/17/2013 16:08'!
runAllTestsBlind

	1 to: 16 do: [:idx |
		self new
			setUp;
			runTestDark: idx].! !

TestCase subclass: #BitBltSimTest
	instanceVariableNames: 'path'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'GraphicsTests-Primitives'!

!BitBltSimTest reorganize!
('sourceForm' destForm sourceForm)
('testing' test1 test10 test11 test12 test13 test14 test15 test16 test2 test3 test4 test5 test6 test7 test8 test9)
('test data' bitsForTest:)
('running-modes' runTest: runTestDark: runTestLarge: runTestVisual:)
('initialize-release' initialize)
('running' setUp)
!

BitBlt initialize!
Object subclass: #BitBlt
	instanceVariableNames: 'destForm sourceForm halftoneForm combinationRule destX destY width height sourceX sourceY clipX clipY clipWidth clipHeight colorMap simW simH simSx simSy simDx simDy simDestBits simDestRaster simSourceBits simSourceRaster simHalftoneBits simSkew simMask1 simMask2 simSkewMask simNWords simHDir simVDir simPreload simSourceIndex simDestIndex simSourceDelta simDestDelta simInDebug'
	classVariableNames: 'AllOnes RightMasks WordSize WordSize0'
	poolDictionaries: ''
	category: 'Graphics-Support'!
