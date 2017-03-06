copying
copyReplaceFrom: start to: stop with: replacementCollection 

	| intermediateHolder endReplacement newSize |
	newSize := self size - (stop - start + 1) + replacementCollection size.
	endReplacement := start - 1 + replacementCollection size.
	intermediateHolder := self species
		new: newSize
		streamContents: [:aStream |
			start > 1 ifTrue: [
				1 to: start - 1 do: [:index | 
					aStream nextPut: (self at: index)]].
			start <= endReplacement ifTrue: [
				1 to: replacementCollection size do: [:index | 
					aStream nextPut: (replacementCollection at: index)]].
			endReplacement < newSize ifTrue: [
				stop + 1 to: self size do: [:index |
					aStream nextPut: (self at: index)]]].
	^ self class newFrom: intermediateHolder