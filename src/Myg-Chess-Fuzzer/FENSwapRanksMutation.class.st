Class {
	#name : 'FENSwapRanksMutation',
	#superclass : 'Object',
	#category : 'Myg-Chess-Fuzzer',
	#package : 'Myg-Chess-Fuzzer'
}

{ #category : 'mutating' }
FENSwapRanksMutation >> mutate: aFENString [
    | ranks FENParts metadata index1 index2 tempRank newFEN |

	  FENParts := aFENString findTokens: ' '.
	  ranks := FENParts at:1.
	  ranks := ranks findTokens: '/'.
     FENParts size > 2 ifTrue: [ 
        metadata := FENParts copyFrom: 2 to: FENParts size.
    ].
		
    index1 := ranks size atRandom.
    index2 := index1.
    [index2 = index1] whileTrue: [ index2 := ranks size atRandom ].
    
    "swap the two selected ranks"
    tempRank := ranks at: index1.
    ranks at: index1 put: (ranks at: index2).
    ranks at: index2 put: tempRank.

    "rebuilding the FEN string"
    newFEN := String streamContents: [ :stream |
        ranks do: [ :rank | stream nextPutAll: rank; nextPut: $/ ].
        stream position: stream position - 1.
			metadata ifNotNil: [
				metadata do:[ :option | stream nextPutAll: ' '; nextPutAll: option.].
			].
    ].
    ^ newFEN
]
