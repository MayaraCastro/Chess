Class {
	#name : 'FENSwapMetadataMutation',
	#superclass : 'Object',
	#category : 'Myg-Chess-Fuzzer',
	#package : 'Myg-Chess-Fuzzer'
}

{ #category : 'mutating' }
FENSwapMetadataMutation >> mutate: aFENString [
    | ranks FENParts metadata index1 index2 tempMetadata newFEN |

	  FENParts := aFENString findTokens: ' '.
	  ranks := FENParts at:1.
     FENParts size > 2 ifTrue: [ 
        metadata := FENParts copyFrom: 2 to: FENParts size.
    ] ifFalse: [^aFENString ].
		
    index1 := metadata size atRandom.
    index2 := index1.
    [index2 = index1] whileTrue: [ index2 := metadata size atRandom ].
    
    "swap the two selected metadata"
    tempMetadata := metadata at: index1.
    metadata at: index1 put: (metadata at: index2).
    metadata at: index2 put: tempMetadata.

    "rebuilding the FEN string"
    newFEN := String streamContents: [ :stream |
		  stream nextPutAll: ranks.
        stream position: stream position - 1.
			metadata ifNotNil: [
				metadata do:[ :option | stream nextPutAll: ' '; nextPutAll: option.].
			].
    ].
    ^ newFEN
]
