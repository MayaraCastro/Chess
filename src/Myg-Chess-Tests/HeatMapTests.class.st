Class {
	#name : 'HeatMapTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
HeatMapTests >> testActivateHeatMap [

	| rook squares board influence |
	board := MyChessBoard empty.
	board at: 'e4' put: (rook := MyRook white).
	board at: 'e6' put: (rook := MyRook black).
	
	board activateHeatMap .
	squares := board pieces.
	squares do: [ :square |  
		 "assert the expected background color on the squares"
		].


]

{ #category : 'tests' }
HeatMapTests >> testCalculateInfluenceForBlackPiece [

	| squares board |
	board := MyChessBoard empty.
	board at: 'e2' put: MyPawn black .
	
	board calculateInfluence.

	squares := board squares.
	squares do: [ :square |  
		
		square ifNotNil: [ 
			"assert the expected influence on the squares"
			square name caseOf: {
    		'e2' -> [ self assert: square engagement = -1. ].
    		'e1' -> [ self assert: square engagement = -2. ].
			} otherwise: [ self assert: square engagement isNil. ].
		].
	].


]

{ #category : 'tests' }
HeatMapTests >> testCalculateInfluenceForWhitePiece [

	| squares board |
	board := MyChessBoard empty.
	board at: 'e2' put: MyPawn white.
	
	board calculateInfluence.
	
	squares := board squares.
	squares do: [ :square |  
		square ifNotNil: [ 
			"assert the expected influence on the squares"
			square name caseOf: {
    		'e2' -> [ self assert: square engagement = 1. ].
    		'e3' -> [ self assert: square engagement = 2. ].
			} otherwise: [ self assert: square engagement isNil. ].
		].
	].


]

{ #category : 'tests' }
HeatMapTests >> testCalculateNeutralInfluenceForSquare [
	| squares board |
	board := MyChessBoard empty.
	board at: 'e2' put: MyPawn white.
	board at: 'e4' put: MyPawn black.
	
	board calculateInfluence.
	
	squares := board squares.
	squares do: [ :square |  
		square ifNotNil: [ 
			"assert the expected influence on the squares"
			square name caseOf: {
    		'e2' -> [ self assert: square engagement = 1. ].
    		'e3' -> [ self assert: square engagement = 0. ].
			'e4' -> [ self assert: square engagement = -1. ].
			} otherwise: [ self assert: square engagement isNil. ].
		].
	].


]

{ #category : 'tests' }
HeatMapTests >> testDeactivateHeatMap [

	| rook squares board influence |
	board := MyChessBoard empty.
	board at: 'e4' put: (rook := MyRook white).
	board at: 'e6' put: (rook := MyRook black).
	
	board calculateInfluence.
	squares := board pieces.
	squares do: [ :square |  
		 "assert the expected background color on the squares"
		].


]

{ #category : 'tests' }
HeatMapTests >> testInfluenceColorIntensityForBlackPlayer [
	
	| squares board previousSquareColor currentSquareColor sortedSquaresByEngagement currentSquareEngagement previousSquareEngagement |
	board := MyChessBoard empty.
	board at: 'e2' put: MyPawn black .
	
	
	board paintHeatMap.
	
	sortedSquaresByEngagement := board squares sort: [ :square1 :square2 | 
		(square1 isNotNil and: square2 isNotNil) ifTrue: [
			(square1 engagement isNotNil and: square2 engagement isNotNil) ifTrue: [ 
				square1 engagement abs > square2 engagement abs
			 ] ifFalse: [
				false.]. 
			 ].
		 ].
	
	sortedSquaresByEngagement do: [ :square | 
		square ifNotNil: [ 
			currentSquareColor := square background paint color.
			
			previousSquareColor ifNotNil: [ 
				square engagement ifNotNil: [ 
					currentSquareEngagement := square engagement abs.
					previousSquareEngagement ifNotNil: [ 
						
						Transcript show: 'Engagement: ', (currentSquareEngagement) asString; cr.
						previousSquareEngagement > currentSquareEngagement
						ifTrue: [ 
							self assert: previousSquareColor alpha > currentSquareColor alpha.
							 ]
						ifFalse: [
							"then same engagement"
							self assert: previousSquareColor alpha = currentSquareColor alpha.
							].
					].
					 ].
				
				 ].
			previousSquareColor := square background paint color.
			previousSquareEngagement := currentSquareEngagement.
		].
	].

]

{ #category : 'tests' }
HeatMapTests >> testInfluenceColorIntensityForWhitePlayer [
	
	"Test that the higher influence of white it has a stronger color or green"
	| squares board squareColor |
	board := MyChessBoard empty.
	board at: 'e2' put: MyPawn white .
	
	board paintHeatMap.
	 "TODO"
]

{ #category : 'tests' }
HeatMapTests >> testInfluenceColors [

	| squares board squareColor |
	board := MyChessBoard empty.
	board at: 'a7' put: MyRook black .
	board at: 'a2' put: MyPawn white .
	
	board paintHeatMap.

	squares := board squares.
	squares do: [ :square |  
		squareColor := square background paint color.
		square ifNotNil: [ 
			
			square engagement 
			ifNil:  [
					"Engagement non existent"
					self assert: square engagement isNil.
        		
    		]
    		ifNotNil: [
        		square engagement < 0
            ifTrue: [
                "Engagement is negative"
					self assert: squareColor red > 0.
                self assert: squareColor green equals: 0.
                self assert: squareColor blue equals: 0.
                
            ]
            ifFalse: [
					square engagement > 0
            		ifTrue: [
                	"Engagement is positive"
        				self assert: squareColor red equals: 0.
        				self assert: squareColor green > 0.
        				self assert: squareColor blue equals: 0.
            		]
            		ifFalse: [
                	"Engagement is zero"
                	self assert: squareColor red > 0.
                	self assert: squareColor green > 0.
                	self assert: squareColor blue equals: 0.
					].
            ].
    		].
		].
	].
]
