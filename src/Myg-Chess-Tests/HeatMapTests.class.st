Class {
	#name : 'HeatMapTests',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'activationDeactivation' }
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

{ #category : 'pieceInfluence' }
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
    		'e2' -> [ self assert: square engagement equals: -1. ].
    		'e1' -> [ self assert: square engagement equals: -2. ].
			} otherwise: [ self assert: square engagement isNil. ].
		].
	].


]

{ #category : 'pieceInfluence' }
HeatMapTests >> testCalculateInfluenceForPiecesWithNoLegalMoves [

	| squares board |
	board := MyChessBoard empty.
	board at: 'e1' put: MyPawn black .
	
	board calculateInfluence.

	squares := board squares.
	squares do: [ :square |  
		
		square ifNotNil: [ 
			"assert the expected influence on the squares"
			self assert: square engagement isNil.
		].
	].


]

{ #category : 'pieceInfluence' }
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
    		'e2' -> [ self assert: square engagement equals: 1. ].
    		'e3' -> [ self assert: square engagement equals: 2. ].
			} otherwise: [ self assert: square engagement isNil. ].
		].
	].


]

{ #category : 'pieceInfluence' }
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
    			'e2' -> [ self assert: square engagement equals: 1. ].
    			'e3' -> [ self assert: square engagement equals: 0. ].
				'e4' -> [ self assert: square engagement equals: -1. ].
			} otherwise: [ self assert: square engagement isNil. ].
		].
	].


]

{ #category : 'activationDeactivation' }
HeatMapTests >> testDeactivateHeatMap [

	| rook squares board influence called|

	board := MyChessGame freshGame .
	MyChessBoard class replaceMethod: #paintHeatMap with: [called:=1].
	board at: 'e4' put: (rook := MyRook white).
	board at: 'e6' put: (rook := MyRook black).

	board activateHeatMap.
	squares := board pieces.
	
	self assert: called equals:1.
]

{ #category : 'activationDeactivation' }
HeatMapTests >> testHeatMapActivationDeafultInitialValue [
    | board |
    board := MyChessGame freshGame.
    
    "Initial state: Heatmap is off"
    self assert: board isHeatMapOn equals: false.
]

{ #category : 'colorRendering' }
HeatMapTests >> testInfluenceColorIntensityBlack [
	
	| squares board previousSquareColor currentSquareColor sortedSquaresByEngagement currentSquareEngagement previousSquareEngagement |
	board := MyChessBoard empty.
      
    board at: 'b6' put: MyBishop black.      
    board at: 'a5' put: MyQueen black.        
    board at: 'a3' put: MyKnight black. 
	
	
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
			square engagement ifNotNil: [
				previousSquareColor ifNotNil: [ 
				 
					currentSquareEngagement := square engagement abs.
					previousSquareEngagement ifNotNil: [ 
						
						Transcript show: 'Engagement: ', (currentSquareEngagement) asString; cr.
						previousSquareEngagement > currentSquareEngagement
						ifTrue: [ 
							self assert: previousSquareColor alpha > currentSquareColor alpha.
							 ]
						ifFalse: [
							"then same engagement"
							self assert: previousSquareColor alpha <= currentSquareColor alpha.
							].
					].
					 ].
				previousSquareColor := square background paint color.
				
				 ].
			previousSquareEngagement := currentSquareEngagement.
			
			
		].
	].

]

{ #category : 'colorRendering' }
HeatMapTests >> testInfluenceColorIntensityWhite [
	
	| squares board previousSquareColor currentSquareColor sortedSquaresByEngagement currentSquareEngagement previousSquareEngagement |
	board := MyChessBoard empty.
      
    board at: 'c3' put: MyKnight white.       
    board at: 'e3' put: MyQueen white.        
    board at: 'f3' put: MyBishop white.
	
	
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
			square engagement ifNotNil: [
				previousSquareColor ifNotNil: [ 
				 
					currentSquareEngagement := square engagement abs.
					previousSquareEngagement ifNotNil: [ 
						
						Transcript show: 'Engagement: ', (currentSquareEngagement) asString; cr.
						previousSquareEngagement > currentSquareEngagement
						ifTrue: [ 
							self assert: previousSquareColor alpha > currentSquareColor alpha.
							 ]
						ifFalse: [
							"then same engagement"
							self assert: previousSquareColor alpha <= currentSquareColor alpha.
							].
					].
					 ].
				previousSquareColor := square background paint color.
				
				 ].
			previousSquareEngagement := currentSquareEngagement.
			
			
		].
	].

]

{ #category : 'colorRendering' }
HeatMapTests >> testInfluenceColors [

	| squares board squareColor |
	board := MyChessBoard empty.
	"Place black pieces on the board"
	board at: 'a7' put: MyRook black.
	board at: 'd8' put: MyQueen black.
	board at: 'b6' put: MyKnight black.
	
	"Place white pieces on the board"
	board at: 'a2' put: MyPawn white.
	board at: 'e2' put: MyBishop white.
	board at: 'g1' put: MyKnight white.
	board at: 'd1' put: MyQueen white.
	
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
            ]
            ifFalse: [
					square engagement > 0
            		ifTrue: [
                	"Engagement is positive"
        				self assert: squareColor red equals: 0.
            		]
            		ifFalse: [
                	"Engagement is zero"
                	self assert: squareColor red > 0.
					].
				   self assert: squareColor green > 0.
            ].
				self assert: squareColor blue equals: 0.
    		].
		].
	].
]

{ #category : 'colorRendering' }
HeatMapTests >> testReinitializeBoardColorsContentsPreservation [
    | board originalColors |
    
    board := MyChessBoard empty.
    "Set up some initial contents for squares"
    board at: 'a7' put: MyPawn white.
    board at: 'a2' put: MyKnight black.
    board at: 'a3' put: MyBishop white.
    
    originalColors := board squares collect: [ :square | square background paint color. ].
	
	 board paintHeatMap .
    board reinitializeBoardColors.

    "Ensure the contents of squares are unchanged"
    board squares with: originalColors do: [ :square :originalColor |
        self assert: square background paint color equals: originalColor.
    ].
]
