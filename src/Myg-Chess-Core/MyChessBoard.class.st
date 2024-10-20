Class {
	#name : 'MyChessBoard',
	#superclass : 'BlElement',
	#instVars : [
		'squareMatrix',
		'grid',
		'selected',
		'state',
		'whiteColor',
		'blackColor',
		'game'
	],
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'instance creation' }
MyChessBoard class >> empty [

	^ super new
]

{ #category : 'instance creation' }
MyChessBoard class >> fromFENBoard: aFENBoard [

	^ super new
		  initializeFromFENBoard: aFENBoard;
		  yourself
]

{ #category : 'instance creation' }
MyChessBoard class >> new [

	self error: 'Please use the other constructors'
]

{ #category : 'accessing' }
MyChessBoard >> at: coordinate [

	^ grid at: coordinate ifAbsent: nil
]

{ #category : 'accessing' }
MyChessBoard >> at: aString ifAbsent: aFullBlockClosure [

	^ grid at: aString ifAbsent: aFullBlockClosure
]

{ #category : 'accessing' }
MyChessBoard >> at: coordinate put: aPiece [

	| square |
	square := grid at: coordinate.
	square contents: aPiece.
	aPiece square: square
]

{ #category : 'accessing' }
MyChessBoard >> blackColor [

	^ blackColor
]

{ #category : 'accessing' }
MyChessBoard >> blackColor: anObject [

	blackColor := anObject
]

{ #category : 'accessing' }
MyChessBoard >> calculateInfluence [
		| legalMoves colorMultiplier | 
		"Reset engagement"
		self squares do: [ :square | 
			square engagement: nil.
		].
	
		self pieces do: [ :piece | 
			piece ifNotNil: [ 
				legalMoves := piece legalTargetSquares. 
				"if a piece engages a square than it adds a point to that square in favor of its respective color (+1 for white, -1 for black)"
          	colorMultiplier := piece isWhite ifTrue: [ 1 ] ifFalse: [ -1 ].
            
          	"If a piece has legal moves, increase engagement for its square and target squares"
          	legalMoves ifNotEmpty: [
          		"The square the piece is on"
          		piece square increaseEngagementBy: colorMultiplier.
                
          		"Each legal target square"
           		legalMoves do: [ :targetSquare |
          			targetSquare increaseEngagementBy: (colorMultiplier * 2).
          		].
      			].
			].		
		].
]

{ #category : 'event handling' }
MyChessBoard >> click: aMyChessSquare [ 
	
	state := state clickOn: aMyChessSquare
]

{ #category : 'accessing' }
MyChessBoard >> game [

	^ game
]

{ #category : 'accessing' }
MyChessBoard >> game: aGame [

	game := aGame
]

{ #category : 'highlighting' }
MyChessBoard >> highLightTargets: squares [

	squares do: [ :sq | sq ifNotNil: #highlightTarget ]
]

{ #category : 'initialization' }
MyChessBoard >> initialize [

	super initialize.

	self blackColor: Color blue.
	self whiteColor: Color orange.

	state := MyUnselectedState board: self.
	grid := Dictionary new.

	self background: (BlBackground paint: Color veryVeryLightGray).
	self layout: (BlGridLayout vertical
			 cellSpacing: 5;
			 columnCount: 8;
			 rowCount: 8;
			 yourself).
	self constraintsDo: [ :aLayoutConstraints |
		aLayoutConstraints horizontal fitContent.
		aLayoutConstraints vertical fitContent ].

	self initializeSquares
]

{ #category : 'initialization' }
MyChessBoard >> initializeFromFENBoard: aFENBoard [

	aFENBoard keysAndValuesDo: [ :k :v |
		v ~= 'empty' ifTrue: [
			| substrings color piece |
			substrings := v substrings.
			color := substrings first.
			piece := substrings second.
			self
				at: k
				put:
					((self class environment at: ('My' , piece capitalized) asSymbol)
						 perform: color asLowercase asSymbol) ] ]
]

{ #category : 'initialization' }
MyChessBoard >> initializePieces [

	| black |
	black := false.
	$a asciiValue to: $h asciiValue do: [ :column |
		8 to: 1 by: -1 do: [ :file |
			| squareNotation square |
			squareNotation := column asCharacter asString , file asString.
			square := black
				          ifTrue: [
					          black := false.
					          MyChessSquare basicNew
						          color: Color black;
						          board: self;
						          initialize;
						          yourself ]
				          ifFalse: [
					          black := true.
					          MyChessSquare basicNew
						          color: Color white;
						          board: self;
						          initialize;
						          yourself ].
			square name: squareNotation.
			grid at: squareNotation put: square.
			self addChild: square ].
		black := black not ].

	self at: 'a1' put: MyRook white.
	self at: 'b1' put: MyKnight white.
	self at: 'c1' put: MyBishop white.
	self at: 'd1' put: MyQueen white.
	self at: 'e1' put: MyKing white.
	self at: 'f1' put: MyBishop white.
	self at: 'g1' put: MyKnight white.
	self at: 'h1' put: MyRook white.

	$a asciiValue to: $h asciiValue do: [ :column |
		self at: column asCharacter asString , '2' put: MyPawn white ].

	self at: 'a8' put: MyRook black.
	self at: 'b8' put: MyKnight black.
	self at: 'c8' put: MyBishop black.
	self at: 'd8' put: MyQueen black.
	self at: 'e8' put: MyKing black.
	self at: 'f8' put: MyBishop black.
	self at: 'g8' put: MyKnight black.
	self at: 'h8' put: MyRook black.

	$a asciiValue to: $h asciiValue do: [ :column |
		self at: column asCharacter asString , '7' put: MyPawn black ]
]

{ #category : 'initialization' }
MyChessBoard >> initializeSquares [

	| black |
	black := false.
	$a asciiValue to: $h asciiValue do: [ :column |
		8 to: 1 by: -1 do: [ :file |
			| squareNotation square |
			squareNotation := column asCharacter asString , file asString.
			square := black
				          ifTrue: [
					          black := false.
					          MyChessSquare basicNew
						          color: Color black;
						          board: self;
						          initialize;
						          contents: nil;
						          yourself ]
				          ifFalse: [
					          black := true.
					          MyChessSquare basicNew
						          color: Color white;
						          board: self;
						          initialize;
						          contents: nil;
						          yourself ].
			square name: squareNotation.
			grid at: squareNotation put: square.
			self addChild: square ].
		black := black not ]
]

{ #category : 'accessing' }
MyChessBoard >> paintHeatMap [
	| colorValueEngagement |
	
	self calculateInfluence.
	
	self squares do: [ :square | 
		square color: Color white.
		square engagement ifNotNil: [ 
			square engagement = 0 ifTrue: [ 
				square background: (Color r: 1 g: 1 b: 0 alpha:0.5).
			] ifFalse: [
				colorValueEngagement := (1 - ((1 / square engagement) abs)) min: 1  max: 0.4 .
				square engagement < 0 ifTrue: [ 
					square background: (Color r: 1 g: 0 b: 0 alpha: colorValueEngagement ) .
				] ifFalse: [ 
					square background: (Color r: 0 g: 0.7 b: 0 alpha: colorValueEngagement).
				].
			]. 
		] ifNil: [ 
			square background: Color white.
		].
		square contents: square contents.
	].
]

{ #category : 'accessing' }
MyChessBoard >> pieces [
	
	^ children collect: [ :square | square contents ]
]

{ #category : 'accessing' }
MyChessBoard >> reinitializeBoardColors [
	| black i | 
	black := true.
	i := 1.
	self squares do: [ :square | 
		black 
			ifTrue: [ 
				square color: Color white.
			 ] 
			ifFalse: [ 
				square color: Color black.
			 ].
		square background: Color white.
		
			(i%8)=0 ifFalse: [ black := black not. ].
			i := i +1.
			
			square contents: square contents.
	].
]

{ #category : 'accessing' }
MyChessBoard >> select: aSquare [ 
	
	state select: aSquare
]

{ #category : 'accessing' }
MyChessBoard >> selected: aMyChessSquare [ 
	
	selected := aMyChessSquare
]

{ #category : 'examples' }
MyChessBoard >> squares [
	
	^ children collect: [ :square | square ]
]

{ #category : 'highlighting' }
MyChessBoard >> unhighLightTargets: squares [

	squares do: [ :sq | sq ifNotNil: #unhighlightTarget ]
]

{ #category : 'selection' }
MyChessBoard >> unselectSelected [
	
	selected ifNil: [ ^ self ].
	selected doUnselect
]

{ #category : 'accessing' }
MyChessBoard >> whiteColor [

	^ whiteColor
]

{ #category : 'accessing' }
MyChessBoard >> whiteColor: anObject [

	whiteColor := anObject
]
