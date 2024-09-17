"
# Example

```
board := MyChessGame freshGame.
board size: 800@600.
space := BlSpace new.
space root addChild: board.
space pulse.
space resizable: true.
space show.
```
"
Class {
	#name : 'MyChessGame',
	#superclass : 'BlElement',
	#instVars : [
		'board',
		'currentPlayer',
		'whitePlayer',
		'blackPlayer',
		'moveCount',
		'movesLabel',
		'moveCountLabel',
		'moves',
		'dsp',
		'isHeatMapOn'
	],
	#category : 'Myg-Chess-Core',
	#package : 'Myg-Chess-Core'
}

{ #category : 'instance creation' }
MyChessGame class >> freshGame [

	^ self fromFENString: 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
]

{ #category : 'instance creation' }
MyChessGame class >> fromFENString: aFENString [

	^ super new
		  initializeFromFENGame: (MyFENParser parse: aFENString);
		  yourself
]

{ #category : 'instance creation' }
MyChessGame class >> new [
	
	self error: 'Please use one of the other constructors'
]

{ #category : 'heatmap' }
MyChessGame >> activateHeatMap [
	| legalMoves colorValueEngagement | 
	board squares do: [ :square | 
		square color: Color white.
		square background: Color white.
		square contents: square contents.
		square engagement: nil.
		
	].
	
	
	board pieces do: [ :piece | 
		
		piece ifNotNil: [ 
			legalMoves := piece legalTargetSquares. 
			"if a piece engages a square than it adds a point to that square in favor of its respective color (+1 for white, -1 for black)"
			legalMoves ifNotEmpty: [
				legalMoves do: [ :square | 
					piece isWhite ifTrue: [
						 	square increaseEngagementBy: 1.
						 ]
					ifFalse: [ 
							square increaseEngagementBy: -1.
						 ].
				].	
			].
		].		
	].

	board squares do: [ :square | 
		square engagement isNotNil ifTrue: [ 
			square engagement = 0 ifTrue: [ 
				square background: (Color r: 1 g: 1 b: 0 alpha:0.5).
			] ifFalse: [
			
				colorValueEngagement := (1 - ((1 / square engagement) abs)) min: 1  max: 0.3 .
			square engagement < 0 ifTrue: [ 
				square background: (Color r: 1 g: 0 b: 0 alpha: colorValueEngagement ) .
			].
	
			square engagement > 0 ifTrue: [ 
				square background: (Color r: 0.3 g: 0.7 b: 0.1 alpha: colorValueEngagement).
			].
			]. 
		
			
	
			
		Transcript show: 'Engagement: ', (colorValueEngagement) asString; cr.
		Transcript show: 'square Engagement: ', (square engagement) asString; cr.
		square invalidate.
		]
				
	].

]

{ #category : 'heatmap' }
MyChessGame >> activateHeatMapV2 [
	| legalMoves currentColor newRed newGreen | 
	board squares do: [ :square | 
		square color: Color white.
		square background: Color white.
		square contents: square contents.
		
	].
	
	
	board pieces do: [ :piece | 
		
		piece ifNotNil: [ 
			legalMoves := piece legalTargetSquares. 
		
			legalMoves ifNotEmpty: [
				legalMoves do: [ :square | 
					currentColor := square background isColorBackground ifTrue: [
							square background paint color.
						]. 
					
					piece isWhite ifTrue: [
							newGreen := (currentColor green + 0.1) max: 1.
						 	square background: (Color r: currentColor red g: newGreen b: 0 alpha:0.5).
						 ]
					ifFalse: [ 
							newRed := (currentColor red + 0.1) max: 1.
						 	square background: (Color r: newRed g: currentColor green b: 0 alpha:0.5).
						 ].
					
					].	
			]
		 ]
			
	]
]

{ #category : 'playing' }
MyChessGame >> checkForMate [

	whitePlayer king isCheckMated ifTrue: [ self halt: 'Black wins!' ].
	blackPlayer king isCheckMated ifTrue: [ self halt: 'White wins!' ].
]

{ #category : 'heatmap' }
MyChessGame >> deactivateHeatMap [

	board squares do: [ :square | 
		square color: Color white.
		square background: Color white.
		square contents: square contents.
		
	].
	
]

{ #category : 'heatmap' }
MyChessGame >> disableEnableHeatMap [
	
	self isHeatMapOn: isHeatMapOn not . 
	
	self isHeatMapOn 
		ifTrue: [ 
			self activateHeatMap .
		 ]
	 	ifFalse: [ 
			self deactivateHeatMap .
		 ].
	
	

		
	
]

{ #category : 'heatmap' }
MyChessGame >> heatMap [

	self isHeatMapOn 
		ifTrue: [ 
			self activateHeatMap .
		 ]
	
	

		
	
]

{ #category : 'initialization' }
MyChessGame >> initialize [

	super initialize.
	self initializeFonts.

	moves := OrderedCollection new.
	whitePlayer := MyPlayer new white game: self.
	blackPlayer := MyPlayer new black game: self.
	self isHeatMapOn: false.
]

{ #category : 'initialization' }
MyChessGame >> initializeFonts [

	"We use fonts to render the characters, and they should be loaded (and downloaded if not there yet)"
	| library |
	library := AeFTLibrary newInitialized.
	MyOpenChessDownloadedFont new
		downloadDirectory: AeFilesystemResources downloadedFontsDirectory;
		ensureDownloaded;
		firstFaceUsing: library.


	AeFontManager resetGlobalInstance.
	AeFontManager globalInstance scanDirectory:
		AeFilesystemResources fontsDirectory
]

{ #category : 'initialization' }
MyChessGame >> initializeFromFENGame: aFENGame [

	| button infoPane buttonHeatMap |
	aFENGame sideToMove = 'White'
		ifTrue: [ currentPlayer := whitePlayer ]
		ifFalse: [ currentPlayer := blackPlayer ].
		
	moveCount := aFENGame moveCount.

	board := MyChessBoard fromFENBoard: aFENGame board.
	board game: self.
	button := ToButton new.
	button labelText: 'Play!'.
	button whenClickedDo: [ self play ].
	
	buttonHeatMap := ToButton new.
	buttonHeatMap labelText: 'HeatMap'. 
	buttonHeatMap whenClickedDo: [ self disableEnableHeatMap ].
	
	infoPane := ToElement new.
	infoPane constraintsDo: [ :c | 
		        c vertical fitContent.
		        c horizontal matchParent ].
	infoPane layout: BlLinearLayout vertical.
	infoPane layout cellSpacing: 10.
	infoPane margin: (BlInsets all: 20).
	infoPane matchParent.
	
	movesLabel := ToLabel new beMultiLine.
	movesLabel matchParent.

	self addChild: board.
	self addChild: infoPane.
	infoPane addChild: movesLabel.
	infoPane addChild: button.
	infoPane addChild: buttonHeatMap.

	self layout: BlLinearLayout horizontal.
	self constraintsDo: [ :aLayoutConstraints |
		aLayoutConstraints horizontal fitContent.
		aLayoutConstraints vertical fitContent ]
]

{ #category : 'accessing' }
MyChessGame >> isHeatMapOn [

	^ isHeatMapOn
]

{ #category : 'accessing' }
MyChessGame >> isHeatMapOn: isActivated [

	isHeatMapOn := isActivated
]

{ #category : 'public' }
MyChessGame >> move: piece to: square [

	piece moveTo: square.
	self recordMovementOf: piece to: square.
	self heatMap .

]

{ #category : 'accessing' }
MyChessGame >> pieces [

	^ board pieces
]

{ #category : 'playing' }
MyChessGame >> play [

	currentPlayer play.
	self heatMap .
	currentPlayer = whitePlayer
		ifTrue: [ currentPlayer := blackPlayer ]
		ifFalse: [ 
			moveCount := moveCount + 1.
			currentPlayer := whitePlayer ].
	self checkForMate
]

{ #category : 'moves' }
MyChessGame >> recordMovementOf: aPiece to: aSquare [
	"moves add: (MyMove piece: aPiece square: aSquare name)."

	| prefix movesText |
	prefix := currentPlayer isWhite
		          ifTrue: [ moveCount asString , '.' ]
		          ifFalse: [ '' ].
	moves add: prefix , ' ' , aPiece id , aSquare name.
	
	movesText := String streamContents: [ :stream |
		| currentLine |
		currentLine := ''.
		moves do: [ :m |
			currentLine := currentLine, ' ', m.
			currentLine size > 20 ifTrue: [ 
				stream nextPutAll: currentLine.
				stream cr.
				currentLine := ''.
			]
		].
		stream nextPutAll: currentLine
	].
	
	movesLabel text: movesText
]
