Class {
	#name : 'MyKingTest',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyKingTest >> testIsCheckByOpponentsRook [

	| king board |
	board := MyChessBoard empty.
	board at: 'e4' put: (king := MyKing white).

	"Put an atacker rook on e column"
	board at: 'e1' put: MyRook black.

	self assert: king isInCheck
]

{ #category : 'tests' }
MyKingTest >> testIsCheckMated [

	| king board |
	board := MyChessBoard empty.
	board at: 'a4' put: (king := MyKing white).

	"blocking 5 row and b column"
	board at: 'a5' put: MyRook black.
	"blocking 3 row and b column"
	board at: 'b3' put: (MyRook black).
	board at: 'b6' put: (MyBishop black).
	board at: 'c2' put: (MyBishop black).
	
	self assert: king isCheckMated equals: true.
			
]

{ #category : 'tests' }
MyKingTest >> testIsCheckMatedWithLegalMovesAndNoThreat [

	| king board |
	board := MyChessBoard empty.
	board at: 'a4' put: (king := MyKing white).

	"blocking 5 row and b column"
	board at: 'b5' put: MyRook black.
	
	self assert: king isCheckMated equals: false.
			


]

{ #category : 'tests' }
MyKingTest >> testIsCheckMatedWithLegalMovesAndThreat [

	| king board |
	board := MyChessBoard empty.
	board at: 'a4' put: (king := MyKing white).

	"opponent threatening the king"
	board at: 'a5' put: MyRook black.
	
	self assert: king isCheckMated equals: false.
			


]

{ #category : 'tests' }
MyKingTest >> testIsCheckMatedWithoutLegalMovesAndNoThreat [

	| king board |
	board := MyChessBoard empty.
	board at: 'a4' put: (king := MyKing white).

	"place friendly pieces around the king to prevent it from moving"
	board at: 'a3' put: MyPawn white. 
	board at: 'b3' put: MyPawn white. 
	board at: 'b4' put: MyPawn white. 
	board at: 'a5' put: MyPawn white. 
	board at: 'b5' put: MyRook white.

	
	self assert: king isCheckMated equals: false.
			


]

{ #category : 'tests' }
MyKingTest >> testIsCheckMatedWithoutLegalMovesAndWithThreat [

	| king board |
	board := MyChessBoard empty.
	board at: 'a4' put: (king := MyKing white).

	"blocking 5 row and b column"
	board at: 'a5' put: MyRook black.
	"blocking 3 row and b column"
	board at: 'b3' put: MyRook black.
	
	self assert: king isCheckMated equals: true.
			


]

{ #category : 'tests' }
MyKingTest >> testIsNotCheckBySameColorRook [

	| king board |
	board := MyChessBoard empty.
	board at: 'e4' put: (king := MyKing white).

	"Put an same color rook on e column"
	board at: 'e1' put: MyRook white.

	self deny: king isInCheck
]

{ #category : 'tests' }
MyKingTest >> testMovesWithNoObstacle [

	| king squares board expectedPositions |
	board := MyChessBoard empty.
	board at: 'e4' put: (king := MyKing white).

	squares := king targetSquares.
	expectedPositions := #( d5 e5 f5 d4 f4 d3 e3 f3 ).
	
	self
		assertCollection: squares
		includesAll:
			(expectedPositions collect: [ :name |
				 board at: name ]).
			
	self assert: squares size equals: expectedPositions size.

]

{ #category : 'tests' }
MyKingTest >> testMovesWithNoOponentObstacle [

	| king squares board expectedPositions |
	board := MyChessBoard empty.
	board at: 'e4' put: (king := MyKing white).
	
	"Let's put a white obstacle on e5, blocking the e5"
	board at: 'e5' put: MyKing white.
	squares := king targetSquares.
	
	expectedPositions := #( d5 f5 d4 f4 d3 e3 f3 ).
	
	self
		assertCollection: squares
		includesAll:
			(expectedPositions collect: [ :name |
				 board at: name ]).
	
	self assert: squares size equals: expectedPositions size.
			


]

{ #category : 'tests' }
MyKingTest >> testMovesWithOponentObstacle [

	| king squares board expectedPositions |
	board := MyChessBoard empty.
	board at: 'e4' put: (king := MyKing white).

	"Let's put a black obstacle on e5, blocking the d4-5 f4-5"
	board at: 'e5' put: MyKing black.

	squares := king targetSquares.
	expectedPositions := #( d3 e3 f3 ).
	
	self
		assertCollection: squares
		includesAll:
			(expectedPositions collect: [ :name |
				 board at: name ]).
			
	self assert: squares size equals: expectedPositions size.
			


]
