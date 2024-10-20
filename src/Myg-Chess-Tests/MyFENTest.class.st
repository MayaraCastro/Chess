Class {
	#name : 'MyFENTest',
	#superclass : 'TestCase',
	#category : 'Myg-Chess-Tests',
	#package : 'Myg-Chess-Tests'
}

{ #category : 'tests' }
MyFENTest >> testInitialize [
        | parser piecesDictionary |
        MTRandomMutantSelectionStrategy.
        parser := MyFENParser forString: ''.
        piecesDictionary := parser pieces.

			self assert: (piecesDictionary class = Dictionary). 
        
        self assert: (piecesDictionary at: $P) equals: 'White pawn'.
        self assert: (piecesDictionary at: $N) equals: 'White knight'.
        self assert: (piecesDictionary at: $B) equals: 'White bishop'.
        self assert: (piecesDictionary at: $R) equals: 'White rook'.
        self assert: (piecesDictionary at: $Q) equals: 'White queen'.
        self assert: (piecesDictionary at: $K) equals: 'White king'.

        self assert: (piecesDictionary at: $p) equals: 'Black pawn'.
        self assert: (piecesDictionary at: $n) equals: 'Black knight'.
        self assert: (piecesDictionary at: $b) equals: 'Black bishop'.
        self assert: (piecesDictionary at: $r) equals: 'Black rook'.
        self assert: (piecesDictionary at: $q) equals: 'Black queen'.
        self assert: (piecesDictionary at: $k) equals: 'Black king'.
]

{ #category : 'tests' }
MyFENTest >> testParse [

	| parser game |

        parser := MyFENParser forString: 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'.

        game := parser parse.

        self assert: game notNil.
        self assert: game board notNil.
        self assert: (game sideToMove = 'White').

        self assert: (game moveCount = 1).
]

{ #category : 'tests' }
MyFENTest >> testParseBlackPawn [

	| parser piece |
	parser := MyFENParser forString: 'p'.
	piece := parser parsePiece.
	
	self assert: piece equals: 'Black pawn'
]

{ #category : 'tests' }
MyFENTest >> testParseRank [

	| parser rank |
	parser := MyFENParser forString: 'p2rQknB'.
	rank := parser parseRank.
	
	self assert: rank asArray equals: #(
		'Black pawn'
		$2
		'Black rook'
		'White queen'
		'Black king'
		'Black knight'
		'White bishop'
	)
]

{ #category : 'tests' }
MyFENTest >> testParseWhitePawn [

	| parser piece |
	parser := MyFENParser forString: 'P'.
	piece := parser parsePiece.
	
	self assert: piece equals: 'White pawn'
]
