Class {
	#name : 'MyFENPaserGrammar',
	#superclass : 'GncBaseGrammar',
	#instVars : [
		'ntFEN',
		'ntRanks',
		'ntPiece',
		'ntRow',
		'ntSquarePlacement',
		'ntEmptySquare',
		'ntSeparator',
		'ntActiveColor',
		'ntCastleRights',
		'ntBlackCastleRights',
		'ntWhiteCastleRights',
		'ntEnPassantSquare',
		'ntSquare',
		'ntSquareLetter',
		'ntSquareNumber',
		'ntHalfMoveClock',
		'ntFullMoveNumber',
		'ntRowContent'
	],
	#category : 'Myg-Chess-Fuzzer',
	#package : 'Myg-Chess-Fuzzer'
}

{ #category : 'initialization' }
MyFENPaserGrammar >> defineGrammar [
	super defineGrammar.
	ntFEN --> ntRanks ,' ', ntActiveColor,' ', ntCastleRights ,' ', ntEnPassantSquare ,' ', ntHalfMoveClock ,' ', ntFullMoveNumber .
	
	ntSeparator --> '/'.
	ntRanks --> ntRow , ntSeparator, ntRow , ntSeparator, ntRow , ntSeparator, ntRow , ntSeparator, ntRow , ntSeparator, ntRow , ntSeparator, ntRow , ntSeparator, ntRow.
	
	ntRow -->  ntRowContent @ 3 .
	ntRowContent --> ntPiece , ntPiece , ntPiece , ntPiece , ntPiece , ntPiece , ntPiece , ntPiece | ntSquarePlacement , ntSquarePlacement , ntSquarePlacement , ntSquarePlacement , ntSquarePlacement , ntSquarePlacement , ntSquarePlacement , ntSquarePlacement .
	ntSquarePlacement --> ntEmptySquare @ 50 | ntPiece @ 50 .
	ntEmptySquare --> ($1-$8).
	
	ntPiece --> 'P' | 'p' | 'r' | 'n' | 'b' | 'q' | 'k' | 'b' | 'n' | 'r' | 'R' | 'N' | 'B' | 'Q' | 'K' | 'B' | 'N' | 'R'.
	

	ntActiveColor --> 'w' | 'b'.
	
	ntCastleRights --> ntBlackCastleRights, ntWhiteCastleRights | '-'.
	ntWhiteCastleRights --> 'K' | 'Q' | 'KQ' | '-'.
	ntBlackCastleRights --> 'k' | 'q' | 'kq' | '-'.

	ntEnPassantSquare --> '-' | ntSquare.
	ntSquare --> ntSquareLetter , ntSquareNumber .
	ntSquareLetter --> 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g'.
	ntSquareNumber --> ($1-$8).
	
	ntHalfMoveClock --> ntNumber .
	ntFullMoveNumber --> ntNumber .
	
	^ ntFEN .
]
