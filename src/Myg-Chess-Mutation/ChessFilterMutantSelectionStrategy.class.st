Class {
	#name : 'ChessFilterMutantSelectionStrategy',
	#superclass : 'MTRandomGroupedMutantSelectionStrategy',
	#category : 'Myg-Chess-Mutation',
	#package : 'Myg-Chess-Mutation'
}

{ #category : 'enumerating' }
ChessFilterMutantSelectionStrategy >> groupCondition [

	^ #operator
]

{ #category : 'generating' }
ChessFilterMutantSelectionStrategy >> shuffleMutants: aMutantCollection [

	| shuffledMutants mutationDict mutant |


	"Invariant: mutationsDict values are never empty.
	If they were to be empty, we remove the entry from the dictionary."
	mutationDict := aMutantCollection groupedBy: self groupCondition.

	mutationDict := mutationDict collect: [ :mutationList |
		                (mutationList shuffleBy: randomGenerator)
			                readStream ].

	shuffledMutants := OrderedCollection new.
	1 to: aMutantCollection size do: [ :i |
		| key streamValue |
		key := mutationDict keys atRandom: randomGenerator.
		streamValue := mutationDict at: key.
		mutant := streamValue next.
		
		(key class = MTSubclassReplacementOperator and: [ 
			mutant originalMethod name = 'MyFENParser>>#initialize'.
		]) ifFalse: [shuffledMutants add: mutant.].
	
		streamValue atEnd ifTrue: [ mutationDict removeKey: key ] ].

	^ shuffledMutants
]
