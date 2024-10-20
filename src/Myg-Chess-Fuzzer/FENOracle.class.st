Class {
	#name : 'FENOracle',
	#superclass : 'PzBlockRunner',
	#instVars : [
		'check'
	],
	#category : 'Myg-Chess-Fuzzer',
	#package : 'Myg-Chess-Fuzzer'
}

{ #category : 'running' }
FENOracle >> value: input [
	| result jsonPayload apiResultPass client |
	Transcript show: 'oracle'; cr.

	jsonPayload := '{ "depth": 1, "fen": "'. input .'" }' .
   	
	client := ZnClient new .
  	client url: 'https://tjconsulting.solutions/'; 
  			entity: (ZnEntity 
						with: jsonPayload
						type: ZnMimeType applicationJson); 
  			post.


	client isSuccess
			ifTrue: [ apiResultPass := true. Transcript show: 'worked'; cr.]
			ifFalse: [ apiResultPass := false. Transcript show: 'failed!'; cr. ].

	[ result := self basicRunOn: input ]
		on: Error
		do: [ :err | 
			(expectedException notNil and: [ expectedException handles: err ])
				ifTrue: [ ^ self expectedFailureWith: {input . err freeze} ]. 
			apiResultPass
				ifFalse: [ ^ self expectedFailureWith: {input . err freeze} ].
			^ self failureWith: {input . err freeze} ].
	
	"If no error when running but the API says theres an error"
	apiResultPass
		ifFalse: [ ^ self failureWith: {input . result } ].
	^ self successWith: {input . result}
	
	
]
