Class {
	#name : #BaselineOfBaselineBuilder,
	#superclass : #BaselineOf,
	#category : #BaselineOfBaselineBuilder
}

{ #category : #baseline }
BaselineOfBaselineBuilder >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			spec
				package: 'BaselineBuilder-Core';
				package: 'BaselineBuilder-Tests'.
 ]
]
