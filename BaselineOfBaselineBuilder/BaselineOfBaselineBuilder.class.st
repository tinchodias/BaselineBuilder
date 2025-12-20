Class {
	#name : 'BaselineOfBaselineBuilder',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfBaselineBuilder',
	#package : 'BaselineOfBaselineBuilder'
}

{ #category : 'baseline' }
BaselineOfBaselineBuilder >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			spec
				package: 'BaselineBuilder-Core'.
			spec
				package: 'BaselineBuilder-Tests';
				package: 'BaselineBuilderStub-A';
				package: 'BaselineBuilderStub-B';
				package: 'BaselineBuilderStub-C' ]
]
