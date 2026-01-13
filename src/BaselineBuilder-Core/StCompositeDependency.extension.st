Extension { #name : 'StCompositeDependency' }

{ #category : '*BaselineBuilder-Core' }
StCompositeDependency >> causesLoadingError [

	^ dependencies anySatisfy: #causesLoadingError
]
