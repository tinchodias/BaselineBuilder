Extension { #name : 'StMessageSendDependency' }

{ #category : '*BaselineBuilder-Core' }
StMessageSendDependency >> causesLoadingError [
	"Only if it is sent from a class side initialization, but we ignore this case"

	^ false
]
