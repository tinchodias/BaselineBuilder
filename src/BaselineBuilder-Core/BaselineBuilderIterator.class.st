Class {
	#name : 'BaselineBuilderIterator',
	#superclass : 'Object',
	#instVars : [
		'packageNames',
		'externalRequirements',
		'primaryPackageRequirements',
		'secondaryPackageRequirements',
		'packageRequirementsProcessed',
		'externalPackageNamesFactory'
	],
	#category : 'BaselineBuilder-Core',
	#package : 'BaselineBuilder-Core'
}

{ #category : 'private' }
BaselineBuilderIterator >> definePackageNamesByRegex: internalPackageNamesRegex [

	packageNames := Array streamContents: [ :stream |
		self class packageOrganizer packageNamesDo: [ :each |
			(internalPackageNamesRegex matches: each) ifTrue: [ 
				stream nextPut: each asString ] ] ].
	packageNames sort
]

{ #category : 'private' }
BaselineBuilderIterator >> ensureRequireDependency: aStPackageDependency [

	| sourcePackageName targetPackageName |
	sourcePackageName := aStPackageDependency source packageName asString.
	targetPackageName := aStPackageDependency target packageName asString.

	(self isInternal: targetPackageName) ifTrue: [
		^ ((aStPackageDependency causesLoadingError
				ifTrue: [ primaryPackageRequirements ]
				ifFalse: [ secondaryPackageRequirements ])
					at: sourcePackageName)
						add: targetPackageName ].

	self externalPackageNamesDo: [ :each |
		^ (externalRequirements at: sourcePackageName)
			add: (self externalBaselineNameFor: each asString) ]
]

{ #category : 'private' }
BaselineBuilderIterator >> externalPackageNamesDo: aBlock [

	externalPackageNamesFactory value: aBlock
]

{ #category : 'private' }
BaselineBuilderIterator >> processDependencies [

	| relationGraph |
	externalRequirements := Dictionary new.
	primaryPackageRequirements := Dictionary new.
	secondaryPackageRequirements := Dictionary new.
	packageNames do: [ :each |
		externalRequirements at: each asString put: Set new.
		primaryPackageRequirements at: each asString put: Set new.
		secondaryPackageRequirements at: each asString put: Set new ].

	relationGraph := StPackageRelationGraph onPackagesNamed: packageNames.
"	relationGraph shouldComputeMessageSendDependencies: false." "They dont produce loading errors"
	relationGraph build.
	relationGraph packages do: [ :eachPackage |
		eachPackage dependencies do: [ :eachDependency |
			self ensureRequireDependency: eachDependency ] ].

	self processDependencyCycles
]

{ #category : 'private' }
BaselineBuilderIterator >> processDependencyCycles [

	packageRequirementsProcessed := OrderedDictionary new.

	[ packageNames isEmpty ] whileFalse: [ 
		| rows chosenRow chosenPackageName ignoredRequirements |
		rows := packageNames collect: [ :each |
			{ each.
			(primaryPackageRequirements at: each) reject: [ :req |
				packageRequirementsProcessed includesKey: req ].
			(secondaryPackageRequirements at: each) reject: [ :req |
				packageRequirementsProcessed includesKey: req ] } ].
		rows sort:
			[ :row | row second size ] ascending,
			[ :row | row third size ] ascending,
			#asString ascending.
		chosenRow := rows first. "Most prioritary"

		chosenPackageName := chosenRow first. "Package name"
		ignoredRequirements := chosenRow second, chosenRow third. "Requirements except ignored"

		packageRequirementsProcessed
			at: chosenPackageName
			put: {
				((externalRequirements at: chosenPackageName) |
				((primaryPackageRequirements at: chosenPackageName) |
				(secondaryPackageRequirements at: chosenPackageName)))
					\ ignoredRequirements.
				ignoredRequirements }.

		packageNames := packageNames copyWithout: chosenPackageName ]
]
