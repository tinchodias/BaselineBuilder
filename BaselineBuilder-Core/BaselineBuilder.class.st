Class {
	#name : #BaselineBuilder,
	#superclass : #Object,
	#instVars : [
		'projectName',
		'packageNames',
		'relationGraph',
		'baselineClass',
		'packageRequirements',
		'externalProjectNames',
		'internalPackageNamesRegex'
	],
	#category : #'BaselineBuilder-Core'
}

{ #category : #examples }
BaselineBuilder class >> example0 [
	self new
		projectName: 'BaselineBuilder';
		externalProjectNames: #();
		build;
		browseBuiltBaselineClass
]

{ #category : #examples }
BaselineBuilder class >> example1 [
	self new
		projectName: 'MongoClientMonitor';
		externalProjectNames: #(Roassal3 Tealight TaskIt);
		build;
		browseBuiltBaselineClass
]

{ #category : #examples }
BaselineBuilder class >> example2 [
	self new
		projectName: 'Iceberg';
		externalProjectNames: #(LibGit);
		build;
		browseBuiltBaselineClass
]

{ #category : #accessing }
BaselineBuilder >> baselineClass [
	^ baselineClass
]

{ #category : #accessing }
BaselineBuilder >> baselineClassName [
	^ self baselinePackageName, 'Draft'
]

{ #category : #accessing }
BaselineBuilder >> baselinePackageName [
	^ 'BaselineOf', projectName
]

{ #category : #accessing }
BaselineBuilder >> browseBuiltBaselineClass [
	baselineClass browse
]

{ #category : #building }
BaselineBuilder >> build [
	self processDependencies.
	self createBaselineClass.
]

{ #category : #private }
BaselineBuilder >> createBaselineClass [
	baselineClass := BaselineOf
		subclass: self baselineClassName asSymbol
		instanceVariableNames: ''
		classVariableNames: ''
		package: self baselinePackageName asString.
		
	baselineClass
		compile: self sourceForBaselineMethod
		classified: #baseline.

	externalProjectNames do: [ :each |
		baselineClass
			compile: (self sourceForExternalProject: each)
			classified: #'external projects' ]
]

{ #category : #private }
BaselineBuilder >> ensureRequire: source to: target [
	| sourcePackageName targetPackageName |
	self
		parseTarget: target packageName
		ifInternal: [ :name | targetPackageName := name ]
		ifExternalProject: [ :name | targetPackageName := name ]
		ifNone: [
			('Discarding dependency: {1} -> {2}'
				format: {
					source packageName.
					target packageName }) traceCr.
			^ self ].
	
	sourcePackageName := source packageName asString.
	(packageRequirements at: sourcePackageName) add: targetPackageName
]

{ #category : #accessing }
BaselineBuilder >> externalProjectNames: aCollectionOfStrings [
	externalProjectNames := aCollectionOfStrings
]

{ #category : #accessing }
BaselineBuilder >> internalPackageNamesPrefix: aString [ 
	internalPackageNamesRegex := (aString, '.*') asRegex
]

{ #category : #accessing }
BaselineBuilder >> internalPackageNamesRegex: aRegex [
	internalPackageNamesRegex := aRegex
]

{ #category : #private }
BaselineBuilder >> isInternal: aPackageName [
	^ internalPackageNamesRegex matches: aPackageName
]

{ #category : #private }
BaselineBuilder >> parseTarget: aPackageName ifInternal: internalPackageBlock ifExternalProject: externalProjectBlock ifNone: noneBlock [

	(self isInternal: aPackageName) ifTrue: [
		^ internalPackageBlock value: aPackageName asString ].

	externalProjectNames do: [ :each |
		(aPackageName beginsWith: each)
			ifTrue: [ ^ externalProjectBlock value: each asString ] ].

	^ noneBlock value
]

{ #category : #accessing }
BaselineBuilder >> processDependencies [
	packageNames := Array streamContents: [ :stream |
		 RPackageOrganizer default packageNamesDo: [ :each |
			(self isInternal: each) ifTrue: [ 
				stream nextPut: each asString ] ] ].

	packageRequirements := Dictionary new.
	packageNames do: [ :each |
		packageRequirements at: each asString put: Set new ].

	relationGraph := DAPackageRelationGraph onPackagesNamed: packageNames.
	relationGraph build.
	relationGraph packages do: [:aDAPackage |
		aDAPackage dependencies do: [:each | 
			self ensureRequire: each source to: each target ] ]

]

{ #category : #accessing }
BaselineBuilder >> projectName: aString [ 
	projectName := aString.
	self internalPackageNamesPrefix: aString.
]

{ #category : #private }
BaselineBuilder >> selectorForBaselineMethod [
	^ 'baseline:'
]

{ #category : #private }
BaselineBuilder >> selectorForExternalProject: aString [
	^ 'declare{1}On:' format: {aString}
]

{ #category : #private }
BaselineBuilder >> sourceForBaselineMethod [
	^ '{1} spec
	<baseline>
	spec
		for: #common
		do: [ {2}{3} ]' format: {
		self selectorForBaselineMethod.
		self sourceForExternalProjectDeclarations.
		self sourceForPackageDeclarations }
]

{ #category : #private }
BaselineBuilder >> sourceForExternalProject: aString [
	^ '{1} spec
	spec
		baseline: {2}
		with: [ spec repository: {3} ]'
		format: {
			self selectorForExternalProject: aString.
			aString asString printString.
			'github://USER_NAME/PROJECT_NAME:COMMITISH/SRC_DIRECTORY' printString }
]

{ #category : #private }
BaselineBuilder >> sourceForExternalProjectDeclarations [
	externalProjectNames ifEmpty: [ ^'' ].

	^ String streamContents: [ :stream |
		stream cr; tab; tab; tab; << 'self'; cr.
		externalProjectNames sorted
			do: [ :each |
				stream tab; tab; tab; tab; << (self selectorForExternalProject: each); << ' spec' ]
			separatedBy: [
				stream << ';'; cr ].
		stream << '.'; cr.
		]
]

{ #category : #private }
BaselineBuilder >> sourceForPackageDeclarations [
	^ String streamContents: [ :stream |
		stream cr; tab; tab; tab; << 'spec'; cr.
		packageNames sorted
			do: [ :each |
				stream tab; tab; tab; tab; << 'package: '; print: each.
				(packageRequirements at: each) ifNotEmpty: [ :requirements |
					stream << (' with: [ spec requires: {1} ]' format: { requirements asArray sorted }) ] ]
			separatedBy: [
				stream << ';'; cr ].
		stream << '.'; cr.
		]
]
