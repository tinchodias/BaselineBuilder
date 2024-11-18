Class {
	#name : 'BaselineBuilder',
	#superclass : 'Object',
	#instVars : [
		'projectName',
		'packageNames',
		'relationGraph',
		'baselineClass',
		'packageRequirements',
		'externalProjectNames',
		'internalPackageNamesRegex'
	],
	#category : 'BaselineBuilder-Core',
	#package : 'BaselineBuilder-Core'
}

{ #category : 'examples' }
BaselineBuilder class >> example0 [
	self new
		projectName: 'BaselineBuilder';
		externalProjectNames: #();
		build;
		browseBuiltBaselineClass
]

{ #category : 'examples' }
BaselineBuilder class >> example1 [
	self new
		projectName: 'MongoClientMonitor';
		externalProjectNames: #(Roassal3 Tealight TaskIt);
		build;
		browseBuiltBaselineClass
]

{ #category : 'examples' }
BaselineBuilder class >> example2 [
	self new
		projectName: 'Iceberg';
		externalProjectNames: #(LibGit);
		build;
		browseBuiltBaselineClass
]

{ #category : 'accessing' }
BaselineBuilder >> baselineClass [
	^ baselineClass
]

{ #category : 'accessing' }
BaselineBuilder >> baselineClassName [
	^ self baselinePackageName, 'Draft'
]

{ #category : 'accessing' }
BaselineBuilder >> baselinePackageName [
	^ 'BaselineOf', projectName
]

{ #category : 'accessing' }
BaselineBuilder >> browseBuiltBaselineClass [
	baselineClass browse
]

{ #category : 'building' }
BaselineBuilder >> build [
	self processDependencies.
	self createBaselineClass.
]

{ #category : 'accessing' }
BaselineBuilder >> copyToClipboardInstallMarkdownWith: urlString [

	Clipboard
		clipboardText: (self installMarkdownWith: urlString)
		informing: 'Markdown fragment copied to clipboard'
]

{ #category : 'private' }
BaselineBuilder >> createBaselineClass [

	baselineClass :=
		SystemVersion current major < 12
			ifTrue: [ self newBaselineClassBeforePharo12 ]
			ifFalse: [ self newBaselineClassSincePharo12 ].

	baselineClass
		compile: self sourceForBaselineMethod
		classified: #baseline.

	externalProjectNames do: [ :each |
		baselineClass
			compile: (self sourceForExternalProject: each)
			classified: #'external projects' ]
]

{ #category : 'private' }
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

{ #category : 'accessing' }
BaselineBuilder >> externalProjectNames: aCollectionOfStrings [
	externalProjectNames := aCollectionOfStrings
]

{ #category : 'private' }
BaselineBuilder >> guessLocationForPackageNamed: aString [

	IceRepository registry
		detect: [ :each | each includesPackageNamed: aString ]
		ifFound: [ :iceRepository |
			iceRepository remotes ifNotEmpty: [ :remotes |
				^ self guessLocationFromRemote: remotes first ] ].
	
	^ 'TYPE://USER_NAME/PROJECT_NAME:COMMITISH/SRC_DIRECTORY'

]

{ #category : 'private' }
BaselineBuilder >> guessLocationFromRemote: anIceRemote [

	| type |
	type := anIceRemote host = 'github.com'
		        ifTrue: [ 'github' ]
		        ifFalse: [ 'TYPE' ].

	^ '{1}://{2}/{3}:COMMITISH/{4}' format: { 
			  type.
			  anIceRemote owner.
			  anIceRemote projectName.
			  anIceRemote localRepository project sourceDirectory }
]

{ #category : 'accessing' }
BaselineBuilder >> installMarkdownWith: urlString [

	^ '
## Installation

Evaluate in Pharo:

```Smalltalk
{1}
```
' format: { 
	self installScriptWith: urlString }
]

{ #category : 'accessing' }
BaselineBuilder >> installScriptWith: urlString [

	^ '
Metacello new
	baseline: ''{1}'';
	repository: ''{2}'';
	load
' format: { 
	  projectName.
	  urlString }
]

{ #category : 'accessing' }
BaselineBuilder >> internalPackageNamesPrefix: aString [ 
	self internalPackageNamesRegex: (aString, '.*') asRegex
]

{ #category : 'accessing' }
BaselineBuilder >> internalPackageNamesRegex: aRegex [
	internalPackageNamesRegex := aRegex
]

{ #category : 'private' }
BaselineBuilder >> isInternal: aPackageName [
	^ internalPackageNamesRegex matches: aPackageName
]

{ #category : 'private' }
BaselineBuilder >> newBaselineClassBeforePharo12 [

	^ BaselineOf
		subclass: self baselineClassName asSymbol
		instanceVariableNames: ''
		classVariableNames: ''
		package: self baselinePackageName asString
]

{ #category : 'private' }
BaselineBuilder >> newBaselineClassSincePharo12 [

	^ BaselineOf classInstaller make: [ :builder |
		  builder
			  superclass: BaselineOf;
			  name: self baselineClassName asSymbol;
			  slotsFromString: '';
			  sharedVariablesFromString: '';
			  package: self baselinePackageName asString;
			  environment: BaselineOf environment ]
]

{ #category : 'private' }
BaselineBuilder >> parseTarget: aPackageName ifInternal: internalPackageBlock ifExternalProject: externalProjectBlock ifNone: noneBlock [

	(self isInternal: aPackageName) ifTrue: [
		^ internalPackageBlock value: aPackageName asString ].

	externalProjectNames do: [ :each |
		(aPackageName beginsWith: each)
			ifTrue: [ ^ externalProjectBlock value: each asString ] ].

	^ noneBlock value
]

{ #category : 'accessing' }
BaselineBuilder >> processDependencies [

	packageNames := Array streamContents: [ :stream |
		self packageOrganizer packageNamesDo: [ :each |
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

{ #category : 'accessing' }
BaselineBuilder >> projectName: aString [ 
	projectName := aString.
	self internalPackageNamesPrefix: aString.
]

{ #category : 'private' }
BaselineBuilder >> selectorForBaselineMethod [
	^ 'baseline:'
]

{ #category : 'private' }
BaselineBuilder >> selectorForExternalProject: aString [
	^ 'declare{1}On:' format: {aString}
]

{ #category : 'private' }
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

{ #category : 'private' }
BaselineBuilder >> sourceForExternalProject: aString [

	^ '{1} spec
	spec
		baseline: {2}
		with: [ spec repository: {3} ]'
		format: {
			self selectorForExternalProject: aString.
			aString asString printString.
			(self guessLocationForPackageNamed: aString) printString }
]

{ #category : 'private' }
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

{ #category : 'private' }
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
