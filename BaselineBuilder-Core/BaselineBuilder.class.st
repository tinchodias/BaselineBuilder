"
I'm a builder for Metacello's `BaselineOf`.

Quickly create a BaselineOf for your new Pharo project. You specify a project name (the prefix of your packages) and a list of external project names (the prefix of packages in which your packages depend). The builder analyses your code to establish the internal and external relations between packages, and generates a boilerplate BaselineOf for you. The package relations are established using the static analysis provided by Pharo's Tool-DependencyAnalyser.

## How to use it

**Example scenario:** You started to code `XYZ`, a new project with several packages such as `XYZ-Core`, `XYZ-Examples` and `XYZ-Tests`.
The project needs other (""external"") projects that you loaded via Metacello during the coding session: [Bloc](https://github.com/pharo-graphics/Bloc) and [Chalten](https://github.com/ba-st/Chalten). 
Everything looks good enough to be exported out of the current Pharo image.
So, you already created a github repository, you create the `.project` meta-data file via Iceberg, and you added and pushed all `XYZ-*` packages to the repository.

Now is the moment when this builder can help:

### Steps

First. Specify the parameters and build the new (draft) baseline:
```
BaselineBuilder new
	projectName: 'XYZ';
	externalProjectNames: #(Bloc Chalten);
	build;
	browseBuiltBaselineClass.
```

Second. for each external project, you must fill the repo's URL, and may need choose a specific baseline group such as `loads: #full`. 

Third. Remove the `Draft` suffix from the class name, when ready.

Fourth. In Iceberg, add the new `BaselineOfXYZ` package and commit.

Fifth. This script copies to clipboard a ""Install"" script for your `README.md` (now just paste it):
```
BaselineBuilder new
	projectName: 'XYZ';
	copyToClipboardInstallMarkdownWith: 'github://MyUserName/XYZ'
```

## Custom packages prefix/regex

By default, the builder considers internal to any package prefixed by the project's name as a prefix (`XYZ-*` in the example). You need to customize the criteria when it's not the case. Builder's API provides `internalPackageNamesPrefix:` and `internalPackageNamesRegex:` to specify something else.

"
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

	^ 'BaselineOf', (projectName copyWithoutAll: '-')
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
		self class packageOrganizer packageNamesDo: [ :each |
			(self isInternal: each) ifTrue: [ 
				stream nextPut: each asString ] ] ].

	packageRequirements := Dictionary new.
	packageNames do: [ :each |
		packageRequirements at: each asString put: Set new ].

	relationGraph := StPackageRelationGraph onPackagesNamed: packageNames.
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
