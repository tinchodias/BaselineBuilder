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
		'baselineClass',
		'internalPackageNamesRegex',
		'externalPackageNamePrefixes',
		'packageRequirementsProcessed',
		'baselineClassSuffix',
		'primaryPackageRequirements',
		'secondaryPackageRequirements',
		'externalRequirements'
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

	^ self baselinePackageName, baselineClassSuffix
]

{ #category : 'accessing' }
BaselineBuilder >> baselineClassSuffix: aString [

	baselineClassSuffix := aString
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

	baselineClass := self newBaselineClass.

	baselineClass
		compile: self sourceForBaselineMethod
		classified: #baselines.

	"Add declaration methods (unless they already exist)"
	externalPackageNamePrefixes
		reject: [ :each |
			(baselineClass includesSelector: (self selectorForExternalProject: each) asSymbol) ]
		thenDo: [ :each |
			baselineClass
				compile: (self sourceForExternalProject: each)
				classified: #'external projects' ]
]

{ #category : 'private' }
BaselineBuilder >> ensureRequireDependency: aStPackageDependency [

	| sourcePackageName |
	sourcePackageName := aStPackageDependency source packageName asString.

	self
		parseTarget: aStPackageDependency target packageName
		ifInternal: [ :name |
			((aStPackageDependency causesLoadingError
				ifTrue: [ primaryPackageRequirements ]
				ifFalse: [ secondaryPackageRequirements ]) at: sourcePackageName) add: name ]
		ifExternalProject: [ :name |
			(externalRequirements at: sourcePackageName) add: name ]
		ifNone: [ ^ self ].
	

]

{ #category : 'private' }
BaselineBuilder >> externalBaselineNameFor: aPackageNamePrefix [

	^ aPackageNamePrefix copyWithoutAll: '-'
]

{ #category : 'accessing' }
BaselineBuilder >> externalProjectNames: aCollectionOfStrings [

	externalPackageNamePrefixes := aCollectionOfStrings
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

{ #category : 'initialization' }
BaselineBuilder >> initialize [

	super initialize.
	
	externalPackageNamePrefixes := #().
	baselineClassSuffix := 'Draft'
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

	self internalPackageNamesRegex: (aString , '.*') asRegex
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
BaselineBuilder >> newBaselineClass [

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

	externalPackageNamePrefixes do: [ :each |
		(aPackageName beginsWith: each)
			ifTrue: [ ^ externalProjectBlock value: (self externalBaselineNameFor: each asString) ] ].

	^ noneBlock value
]

{ #category : 'private' }
BaselineBuilder >> printRequirements: requirements on: stream [

	stream cr; tab: 4; << 'with: [ spec requires: '.
	(requirements sum: #size) < 50
		ifTrue: [
			"inline"
			stream print: requirements; << ' ]' ]
		ifFalse: [
			"multiple lines"
			stream << '#('.
			requirements do: [ :eachRequirement |
				stream cr; tab: 5; print: eachRequirement ].
			stream << ') ]' ]
]

{ #category : 'private' }
BaselineBuilder >> processDependencies [

	| relationGraph |
	packageNames := Array streamContents: [ :stream |
		self class packageOrganizer packageNamesDo: [ :each |
			(self isInternal: each) ifTrue: [ 
				stream nextPut: each asString ] ] ].

	externalRequirements := Dictionary new.
	primaryPackageRequirements := Dictionary new.
	secondaryPackageRequirements := Dictionary new.
	packageNames do: [ :each |
		externalRequirements at: each asString put: Set new.
		primaryPackageRequirements at: each asString put: Set new.
		secondaryPackageRequirements at: each asString put: Set new ].

	relationGraph := StPackageRelationGraph onPackagesNamed: packageNames.
"	relationGraph shouldComputeMessageSendDependencies: false. ""They dont produce loading errors""
"	relationGraph build.
	relationGraph packages do: [ :eachPackage |
		eachPackage dependencies do: [ :eachDependency |
			self ensureRequireDependency: eachDependency ] ].

	self processDependencyCycles
]

{ #category : 'private' }
BaselineBuilder >> processDependencyCycles [

	| pendingPackageNames |
	packageRequirementsProcessed := OrderedDictionary new.
	pendingPackageNames := packageNames sorted.

	[ pendingPackageNames isEmpty ] whileFalse: [ 
		| rows chosenRow chosenPackageName ignoredRequirements |
		rows := pendingPackageNames collect: [ :each |
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

		pendingPackageNames := pendingPackageNames copyWithout: chosenPackageName ]
]

{ #category : 'accessing' }
BaselineBuilder >> projectName: aString [

	projectName := aString.
	self internalPackageNamesPrefix: aString
]

{ #category : 'private' }
BaselineBuilder >> selectorForBaselineMethod [

	^ 'baseline:'
]

{ #category : 'private' }
BaselineBuilder >> selectorForExternalProject: aString [

	^ 'declare{1}On:' format: { self externalBaselineNameFor: aString }
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

	externalPackageNamePrefixes ifEmpty: [ ^ '' ].

	^ String streamContents: [ :stream |
		stream cr; tab: 3; << 'self'; cr.
		externalPackageNamePrefixes sorted
			do: [ :each |
				stream tab: 4; << (self selectorForExternalProject: each); << ' spec' ]
			separatedBy: [
				stream << ';'; cr ].
		stream << '.'; cr ]
]

{ #category : 'private' }
BaselineBuilder >> sourceForPackageDeclarations [

	^ String streamContents: [ :stream |
		packageRequirementsProcessed associations
			do: [ :assoc |
				assoc value second ifNotEmpty: [ :ignored |
					stream
						cr; tab: 3;
						<< '"Cycles ignored '.
					ignored asArray sorted printElementsOn: stream.
					stream << '"' ].
				stream cr; tab: 3; << 'spec'.
				assoc value first asArray sorted
					ifEmpty: [ stream << ' package: '; print: assoc key ]
					ifNotEmpty: [ :requirements |
						stream cr; tab: 4; << 'package: '; print: assoc key.
						self printRequirements: requirements on: stream ] ]
			separatedBy: [ stream << '.'; cr ] ]
]
