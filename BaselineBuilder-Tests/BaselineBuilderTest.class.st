Class {
	#name : #BaselineBuilderTest,
	#superclass : #TestCase,
	#instVars : [
		'builder'
	],
	#category : #'BaselineBuilder-Tests'
}

{ #category : #tests }
BaselineBuilderTest >> tearDown [
	builder baselineClass ifNotNil: #removeFromSystem
]

{ #category : #tests }
BaselineBuilderTest >> test01NoExternalProject [
	| builtClass |
	builder := BaselineBuilder new
		projectName: 'Iceberg';
		externalProjectNames: #();
		build;
		yourself.

	builtClass := builder baselineClass.
	self assert: builtClass isClass.
	self assert: builtClass selectors sorted equals: #(baseline:).
]

{ #category : #tests }
BaselineBuilderTest >> test02OneExternalProject [
	| builtClass |
	builder := BaselineBuilder new
		projectName: 'Iceberg';
		externalProjectNames: #(LibGit);
		build;
		yourself.
	
	builtClass := builder baselineClass.
	self assert: builtClass isClass.
	self assert: builtClass selectors sorted equals: #(baseline: declareLibGitOn:).
]
