# pharo-metacello-baseline-builder
Create a Metacello Baselines from scratch on your new project.

## How to use it

You started to code a new project, you loaded external projects via Metacello each time you needed one, and everything looks good to export and publish the code.
You will create a git repository, create the project meta-data using Iceberg, add the packages to the repository. 
Everything fine for the moment, but you also to create a Metacello Baseline to ease installation in new Pharo images.
This builder helps you to new subclass of `BaselineOf` with methods that declare the internal and external dependencies of your packages.

~~~Smalltalk
BaselineBuilder new
		projectName: 'Iceberg';
		externalProjectNames: #(LibGit);
		build;
		browseBuiltBaselineClass.
~~~

The result with be a browser open on this new class:

~~~Smalltalk
BaselineOf subclass: #BaselineOfIcebergDraft
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'BaselineOfIceberg'
~~~

This is a template where you still need to fill urls. 
Note you will have to rename the class without the 'Draft' suffix before committing it.


# Installation

Evaluate the following script in a Pharo 8 or 9:

~~~Smalltalk
Metacello new
    baseline: 'BaselineBuilder';
    repository: 'github://tinchodias/pharo-metacello-baseline-builder';
    load.
~~~


