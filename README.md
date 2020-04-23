# pharo-metacello-baseline-builder
Fastly create a BaselineOf for your new Pharo project.

## Scenario & How to use it

You started to code `XYZ`, a new project in Pharo that has several packages such as `XYZ-Core`, `XYZ-Examples` and `XYZ-Tests`.
The project needs other ("external") projects that you loaded via Metacello during the coding session: [Roassal3](https://github.com/ObjectProfile/Roassal3) and [Chalten](https://github.com/ba-st/Chalten). 
Everything looks good enough to be exported out of the current Pharo image.
You created a git repository, Iceberg helped you create the `.project` meta-data file, and you selected the `XYZ-*` packages to the repository and finally commited them. 

Now, you want to create a first `BaselineOfXYZ` to automate the installation in new Pharo images.
It will be a new subclass of `BaselineOf` with some methods that declare the *internal* dependencies (between your `XYZ-*` packages), and the *external* dependencies (with other projects).

This is the moment where this builder will help you. Evaluate in a workspace:
~~~Smalltalk
BaselineBuilder new
	projectName: 'XYZ';
	externalProjectNames: #(Roassal3 Chalten);
	build;
	browseBuiltBaselineClass.
~~~

You get a browser on this new class:
~~~Smalltalk
BaselineOf subclass: #BaselineOfXYZDraft
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'BaselineOfXYZ'
~~~

The class still needs some tasks to be ready:
* Rename the class without the 'Draft' suffix.
* For each external project you will need to fill the url and tweak a bit (e.g. add a `loads: #full` to choose a specific baseline group). 
* Potentially, add you own package groups.

Finally, you just need to add the new `BaselineOfXYZ` package and commit.


# Installation

Evaluate the following script in a Pharo 8 or 9:

~~~Smalltalk
Metacello new
    baseline: 'BaselineBuilder';
    repository: 'github://tinchodias/pharo-metacello-baseline-builder';
    load.
~~~


