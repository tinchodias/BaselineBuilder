# Metacello's BaselineOf Builder for Pharo

[![Build Status](https://travis-ci.org/tinchodias/BaselineBuilder.png)](http://travis-ci.org/tinchodias/BaselineBuilder)

Fastly create a `BaselineOf` for your new [Pharo](https://pharo.org/) project. You specify a project name (the prefix of your packages) and a list of external project names (the prefix of packages in which your packages depend). The builder analyses your code to establish the internal and external relations between packages, and generates a boilerplate `BaselineOf` for you. The package relations are established using the static analysis provided by [Christophe Demarey's](https://github.com/demarey/) `Tool-DependencyAnalyser`, which comes with Pharo.

## Installation

Evaluate the following script in a Pharo (7...11 should work):

~~~Smalltalk
Metacello new
    baseline: 'BaselineBuilder';
    repository: 'github://tinchodias/BaselineBuilder';
    load.
~~~

## Example of use

### Your new XYZ project

You started to code `XYZ`, a new project in Pharo that has several packages such as `XYZ-Core`, `XYZ-Examples` and `XYZ-Tests`.
The project needs other ("external") projects that you loaded via Metacello during the coding session: [Roassal3](https://github.com/ObjectProfile/Roassal3) and [Chalten](https://github.com/ba-st/Chalten). 
Everything looks good enough to be exported out of the current Pharo image.
You created a git repository, Iceberg helped you create the `.project` meta-data file, and you selected the `XYZ-*` packages to the repository and finally commited them. 

Now, you want to create a first `BaselineOfXYZ` to automate the installation in new Pharo images.
It will be a new subclass of `BaselineOf` with some methods that declare the *internal* dependencies (between your `XYZ-*` packages), and the *external* dependencies (with other projects).

### Create BaselineOfXYZ
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
* Rename the class without the `Draft` suffix.
* For each external project you will need to fill the url and tweak a bit (e.g. add a `loads: #full` to choose a specific baseline group). 
* Potentially, add you own package groups and other possible customizations.

Finally, you just need to add the new `BaselineOfXYZ` package and commit.

**Note**: By default, the project name is a taken as a prefix of the internal package names. If it's not the case, there are `internalPackageNamesPrefix:` and `internalPackageNamesRegex:` to specify something else.

### Add install subsection to your README

The builder can help by creating a markdown fragment to be appended to the project README:

~~~Smalltalk
BaselineBuilder new
	projectName: 'XYZ';
	copyToClipboardInstallMarkdownWith: 'github://MyUserName/XYZ'
~~~

Then, paste the clipboard contents at the end of your README file.

## License
The code is licensed under [MIT](LICENSE).
