# Baseline Builder for Pharo

[![CI](https://github.com/tinchodias/BaselineBuilder/actions/workflows/main.yml/badge.svg)](https://github.com/tinchodias/BaselineBuilder/actions/workflows/main.yml)

Quickly create a `BaselineOf` for your new [Pharo](https://pharo.org/) project. You specify a project name (the prefix of your packages) and a list of external project names (the prefix of packages in which your packages depend). The builder analyses your code to establish the internal and external relations between packages, and generates a boilerplate `BaselineOf` for you. The package relations are established using the static analysis provided by Pharo's `Tool-DependencyAnalyser`.


## How to use it

**Example scenario:** You started to code `XYZ`, a new project with several packages such as `XYZ-Core`, `XYZ-Examples` and `XYZ-Tests`.
The project needs other ("external") projects that you loaded via Metacello during the coding session: [Bloc](https://github.com/pharo-graphics/Bloc) and [Chalten](https://github.com/ba-st/Chalten). 
Everything looks good enough to be exported out of the current Pharo image.
So, you already created a github repository, you create the `.project` meta-data file via Iceberg, and you added and pushed all `XYZ-*` packages to the repository.

**GREAT**👍 Now this builder can help you!

### Steps

1. Install the builder:
~~~Smalltalk
Metacello new
    baseline: 'BaselineBuilder';
    repository: 'github://tinchodias/BaselineBuilder';
    load.
~~~

2. Specify the parameters and build the new (draft) baseline:
~~~Smalltalk
BaselineBuilder new
	projectName: 'XYZ';
	externalProjectNames: #(Bloc Chalten);
	build;
	browseBuiltBaselineClass.
~~~

3. For each external project, you must fill the repo's URL, and may need choose a specific baseline group such as `loads: #full`. 

4. Remove the `Draft` suffix from the class name, when ready.

5. In Iceberg, add the new `BaselineOfXYZ` package and commit.

6. This script copies to clipboard a "Install" script for your `README.md` (now just paste it):
~~~Smalltalk
BaselineBuilder new
	projectName: 'XYZ';
	copyToClipboardInstallMarkdownWith: 'github://MyUserName/XYZ'
~~~


## Custom packages prefix/regex

By default, the builder considers internal to any package prefixed by the project's name as a prefix (`XYZ-*` in the example). You need to customize the criteria when it's not the case. Builder's API provides `internalPackageNamesPrefix:` and `internalPackageNamesRegex:` to specify something else.


## License

The code is licensed under [MIT](LICENSE).
