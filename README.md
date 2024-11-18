# Metacello's BaselineOf Builder for Pharo

[![CI](https://github.com/tinchodias/BaselineBuilder/actions/workflows/main.yml/badge.svg)](https://github.com/tinchodias/BaselineBuilder/actions/workflows/main.yml)

Fastly create a `BaselineOf` for your new [Pharo](https://pharo.org/) project. You specify a project name (the prefix of your packages) and a list of external project names (the prefix of packages in which your packages depend). The builder analyses your code to establish the internal and external relations between packages, and generates a boilerplate `BaselineOf` for you. The package relations are established using the static analysis provided by [Christophe Demarey's](https://github.com/demarey/) `Tool-DependencyAnalyser`, which comes with Pharo.


## Example of use

**Scenario:** You started to code `XYZ`, a new project in Pharo that has several packages such as `XYZ-Core`, `XYZ-Examples` and `XYZ-Tests`.
The project needs other ("external") projects that you loaded via Metacello during the coding session: [Bloc](https://github.com/pharo-graphics/Bloc) and [Chalten](https://github.com/ba-st/Chalten). 
Everything looks good enough to be exported out of the current Pharo image.
So, you already created a github repository, Iceberg helped you create the `.project` meta-data file, and you added all `XYZ-*` packages to the repository. 

**GREAT**👍 Now this builder will help you!

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

5. Add the new `BaselineOfXYZ` package in Iceberg, commit and push.

6. Copy to clipboard "Install" script for your `README.md`:
~~~Smalltalk
BaselineBuilder new
	projectName: 'XYZ';
	copyToClipboardInstallMarkdownWith: 'github://MyUserName/XYZ'
~~~

7. Paste from clipboard in your `README.md`.


## Troubleshooting

By default, the project name is a taken as a prefix of the internal package names. If it's not the case, there are `internalPackageNamesPrefix:` and `internalPackageNamesRegex:` to specify something else.

## License
The code is licensed under [MIT](LICENSE).
