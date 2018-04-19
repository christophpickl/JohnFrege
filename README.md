# About

This is a repo containing a small workshop to work with Frege, a Haskell implementation for the JVM.

https://github.com/Frege/frege


# Misc

* use eclipse plugin (better plugin support so far)
	* from here: https://github.com/Frege/eclipse-plugin
	* or use good old intellij https://github.com/Frege/frege/wiki/Using-Frege-in-Intellij-IDEA
* documentation:
	* MUST:
		* Nice tutorial: https://dierk.gitbooks.io/fregegoodness/
		* API documentation: http://www.frege-lang.org/doc/fregedoc.html
	* OPTIONAL
		* Haskell tutorial: http://haskellbook.com/
		* Why functional langs paper: https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf
		* for Mac only: use Dash with haskell ;)

# TODO

* where can get some nice docu?
	* use dash for haskell

# Workshop Notes

* present basic programming language concepts (paradigms: declarative VS imperative)
* who was gottlob frege => philo studies samples
* use FOP complang samples
* topics:
    * rough basics of functional langs
    * kotlin std lib functionals

# Basic Intro

* Maven and Gradle support
	* Last commit for Gradle plugin 2016 :( https://github.com/Frege/frege-gradle-plugin
* Setup for IntelliJ
    * Configure Frege lib
        * Download latest Frege JAR: https://github.com/Frege/frege/releases
        * Add new global lib
    * Open HelloFrege project
    * Run frege compiler manually like so: java -Xss1m -classpath /Users/cpickl/Workspace/_private/JohnFrege/frege3.24.405.jar frege.compiler.Main -inline -make -d /Users/cpickl/Workspace/_private/JohnFrege/HelloFrege/target/frege -sp /Users/cpickl/Workspace/_private/JohnFrege/HelloFrege/src/main/frege /Users/cpickl/Workspace/_private/JohnFrege/HelloFrege/


Cannot complete the install because one or more required items could not be found.
  Software being installed: Frege Development 3.24.366 (frege.feature.feature.group 3.24.366)
  Missing requirement: Frege Development 3.24.366 (frege.feature.feature.group 3.24.366) requires 'impulse 0.0.0' but it could not be found
