Session 1 - Introduction
---

# Introduction

* Frege who...? => philo studies samples
* Programming paradigms
* Functional concepts
* Why functional?

# Setup

* download latest eclipse and configure it properly
	* increase memory settings in `eclipse.ini`:
	```
	-vmargs
	-Dosgi.requiredJavaVersion=1.8
	-Xss4m
	-Xms400m
	-Xmx2048m
	```
	* use UTF-8 encoding
	* use spaces instead of tabs
* install the eclipse plugin
	* basically just an update site pointing to:  http://www.frege-lang.org/fregide/
	* or follow the complete tutorial: https://github.com/Frege/eclipse-plugin
	* PS: eclipse plugin is far superior than working with intellij
* create frege project:
	* hit New / Other
	* in the dialog select Frege Project
	* enter some arbitrary name and hit finish
* create and run file:
	* create a file `hello.fr` in the `src/` folder
	```
	module HelloFrege where

	main _ = do
    	println "Hello Frege"
    ```
    * create a new run configuration:
    	* select Java Application
    	* the Main class has to be the same name as the module name defined in the frege file, which is in our case: `HelloFrege`
    * hit run and you should see the text in the console, bravo!

## Tools


* Maven and Gradle support
	* Last commit for Gradle plugin 2016 :( https://github.com/Frege/frege-gradle-plugin

# First Steps

* custom functions
* type inference
* list comprehension
* recursions! (no more loops)

