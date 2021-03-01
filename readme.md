# adam-james-scripts

A collection of scripts. Primarily built with Clojure/babashka, but may use other methods occasionally.

This is a long-term, slow moving project, perhaps only ever useful to me. But I like the idea of keeping some utility scripts nicely organized in this repo.

Notes can be found in the [scripts.org](https://github.com/adam-james-v/scripts/blob/main/scripts.org) file at the top level of this project.

Everything's free to use and modify.

## Usage
I use scripter.clj and uberscripter.clj to 'build' my src scripts. 

```
bb uberscripter.clj src/some-script.clj
```

Uberscripter uses babashka's --uberscript option to create a single-file script. It looks at the file's deps and copies code into the file.

Scripter is much simpler and just copies a src script into build and makes it executable. This is useful for situations where a script doesn't work nicely with uberscript, for example, if your script uses clojure.spec.

The build folder is on my PATH env variable, so as soon as a script is moved and made executable, it becomes available from anywhere. You will need to start a new terminal session or refresh your current one to see this take effect.
