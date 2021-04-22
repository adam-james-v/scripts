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

You can also open scripts.org in emacs and use cider-connect to build/use script code interactively.

In a terminal, in the project's top level launch an nrepl with babashka

```
bb --nrepl-server
```

Then, in emacs using CIDER, you can connet to the REPL. The below example has the default host/port.

```
M-x cider-connect RET
localhost
1667
```

You can use other REPL/emacs plugin combinations. The [babashka book](https://book.babashka.org/) is a good place to start when looking for setup instructions and ideas.

Once you're connected, you can load any script and get hacking! If I want to mess around with the 'vidwiz' script, I would eval the following in the REPL to load the code and enter the ns:

```clojure
(load-file "src/vidwiz.clj")

(ns vidwiz.main
  "This is a prototype script for automating a portion of my video editing using ffmpeg."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [hiccup.core :refer [html]]
            [svg-clj.main :as svg]
            [svg-clj.path :as path]
            [svg-clj.transforms :as tf]))
```

Just be aware of any scripts with side-effects!

## Scripter/Uberscripter

Uberscripter uses babashka's --uberscript option to create a single-file script. It looks at the file's deps and copies code into the file.

Scripter is much simpler and just copies a src script into build and makes it executable. This is useful for situations where a script doesn't work nicely with uberscript, for example, if your script uses clojure.spec.

The build folder is on my PATH env variable, so as soon as a script is moved and made executable, it becomes available from anywhere. You will need to start a new terminal session or refresh your current one to see this take effect.

## Contribution

Feel free to fork, clone, make PRs, whatever.

Changes will be implemented at my discretion, because these are scripts tuned to my needs/workflow anyway.

You can create issues to bring up improvements, questions, and ideas as well, if you'd like. 

Be kind and constructive.
