#!/bin/bash
# Standard building
# Compile Elm
elm make src/Main.elm --optimize --output=public/index.js

# Compile SASS -> CSS
# sass --no-cache --sourcemap=none --update public/css/main.sass:public/css/main.css

# Debug Commands
# elm make src/Main.elm --output=public/index.js
# elm reactor
