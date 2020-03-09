#!/bin/bash

# Back to root directory.
if ! [[ -n "$TRAVIS" ]]; then
    cd ..
fi

# Stop the server.
forever stop main.js
