#!/bin/bash

if [ ! -d FSharpx.Core.* ]; then
	nuget install FSharpx.Core
fi

fsharpc -r FSharpx.Core.1.8.41/lib/40/FSharpx.Core.dll 2048.fsi 2048.fs
