all: 2048.exe

FSharpx.Core.1.8.41/lib/40/FSharpx.Core.dll:
	nuget install FSharpx.Core

2048.exe: 2048.fs 2048.fsi FSharpx.Core.1.8.41/lib/40/FSharpx.Core.dll
	fsharpc -r FSharpx.Core.1.8.41/lib/40/FSharpx.Core.dll 2048.fsi 2048.fs
