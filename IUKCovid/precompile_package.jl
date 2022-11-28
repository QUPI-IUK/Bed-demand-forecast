using Pkg


Pkg.activate("IUKCovid")

Pkg.Registry.update()
Pkg.resolve() # Update the manifest with possible changes in the packages
Pkg.instantiate() # should install not up to date dependencies

Pkg.add("PackageCompiler"); # in case it was not installed yet

import PackageCompiler

Pkg.precompile()

PackageCompiler.create_sysimage(["IUKCovid"];
	sysimage_path = ARGS[1],
	precompile_execution_file = joinpath("IUKCovid", "call_template.jl") # should we make this into an argument too?
)

rm(ARGS[2]) # remove std-out
rm(ARGS[3]) # remove std-err
