Options:Default "trace"

Tasks:clean()

Tasks:minify "minify" {
	input = "/bin/kernel.lua",
	output = "/bin/kernel.min.lua",
}

Tasks:require "main" {
	include = "*.lua",
	startup = "kernel.lua",
	output = "/bin/kernel.lua",
}

Tasks:Task "build" { "clean", "minify" } :Description "Main build task"


Tasks:Default "main"