Options:Default "trace"

Tasks:clean()

Tasks:minify "minify" {
	input = "/bin/userspace.lua",
	output = "/bin/userspace.min.lua",
}

Tasks:require "main" {
	include = "*.lua",
	startup = "userspace.lua",
	output = "/bin/userspace.lua",
}

Tasks:Task "build" { "clean", "minify" } :Description "Main build task"


Tasks:Default "main"