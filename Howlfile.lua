Options:Default "trace"

Tasks:clean()

Tasks:minify "minify" {
	input = "/sys/boot/kernel.lua",
	output = "/sys/boot/kernel.min.lua",
}

Tasks:require "main" {
	include = "*.lua",
	startup = "kernel.lua",
	output = "/bin/kernel.lua",
}

Tasks:Task "build" { "clean", "minify" } :Description "Main build task"


Tasks:Default "main"