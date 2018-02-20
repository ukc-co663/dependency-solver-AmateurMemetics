STACK := $(shell command -v stack 2> /dev/null)

all:
ifndef STACK
	@apt-get install haskell-stack -y
endif
	@stack setup
	@stack build
