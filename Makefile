STACK := $(shell command -v stack 2> /dev/null)

all:
ifndef STACK
	@apt install curl
	@curl -sSL https://get.haskellstack.org/ | sh
endif
	@stack setup
	@stack build
