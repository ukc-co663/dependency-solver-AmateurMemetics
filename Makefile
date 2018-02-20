STACK := $(shell command -v stack 2> /dev/null)

all:
ifndef STACK
	@apt update
	@apt install curl -y
	@curl -sSL https://get.haskellstack.org/ | sh
endif
	@stack setup
	@stack build
