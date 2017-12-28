.PHONY: build clean test

build:
	jbuilder build @install

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

doc:
	jbuilder build @doc
