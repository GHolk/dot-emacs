
all: *.elc

%.elc: %.el
	emacsclient --eval '(byte-compile-file "$<")'
