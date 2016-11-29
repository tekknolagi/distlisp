CC=erlc
SOURCES=basis.erl concurrency.erl eval.erl parser.erl reader.erl repl.erl\
		thread_pool.erl
OBJECTS=$(SOURCES:.erl=.beam)

all: $(OBJECTS) parser

%.beam: %.erl
	$(CC) $<

parser: scanner.xrl parser.yrl
	./mkparser.erl
