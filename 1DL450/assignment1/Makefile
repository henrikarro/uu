.PHONY: all compile compile_tests beamdir dialyzer eunit proper doc clean distclean
.SUFFIXES: .erl .beam

APPNAME=vector_server

ERLCFLAGS=-Werror +debug_info
DIALYZERFLAGS=-n -Wunderspecs -Wunmatched_returns

SRCDIR=.
TESTDIR=./
BEAMDIR=./ebin
DOCDIR=./doc

SOURCES=$(wildcard $(SRCDIR)/*.erl)
TESTSOURCES=$(wildcard $(TESTDIR)/*.erl)
OBJECTS=$(SOURCES:$(SRCDIR)/%.erl=$(BEAMDIR)/%.beam)
TESTOBJECTS=$(TESTSOURCES:$(TESTDIR)/%.erl=$(BEAMDIR)/%.beam)

#VPATH=$(SRCDIR):$(TESTDIR)

all: eunit proper

$(BEAMDIR)/%.beam: %.erl
	erlc $(ERLCFLAGS) -o $(BEAMDIR) $<

compile: $(BEAMDIR) $(OBJECTS)

compile_tests: compile $(TESTOBJECTS)

$(BEAMDIR):
	mkdir -p $(BEAMDIR)

dialyzer: compile
	dialyzer $(DIALYZERFLAGS) $(BEAMDIR)/*.beam

eunit: compile_tests
	erl -noshell -pa ebin -eval 'eunit:test("$(BEAMDIR)",[verbose])' -s init stop

proper: compile_tests
	erl -noshell -pa $(BEAMDIR) -eval "proper:module($(APPNAME)_proper)" -s init stop

doc:
	mkdir -p $(DOCDIR)
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop

clean:
	rm -rf $(BEAMDIR)
	rm -rf erl_crash.dump

distclean: clean
	rm -rf $(DOCDIR)
