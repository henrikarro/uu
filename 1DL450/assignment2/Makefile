.PHONY: all compile compile_tests beamdir dialyzer eunit proper doc clean distclean
.SUFFIXES: .erl .beam

APPNAME=afp_assignment_2

ZIPFILE=afp_assignment2.zip

ERLCFLAGS=-Werror +debug_info
#ERLCFLAGS=+debug_info
DIALYZERFLAGS=-n -Wunderspecs -Wunmatched_returns
EDOCOPTS={private, true}

SRCDIR=.
TESTDIR=./
BEAMDIR=./ebin
DOCDIR=./doc

SOURCES=$(wildcard $(SRCDIR)/*.erl)
TESTSOURCES=$(wildcard $(TESTDIR)/*.erl)
OBJECTS=$(SOURCES:$(SRCDIR)/%.erl=$(BEAMDIR)/%.beam)
TESTOBJECTS=$(TESTSOURCES:$(TESTDIR)/%.erl=$(BEAMDIR)/%.beam)

DISTFILES=README.txt magic.erl contracts.rkt dice.erl dice.rkt relations.rkt

#VPATH=$(SRCDIR):$(TESTDIR)

all: dialyzer eunit proper

$(BEAMDIR)/%.beam: %.erl
	erlc $(ERLCFLAGS) -o $(BEAMDIR) $<

compile: $(BEAMDIR) $(OBJECTS)

compile_tests: compile $(TESTOBJECTS)

$(BEAMDIR):
	mkdir -p $(BEAMDIR)

dialyzer: compile
	-dialyzer $(DIALYZERFLAGS) $(BEAMDIR)/*.beam

eunit: compile_tests
	erl -noshell -pa ebin -eval 'eunit:test("$(BEAMDIR)",[verbose])' -s init stop

rackunit:
	-raco test *.rkt

proper: compile_tests
	erl -noshell -pa $(BEAMDIR) -eval "proper:module(magic, [{numtests, 10000}])" -s init stop

doc:
	mkdir -p $(DOCDIR)
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(EDOCOPTS)])" -s init stop

dist: distclean
	zip $(ZIPFILE) $(DISTFILES)
clean:
	rm -rf $(BEAMDIR)
	rm -f *.beam
	rm -rf erl_crash.dump
	rm -rf $(ZIPFILE)

distclean: clean
	rm -rf $(DOCDIR)
