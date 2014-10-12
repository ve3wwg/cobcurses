# test/Makefile.am for CobCurses

#############################################################
#       COBOL RELATED OPTIONS
#############################################################

COBWARN=-Wall -Wno-call-params
COBSTD=-std=cobol2002
COBOPTS=$(COBWARN) $(COBDBG) $(COBSTD) -I../copybk -free
COBOL=cobc

.cob.$(OBJEXT):
	$(COBOL) -c $(COBOPTS) $< -o $*.$(OBJEXT)

#############################################################

.PHONY:	preptest $(TESTS)

TESTS=test001.sh test002.sh test003.sh test004.sh test005.sh test006.sh \
    test007.sh test008.sh test009.sh test010.sh test011.sh test012.sh
BUILT_TESTS=test001$(EXEEXT) test002$(EXEEXT) test003$(EXEEXT) test004$(EXEEXT) \
    test005$(EXEEXT) test006$(EXEEXT) test007$(EXEEXT) test008$(EXEEXT) \
    test009$(EXEEXT) test010$(EXEEXT) test011$(EXEEXT) test012$(EXEEXT)
BUILT_EXES=templ$(EXEEXT) 3270test$(EXEEXT) testcurses$(EXEEXT)
BUILT_SRCS=TEMPLATE-WS.cbl TEMPLATE-PD.cbl 3270TEST-WS.cbl 3270TEST-PD.cbl

check_PROGRAMS=$(BUILT_TESTS)

all-local: $(BUILT_EXES)
	@chmod u+rx test*.sh
	@echo "---"
	@echo "You may also run optional 'make check' now."

login$(EXEEXT): login.$(OBJEXT)
	$(COBOL) -x $(COBOPTS) themain.cob login.$(OBJEXT) -o login$(EXEEXT)

templ$(EXEEXT): template_screen.$(OBJEXT)
	$(COBOL) -x $(COBOPTS) template_main.cob template_screen.$(OBJEXT) -o templ$(EXEEXT)

template_screen.$(OBJEXT): TEMPLATE-WS.cbl TEMPLATE-PD.cbl

3270test$(EXEEXT): 3270TEST-WS.cbl 3270TEST-PD.cbl
	$(COBOL) -x $(COBOPTS) 3270test.cob -o 3270test$(EXEEXT)

testcurses$(EXEEXT):
	$(COBOL) -x $(COBOPTS) testcurses.cob -o testcurses$(EXEEXT)

test001$(EXEEXT): test001.cob
	$(COBOL) -x $(COBOPTS) test001.cob -o test001$(EXEEXT)

test002$(EXEEXT): test002.cob
	$(COBOL) -x $(COBOPTS) test002.cob -o test002$(EXEEXT)

test003$(EXEEXT): test003.cob
	$(COBOL) -x $(COBOPTS) test003.cob -o test003$(EXEEXT)

test004$(EXEEXT): test004.cob
	$(COBOL) -x $(COBOPTS) test004.cob -o test004$(EXEEXT)

test005$(EXEEXT): test005.cob
	$(COBOL) -x $(COBOPTS) test005.cob -o test005$(EXEEXT)

test006$(EXEEXT): test006.cob
	$(COBOL) -x $(COBOPTS) test006.cob -o test006$(EXEEXT)

test007$(EXEEXT): test007.cob
	$(COBOL) -x $(COBOPTS) test007.cob -o test007$(EXEEXT)

test008$(EXEEXT): test008.cob
	$(COBOL) -x $(COBOPTS) test008.cob -o test008$(EXEEXT)

test009$(EXEEXT): test009.cob
	$(COBOL) -x $(COBOPTS) test009.cob -o test009$(EXEEXT)

test010$(EXEEXT): test010.cob
	$(COBOL) -x $(COBOPTS) test010.cob -o test010$(EXEEXT)

test011$(EXEEXT): test011.cob
	$(COBOL) -x $(COBOPTS) test011.cob -o test011$(EXEEXT)

test012$(EXEEXT): test012.cob
	$(COBOL) -x $(COBOPTS) test012.cob -o test012$(EXEEXT)

preptest:
	chmod u+rx dotest

TEMPLATE-WS.cbl TEMPLATE-PD.cbl:
	sdgenerate TEMPLATE
	@rm -f TEMPLATE.txt

3270TEST-WS.cbl 3270TEST-PD.cbl:
	sdgenerate 3270TEST
	@rm -f 3270TEST.txt

clean-local:
	rm -f *.$(OBJEXT) core *.out *.exe.stackdump test005.csv
	rm -f $(BUILT_SRCS)
	rm -f $(BUILT_EXES)

distclean-local: clean-local
	rm -f Makefile 
	rm -fr .deps

# End test/Makefile.am
