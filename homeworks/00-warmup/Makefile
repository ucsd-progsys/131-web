STACK=stack

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

COURSE=cs131e
ASGN=00
COMPILER=warmup

test: clean
	$(STACK) test 

bin:
	$(STACK) build

clean: distclean

distclean: 
	$(STACK) clean
	rm -rf .stack_work 

tags:
	hasktags -x -c lib/

turnin: 
	rm -rf .stack-work
	tar -zcvf ../$(ASGN)-$(COMPILER).tgz ../$(ASGN)-$(COMPILER) 
	mv ../$(ASGN)-$(COMPILER).tgz . 
	turnin -c $(COURSE) ../$(ASGN)-$(COMPILER).tgz

