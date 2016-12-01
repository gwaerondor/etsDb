OUTDIR=bin/

compile:
	mkdir -p $(OUTDIR)
	erlc -o $(OUTDIR) src/etsDb.erl 
	erlc -o $(OUTDIR) src/etsDb_tests.erl 

test:
	$(MAKE) compile
	erl -noshell -pa $(OUTDIR) -eval "eunit:test(etsDb, [verbose])" -s init stop

tests:
	$(MAKE) test
