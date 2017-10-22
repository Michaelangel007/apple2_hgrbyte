TARGETS = bin/hgr.byte bin/view.dhgr bin/dhgr.byte
all: $(TARGETS)

.PHONY: clean
clean:
	$(RM) $(TARGETS)

bin/hgr.byte: src/hgr.byte.s
	merlin32 $<
	mv src/hgr.byte bin/

bin/view.dhgr: src/view.dhgr.s
	merlin32 $<
	mv src/view.dhgr bin/

bin/dhgr.byte: src/dhgr.byte.s
	merlin32 $<
	mv src/dhgr.byte bin/

