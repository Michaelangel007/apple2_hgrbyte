TARGETS = hgr.byte view.dhgr dhgr.byte
all: $(TARGETS)

.PHONY: clean
clean:
	$(RM) $(TARGETS)

hgr.byte: hgr.byte.s
	merlin32 $<

view.dhgr: view.dhgr.s
	merlin32 $<

dhgr.byte: dhgr.byte.s
	merlin32 $<

