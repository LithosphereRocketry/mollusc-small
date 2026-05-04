.PHONY: test clean INVOKES_CABAL
.DEFAULT_GOAL=test

# Override to change the number of cycles recorded in VCD traces
TRACE_CYCLES=200000

DIRS = test-data waveforms

# Copy pasted from toolchain makefile. probably a better way to do this
TEST_EMU_DIR = mollusc-toolchain/tests/emu
TEST_EMU_OUT_DIR = mollusc-toolchain/test_out/emu
TEST_EMU_SUPPORT = platform.inc platform.S platform.s platform.o
EMU_SUPPORT_PATHS = $(TEST_EMU_SUPPORT:%=$(TEST_EMU_DIR)/%)
EMU_TEST_DIRS = $(filter-out $(EMU_SUPPORT_PATHS),$(wildcard $(TEST_EMU_DIR)/*))
EMU_TESTS = $(EMU_TEST_DIRS:$(TEST_EMU_DIR)/%=%)

INVOKES_CABAL:

test: $(EMU_TESTS:%=test-data/%_rom.bin.txt) $(EMU_TESTS:%=test-data/%_verify.bin) INVOKES_CABAL
	cabal test

test-data/%.bin.txt: test-data/%.bin bin_to_clash_record.py
	./bin_to_clash_record.py $<

test-data/%.bin: $(TEST_EMU_OUT_DIR)/%.bin | test-data
	cp $(TEST_EMU_OUT_DIR)/$*.bin $@

$(TEST_EMU_OUT_DIR)/%.bin:
	make -j$(nproc) -C mollusc-toolchain $(@:mollusc-toolchain/%=%)	

$(DIRS): %:
	mkdir -p $@

test-data/%_trace.vcd: test-data/%_rom.bin.txt INVOKES_CABAL
	cabal run soctrace -- $* $(TRACE_CYCLES)

clean:
	make -C mollusc-toolchain clean
	rm -rf $(DIRS)