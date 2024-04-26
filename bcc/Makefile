FILES = CPU1E CPU2E CPU3E CPU4E CPU5E CPU6E CPU7E
UAS = CPU1E.uas CPU2E.uas CPU3E.uas CPU4E.uas CPU5E.uas CPU6E.uas CPU7E.uas

CPU.bin: CPU.uas
	python3.10 uas.py CPU.uas > CPU.bin

CPU.uas: $(UAS)
	cat preamble.uas $(UAS) > CPU.uas

%.uas: %.mc
	tr -d '\r' < $< | awk -f strip.awk | sed 's/^,/\t/g' > $@

clean: 
	rm CPU*.uas CPU.bin
