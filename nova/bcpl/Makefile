# Makefile for obcpl/src

PREFIX?=/usr/local

AFLAGS=--32
CFLAGS=-m32

all: cg-nova op st

cg-nova: bcpl-cg-nova.o st.o 
	$(CC) $(CFLAGS) -o cg-nova st.o bcpl-cg-nova.o

bcpl-cg-nova.o: bcpl-cg-nova.b
	obcpl bcpl-cg-nova.b

oc.o: oc.c
	$(CC) $(CFLAGS) -c oc.c

op: op.o pt.o
	$(CC) $(CFLAGS) -o op op.o pt.o

op.o: op.c
	$(CC) $(CFLAGS) -c op.c

pt.o: pt.c
	$(CC) $(CFLAGS) -c pt.c

st: su.o st.o blib.o global.o rt.o sys.o
	ld -m elf_i386 -o st su.o st.o blib.o global.o rt.o sys.o

su.o: su.s
	$(AS) $(AFLAGS) -o su.o su.s

st.o: st.s
	$(AS) $(AFLAGS) -o st.o st.s

blib.o: blib.s
	$(AS) $(AFLAGS) -o blib.o blib.s

global.o: global.s
	$(AS) $(AFLAGS) -o global.o global.s

rt.o: rt.s
	$(AS) $(AFLAGS) -o rt.o rt.s

sys.o: sys.s
	$(AS) $(AFLAGS) -o sys.o sys.s

check:	blib.O blib.chk st.O st.chk
	cmp blib.O blib.chk
	cmp st.O st.chk

blib.chk: st blib.b
	./st < blib.b > blib.chk

st.chk:	st st.b
	./st < st.b > st.chk

clean:
	rm -f cg cg.o oc.o op op.o pt.o
	rm -f st st.o st.s blib.o blib.s su.o global.o rt.o sys.o
	rm -f blib.chk st.chk
