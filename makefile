default: all
all: mh

FSC = fsharpc

mh: mh.fs
	$(FSC) $^ -O
