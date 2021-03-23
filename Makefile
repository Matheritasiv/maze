NAME	:= $(shell basename `pwd`)

all: run

run: $(NAME).ss libdemo.so
	scheme --script $<

compile: $(NAME).so

demo: libdemo.so

libdemo.so: demo.c
	gcc -shared -fPIC -Os -s $< -o$@

$(NAME).so: $(NAME).ss
	./compile $<

edit:
	vim -c 'set nu et fdm=marker bg=dark' $(NAME).ss

.PHONY: all run compile demo edit
