.c.o:
	gcc -g -c -I.. -I. -Wall $< -o $*.o

all:	tmenu.exe
	
OBJS=tmenu.o terminal.o term_curses.o term_curses_menu.o cc_menu.o misc.o dynstr.o term_curses_conv.o

tmenu.exe: $(OBJS)
	gcc $(OBJS) -o tmenu.exe -lncurses

clean:
	rm -f $(OBJS)

clobber: clean
	rm -f tmenu.exe
