/* Generate COPY BOOK for ncurses KEY CODES
 * Warren Gay
 * Wed Dec 20 14:05:20 2006
 */
#include <cobcurses.h>
#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <terminal.h>

#define KEY(key,desc)	{ cck_##key, #key, (const char *) (desc) }

static struct {
	unsigned	key_code;
	const char	*key_name;
	const char	*key_desc;
} keys[] = {
	KEY(MIN,"MINIMUM KEY VALUE"),
	KEY(BREAK,"BREAK KEY (UNRELIABLE)"),
	KEY(SRESET,"SOFT (PARTIAL) RESET (UNRELIABLE)"),
	KEY(RESET,"RESET OR HARD RESET (UNRELIABLE)"),
	KEY(DOWN,"DOWN-ARROW KEY"),
	KEY(UP,"UP-ARROW KEY"),
	KEY(LEFT,"LEFT-ARROW KEY"),
	KEY(RIGHT,"RIGHT-ARROW KEY"),
	KEY(HOME,"HOME KEY"),
	KEY(BACKSPACE,"BACKSPACE KEY"),
	KEY(F0,"FUNCTION KEY 0"),
	KEY(F1,"FUNCTION KEY 1"),
	KEY(F2,"FUNCTION KEY 2"),
	KEY(F3,"FUNCTION KEY 3"),
	KEY(F4,"FUNCTION KEY 4"),
	KEY(F5,"FUNCTION KEY 5"),
	KEY(F6,"FUNCTION KEY 6"),
	KEY(F7,"FUNCTION KEY 7"),
	KEY(F8,"FUNCTION KEY 8"),
	KEY(F9,"FUNCTION KEY 9"),
	KEY(F10,"FUNCTION KEY 10"),
	KEY(F11,"FUNCTION KEY 11"),
	KEY(F12,"FUNCTION KEY 12"),
	KEY(DL,"DELETE-LINE KEY"),
	KEY(IL,"INSERT-LINE KEY"),
	KEY(DC,"DELETE-CHARACTER KEY"),
	KEY(IC,"INSERT-CHARACTER KEY"),
	KEY(EIC,"SENT BY RMIR OR SMIR IN INSERT MODE"),
	KEY(CLEAR,"CLEAR-SCREEN OR ERASE KEY"),
	KEY(EOS,"CLEAR-TO-END-OF-SCREEN KEY"),
	KEY(EOL,"CLEAR-TO-END-OF-LINE KEY"),
	KEY(SF,"SCROLL-FORWARD KEY"),
	KEY(SR,"SCROLL-BACKWARD KEY"),
	KEY(NPAGE,"NEXT-PAGE KEY"),
	KEY(PPAGE,"PREVIOUS-PAGE KEY"),
	KEY(STAB,"SET-TAB KEY"),
	KEY(CTAB,"CLEAR-TAB KEY"),
	KEY(CATAB,"CLEAR-ALL-TABS KEY"),
	KEY(ENTER,"ENTER/SEND KEY"),
	KEY(PRINT,"PRINT KEY"),
	KEY(LL,"LOWER-LEFT KEY (HOME DOWN)"),
	KEY(A1,"UPPER LEFT OF KEYPAD"),
	KEY(A3,"UPPER RIGHT OF KEYPAD"),
	KEY(B2,"CENTER OF KEYPAD"),
	KEY(C1,"LOWER LEFT OF KEYPAD"),
	KEY(C3,"LOWER RIGHT OF KEYPAD"),
	KEY(BTAB,"BACK-TAB KEY"),
	KEY(BEG,"BEGIN KEY"),
	KEY(CANCEL,"CANCEL KEY"),
	KEY(CLOSE,"CLOSE KEY"),
	KEY(COMMAND,"COMMAND KEY"),
	KEY(COPY,"COPY KEY"),
	KEY(CREATE,"CREATE KEY"),
	KEY(END,"END KEY"),
	KEY(EXIT,"EXIT KEY"),
	KEY(FIND,"FIND KEY"),
	KEY(HELP,"HELP KEY"),
	KEY(MARK,"MARK KEY"),
	KEY(MESSAGE,"MESSAGE KEY"),
	KEY(MOVE,"MOVE KEY"),
	KEY(NEXT,"NEXT KEY"),
	KEY(OPEN,"OPEN KEY"),
	KEY(OPTIONS,"OPTIONS KEY"),
	KEY(PREVIOUS,"PREVIOUS KEY"),
	KEY(REDO,"REDO KEY"),
	KEY(REFERENCE,"REFERENCE KEY"),
	KEY(REFRESH,"REFRESH KEY"),
	KEY(REPLACE,"REPLACE KEY"),
	KEY(RESTART,"RESTART KEY"),
	KEY(RESUME,"RESUME KEY"),
	KEY(SAVE,"SAVE KEY"),
	KEY(SBEG,"SHIFTED BEGIN KEY"),
	KEY(SCANCEL,"SHIFTED CANCEL KEY"),
	KEY(SCOMMAND,"SHIFTED COMMAND KEY"),
	KEY(SCOPY,"SHIFTED COPY KEY"),
	KEY(SCREATE,"SHIFTED CREATE KEY"),
	KEY(SDC,"SHIFTED DELETE-CHARACTER KEY"),
	KEY(SDL,"SHIFTED DELETE-LINE KEY"),
	KEY(SELECT,"SELECT KEY"),
	KEY(SEND,"SHIFTED END KEY"),
	KEY(SEOL,"SHIFTED CLEAR-TO-END-OF-LINE KEY"),
	KEY(SEXIT,"SHIFTED EXIT KEY"),
	KEY(SFIND,"SHIFTED FIND KEY"),
	KEY(SHELP,"SHIFTED HELP KEY"),
	KEY(SHOME,"SHIFTED HOME KEY"),
	KEY(SIC,"SHIFTED INSERT-CHARACTER KEY"),
	KEY(SLEFT,"SHIFTED LEFT-ARROW KEY"),
	KEY(SMESSAGE,"SHIFTED MESSAGE KEY"),
	KEY(SMOVE,"SHIFTED MOVE KEY"),
	KEY(SNEXT,"SHIFTED NEXT KEY"),
	KEY(SOPTIONS,"SHIFTED OPTIONS KEY"),
	KEY(SPREVIOUS,"SHIFTED PREVIOUS KEY"),
	KEY(SPRINT,"SHIFTED PRINT KEY"),
	KEY(SREDO,"SHIFTED REDO KEY"),
	KEY(SREPLACE,"SHIFTED REPLACE KEY"),
	KEY(SRIGHT,"SHIFTED RIGHT-ARROW KEY"),
	KEY(SRSUME,"SHIFTED RESUME KEY"),
	KEY(SSAVE,"SHIFTED SAVE KEY"),
	KEY(SSUSPEND,"SHIFTED SUSPEND KEY"),
	KEY(SUNDO,"SHIFTED UNDO KEY"),
	KEY(SUSPEND,"SUSPEND KEY"),
	KEY(UNDO,"UNDO KEY"),
	KEY(MOUSE,"MOUSE EVENT HAS OCCURRED"),
	KEY(RESIZE,"TERMINAL RESIZE EVENT"),
	KEY(EVENT,"WE WERE INTERRUPTED BY AN EVENT"),
	KEY(IDLE,"IDLE/TIMEOUT EVENT"),
	KEY(MAX,"MAXIMUM KEY VALUE"),
	{ 0, 0 }
};

#define CLR(colour)	{ clr_##colour, (const char *) (#colour) }

static struct {
	unsigned	colour;
	const char	*name;
} colours[] = {
	CLR(BLACK),
	CLR(RED),
	CLR(GREEN),
	CLR(YELLOW),
	CLR(BLUE),
	CLR(MAGENTA),
	CLR(CYAN),
	CLR(WHITE),
	{ 0, 0 }
};

#define ATR(attr)	{ CCA_##attr, (const char *) (#attr) }

static struct {
	unsigned long	attr;
	const char	*name;
} attrs[] = {
	ATR(ATTRIBUTES),
	ATR(NORMAL),
	ATR(STANDOUT),
	ATR(UNDERLINE),
	ATR(REVERSE),
	ATR(BLINK),
	ATR(DIM),
	ATR(BOLD),
#ifdef A_ALTCHARSET
	ATR(ALTCHARSET),
#endif
#ifdef A_INVIS
	ATR(INVIS),
#endif
#ifdef A_PROTECT
	ATR(PROTECT),
#endif
#ifdef A_HORIZONTAL
	ATR(HORIZONTAL),
#endif
#ifdef A_LEFT
	ATR(LEFT),
#endif
#ifdef A_LOW
	ATR(LOW),
#endif
#ifdef A_RIGHT
	ATR(RIGHT),
#endif
#ifdef A_TOP
	ATR(TOP),
#endif
#ifdef A_VERTICAL
	ATR(VERTICAL),
#endif
	{ 0, 0 }
};

int
main(int argc,char **argv) {
	int x;
	FILE *f;
	int rc = 0;

	/*
	 * COPY BOOK COBCKEYS :
	 */
	f = fopen("COBCKEYS.cbl","w");
	fputs("     >*\n",f);
	fputs("     >* NCURSES KEY DEFINITIONS :\n",f);
	fputs("     >*\n",f);
        fputs("        01  NC-KEY-DEFINITIONS.\n",f);
	for ( x=0; keys[x].key_code != 0; ++x ) {
		fprintf(f,"     >*         %s\n",keys[x].key_desc);
                fprintf(f,"            10  NC-KEY-%-11s PIC 9(4) COMP-5 VALUE %u\n",
                    keys[x].key_name,keys[x].key_code);
		fprintf(f,"                                            SYNCHRONIZED.\n");
                if ( keys[x].key_code > 9999 ) {
			rc = 1;
			fprintf(stderr,"*** The value for NC-KEY-%s has exceeded PIC 9(4).\n",
				keys[x].key_name);
			fputs("*** Please report this to the CobCurses maintainer(s).\n",stderr);
		}
	}
	fputs("     >* END NCURSES KEY DEFINITONS.\n\n",f);
	fclose(f);

	/*
	 * COPY BOOK NCCOLOUR :
	 */
	f = fopen("COBCCOLOUR.cbl","w");
	fputs("     >*\n",f);
	fputs("     >* NCURSES COLOUR DEFINITIONS :\n",f);
	fputs("     >*\n",f);
        fputs("        01  NC-COLOUR-DEFINITIONS.\n",f);
	for ( x=0; colours[x].name != 0; ++x ) {
                fprintf(f,"            10  NC-COLOUR-%-8s PIC 9(4) COMP-5 VALUE %u\n",
                    colours[x].name,colours[x].colour);
		fprintf(f,"                                            SYNCHRONIZED.\n");
                if ( colours[x].colour > 9999 ) {
			rc = 1;
			fprintf(stderr,"*** The value for NC-COLOUR-%s has exceeded PIC 9(4).\n",
				colours[x].name);
			fputs("*** Please report this to the CobCurses maintainer(s).\n",stderr);
		}
	}
	fputs("     >* END NCURSES COLOUR DEFINITONS.\n\n",f);
	fclose(f);

	/*
	 * COPY BOOK COBCATTR :
	 */
	f = fopen("COBCATTR.cbl","w");
	fputs("     >*\n",f);
	fputs("     >* NCURSES ATTRIBUTE DEFINITIONS :\n",f);
	fputs("     >*\n",f);
        fputs("        01  NC-ATTRIBUTE-DEFINITIONS.\n",f);
	for ( x=0; attrs[x].name != 0; ++x ) {
                fprintf(f,"            10  NC-ATTR-%-10s PIC 9(9) COMP-5\n",
                    attrs[x].name);
                fprintf(f,"                                   VALUE %lu SYNCHRONIZED.\n",
                    attrs[x].attr & 0x00000000FFFFFFFFL);
	}
	fputs("     >* END NCURSES ATTRIBUTE DEFINITONS.\n\n",f);
	fclose(f);

#ifdef HAVE_SLEEP
	sleep(1);	/* Prevents clock skew make warnings in cygwin */
#endif
	return rc;
}
