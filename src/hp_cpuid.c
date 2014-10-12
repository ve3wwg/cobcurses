/* Test to determine system and hardware architecture types.
 * Adapted from :
 * http://docs.hp.com/en/B3906-90005/ch01s02.html#apzmiie8haasz
 */
#include <stdio.h>
#include <sys/utsname.h>
 
extern int _SYSTEM_ID;
extern int _CPU_REVISION;
 
int
main(void) {
	struct utsname uts;

	uname(&uts);
	printf("%s %x %x\n",
		uts.release,
		_SYSTEM_ID,
		_CPU_REVISION);
}

/* End */
