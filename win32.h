#ifndef __GF_LINUX_H_
#define __GF_LINUX_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


static struct termios initial_settings, new_settings;
static int peek_character = -1;


// ----------------- The memory Map ------------------------------------------

// Entire VM memory, in kilobytes
#define MEMSIZE			(64*1024/CELL)

// All sizes are specified in bytes, must be divisable by CELL!
// Stack sizes
#define RETURN_STACK 		(8192/CELL)
#define PAR_STACK 		(1024/CELL)
// User variable area
#define USER_S			(400/CELL)
// Terminal input buffer
#define TIB_S			(80/CELL)
// Variable memory storage
#define VM_S			(400/CELL)
// Pad text buffer
#define PAD_S			(80/CELL)

#define LINEFEED	'\n'

// ------------------- Platform specific IO Interface ------------------------

void initIO(void)
{
}


int kbhit()	// Returns non-zero if KB hit, 0 if not.
{
    return(_kbhit());
}


int getCharacter()
{
    return(_getche());
}

void putCharacter(int ch)
{
    _putch(ch);
}


#endif // __GF_LINUX_H_
