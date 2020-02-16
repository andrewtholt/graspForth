
#ifndef __LINUX_H
#define __LINUX_H


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>


static struct termios initial_settings, new_settings;
static int peek_character = -1;

// ----------------- The memory Map ------------------------------------------

// Entire VM memory, in kilobytes 
#define MEMSIZE			(64*1024/CELL)

// All sizes are specified in bytes, must be divisable by CELL!	
// Stack sizes
#define RETURN_STACK 	(8192/CELL)	
#define PAR_STACK 		(1024/CELL)	
// User variable area
#define USER_S			(400/CELL)	
// Terminal input buffer
#define TIB_S			(80/CELL)
// Variable memory storage
#define VM_S			(400/CELL)
// Pad text buffer	
#define PAD_S			(80/CELL)

// Prototypes
void   init_keyboard(void);
void   close_keyboard(void);
int    keyhit(void);
int    readch(void);
void echo(int *);

// ------------------- Platform specific IO Interface ------------------------

#define LINEFEED	'\n'
	

void initIO(void)
{
	init_keyboard();
}


int kbhit()	// Returns non-zero if KB hit, 0 if not.
{
	return(keyhit());
}


int getCharacter()
{
	int ch;

	ch = readch();
	echo(&ch);
	return(ch);
}

void putCharacter(int ch)
{
	putchar((char) ch);
}

// --------------------- Linux TTY stuff -------------------------------------



void init_keyboard()
{
    tcgetattr(0,&initial_settings);
    new_settings = initial_settings;
    new_settings.c_lflag &= ~ICANON;
    new_settings.c_lflag &= ~ECHO;
    new_settings.c_lflag &= ~ISIG;
    new_settings.c_cc[VMIN] = 1;
    new_settings.c_cc[VTIME] = 0;
    tcsetattr(0, TCSANOW, &new_settings);
}

void close_keyboard()
{
    tcsetattr(0, TCSANOW, &initial_settings);
}


int keyhit()
{
	unsigned char ch;
	int nread;

    if (peek_character != -1) return 1;
    new_settings.c_cc[VMIN]=0;
    tcsetattr(0, TCSANOW, &new_settings);
    nread = read(0,&ch,1);
    new_settings.c_cc[VMIN]=1;
    tcsetattr(0, TCSANOW, &new_settings);
    if(nread == 1) 
    {
        peek_character = ch;
        return 1;
    }
    return 0;
}

int readch()
{
	char ch;

    if(peek_character != -1) 
    {
        ch = peek_character;
        peek_character = -1;
        return ch;
    }
    read(0,&ch,1);
    return ch;
}

void echo(int* ch)
{
	write(0,ch,1);
}

#endif
