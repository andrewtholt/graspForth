#ifndef __GF_LEON_H_
#define __GF_LEON_H_


#define UART_REG(i)	(*(volatile unsigned *)(0x80000100+4*i))

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

/******************************************************************************
* Keyboard/UART routines used by graspForth kernel
******************************************************************************/
void initIO(void)
{
    /*
    * Enable transmitter, enable receiver, no RTS/CTS, no interrupts
    */
    UART_REG(1) = 0x0000;
    UART_REG(2) = 0x0003;
}

void putCharacter(int c)
{
    /*
    * UART status register: Wait until Transmitter FIFO empty
    * UART data register:   Write character
    */
    while( (UART_REG(1) & 0x0004) == 0 ) ;
    UART_REG(0) = (unsigned) c;
}

int kbhit(void)
{
    /*
    * UART status register: Check data ready bit
    */
    return( (UART_REG(1) & 0x0001) != 0);
}

int getCharacter(void)
{
    int c;

    /*
    * UART status register: Check data ready bit
    * Echo incoming characters
    */
    while( (UART_REG(1) & 0x0001) == 0 ) ;
    c = (int)UART_REG(0);
//    putCharacter(c);

    return(c);
}

#endif // __GF_LEON_H_
