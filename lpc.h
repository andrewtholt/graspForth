

#include "lpc21xx.h"

// ----------------- The memory Map ------------------------------------------

// Entire VM memory, in kilobytes 
#define MEMSIZE			(14*1024/CELL)

// All sizes are specified in bytes, must be divisable by CELL!	
// Stack sizes
#define RETURN_STACK 	(1024/CELL)	
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
void putCharacter(int ch);

// ------------------- Platform specific IO Interface ------------------------

#define LINEFEED        '\r'


void initIO(void)
{
	// configure IO pins
	PINSEL0 = 0x005505;  
 	//config the uart 0
    UART0_LCR = 0x83;             // 8 bits, no Parity, 1 Stop bit          
    UART0_DLL = 5;                // 56700 Baud @ 18432kHz xtal Clock       
    UART0_DLM = 0;
    UART0_LCR = 0x03;             // DLAB = 0  
    UART0_FCR = 0x01;   // Must set bit0 for UART to work at alll(apparently)
    //UART0_IER = 0x01;          //enable rx data availbe interrupt
    //VICVectAddr5 = (unsigned long)uart_rx0;     // set interrupt vector in 0
    //VICVectCntl5 = 0x20 | 6;   // use it for use it for uart0 rx Interrupt
    //VICIntEnable = (1<<6);     //set bit 6 to enable uart0 interrupt!
}

int kbhit()	// Returns non-zero if KB hit, 0 if not.
{
	return (UART0_LSR & 0x01);
}


int getCharacter()
{
	int ch;
	 
	ch = UART0_RBR;
	return (ch);
}

void putCharacter(int ch)
{
	while (((UART0_LSR & 0x20) == 0)) ;	// wait till empty
	UART0_THR = ch;	// send character
}


