
//----------------------------------------------------------------------------
//
// graspForth
//
//	A Gcc/Retargetable/fAst/Small/Portable ITC FORTH written in C.
//		Ver 0.85 (C) Copyright  Bernard Mentink ........ 2004
//		This code is released for free "personal" use, any commercial interest
//		should be directed to the following address:
//		<bmentink-at-gmail-dot-com>
//
//		Please keep this header intact.
// TODO:
//		Need to add the DOES> construct word for CREATE.
//		Need to add a ROM load function, that loads and boots a ROM application.
//		
// Ver 0.80	Initial version 2004
//		
// Ver 0.85 	Bug Fixes and added features Sept 2006
//		Fixed bugs in "0=" "TRUE" "FALSE" words
//		Strings now correct for BIG ENDIAN and LITTLE ENDIAN
// 		Fixed seg_fault problem in FILE word 
// ACKNOWLEDGEMENTS
//		eForth 1.0 by Bill Muench and C. H. Ting, 1990;
//		Rolf Schroedter for endian fix.
// 	Please send any improvements/bug fixes to the above address, I will
//	incorporate and periodically publish a new release.
// NOTES:
//	When using primitives with DOLIT, use underscore names ie _DROP, not DROP.

// --------------------------------- Includes --------------------------------

#include "platform.h"
#include "forthdef.h"

//--------------------------------  Defines ----------------------------------

#define TTRUE		-1
#define FFALSE  	0
#define CELL		sizeof(INT)

#define COMPO		0x40		// lexicon compile only bit
#define IMEDD		0x80		// lexicon immediate bit
#define	MASK		0x1F		// mask above bits
#define ERR		27

// Size of kernel name field (fixed width, by necessecity)
#define NAME_S		11

// ------------------------------- typedef's ---------------------------------
//  NOTE: int and *int must be same size!

typedef int INT;
typedef unsigned int UINT;
typedef int * PTR;
typedef unsigned int * UPTR;
typedef unsigned char *CPTR;

// The dictionary Structure.
// Note. Fixed name field for internal words, user words are variable width.
typedef struct {
	INT		cfa;		// code-field-address
	INT		lfa;		// link-field-address
	unsigned char	count;
	char		name[NAME_S];
} HEADS;

// The ROM header, followed by rom image. 
typedef struct {
	INT		magic_no;	// start of header
	INT		boot_vector;	// address of user application word. 0 if interpreter 
	INT		rom_start;
	INT		rom_end;	
	INT		ram_start;	// address of rom image and where to put it.
	INT		cp_;		// Forth pointers
	INT		np_;
	INT		vp_;
	INT		last;
} ROM_HEADER;				// rom image follows

#define MAGIC_NO	0x12345678	// First word of the rom header

// ------------------------------ Virtual Machine Registers ------------------

#define IP		mem[0] 	// Instruction Pointer, address of current memory cell
#define W		mem[1]	// Word Pointer, address of next code to execute
#define R0		mem[2]	// Address of bottom of address stack
#define S0		mem[3]	// Address of bottom of parameter stack
#define RP		mem[4]	// RP and SP, return stack pointer and stack pointer
#define SP		mem[5]
#define X		mem[6]	// Temporary register

// -------------------------- System and User Variables ----------------------
// These are initialized in Main()

#define tqkey		mem[7]		// '?key
#define temit		mem[8]		// 'emit
#define texpect		mem[9]		// 'expect
#define ttap		mem[10]		// 'tap
#define techo		mem[11]		// 'echo
#define tprompt		mem[12]		// 'prompt
#define teval		mem[13]		// 'eval
#define tnumber 	mem[14]		// 'number
#define tboot		mem[15]		// 'boot
#define base		mem[16]		// base
#define span		mem[17]		// span
#define in		mem[18]		// >in
#define ntib		mem[19]		// #tib
#define	hld		mem[20]		// hld
#define handler		mem[21]		// handler
#define cp		mem[22]		//	cp
#define np		mem[23]		//	np
#define vp		mem[24]		//	vp
#define llast		mem[25]		//	last
#define vfrth		mem[26]		//	forth
#define context 	mem[27] 	// context pointer
					// vocab storage
#define context_end 	mem[35]
#define current		mem[36]		//	current link
					// vocab link
#define current_end 	mem[37]
#define tmp		mem[38]
#define csp		mem[39]
#define up		mem[40]
#define user		mem[41]				//	user area
#define	tib		mem[41+USER_S] 			// tib buffer
#define vm		mem[41+USER_S+TIB_S]		// variable area
#define pad_start	mem[41+USER_S+TIB_S+VM_S]	// text buffer
#define pad_end		mem[41+USER_S+TIB_S+VM_S+PAD_S]

// ----------------------------------------- Macros --------------------------

#define INC(x) 		(x += CELL)
#define DEC(x) 		(x -= CELL)
#define DDEC(x)		(x -= CELL*2)
#define DINC(x)		(x += CELL*2)
#define TOS		(*(PTR)SP)		// top of stack
#define UTOS		(*(UPTR)SP)		// top of stack
#define NOS		(*(PTR)(SP + CELL))	// next on stack
#define UNOS		(*(UPTR)(SP + CELL))	// next on stack
#define TOR		(*(PTR)RP)		// top of return stack
#define FORTHWORDS 	static const INT
#define FORTHDICT  	static const HEADS

// ------------------------------------ Globals ------------------------------

INT mem[MEMSIZE];	// The VM memory

// ------------------------------------- Boot ROM image ----------------------
// If a Forth rom exists, try and boot it.
//
void boot_rom (void)
{
#ifdef ROM_PRESENT

   	INT *src,*dst,*end;
	static volatile ROM_HEADER *rom=(ROM_HEADER *)ROM;
	
	// Check is we have a rom image with the correct header
	if(rom->magic_no == MAGIC_NO)
	{ 
		// Copy rom to ram
		src = (INT *)rom->rom_start;
		end = (INT *)rom->rom_end;		
		dst = (INT *)rom->ram_start;
		for( ; src <= end ; src++, dst++)
			*dst = *src;
		// restore forth pointers
		cp = rom->cp_;
		np = rom->np_;
		vp = rom->vp_;
		vfrth =  llast = rom->last;
		// restore boot vector if non-zero
		if(rom->boot_vector)
			tboot = rom->boot_vector;
	}
#endif
}

// --------------------------------------- Main ------------------------------

int main(void)
{
	INT *adr,*adr2;
	unsigned char count,*badr;	// used by NAME?
	INT i,j;
	HEADS *head;

		// Initial values for system & user variables
	FORTHWORDS sys_var[] = {
		_QRX,_TXSTORE,/*accept*/0,/*KTAP*/0,_TXSTORE,
		/*DOTOK*/0,/*INTERP*/0,/*NUMBQ*/0,
		0,10,0,0,0,0,0,0,0,0,0,0
	};

// ---------------------------------- Kernel Primitives ----------------------
//  .. all "_words" are actual code address's
	FORTHWORDS cfa [] = {
		_NEXT,_DOCOL,_SEMI,_DOLIT,_DOCON,_DOCASE,_EXECUTE,_ATEXEC,
		_BRANCH,_ZBRANCH,_STORE, _PSTORE,_FETCH,_CSTORE,_CFETCH,
		_RTO,_RFROM,_RFETCH,_RPZ,_RPFETCH,_RPSTORE,_SWAP,_DROP,_DUP,
		_QDUP,_SPZ,_NIP,_SPFETCH,_SPSTORE,_OVER,_ROT,_TXSTORE,_QRX,_ADD,
		_SUB,_UADD,_USUB,_MUL,_DIV,_UMUL,_UDIV,_MULDIV,_UMULDIV,_TWOMUL,
		_TWODIV,_LSHIFT,_RSHIFT,_ZERO,_ONE,_TWO,_THREE,_NONE,_NTWO,
		_NTHREE,_INVERT,_NEGATE,_MOD,_UMOD,_TRUEE,_FALSEE,
		_EQ,_ZEQ,_LT,_ULT,_ZLT,_GT,_WITHIN,_MAX,_MIN,_AND_,_OR,_XOR,_NOT,
		_NAMEQ,_DONEXT,

// -------------------------------- Colon words ------------------------------
//  ..	all colon words (except cold) start with a _DOCOL word and
//	end in SEMI. Number is index into cfa array.

// -------------------------- Sytem & user variables--------------------------
// 75	TQKEY ( -- a)
			_DOCOL,DOLIT,_TQKEY,SEMI,
// 79 	TEMIT ( -- a)
			_DOCOL,DOLIT,_TEMIT,SEMI,
// 83	TEXPECT ( -- a)
			_DOCOL,DOLIT,_TEXPECT,SEMI,
// 87	TTAP ( -- a)
			_DOCOL,DOLIT,_TTAP,SEMI,
// 91	TECHO ( -- a)
			_DOCOL,DOLIT,_TECHO,SEMI,
// 95	TPROMPT ( -- a)
			_DOCOL,DOLIT,_TPROMPT,SEMI,
// 99	TEVAL ( -- a)
			_DOCOL,DOLIT,_TEVAL,SEMI,
// 103	TNUMBER ( -- a)
			_DOCOL,DOLIT,_TNUMBER,SEMI,
// 107	BASE ( -- a)
			_DOCOL,DOLIT,_BASE,SEMI,
// 111	SPAN ( -- a)
			_DOCOL,DOLIT,_SPAN,SEMI,
// 115	IN ( -- a)
			_DOCOL,DOLIT,_IN,SEMI,
// 119	NTIB ( -- a)
			_DOCOL,DOLIT,_NTIB,SEMI,
// 123	HLD ( -- a)
			_DOCOL,DOLIT,_HLD,SEMI,
// 127	HANDLER ( -- a)
			_DOCOL,DOLIT,_HANDLER,SEMI,
// 131	CP ( -- a)
			_DOCOL,DOLIT,_CP,SEMI,
// 135	NP ( -- a)
			_DOCOL,DOLIT,_NP,SEMI,
// 139	VP ( -- a)
			_DOCOL,DOLIT,_VP,SEMI,
// 143	LAST ( -- a)
			_DOCOL,DOLIT,_LAST,SEMI,
// 147	VFRTH ( -- a)
			_DOCOL,DOLIT,_VFRTH,SEMI,
// 151	CURRENT ( -- a)
			_DOCOL,DOLIT,_CURRENT/*_END*/,SEMI,
// 155	UP ( -- a)
			_DOCOL,DOLIT,_UP,SEMI,
// 159	TIB ( -- a)
			_DOCOL,DOLIT,_TIB,SEMI,

// ------------ High level Colon Words (psuedo forth) ------------------------

// 163 EMIT ( c -- ) Send a character to the output device.
			_DOCOL,TEMIT,ATEXEC,SEMI,

// 167 QKEY ( -- c T | F ) wait for key, FALSE if not ready
			_DOCOL,TQKEY,ATEXEC,SEMI,

// 171 KEY	( -- c )
			_DOCOL,
/*172*/			QKEY,ZBRANCH,&cfa[172],SEMI,

// 176  ABS    ( n -- n )   Return the absolute value of n
        		_DOCOL,DUP,ZLT,
        		ZBRANCH,&cfa[182],
        		NEGATE,
/*182*/			SEMI,

// 183 CELLP    ( a -- a ) Add cell size in byte to address.
        		_DOCOL,DOLIT,CELL,ADD,SEMI,

// 188 CELLM    ( a -- a ) Subtract cell size in byte to address.
        		_DOCOL,DOLIT,0-CELL,ADD,SEMI,

// 193 CELLS    ( a -- a ) Multiply by cell size.
        		_DOCOL,DOLIT,CELL,MUL,SEMI,

// 198 ALIGNED  ( b -- a ) Align address to the cell boundary.
        		_DOCOL,DUP,DOLIT,CELL,
        		MOD,DUP,
        		ZBRANCH,&cfa[210],
        		DOLIT,CELL,SWAP,SUB,
/*210*/			ADD,SEMI,


// 212 BLANK ( -- 32 ) Return 32, the blank character.
			_DOCOL,DOLIT,32,SEMI,

// 216 >CHAR ( c -- c ) Filter non-printing characters.
			_DOCOL,DOLIT,0x7F,AND,DUP,		//mask msb
			BLANK,DOLIT,127,WITHIN,NOT,	//check for printable
			ZBRANCH,&cfa[231],
			DROP,DOLIT,0x5F,		//replace non-printables '_'
/*231*/			SEMI,


// 232 DEPTH ( -- n ) Return the depth of the data stack.
			_DOCOL,SPFETCH,SPZ,NOP,SWAP,SUB,
			DOLIT,CELL,DIV,SEMI,

// 242 PICK    ( +n -- w ) Copy the nth stack item to tos.
			_DOCOL,ONE,ADD,CELLS,
			SPFETCH,ADD,FETCH,SEMI,


// 250 COUNT ( b -- b +n ) Return count byte of a string and add 1 to byte address.
			_DOCOL,DUP,ONE,ADD,
			SWAP,CFETCH,SEMI,

// 257 HERE ( -- a ) Return the top of the code dictionary.
			_DOCOL,CP,FETCH,SEMI,

// 261 PAD ( -- a ) return address of text buffer
			_DOCOL,DOLIT,_PAD,SEMI,

// 265 CMOVE ( b1 b2 u -- ) Copy u bytes from b1 to b2.
			_DOCOL,RTO,
			BRANCH,&cfa[279],
/*269*/			RTO,DUP,CFETCH,
			RFETCH,CSTORE,
			ONE,ADD,
			RFROM,ONE,ADD,
/*279*/			DONEXT,&cfa[269],
			DDROP,SEMI,

// 283 FILL ( b u c -- ) Fill u bytes of character c to area beginning at b.
			_DOCOL,SWAP,RTO,SWAP,
			BRANCH,&cfa[293],
/*289*/			DDUP,CSTORE,ONE,ADD,
/*293*/			DONEXT,&cfa[289],
			DROP,SEMI,

// 297 -TRAILING ( b u -- b u ) Adjust the count to eliminate trailing white space.
			_DOCOL,RTO,
			BRANCH,&cfa[313],
/*301*/			BLANK,OVER,RFETCH,ADD,CFETCH,LT,
			ZBRANCH,&cfa[313],
			RFROM,ONE,ADD,SEMI,
/*313*/			DONEXT,&cfa[301],
			ZERO,SEMI,

// 317 PACK$    ( b u a -- a )
			_DOCOL,ALIGNED,DUP,RTO,		//strings only on cell boundary
			OVER,DUP,
			DOLIT,CELL,MOD,				//count mod cell
			SUB,OVER,ADD,
			ZERO,SWAP,STORE,			//null fill cell
			DDUP,CSTORE,ONE,ADD,			//save count
			SWAP,CMOVE,RFROM,SEMI,		//move string

// 340 DIGIT    ( u -- c ) Convert digit u to a character.
			_DOCOL,DOLIT,9,OVER,LT,
			DOLIT,7,AND,ADD,
			DOLIT,0x30,ADD,SEMI,

// 353 U/MOD UDIVMOD ( u1 u2 -- ur uq )division of u1 / u2
//										returning rem & quot
			_DOCOL,OVER,OVER,	// (u1 u2 u1 u2 --)
			UMOD,ROT,ROT,UDIV,SEMI,

// 361 EXTRACT    ( n base -- n c ) Extract the least significant digit from n.
			_DOCOL,UDIVMOD,
			SWAP,DIGIT,SEMI,

// 366 <# BDIGS ( -- ) Initiate the numeric output process.
			_DOCOL,PAD,HLD,STORE,SEMI,

// 371 HOLD ( c -- ) Insert a character into the numeric output string.
			_DOCOL,HLD,FETCH,ONE,SUB,
			DUP,HLD,STORE,CSTORE,SEMI,

// 381 # DIG ( u -- u ) Extract one digit from u and append the digit to output string.
			_DOCOL,BASE,FETCH,UDIVMOD,
			SWAP,DOLIT,9,OVER,
			LT,ZBRANCH,&cfa[395],DOLIT,7,ADD,
/*395*/			DOLIT,'0',ADD,HOLD,SEMI,

// 400 #S ( u -- 0 0 ) Convert u until all digits are added to the output string.
			_DOCOL,
/*401*/			DIG,DUP,
			ZBRANCH,&cfa[407],
			BRANCH,&cfa[401],
/*407*/ 		SEMI,

// 408 	SIGN    ( n -- ) Add a minus sign to the numeric output string.
			_DOCOL,ZLT,
       			ZBRANCH,&cfa[415],
        		DOLIT,'-',HOLD,
/*415*/ 		SEMI,

// 416 #> ( u -- b u ) Prepare the output string to be TYPE'd.
			_DOCOL,DROP,HLD,FETCH,
			PAD,OVER,SUB,SEMI,

// 424	str  ( w -- b u ) Convert a signed integer to a numeric string.
			_DOCOL,DUP,RTO,ABS,
        		BDIGS,DIGS,RFROM,
        		SIGN,EDIGS,SEMI,

// 434 HEX  ( -- ) Use radix 16 as base for numeric conversions.
			_DOCOL,DOLIT,16,BASE,STORE,SEMI,

// 440 DECIMAL   ( -- ) Use radix 10 as base for numeric conversions.
			_DOCOL,DOLIT,10,BASE,STORE,SEMI,

// 446 DIGIT?  ( c base -- u t )Convert a character to its numeric value.
			_DOCOL,RTO,DOLIT,'0',SUB,
        		DOLIT,9,OVER,LT,
        		ZBRANCH,&cfa[465],
        		DOLIT,7,SUB,
        		DUP,DOLIT,10,LT,OR,
/*465*/  		DUP,RFROM,ULT,SEMI,

// 469 DDROP ( n n -- ) drop two items off stack
			_DOCOL,DROP,DROP,SEMI,

// 473 NUMBER?    ( a -- n T | a F ) Convert a number string to integer.
			_DOCOL,BASE,FETCH,RTO,ZERO,OVER,COUNT,
        		OVER,CFETCH,DOLIT,'$',EQ,
        		ZBRANCH,&cfa[496],
        		HEX,SWAP,DOLIT,1,ADD,
        		SWAP,DOLIT,1,SUB,
/*496*/  		OVER,CFETCH,DOLIT,'-',EQ,RTO,
        		SWAP,RFETCH,SUB,SWAP,RFETCH,ADD,QDUP,
        		ZBRANCH,&cfa[550],
        		DOLIT,1,SUB,RTO,
/*515*/   		DUP,RTO,CFETCH,BASE,FETCH,DIGITQ,
        		ZBRANCH,&cfa[543],
        		SWAP,BASE,FETCH,MUL,ADD,RFROM,
        		DOLIT,1,ADD,
        		DONEXT,&cfa[515],
        		RFETCH,SWAP,DROP,
        		ZBRANCH,&cfa[540],
        		NEGATE,
/*540*/ 		SWAP,
        		BRANCH,&cfa[549],
/*543*/ 		RFROM,RFROM,DDROP,DDROP,DOLIT,0,
/*549*/ 		DUP,
/*550*/ 		RFROM,DDROP,
        		RFROM,BASE,STORE,SEMI,

// 556 CR ( -- ) Output a carriage return and a line feed.
			_DOCOL,DOLIT,'\r',EMIT,
        		DOLIT,'\n',EMIT,SEMI,

// 564 NUF? ( -- t ) Return false if no input, else pause and if CR return true.
			_DOCOL,QKEY,DUP,
			ZBRANCH,&cfa[574],
			DDROP,KEY,DOLIT,LINEFEED,EQ,
/*574*/ 		SEMI,

// 575 PACE  ( -- ) Send a pace character for the file downloading process.
			_DOCOL,DOLIT,11,EMIT,SEMI, 

// 580 SPACE    ( -- ) Send the blank character to the output device.
			_DOCOL,BLANK,EMIT,SEMI,

// 584 SPACES  ( +n -- ) Send n spaces to the output device.
			_DOCOL,ZERO,MAX,RTO,
          		BRANCH,&cfa[591],
/*590*/   		SPACE,
/*591*/  		DONEXT,&cfa[590],
          		SEMI,

// 594 TYPE  ( b u -- ) Output u characters from b.
			_DOCOL,RTO,
        		BRANCH,&cfa[603],
/*598*/ 		DUP,CFETCH,EMIT,
        		ONE,ADD,
/*603*/ 		DONEXT,&cfa[598],
        		DROP,SEMI,

// 607 do$  ( -- a ) Return the address of a compiled string.
			_DOCOL,RFROM,RFETCH,RFROM,COUNT,ADD,
        		ALIGNED,RTO,SWAP,RTO,SEMI,

// 618 $"|   ( -- a ) Run time routine compiled by $".
//                       Return address of a compiled string.
			_DOCOL,DOSTR,SEMI,        // force a call to do$

// 621 ."|  ( -- ) Run time routine of ." . Output a compiled string.
			_DOCOL,DOSTR,COUNT,TYPES,SEMI,

// 626 .R  ( n +n -- ) Display an integer in a field of n columns, right justified.
			_DOCOL,RTO,STR,RFROM,OVER,SUB,
        		SPACES,TYPES,SEMI,

// 635 U.R ( u +n -- ) Display an unsigned integer in n column, right justified.
			_DOCOL,RTO,BDIGS,DIGS,EDIGS,
        		RFROM,OVER,SUB,
        		SPACES,TYPES,SEMI,

// 646 U.  ( u -- ) Display an unsigned integer in free format.
			_DOCOL,BDIGS,DIGS,EDIGS,
        		SPACE,TYPES,SEMI,

// 653 _TYPE    ( b u -- ) Display a string. Filter non-printing characters.
			_DOCOL,RTO,            		//start count down loop
        		BRANCH,&cfa[664],      		//skip first pass
/*657*/ 		DUP,CFETCH,TOCHAR,EMIT,		//display only printable
        		DOLIT,1,ADD,        		//increment address
/*664*/ 		DONEXT,&cfa[657],        	//loop till done
        		DROP,SEMI,

// 668 .  ( w -- ) Display an integer in free format, preceeded by a space.
			_DOCOL,BASE,FETCH,DOLIT,10,XOR,    //?decimal
			ZBRANCH,&cfa[678],
			UDOT,SEMI,			//no, display unsigned
/*678*/ 		STR,SPACE,TYPES,SEMI,   //yes, display signed

// 682 ?  ( a -- ) Display the contents in a memory cell.
			_DOCOL,FETCH,DOT,SEMI,

// 686 	parse ( b u c -- b u delta ) Scan string delimited by c
//			Return found string and its offset.
			_DOCOL,TEMP,STORE,OVER,RTO,DUP,
			ZBRANCH,&cfa[763],
			DOLIT,1,SUB,TEMP,FETCH,BLANK,EQ,
			ZBRANCH,&cfa[724],
			RTO,
/*704*/			BLANK,OVER,CFETCH,        // skip leading blanks ONLY
			SUB,ZLT,INVERT,
			ZBRANCH,&cfa[723],
			DOLIT,1,ADD,
			DONEXT,&cfa[704],
			RFROM,DROP,DOLIT,0,DUP,SEMI,
/*723*/ 		RFROM,
/*724*/ 		OVER,SWAP,
			RTO,
/*727*/ 		TEMP,FETCH,OVER,CFETCH,SUB,    //scan for delimiter
			TEMP,FETCH,BLANK,EQ,
			ZBRANCH,&cfa[739],
			ZLT,
/*739*/ 		ZBRANCH,&cfa[750],
			DOLIT,1,ADD,
			DONEXT,&cfa[727],
			DUP,RTO,
			BRANCH,&cfa[757],
/*750*/ 		RFROM,DROP,DUP,
			DOLIT,1,ADD,RTO,
/*757*/ 		OVER,SUB,
			RFROM,RFROM,SUB,SEMI,
/*763*/ 		OVER,RFROM,SUB,SEMI,

// 767 PARSE ( c -- b u) Scan input stream and return counted string delimited by c.
			_DOCOL,RTO,TIB,IN,FETCH,ADD,    //current input buffer pointer
			NTIB,FETCH,IN,FETCH,SUB,	//remaining count
			RFROM,PARS,IN,PSTORE,SEMI,

// 783 .(  ( -- ) Output following string up to next ) .
			_DOCOL,DOLIT,')',PARSE,TYPES,SEMI,

// 789 ( ( -- ) Ignore following string up to next ) . A comment.
			_DOCOL,DOLIT,')',PARSE,DDROP,SEMI,

// 795 \ ( -- ) Ignore following text till the end of line.
			_DOCOL,NTIB,FETCH,IN,STORE,SEMI,

// 801 CHAR ( -- c ) Parse next word and return its first character.
			_DOCOL,BLANK,PARSE,DROP,CFETCH,SEMI,

// 807 TOKEN ( -- a  ) Parse a word from input stream and copy it to name dictionary.
			_DOCOL,BLANK,PARSE,DOLIT,31,MIN,
			NP,FETCH,OVER,SUB,CELLM,
			PACKS,SEMI,

// 820 WORD ( c -- a ) Parse a word from input stream and copy it to code dictionary.
			_DOCOL,PARSE,HERE,PACKS,SEMI,

// 825 NAME> ( na -- ca ) Return a code address given a name address.
			_DOCOL,CELLM,CELLM,FETCH,NOP,NOP,NOP,NOP,SEMI,

// 834 ^H ( bot eot cur -- bot eot cur ) Backup the cursor by one character.
			_DOCOL,RTO,OVER,RFROM,SWAP,OVER,XOR,
			ZBRANCH,&cfa[856],
			DOLIT,8,TECHO,ATEXEC,ONE,SUB,
			BLANK,TECHO,ATEXEC,
			DOLIT,8,TECHO,ATEXEC,
/*856*/			SEMI,

// 857 TAP ( bot eot cur c -- bot eot cur ) Accept and echo the key stroke and bump the cursor.
			_DOCOL,DUP,TECHO,ATEXEC,
			OVER,CSTORE,ONE,ADD,SEMI,

// 866 kTAP ( bot eot cur c -- bot eot cur ) Process a key stroke, CR or backspace.
			_DOCOL,DUP,DOLIT,LINEFEED,XOR,
			ZBRANCH,&cfa[883],
			DOLIT,8,XOR,
			ZBRANCH,&cfa[881],
			BLANK,TAP,SEMI,
/*881*/ 		BKSP,SEMI,
/*883*/ 		DROP,SWAP,DROP,DUP,SEMI,

// 888 accept ( b u -- b u ) Accept characters to input buffer.
//								Return with actual count.
			_DOCOL,OVER,ADD,OVER,
/*892*/ 		DDUP,XOR,
			ZBRANCH,&cfa[911],
			KEY,DUP,
			BLANK,DOLIT,127,WITHIN,
			ZBRANCH,&cfa[907],
			TAP,
			BRANCH,&cfa[909],
/*907*/ 		TTAP,ATEXEC,
/*909*/ 		BRANCH,&cfa[892],
/*911*/ 		DROP,OVER,SUB,SEMI,

// 915 EXPECT ( b u -- ) Accept input stream and store count in SPAN.
			_DOCOL,TEXPECT,ATEXEC,SPAN,STORE,DROP,SEMI,

// 922 QUERY ( -- ) Accept input stream to terminal input buffer.
			_DOCOL,TIB,DOLIT,80,TEXPECT,ATEXEC,NTIB,STORE,
			DROP,ZERO,IN,STORE,SEMI,

// 935 CATCH ( ca -- 0 | err# ) Execute word at ca and set up an error frame for it.
			_DOCOL,SPFETCH,RTO,HANDLER,FETCH,RTO,//save error frame
			RPFETCH,HANDLER,STORE,EXECUTE,//execute
			RFROM,HANDLER,STORE,//restore error frame
			RFROM,DROP,ZERO,SEMI,//no error

// 952 THROW ( err# -- err# ) Reset system to current local error frame,
			_DOCOL,HANDLER,FETCH,RPSTORE,//restore return stack
			RFROM,HANDLER,STORE,//restore handler frame
			RFROM,SWAP,RTO,SPSTORE,//restore data stack
			DROP,RFROM,SEMI,

// 966 doVAR    ( -- a ) Run time routine for CREATE.
			_DOCOL,RFROM,SEMI,

// 969 doVRAM    ( -- a ) Run time routine for VARIABLE & CONSTANT.
			_DOCOL,RFROM,FETCH,SEMI,

// 973 NULL$ ( -- a ) Return address of a null string with zero count.
			_DOCOL,DOVAR,     // emulate CREATE
       			0,
#if (ENDIAN == BIG)
			0x636f796f,0x74650000,		// ASCII "coyote"
#else
			0x6f796f63,0x00006574,		// DC.B  99,111,121,111,116,101 ENDIANESS?
#endif

// 978 ABORT  ( -- ) Reset data stack and jump to QUIT.
			_DOCOL,NULLS,THROW,

// 981 (abort") ( f -- ) Run time routine of ABORT" . Abort with a message.
			_DOCOL,ZBRANCH,&cfa[986],	//text flag
			DOSTR,THROW,			//pass error string
/*986*/ 		DOSTR,DROP,SEMI,        	//drop error

// 989 $INTERPRET ( a -- ) Interpret a word. If failed, try to convert it to an integer.
			_DOCOL,NAMEQ,QDUP,        // ?defined
        		ZBRANCH,&cfa[1005],
        		CFETCH,DOLIT,COMPO,AND,    //?compile only lexicon bits
        		//DROP,NOP,NOP,NOP,NOP,
        		ABORQ,
#if (ENDIAN == BIG)
			0x0d20636f, 0x6d70696c, 0x6e6f2065, 0x6c790000,  //13,' compile only'
#else
        		0x6f63200d, 0x6c69706d, 0x6e6f2065, 0x0000796c,  //13,' compile only'
#endif
        		EXECUTE,SEMI,         //execute defined word
/*1005*/		TNUMBER,ATEXEC,        //convert a number
        		ZBRANCH,&cfa[1010],
        		SEMI,
/*1010*/		THROW,              //error

// 1011   [        ( -- ) Start the text interpreter.
			_DOCOL,DOLIT,INTERP,TEVAL,STORE,SEMI,

// 1017   .OK   ( -- ) Display 'ok' only while interpreting.
			_DOCOL,DOLIT,INTERP,TEVAL,FETCH,EQ,
        		ZBRANCH,&cfa[1027],
        		DOTQP,
#if (ENDIAN == BIG)
			0x03206f6b,	// 3," ok"
#else
        		0x6b6f2003,	// 3," ok"
#endif
/*1027*/		CR,SEMI,

// 1029  ?STACK ( -- ) Abort if the data stack underflows.
			_DOCOL,DEPTH,ZLT,        //check only for underflow
        		ABORQ,
#if (ENDIAN == BIG)
			0x0a20756e, 0x64657266, 0x6c6f7700,	// 10," undeflow"
#else
        		0x6e75200a, 0x66726564, 0x00776f6c, 	// 10,' underflow'
#endif
        		NOP/*0*/, SEMI,

// 1038   EVAL ( -- ) Interpret the input stream.
			_DOCOL,
/*1039*/  		TOKEN,DUP,CFETCH,        //?input stream empty
        		ZBRANCH,&cfa[1049],
        		TEVAL,ATEXEC,QSTACK,    //evaluate input, check stack
        		BRANCH,&cfa[1039],
/*1049*/		DROP,TPROMPT,ATEXEC,SEMI,    //prompt

// 1053   PRESET ( -- ) Reset data stack pointer and the terminal input buffer.
			_DOCOL,SPZ,SPSTORE,
        		DOLIT,TIB,NTIB,CELLP,STORE,SEMI,

// 1062    2!  ( d a -- ) Store the double integer to address a.
			_DOCOL,SWAP,OVER,STORE,
        		CELLP,STORE,SEMI,

// 1069   2@  ( a -- d ) Fetch double integer from address a.
			_DOCOL,DUP,CELLP,FETCH,
        		SWAP,FETCH,SEMI,

// 1076  xio  ( a a a -- ) Reset the I/O vectors 'EXPECT, 'TAP, 'ECHO and 'PROMPT.
			_DOCOL,DOLIT,ACCEPT,TEXPECT,DSTORE,
        		TECHO,DSTORE,SEMI,

// 1084  FILE ( -- ) Select I/O vectors for file download.
			_DOCOL,DOLIT,PACE,DOLIT,_DROP,
        		DOLIT,KTAP,XIO,SEMI,

//  1093  HAND ( -- )   Select I/O vectors for terminal interface.
			_DOCOL,DOLIT,DOTOK,DOLIT,EMIT,
        		DOLIT,KTAP,XIO,SEMI,

// 1102   I/O  ( -- a ) Array to store default I/O vectors.
			_DOCOL,DOVAR,            //emulate CREATE
        		_QRX,_TXSTORE,        // default I/O vectors

// 1106  CONSOLE  ( -- ) Initiate terminal interface.
			_DOCOL,ISLO,DFETCH,TQKEY,DSTORE,    //restore default I/O device
       			HAND,SEMI,        //keyboard input

// 1113   QUIT ( -- ) Reset return stack pointer and start text interpreter.
			_DOCOL,RPZ,RPSTORE,        	//reset return stack pointer
/*1116*/  		LBRAC,            		// start interpretation
/*1117*/  		QUERY,           		// get input
        		DOLIT,EVAL,CATCH,QDUP,    	//evaluate input
        		ZBRANCH,&cfa[1117],        	//continue till error
        		TPROMPT,FETCH,RTO,        	//save input device
        		CONSOLE,NULLS,OVER,XOR,    	//?display error message
        		ZBRANCH,&cfa[1138],
        		SPACE,COUNT,TYPES,    		//error message
        		DOTQP,
#if (ENDIAN == BIG)
			0x03203f20,			// 3," ? "
#else
        		0x203f2003, 			// 3,' ? '            ;error prompt
#endif
/*1138*/		RFROM,DOLIT,DOTOK,XOR,    	//?file input
        		ZBRANCH,&cfa[1147],
        		DOLIT,ERR,EMIT,       		//;file error, tell host
/*1147*/		PRESET,            		//some cleanup
        		BRANCH,&cfa[1116],

// 1150 2DUP ( n1 n2 -- n1 n2 n1 n2)
			_DOCOL,OVER,OVER,SEMI,

// 1154 NOP --
			_DOCOL,SEMI,

// 1156 TEMP ( -- a ) return address of temp variable
			_DOCOL,DOLIT,_TMP,SEMI,

// 1160   '  ( -- ca )Search context vocabularies for the next word in input stream.
			_DOCOL,TOKEN,NAMEQ,        	//?defined
        		ZBRANCH,&cfa[1166],
        		SEMI,            		//yes, push code address
/*1166*/		THROW,            		//no, error

// 1167   ALLOT    ( n -- ) Allocate n bytes to the variable memory.
			_DOCOL,VP,PSTORE,SEMI,        //adjust code pointer

// 1171   ,    ( w -- ) Compile an integer into the code dictionary.
			_DOCOL,HERE,DUP,CELLP,        	//cell boundary
        		CP,STORE,STORE,SEMI,    			//adjust code pointer and compile

// 1179   [COMPILE]    ( -- ; <string> ) Compile the next immediate word
//									into code dictionary.
			_DOCOL,TICK,COMMA,SEMI,

// 1183   COMPILE ( -- ) Compile the next address in colon list to code dictionary.
			_DOCOL,RFROM,DUP,FETCH,COMMA,    //compile address
        		CELLP,RTO,SEMI,        		//adjust return address

// 1191   LITERAL  ( w -- ) Compile tos to code dictionary as an integer literal.
			_DOCOL,COMPILE,DOLIT,COMMA,SEMI,

// 1196  $," ( -- ) Compile a literal string up to next " .
			_DOCOL,DOLIT,0x22,WORDD,        // '"' ,move string to code dictionary
        		COUNT,ADD,ALIGNED,    //calculate aligned end of string
        		CP,STORE,SEMI,        //adjust the code pointer

// 1206  RECURSE  ( -- ) Make the current word available for compilation.
			_DOCOL,LAST,FETCH,NAMET,COMMA,SEMI,

// 1212   FOR  ( -- a ) Start a FOR-NEXT loop structure in a colon definition.
			_DOCOL,COMPILE,RTO,HERE,SEMI,

// 1217  BEGIN    ( -- a ) Start an infinite or indefinite loop structure.
			_DOCOL,HERE,SEMI,

// 1220  NEXT ( a -- ) Terminate a FOR-NEXT loop structure.
			_DOCOL,COMPILE,DONEXT,COMMA,SEMI,

// 1225  UNTIL  ( a -- ) Terminate a BEGIN-UNTIL indefinite loop structure.
			_DOCOL,COMPILE,ZBRANCH,COMMA,SEMI,

// 1230  AGAIN ( a -- ) Terminate a BEGIN-AGAIN infinite loop structure.
			_DOCOL,COMPILE,BRANCH,COMMA,SEMI,

// 1235   IF  ( -- A ) Begin a conditional branch structure.
			_DOCOL,COMPILE,ZBRANCH,HERE,
        		ZERO,COMMA,SEMI,

// 1242   AHEAD ( -- A ) Compile a forward branch instruction.
			_DOCOL,COMPILE,BRANCH,HERE,ZERO,COMMA,SEMI,

// 1249   REPEAT ( A a -- ) Terminate a BEGIN-WHILE-REPEAT indefinite loop.
			_DOCOL,AGAIN,HERE,SWAP,STORE,SEMI,

// 1255   THEN ( A -- ) Terminate a conditional branch structure.
			_DOCOL,HERE,SWAP,STORE,SEMI,

// 1260   AFT ( a -- a A ) Jump to THEN in a FOR-AFT-THEN-NEXT
//									loop the first time through.
			_DOCOL,DROP,AHEAD,BEGIN,SWAP,SEMI,

// 1266   ELSE  ( A -- A ) Start the false clause in an IF-ELSE-THEN structure.
			_DOCOL,AHEAD,SWAP,THENN,SEMI,

// 1271   WHILE  ( a -- A a ) Conditional branch out of a BEGIN-WHILE-REPEAT loop.
			_DOCOL, IFF,SWAP,SEMI,

// 1275   ABORT" ( -- ; <string> ) Conditional abort with an error message.
			_DOCOL,COMPILE,ABORQ,STRCQ,SEMI,

// 1280   $"   ( -- ; <string> ) Compile an inline string literal.
			_DOCOL,COMPILE,STRQP,STRCQ,SEMI,

// 1285   ." ( -- ; <string> ) Compile an inline string literal to be typed out at run time.
			_DOCOL,COMPILE,DOTQP,STRCQ,SEMI,

// 1290   ?UNIQUE ( a -- a ) Display a warning message if the word already exists.
			_DOCOL,DUP,NAMEQ,       	//?name exists
        		ZBRANCH,&cfa[1301],
        		DOTQP,            		// redefinitions are OK
#if (ENDIAN == BIG)
			0x07207265, 0x44656620,		//  7," reDef "
#else
        		0x65722007, 0x20666544, 	//  7,' reDef '  but the user should be warned
#endif
        		OVER,COUNT,TYPES,    		// just in case its not planned
/*1301*/		DROP,SEMI,

// 1303   $,n   ( na -- ) Build a new dictionary name using the string at na.
			_DOCOL,DUP,CFETCH,       	//?null input
        		ZBRANCH,&cfa[1327],
        		UNIQUE,            		//?redefinition
        		DUP,LAST,STORE,        		//save na for vocabulary link
        		HERE,ALIGNED,SWAP,      	//align code address
        		CELLM,            		//link address
        		CURRENT,FETCH,FETCH,OVER,STORE,	// save link to previous word
        		CELLM,
        		DUP,NP,STORE,    		//adjust name pointer
        		STORE,SEMI,      		//save code pointer
/*1327*/  		STRQP,
#if (ENDIAN == BIG)
        		0x05206e61, 0x6d650000,		//   5,' name'        ;null input
#else
        		0x616e2005, 0x0000656d,		//   5,' name'        ;null input
#endif
        		THROW,

// FORTH compiler

// 1331   $COMPILE  ( a -- ) Compile next word to code dictionary as a token or literal.
			_DOCOL,NAMEQ,QDUP,        	//?defined
       			ZBRANCH,&cfa[1346],
        		CFETCH,DOLIT,IMEDD,AND,    	//?immediate
        		ZBRANCH,&cfa[1344],
        		EXECUTE,SEMI,       		//its immediate, execute
/*1344*/		COMMA,SEMI,        		//its not immediate, compile
/*1346*/		TNUMBER,ATEXEC,    		//try to convert to number
        		ZBRANCH,&cfa[1352],
        		LITERAL,SEMI,      		//compile number as integer
/*1352*/		THROW,            		//error

// 1353  OVERT  ( -- ) Link a new word into the current vocabulary.
			_DOCOL,LAST,FETCH,CURRENT,FETCH,STORE,SEMI,
// 1360  ;  ( -- ) Terminate a colon definition.
			_DOCOL,COMPILE,SEMI,LBRAC,OVERT,SEMI,

// 1366   ]  ( -- ) Start compiling the words in the input stream.
			_DOCOL,DOLIT,SCOMP,TEVAL,STORE,SEMI,

// 1372   :  ( -- ; <string> ) Start a new colon definition using next word as its name.
			_DOCOL,TOKEN,SNAME,DOLIT,_DOCOL,
        		COMMA,RBRAC,SEMI,

// 1380  IMMEDIATE ( -- ) Make the last compiled word an immediate word.
			_DOCOL,DOLIT,IMEDD,LAST,FETCH,CFETCH,OR,
        		LAST,FETCH,CSTORE,SEMI,

// 1391  COMPILE-ONLY ( -- ) Make the last compiled word a compile-only word.
			_DOCOL,DOLIT,COMPO,LAST,FETCH,CFETCH,OR,
        		LAST,FETCH,CSTORE,SEMI,

 // Defining words


// 1402 HEADER  ( -- ; <string> ) Compile a new header.
			_DOCOL,TOKEN,SNAME,OVERT,
        		DOLIT,_DOCOL,COMMA,SEMI,

// 1410  USER ( u -- ; <string> )Compile a new user variable.
			_DOCOL,HEADER,
        		DOLIT,DOUSER,COMMA,
        		COMMA,SEMI,

// 1417 CREATE ( -- ; <string> ) Compile a new dict entry without
//	allocating code space. Added NOP for DOES> word use.
			_DOCOL,HEADER,COMPILE,NOP,
        		COMPILE,DOVAR,SEMI,

// 1424 VARIABLE ( -- ; <string> ) Compile a new variable in RAM .
			_DOCOL,HEADER,
        		DOLIT,DOVRAM,COMMA,
        		VP,FETCH,COMMA,            	//Store address of variable pointer
        		TWO,VP,PSTORE,SEMI,  		//increment the pointer

// 1436 CONSTANT ( u -- ; <string> ) Compile a constant in code space.
			_DOCOL,HEADER,
        		DOLIT,DOVRAM,COMMA,
        		COMMA,SEMI,

// Tools

// 1443 _TYPE ( b u -- ) Display a string. Filter non-printing characters.
			_DOCOL,RTO,			//start count down loop
        		BRANCH,&cfa[1453],		//skip first pass
/*1447*/  		DUP,CFETCH,TOCHAR,EMIT,    	//display only printable
        		ONE,ADD,			//increment address
/*1453*/		DONEXT,&cfa[1447],		//loop till done
        		DROP,SEMI,

// 1457 dm+ ( a u -- a ) Dump u bytes from , leaving a+u on the stack.
			_DOCOL,OVER,DOLIT,4,UDOTR,    	//display address
        		SPACE,RTO,			//start count down loop
        		BRANCH,&cfa[1473],		//skip first pass
/*1466*/  		DUP,CFETCH,DOLIT,3,UDOTR,	//display numeric data
        		ONE,ADD,			//increment address
/*1473*/  		DONEXT,&cfa[1466],		//loop till done
        		SEMI,

// 1476 DUMP ( a u -- ) Dump u bytes from a, in a formatted manner.
			_DOCOL,BASE,FETCH,RTO,HEX,      //save radix, set hex
        		DOLIT,16,DIV,			//change count to lines
        		RTO,				//start count down loop
/*1485*/  		CR,DOLIT,16,DDUP,DUMPP,    	//display numeric
        		ROT,ROT,
        		DOLIT,2,SPACES,UTYPE,		//display printable characters
        		NUFQ,INVERT,			//user control
        		ZBRANCH,&cfa[1504],
        		DONEXT,&cfa[1485],		//loop till done
        		BRANCH,&cfa[1506],
/*1504*/  		RFROM,DROP,			//cleanup loop stack, early exit
/*1506*/  		DROP,RFROM,BASE,STORE,		//restore radix
        		SEMI,

// 1511 .S ( -- ) Display the contents of the data stack.
			_DOCOL,CR,DEPTH,		//stack depth
        		RTO,				//start count down loop
        		BRANCH,&cfa[1520],		//skip first pass
/*1517*/  		RFETCH,PICK,DOT,		//index stack, display contents
/*1520*/  		DONEXT,&cfa[1517],		//loop till done
        		DOTQP,
#if (ENDIAN == BIG)
			0x04203c73, 0x70000000,		// 4," <sp",0
#else
        		0x733c2004, 0x00000070,		// 4,' <sp',0
#endif
        		SEMI,

// 1526 !CSP ( -- ) Save stack pointer in CSP for error checking.
			_DOCOL,SPFETCH,CSP,STORE,SEMI,	//save pointer

// 1531 ?CSP ( -- ) Abort if stack pointer differs from that saved in CSP.
			_DOCOL,SPFETCH,CSP,FETCH,XOR,    //compare pointers
        		ABORQ,            		//abort if different
#if (ENDIAN == BIG)
			0x06737461, 0x636b7300,		// 6,"stacks",0
#else
        		0x61747306, 0x00736b63, 	// 6,'stacks',0
#endif
        		SEMI,

// 1540  >NAME ( ca -- na | F ) Convert code address to a name address.
			_DOCOL,CURRENT,			//vocabulary link
/*1542*/  		CELLP,FETCH,QDUP,		//check all vocabularies
        		ZBRANCH,&cfa[1570],
        		DDUP,
/*1548*/		FETCH,DUP,			//?last word in a vocabulary
        		ZBRANCH,&cfa[1560],
        		DDUP,NAMET,XOR,			//compare
        		ZBRANCH,&cfa[1560],
        		CELLM,				//continue with next word
        		BRANCH,&cfa[1548],
/*1560*/  		SWAP,DROP,QDUP,
        		ZBRANCH,&cfa[1542],
        		SWAP,DROP,SWAP,DROP,SEMI,
/*1570*/  		DROP,ZERO,SEMI,

// 1573 .ID ( na -- ) Display the name at address.
			_DOCOL,QDUP,			//if zero no name
        		ZBRANCH,&cfa[1583],
        		COUNT,DOLIT,MASK,AND,		//mask lexicon bits
        		UTYPE,SEMI,					//display name string
/*1583*/ 		DOTQP,
#if (ENDIAN == BIG)
			0x09207b6e, 0x6f4e616d, 0x657d0000, // 9," {noName}"
#else
        		0x6e7b2009, 0x6d614e6f, 0x00007d65, // 9,' {noName}'
#endif
        		SEMI,

// 1588 SEE ( -- ; <string> ) A simple decompiler.
			_DOCOL,TICK,            	//starting address
        		CR,CELLP,
/*1592*/ 		CELLP,DUP,FETCH,DUP,   		//?does it contain a zero
        		ZBRANCH,&cfa[1599],
        		TNAME,				//?is it a name
/*1599*/ 		QDUP,				//name address or zero
        		ZBRANCH,&cfa[1606],
        		SPACE,DOTID,			//display name
        		BRANCH,&cfa[1609],
/*1606*/ 		DUP,FETCH,UDOT,        		//display number
/*1609*/ 		NUFQ,				//user control
        		ZBRANCH,&cfa[1592],
        		DROP,SEMI,

// 1614 WORDS ( -- ) Display the names in the context vocabulary.
			_DOCOL,CR,CONTEXT,FETCH,	//only in context
/*1618*/ 		FETCH,QDUP,NOP,NOP,NOP,NOP,NOP,	//?at end of list,look for null at lfa
        		ZBRANCH,&cfa[1636],
        		DUP,SPACE,DOTID,		//display a name
        		CELLM,NUFQ,NOP,
        		ZBRANCH,&cfa[1618],
        		DROP,
/*1636*/ 		SEMI,NOP,

// 1638	CSP ( -- a)
			_DOCOL,DOLIT,_CSP,SEMI,

// 1642	UP ( -- a)
			_DOCOL,DOLIT,_UP,SEMI,

// 1646  doUSER ( -- a ) Run time routine for user variables.
			_DOCOL,RFROM,FETCH,UP,FETCH,ADD,SEMI,

// 1653   VERSION  ( -- n ) Return the version number of this implementation.
			_DOCOL,DOLIT,80,SEMI,

// 1657  'BOOT ( -- a ) The initial application startup vector.
			_DOCOL,DOLIT,_TBOOT,SEMI,          //user location TBOOT has vector.

// 1661   hi ( -- ) Display the sign-on message of eForth.
			_DOCOL,CR,DOTQP,         //initialize I/O
#if (ENDIAN == BIG)
			0x1d677261, 0x7370466f, 0x72746820, 0x56302e38, // 29, graspForth V0.85 (c) 2004 BRM
			0x35202863, 0x29203230, 0x30342042, 0x524d0000,
#else
			0x6172671d, 0x6f467073, 0x20687472, 0x382e3056, // 29, graspForth V0.85 (c) 2004 BRM
			0x63282035, 0x30322029, 0x42203430, 0x00004d52,
#endif
          		CR,SEMI,

// 1674	CONTEXT ( -- a)
			_DOCOL,DOLIT,_CONTEXT,SEMI,

// 1678   FORTH    ( -- ) Make FORTH the context vocabulary.
			_DOCOL,VFRTH,CONTEXT,STORE,SEMI,

// 1683 COLD		 ( -- ) The hilevel cold start sequence.
			_DOCOL,
// 1684  _COLD
			PRESET,              		//initialize data stack and TIB
			DECIMAL,
        		TBOOT,ATEXEC,               	//application boot
        		FORTH,CONTEXT,FETCH,DUP,        //initialize search order
        		CURRENT,DSTORE,OVERT,
        		QUIT,                		//start interpretation
        		BRANCH,_COLD                	//just in case

//----------------------------------------------------------------------------

	}; // end of high level colon words.

// ------------------------------- The Dictionary ----------------------------
	FORTHDICT dict[] = {
	//	  CFA		LFA			COUNT		NAME
	// Compiling words
		{ NEXT,		0,			4+COMPO,	"next" 		},
		{ DOCOL,	&dict[0].count,		5+COMPO,	"docol"		},
		{ SEMI,		&dict[1].count,		4,		"semi"		},
		{ DOLIT,	&dict[2].count,		5+COMPO,	"dolit"		},
		{ DOCON,	&dict[3].count,		5,		"docon"		},
		{ DOCASE,	&dict[4].count,		6+COMPO,	"docase"	},
		{ EXECUTE,	&dict[5].count,		7,		"execute"	},
		{ ATEXEC,	&dict[6].count,		8,		"@execute"	},
	// Branching
		{ BRANCH,	&dict[7].count,		6+COMPO,	"branch"	},
		{ ZBRANCH,	&dict[8].count,		7+COMPO,	"zbranch"	},
	// Memory
		{ STORE,	&dict[9].count,		1,		"!"		},
		{ PSTORE,	&dict[10].count,	2,		"+!"		},
		{ FETCH,	&dict[11].count,	1,		"@"		},
		{ CSTORE,	&dict[12].count,	2,		"c!"		},
		{ CFETCH,	&dict[13].count,	2,		"c@"		},
	// Return Stack
		{ RTO,		&dict[14].count,	2+COMPO,	">r"		},
		{ RFROM,	&dict[15].count,	2+COMPO,	"r>"		},
		{ RFETCH,	&dict[16].count,	2,		"r@"		},
		{ RPZ,		&dict[17].count,	3,		"rp0"		},
		{ RPFETCH,	&dict[18].count,	3+COMPO,	"rp@"		},
		{ RPSTORE,	&dict[19].count,	3+COMPO,	"rp!"		},
	// Parameter Stack
		{ SWAP,		&dict[20].count,	4,		"swap"		},
		{ DROP,		&dict[21].count,	4,		"drop"		},
		{ DUP,		&dict[22].count,	3,		"dup"		},
		{ QDUP,		&dict[23].count,	4,		"?dup"		},
		{ SPZ,		&dict[24].count,	3,		"sp0"		},
		{ NIP,		&dict[25].count,	3,		"nip"		},
		{ SPFETCH,	&dict[26].count,	3,		"sp@"		},
		{ SPSTORE,	&dict[27].count,	3,		"sp!"		},
		{ OVER,		&dict[28].count,	4,		"over"		},
		{ ROT,		&dict[29].count,	3,		"rot"		},
	// Input/Output
		{ TXSTORE,	&dict[30].count,	5,		"(tx!)"		},
		{ QRX,		&dict[31].count,	5,		"(?rx)"		},
	// Arithmetic
		{ ADD,		&dict[32].count,	1,		"+"		},
		{ SUB,		&dict[33].count,	1,		"-"		},
		{ UADD,		&dict[34].count,	2,		"u+"		},
		{ USUB,		&dict[35].count,	2,		"u-"		},
		{ MUL,		&dict[36].count,	1,		"*"		},
		{ DIV,		&dict[37].count,	1,		"/"		},
		{ UMUL,		&dict[38].count,	2,		"u*"		},
		{ UDIV,		&dict[39].count,	2,		"u/"		},
		{ MULDIV,	&dict[40].count,	2,		"*/"		},
		{ UMULDIV,	&dict[41].count,	3,		"u*/"		},
		{ TWOMUL,	&dict[42].count,	2,		"2*"		},
		{ TWODIV,	&dict[43].count,	2,		"2/"		},
		{ LSHIFT,	&dict[44].count,	6,		"lshift"	},
		{ RSHIFT,	&dict[45].count,	6,		"rshift"	},
		{ ZERO,		&dict[46].count,	1,		"0"		},
		{ ONE,		&dict[47].count,	1,		"1"		},
		{ TWO,		&dict[48].count,	1,		"2"		},
		{ THREE,	&dict[49].count,	1,		"3"		},
		{ NONE,		&dict[50].count,	2,		"-1"		},
		{ NTWO,		&dict[51].count,	2,		"-2"		},
		{ NTHREE,	&dict[52].count,	2,		"-3"		},
		{ INVERT,	&dict[53].count,	6,		"invert"	},
		{ NEGATE,	&dict[54].count,	6,		"negate" 	},
		{ MOD,		&dict[55].count,	3,		"mod"	 	},
		{ UMOD,		&dict[56].count,	4,		"umod"	 	},
// Logical
		{ TRUEE,	&dict[57].count,	4,		"true"		},
		{ FALSEE,	&dict[58].count,	5,		"false"		},
		{ EQ,		&dict[59].count,	1,		"="		},
		{ ZEQ,		&dict[60].count,	2,		"0="		},
		{ LT	,	&dict[61].count,	1,		"<"		},
		{ ULT,		&dict[62].count,	2,		"u<"		},
		{ ZLT,		&dict[63].count,	2,		"0<"		},
		{ GT,		&dict[64].count,	1,		">"		},
		{ WITHIN,	&dict[65].count,	6,		"within"	},
		{ MAX,		&dict[66].count,	3,		"max"		},
		{ MIN,		&dict[67].count,	3,		"max"		},
		{ AND,		&dict[68].count,	3,		"and"		},
		{ OR,		&dict[69].count,	2,		"or"		},
		{ XOR,		&dict[70].count,	3,		"xor"		},
		{ NOT,		&dict[71].count,	3,		"not"		},
// Dictionary
		{ NAMEQ,	&dict[72].count,	5,		"name?"	 	},
// Looping
		{ DONEXT,	&dict[73].count,	6,		"donext"	},
// System & user variables
		{ TQKEY,	&dict[74].count,	5,		"'?key"		},
		{ TEMIT,	&dict[75].count,	5,		"'emit"		},
		{ TEXPECT,	&dict[76].count,	7,		"'expect"	},
		{ TTAP,		&dict[77].count,	4,		"'tap"		},
		{ TECHO,	&dict[78].count,	5,		"'echo"		},
		{ TPROMPT,	&dict[79].count,	7,		"'prompt"	},
		{ TEVAL,	&dict[80].count,	5,		"'evel"		},
		{ TNUMBER,	&dict[81].count,	7,		"'number"	},
		{ BASE,		&dict[82].count,	4,		"base"		},
		{ SPAN,		&dict[83].count,	4,		"span"		},
		{ IN,		&dict[84].count,	3,		">in"		},
		{ NTIB,		&dict[85].count,	4,		"#tib"		},
		{ HLD,		&dict[86].count,	3,		"hld"		},
		{ HANDLER,	&dict[87].count,	7,		"handler"	},
		{ CP,		&dict[88].count,	2,		"cp"		},
		{ NP,		&dict[89].count,	2,		"np"		},
		{ VP,		&dict[90].count,	2,		"vp"		},
		{ LAST,		&dict[91].count,	4,		"last"		},
		{ VFRTH,	&dict[92].count,	7,		"(forth)"	},
		{ CURRENT,	&dict[93].count,	7,		"current"	},
		{ UP,		&dict[94].count,	2,		"up"		},
		{ TIB,		&dict[95].count,	3,		"tib"		},
// High level colon words
		{ EMIT,		&dict[96].count,	4,		"emit"		},
		{ QKEY,		&dict[97].count,	4,		"?key"		},
		{ KEY,		&dict[98].count,	3,		"key"		},
		{ ABS,		&dict[99].count,	3,		"abs"		},
		{ CELLP,	&dict[100].count,	5,		"cell+"		},
		{ CELLM,	&dict[101].count,	5,		"cell-"		},
		{ CELLS,	&dict[102].count,	5,		"cells"		},
		{ ALIGNED,	&dict[103].count,	7,		"aligned"	},
		{ BLANK,	&dict[104].count,	5,		"blank"		},
		{ TOCHAR,	&dict[105].count,	5,		">char"		},
		{ DEPTH,	&dict[106].count,	5,		"depth"		},
		{ PICK,		&dict[107].count,	4,		"pick"		},
		{ COUNT,	&dict[108].count,	5,		"count"		},
		{ HERE,		&dict[109].count,	4,		"here"		},
		{ PAD,		&dict[110].count,	3,		"pad"		},
		{ CMOVE,	&dict[111].count,	5,		"cmove"		},
		{ FILL,		&dict[112].count,	4,		"fill"		},
		{ NTRAIL,	&dict[113].count,	9,		"-trailing"	},
		{ PACKS,	&dict[114].count,	5,		"pack$"		},
		{ DIGIT,	&dict[115].count,	5,		"digit"		},
		{ UDIVMOD,	&dict[116].count,	5,		"u/mod"		},
		{ EXTRACT,	&dict[117].count,	7,		"extract"	},
		{ BDIGS,	&dict[118].count,	2,		"<#"		},
		{ HOLD,		&dict[119].count,	4,		"hold"		},
		{ DIG,		&dict[120].count,	1,		"#"		},
		{ DIGS,		&dict[121].count,	2,		"#s"		},
		{ SIGN,		&dict[122].count,	4,		"sign"		},
		{ EDIGS,	&dict[123].count,	2,		"#>"		},
		{ STR,		&dict[124].count,	3,		"str"		},
		{ HEX,		&dict[125].count,	3,		"hex"		},
		{ DECIMAL,	&dict[126].count,	7,		"decimal"	},
		{ DIGITQ,	&dict[127].count,	6,		"digit?"	},
		{ DDROP,	&dict[128].count,	5,		"2drop"		},
		{ NUMBERQ,	&dict[129].count,	7,		"number?"	},
		{ CR,		&dict[130].count,	2,		"cr"		},
		{ NUFQ,		&dict[131].count,	4,		"nuf?"		},
		{ PACE ,	&dict[132].count,	4,		"pace"		},
		{ SPACE,	&dict[133].count,	5,		"space"		},
		{ SPACES,	&dict[134].count,	6,		"spaces"	},
		{ TYPES,	&dict[135].count,	4,		"type"		},
		{ DOSTR,	&dict[136].count,	3+COMPO,	"do$"		},
		{ STRQP,	&dict[137].count,	3+COMPO,	"$\"|"		},
		{ DOTQP,	&dict[138].count,	3+COMPO,	".\"|"		},
		{ DOTR,		&dict[139].count,	2,		".r"		},
		{ UDOTR,	&dict[140].count,	3,		"u.r"		},
		{ UDOT,		&dict[141].count,	2,		"u."		},
		{ UTYPE,	&dict[142].count,	5,		"_type"		},
		{ DOT,		&dict[143].count,	1,		"."		},
		{ QUEST,	&dict[144].count,	1,		"?"		},
		{ PARS ,	&dict[145].count,	7,		"(parse)"	},
		{ PARSE,	&dict[146].count,	5,		"parse"		},
		{ DOTPR,	&dict[147].count,	2+IMEDD,	".("		},
		{ PAREN,	&dict[148].count,	1+IMEDD,	"("		},
		{ BKSLA,	&dict[149].count,	1+IMEDD,	"\\"		},
		{ CHAR ,	&dict[150].count,	4,		"char"		},
		{ TOKEN ,	&dict[151].count,	5,		"token"		},
		{ WORDD,	&dict[152].count,	4,		"word"		},
		{ NAMET,	&dict[153].count,	5,		"name>"		},
		{ BKSP,		&dict[154].count,	2,		"^h"		},
		{ TAP,		&dict[155].count,	3,		"tap"		},
		{ KTAP,		&dict[156].count,	4,		"ktap"		},
		{ ACCEPT,	&dict[157].count,	6,		"accept"	},
		{ EXPECT,	&dict[158].count,	6,		"expect"	},
		{ QUERY,	&dict[159].count,	5,		"query"		},
		{ CATCH ,	&dict[160].count,	5,		"catch"		},
		{ THROW,	&dict[161].count,	5,		"throw"		},
		{ DOVAR,	&dict[162].count,	5+COMPO,	"dovar"		},
		{ DOVRAM,	&dict[163].count,	6+COMPO,	"dovram"	},
		{ NULLS,	&dict[164].count,	5,		"null$"		},
		{ ABORT,	&dict[165].count,	5,		"abort"		},
		{ ABORQ,	&dict[166].count,	8+COMPO,	"(abort\")"	},
		{ INTERP,	&dict[167].count,	10,		"$interpret"	},
		{ LBRAC,	&dict[168].count,	1+IMEDD,	"["		},
		{ DOTOK,	&dict[169].count,	3,		".ok"		},
		{ QSTACK,	&dict[170].count,	6,		"?stack"	},
		{ EVAL ,	&dict[171].count,	4,		"eval"		},
		{ PRESET,	&dict[172].count,	6,		"preset"	},
		{ DSTORE,	&dict[173].count,	2,		"2!"		},
		{ DFETCH,	&dict[174].count,	2,		"2@"		},
		{ XIO,		&dict[175].count,	3+COMPO,	"xio"		},
		{ FILE,		&dict[176].count,	4,		"file"		},
		{ HAND,		&dict[177].count,	4,		"hand"		},
		{ ISLO ,	&dict[178].count,	3,		"i/o"		},
		{ CONSOLE,	&dict[179].count,	7,		"console"	},
		{ QUIT,		&dict[180].count,	4,		"quit"		},
		{ DDUP,		&dict[181].count,	4,		"2dup"		},
// The compiler
		{ TICK,		&dict[182].count,	1,		"'"		},
		{ ALLOT,	&dict[183].count,	5,		"allot"		},
		{ COMMA,	&dict[184].count,	1,		","		},
		{ BCOMP,	&dict[185].count,	9+IMEDD,	"[compile]"	},
		{ COMPILE,	&dict[186].count,	7+COMPO,	"compile"	},
		{ LITERAL,	&dict[187].count,	7+IMEDD,	"literal"	},
		{ STRCQ,	&dict[188].count,	3,		"$,\""		},
		{ RECURSE,	&dict[189].count,	7+IMEDD,	"recurse"	},
// Structures
		{ FORR ,	&dict[190].count,	3+IMEDD,	"for"		},
		{ BEGIN,	&dict[191].count,	5+IMEDD,	"begin"		},
		{ NEXT,		&dict[192].count,	4+IMEDD,	"next	"	},
		{ UNTIL,	&dict[193].count,	5+IMEDD,	"until"		},
		{ AGAIN,	&dict[194].count,	5+IMEDD,	"again"		},
		{ IFF,		&dict[195].count,	2+IMEDD,	"if"		},
		{ AHEAD,	&dict[196].count,	5+IMEDD,	"ahead"		},
		{ REPEAT,	&dict[197].count,	6+IMEDD,	"repeat"	},
		{ THENN,	&dict[198].count,	4+IMEDD,	"then"		},
		{ AFT,		&dict[199].count,	3+IMEDD,	"aft"		},
		{ ELSEE,	&dict[200].count,	4+IMEDD,	"else"		},
		{ WHILE,	&dict[201].count,	5+IMEDD,	"while"		},
		{ ABORTQ,	&dict[202].count,	6+IMEDD,	"abort\""	},
		{ STRQ,		&dict[203].count,	2+IMEDD,	"$\""		},
		{ DOTQ ,	&dict[204].count,	2+IMEDD,	".\""		},
// Name Compiler
		{ UNIQUE,	&dict[205].count,	7,		"?unique"	},
		{ SNAME,	&dict[206].count,	3,		"$,n"		},
// Forth Compiler
		{ SCOMP,	&dict[207].count,	8,		"$compile"	},
		{ OVERT,	&dict[208].count,	5,		"overt"		},
		{ SEMIS,	&dict[209].count,	1+IMEDD+COMPO,	";"		},
		{ RBRAC,	&dict[210].count,	1,		"]"		},
		{ COLON,	&dict[211].count,	1,		":"		},
		{ IMMEDIATE,	&dict[212].count,	9+IMEDD,	"immediate"	},
		{ COMPON,	&dict[213].count,	9,		"comp-only"	},
// Defining words
		{ HEADER,	&dict[214].count,	6,		"header"	},
		{ USER ,	&dict[215].count,	4,		"user"		},
		{ CREATE,	&dict[216].count,	6,		"create"	},
		{ VARIABLE,	&dict[217].count,	8,		"variable"	},
		{ CONSTANT,	&dict[218].count,	8,		"constant",	},
// Tools
		{ DUMPP,	&dict[219].count,	3,		"dm+"		},
		{ DUMP,		&dict[220].count,	4,		"dump"		},
		{ DOTS ,	&dict[221].count,	2,		".s"		},
		{ STCSP,	&dict[222].count,	4,		"!csp"		},
		{ QCSP,		&dict[223].count,	4,		"?csp"		},
		{ TNAME,	&dict[224].count,	5,		">name"		},
		{ DOTID,	&dict[225].count,	3,		".id"		},
		{ SEE,		&dict[226].count,	3,		"see"		},
		{ WORDS,	&dict[227].count,	5,		"words"		},
		{ DOUSER,	&dict[228].count,	6+COMPO,	"douser"	},
		{ VERSION,	&dict[229].count,	7,		"version"	},
		{ TBOOT,	&dict[230].count,	5,		"'boot"		},
		{ HI,		&dict[231].count,	2,		"hi"		},
		{ CONTEXT,	&dict[232].count,	7,		"context"	},
		{ FORTH,	&dict[233].count,	5,		"forth"		},
		{ COLD,		&dict[234].count,	4,		"cold"		}


	}; // end of cfa[]

// ----------------------- Initialization ------------------------------------

	// Initialize system & user variable space
	j = 7;	// start of variable space
	for(i=0;i<sizeof(sys_var)-1;++i,++j)
		mem[j] = sys_var[i];
	texpect	= (ACCEPT);
	ttap	= (KTAP);
	tprompt = (DOTOK);
	teval	= (INTERP);
	tnumber = (NUMBERQ);
	tboot = (HI);

    // Allocate space for stacks. Both stacks grow downward.
    RP = R0 = (INT) &mem[MEMSIZE]; // Highest
    SP = S0 = (INT) &mem[MEMSIZE - RETURN_STACK]; // Just below it
    // Name pointer to top of ram.
    np = &mem[MEMSIZE - (RETURN_STACK + PAR_STACK)];
    // Code pointer above pad
    cp = &pad_end;
    // User pointer area
    up = &user;
    // Variable pointer area
    vp = &vm;
    // point to last name in dictionary
    vfrth =  llast = &dict[(sizeof(dict) / sizeof(HEADS) - 1 )].count;
    // load/boot application ROM if present
    boot_rom();
    // Iniatialize character IO
    initIO();
    // Set IP to initial value
    IP = (INT)_COLD;

    // Fall through to next & go for it!

//------------------------------ primitive word codes   -----------------------

// ---------------------------------- compiling ------------------------------

next:
    // The address interpreter of Forth (ITC)
    // Load W with what's inside the Code Field
    // Advance IP
    // Goto the label whose address now in W
    W = *(PTR)IP;
    INC(IP);
    goto  **(PTR)W;	// W is not incremented here, which means we must do it
  			// in docol, docon and dodoes ... but is faster overall since
    			// next is faster.

docol:
    // run time of  colon
    DEC(RP);		// make room on return stack
    TOR = IP;		// save IP .. restored by semi
    IP = W + CELL;	// new IP
    goto next;

semi:
    // end of colon definition (exit)
    IP = TOR;
    INC(RP);
    goto next;

dolit:
    // plit ( -- n)
    // Make room for the value
    DEC(SP);
    TOS = *(PTR)IP;
    // Skip by the value
    INC(IP);
    goto next;

docon:
    // ( -- n ) docon,  run time of constant
    DEC(SP);		// make room
    TOS = (*(PTR)(W + CELL)); 	// get contents of parameter field = W+CELL
    goto next;

docase:
    // ( switch n  --  switch | empty ) docase, run time action of case
    if(TOS==NOS)	// n = switch?
    {
	INC(SP); 	// yes, pop stack
	INC(IP);	// all done, skip branch offset
	goto next;
    }
    IP= *(PTR)IP;	// next case
    goto next;

execute:
    // (ca -- ) execute cfa on stack.
    W = TOS;
    INC(SP);	// pop stack
    goto **(PTR)W;	// jump to code

atexec:
    // (a -- ) execute @ contents of address on stack.
    W = TOS;
    INC(SP);			// pop stack
    adr = (PTR)(DONEXT);	// End of primitives
    adr2 = *(PTR)W;
    // Add extra level of indirection for COLON words
    if( (PTR)adr2 >= (PTR)adr)  W =  *(PTR)W;
    goto **(PTR)W;

// ----------------------------- branching -----------------------------------
branch:
    IP = *(PTR)IP;
    goto next;

zbranch:
    // ( addr n -- )
    if(TOS!=0)
	{ INC(IP); INC(SP); goto next;}
    IP = *(PTR)IP;
    INC(SP);
    goto next;


// -------------------------------- memory store------------------------------
store:
    // ! ( n addr -- )
    *(PTR)TOS = NOS;
    DINC(SP);
    goto next;

pstore:
    // +! ( n addr -- )
     *(PTR)TOS += NOS;
    DINC(SP);
    goto next;

fetch:
    // @ ( addr -- n )
    TOS = *(PTR)TOS;
    goto next;

cstore:
    // C! ( n addr -- )
    *(CPTR)TOS = (unsigned char)NOS;
    DINC(SP);
    goto next;

cfetch:
    // C@ ( addr -- n )
    TOS = *(CPTR)TOS;
    goto next;

// ----------------------------- return stack operations ---------------------
rto:
    // >r ( n -- )
    DEC(RP); 		// make room on return stack
    TOR = TOS;
    INC(SP);		// pop stack
    goto next;

rfrom:
    // r> ( -- n)
    DEC(SP); 		// make room on stack
    TOS = TOR;
    INC(RP);		// pop return stack
    goto next;

rfetch:
    // r@ ( -- n)  get copy of return stack
    DEC(SP);		// make room
	TOS = TOR;
    goto next;

rpz:
    // rp0( -- n) get  pointer to bottom of return stack
    DEC(SP);		// make room
    TOS = R0;
    goto next;

rpfetch:
    // rp@ ( -- n)  get return stack pointer
    DEC(SP);		// make room
    TOS = RP;
    goto next;

rpstore:
    // rp! ( n -- )
    RP  = TOS;
    INC(SP);		// pop stack
    goto next;

// ------------------------------- param stack operations --------------------
swap:
    // swap ( n m -- m n )
    X = TOS;
    TOS = NOS;
    NOS = X;
    goto next;

drop:
    // drop ( n -- )
    INC(SP);
    goto next;

dup:
    // dup ( n -- n n)
    DEC(SP);
    TOS = NOS;
    goto next;

qdup:
    // ?dup ( n -- n n | 0 )   Duplicate only if non zero
    if(TOS !=0)
    { DEC(SP); TOS = NOS; }
    else
    { TOS = 0; }
    goto next;

spz:
	// sp0 ( -- n) get  pointer to bottom of param stack
    DEC(SP);
    TOS = S0;
    goto next;

nip:
    // nip ( n m -- m)
    NOS = TOS;
    INC(SP);
    goto next;

spfetch:
    // sp@ ( -- n)  put stack pointer on stack
    adr = SP;		// backup SP
    DEC(SP);		// make room
    TOS = adr;
    goto next;

spstore:
    // sp! ( n -- )
    SP  = TOS;
    goto next;

over:
	// over ( n1 n2 -- n1 n2 n1 )
    X = NOS;	// save n1
    DEC(SP);	// make room
    TOS = X;
    goto next;

rot:
    // rot ( n1 n2 n3 -- n2 n3 n1 )
    i = TOS;	// i=n3
    INC(SP);	// pop stack
    j = NOS;	// j = n1
    NOS = TOS;
    TOS = i;
    DEC(SP);	// push stack
    TOS = j;
    goto next;

// ---------------------------------- IO--------------------------------------

qrx:
    // (?rx)  ( -- c T | F ) returns TRUE if char avail, FALSE otherwise
    DEC(SP);	// make room for flag
    if(kbhit()==0)
    {
	TOS = FFALSE;
	goto next;
    }
    DEC(SP);	// room for char
    NOS = getCharacter();
    TOS = TTRUE;
    goto next;


txstore:
    // (tx!) ( c -- ) send character to output device
    putCharacter(TOS);
    INC(SP);
    goto next;

// ------------------------------ arithmetic ---------------------------------
add:
     // + ( n n -- n ) Add TOS to NOS
    NOS = NOS + TOS;
    INC(SP);
    goto next;

sub:
    // - ( n n -- n ) Subtract TOS from NOS
    NOS = NOS - TOS;
    INC(SP);
    goto next;

uadd:
     // U+ ( n n -- n ) unsigned Add TOS to NOS
    UNOS = UNOS + UTOS;
    INC(SP);
    goto next;

usub:
    // U- ( n n -- n ) unsigned Subtract TOS from NOS
    UNOS = UNOS - UTOS;
    INC(SP);
    goto next;

mul:
	 // * ( n n -- n ) Multiply TOS and NOS
    NOS = NOS * TOS;
    INC(SP);
    goto next;

div:
    // / ( n n -- n )Divide NOS by TOS
    NOS = NOS / TOS;
    INC(SP);
    goto next;

umul:
	 // U* ( n n -- n ) unsigned Multiply TOS and NOS
    UNOS = UNOS * UTOS;
    INC(SP);
    goto next;

udiv:
    // U/ ( n n -- n ) unsigned Divide NOS by TOS
    UNOS = UNOS / UTOS;
    INC(SP);
    goto next;

muldiv:
    //  */ ( n1 n2 n3 -- n4 ) mutiply and divide (scale function)
    // (n1 x n2) /n3 long intermediate
    X = TOS;
    INC(SP);		// pop stack
    NOS = (INT) ((long)(TOS * NOS)/X);
    INC(SP);
    goto next;

umuldiv:
    //  U*/ ( n1 n2 n3 -- n4 ) unsigned mutiply and divide (scale function)
    // (n1 x n2) /n3 long intermediate
    X = UTOS;
    INC(SP);		// pop stack
    UNOS = (UINT) ((unsigned long)(UTOS * UNOS)/X);
    INC(SP);
    goto next;

twomul:
    // 2* ( n -- n )Mutiply TOS x 2
    TOS = TOS << 1;
    goto next;

twodiv:
    // 2/ ( n -- n ) Divide TOS by 2
    TOS = TOS >> 1;
    goto next;

lshift:
    // LSHIFT ( n u -- n ) shift NOS left by TOS
    NOS = NOS << TOS;
    INC(SP);
    goto next;

rshift:
    // RSHIFT ( n u -- n ) shift NOS right by TOS
    NOS = NOS >> TOS;
    INC(SP);
    goto next;

zero:
    // 0 ( -- 0 ) leave constant zero on stack
    DEC(SP);
    TOS = 0;
    goto next;

one:
    // ( -- 1 ) leave constant 1 on stack
    DEC(SP);
    TOS = 1;
    goto next;

two:
    // ( -- 2 ) leave constant 2 on stack
    DEC(SP);
    TOS = 2;
    goto next;

three:
    // ( -- 3 ) leave constant 3 on stack
    DEC(SP);
    TOS = 3;
    goto next;


none:
    // ( -- -1 ) leave constant -1 on stack
    DEC(SP);
    TOS = -1;
    goto next;

ntwo:
    // ( -- -2 ) leave constant -2 on stack
    DEC(SP);
    TOS = -2;
    goto next;

nthree:
    // ( -- -3 ) leave constant -3 on stack
    DEC(SP);
    TOS = -3;
    goto next;

invert:
    // INVERT ( n -- n) 1's complement of TOS
    TOS = TOS ^ -1;
    goto next;

negate:
    // NEGATE ( n -- n ) 2's complement of TOS
    TOS = (TOS ^ -1) + 1;
    goto next;

mod:
	 // mod ( n n -- n ) signed divide NOS by TOS
    NOS = NOS % TOS;
    INC(SP);
    goto next;

umod:
	 // umod ( n n -- n ) unsigned divide NOS by TOS
    UNOS = UNOS % UTOS;
    INC(SP);
    goto next;

// --------------------------------- logical----------------------------------

truee:
    // TRUE ( -- n ) returns TRUE
    DEC(SP) ;		// make room
    TOS = TTRUE;
    goto next;

falsee:
    // FALSE( -- n ) returns FALSE
    DEC(SP) ;		// make room
    TOS = FFALSE;
    goto next;

eq:
    // = ( n n -- n ) Returns true if equal
    NOS = (TOS == NOS) ? TTRUE : FFALSE ;
    INC(SP) ;
    goto next;

zeq:
    // 0= ( n -- n ) Returns true if TOS = 0
    TOS = (TOS == 0) ? TTRUE : FFALSE ;
    goto next;

lt:
    // < ( n n -- n ) If NOS < TOS return TRUE
    NOS = (NOS < TOS) ? TTRUE : FFALSE ;
    INC(SP) ;
    goto next;

ult:
    // U< ( n n -- n )If unsigned NOS < TOS return TRUE
    NOS = (UNOS < UTOS) ? TTRUE : FFALSE ;
    INC(SP) ;
    goto next;
zlt:
    // 0< ( n -- n )If TOS < 0 return TRUE
    TOS = (TOS < 0) ? TTRUE : FFALSE ;
    goto next;

gt:
   // > ( n n -- n )If NOS > TOS return TRUE
   NOS = (NOS > TOS) ? TTRUE : FFALSE ;
   INC(SP) ;
    goto next;

within:
    //  WITHIN ( n l h  -- f ) TRUE if TOS within bounds
    X = TOS;		// save TOS
    INC(SP);		// pop
    NOS = (NOS >= TOS && NOS < X) ? TTRUE : FFALSE ;
    INC(SP);
    goto next;

max:
    // MAX (n n -- n) return maximum
    NOS = (NOS > TOS) ? NOS : TOS ;
    INC(SP);
    goto next;

min:
    // MIN (n n -- n) return minimum
    NOS = (NOS < TOS) ? NOS : TOS ;
    INC(SP);
    goto next;

and:
    // AND ( n n -- n ) and TOS with NOS
     NOS = NOS & TOS;
     INC(SP);
    goto next;

or:
    // OR ( n n -- n ) or TOS and NOS
    NOS = NOS | TOS;
    INC(SP);
    goto next;

xor:
    // XOR ( n n -- n ) xor TOS and NOS
    NOS = NOS ^ TOS;
    INC(SP);
    goto next;

not:
    // NOT ( n -- n )
    TOS = !TOS;
    goto next;

// --------------------------------- dict search -------------------------------

nameq:
    // NAME?(a -- ca na | a F ) Find counted string at 'a' and return cfa and nfa,
    //  or false.
    DEC(SP);		// make room on stack
    TOS = FFALSE;	// predict false ( -- a F )
    head = (HEADS *)(vfrth-2*CELL);		// start at end of dictionary, work back
    while(head->lfa |= 0)
    {
	badr = NOS;
	count = *badr++ & MASK;	// get count byte, mask lex bits, skip count
	if(count != (head->count & MASK))	// bail if count not equal
	     goto name1;
	for(j=0;j<count;j++)
	{
	    if(*badr++ != head->name[j])	// check each character
		goto name1;						// bail, not equal!
	}
	// Found name!
	NOS = (head->cfa);
	TOS = (INT)&head->count;	// return address of count byte
	goto next;
name1:	head = head->lfa - 2*CELL; // next entry
	}
	goto next;

// ------------------------- single index loop primitive ---------------------
donext:
	// doNEXT( -- ) Run time code for the single index loop.
	TOR -= 1;	// dec index
	if(TOR >= 0 )	//loop
	{
	    IP = *(PTR)IP;
	    goto next;
	}
	INC(RP);	// index < 0
	INC(IP);
	goto next;


    // NOTREACHED
    return 0;

} // end of main
