#ifndef __PLATFORM_H_
#define __PLATFORM_H_

//
//	This is the global platform header file, include your platform specific header file here.
//
//

// Uncomment if there is a Forth application ROM to run.
//#define ROM_PRESENT	

#ifdef ROM_PRESENT
// Place the start address of your forth ROM here.
#  define ROM	0xdeaddead	// start address of user rom header, see graspforth.c for structure
#endif


#define BIG		1
#define SMALL		0
// Define the endian'ess of your processor here
#define ENDIAN		SMALL				

// Put your target specific header in here and use -DLINUX, -DLPC etc in your gcc options to include it. 
#if defined(LEON)
#  include "leon.h"
#elif defined(LPC)
#  include "lpc.h"
#elif defined(CYGWIN)
#  include "linux.h"
#elif defined(LINUX)
#  include "linux.h"
#elif defined(WIN32)
#  include "win32.h"
#else
#  error "No platform defined"
#endif

#endif // __PLATFORM_H_
