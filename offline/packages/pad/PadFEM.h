
/* -------------------------------------------------------------------- */
/* General purpose macros and declaration for PadFEM module 		*/
/* -------------------------------------------------------------------- */


#ifndef PADFEM_H
#define PADFEM_H

/* System include files we need						*/
#include <stdio.h>


/* Constant definitions */
/* Note :: PADID is DC detector Id. Please, fill in proper value	*/
/* and supress this comment ..						*/

#define BASEADDR    0xA0000	/* Base adress for ModuleAdress (Julia) */
#define MAXNUMFEM   32		/* Maximum number of FEM for one Pc	*/
#define MAXNUMDWRD  108		/* Number of data words			*/
#define MAXNUMWORDS 117		/* Number of words in 1 FEM card	*/
#define XFREQ	    12		/* Xtra word frequency (one more every)	*/
#define WORDLEN     20		/* Bit Length of a word			*/
#define PADID       0x400	/* As convention			*/
#define PADFLAG     0xFF333	/* UserWord loaded from ArcNet		*/
#define NUMPCDET    3		/* Number of PadChambers		*/
#define DCMHEADER   6		/* Number of DCM headers		*/
#define DCMTRAILER  10		/* Number of DCM trailer words		*/
#define LASTWORD    0x0		/* Last FEM/DCM word			*/

#define	PADUNPACK   1		/* Unpack tag				*/
#define PADPACK	    0		/* Pack tag (FEM module)		*/


#define MAXNENTRIES 50000	/* Maximum expected num entries in PadRaw */
#define SKIPGEANT   (dPadFEMPar[0].skipg == 1)
#define DOGEANT     (dPadFEMPar[0].skipg == 0)




/* Macro (re)definitions 						*/
#ifdef MAX
#undef MAX
#endif
#ifdef MIN
#undef MIN
#endif

#define MAX(x,y)	( x>y?x:y )
#define MIN(x,y)	( x>y?y:x )
#define SUBSECTOR(s,n)	(n==0?s*2:s)
#define BITS(b,v)	( (v) << (b) )	/* Avoid side effects		*/
#define RBIT(b,n,v)	(  ((v) & BITS(b,n)) >> b   )
                                        /* Return specific bit b from value v */

#ifndef TRUE
#define TRUE (1==1)
#endif
#ifndef FALSE
#define FALSE (!(TRUE))
#endif

/* I have already try type casting (unsigned long). See mPadUnpack	*/
/* for bug description on Osf1/cc. 28-JAN-1999				*/
#define LASTDCMWORD (LASTWORD	| BITS(31,1) | BITS(20,15) | BITS(28,2))







/* This one is for the offset calculation. David says that the final	*/
/* conventions are :                                                    */
/*                                                                      */
/* West arm				Arm-Side                        */
/* ------                                                               */
/* North side				01                              */
/* FEM 0: 0 <= padx <= 19                                               */
/* FEM 1: 20 <= padx <= 39                                              */
/*                                                                      */
/* South side				00                              */
/* FEM 0: 20 <= padx <= 39                                              */
/* FEM 1: 0 <= padx <= 19                                               */
/*                                                                      */
/* East arm                                                             */
/* ------                                                               */
/* North Side				11                              */
/* FEM 0: 20 <= padx <= 39                                              */
/* FEM 1: 0 <= padx <= 19                                               */
/*                                                                      */
/* South Side                                                           */
/* FEM 0: 0 <= padx <= 19		10                      	*/
/* FEM1: 20 <= padx <= 39                                               */
/*                                                                      */
/* there is an offset swap at arm == side.                              */
/* last addition : because Pc2 is mounted backward, the above is true	*/
/* for Pc3 only ... For PC2, the test would be arm != side		*/
/*                                                                      */
/*                                                                      */
/* Even if those macro are rarely used, I prefer to group them here     */
/* for easier correction (in case :) ).                                 */
/*                                                                      */
/* This leads to FEMOFFSET() which gives the FEMnum offset comparing to	*/
/* based numbering. To be considered as sub-sectors numbering (easer 0 	*/
/* or 1) and has nothing to do with the padx numbering itself.		*/
/*                                                                      */
/* SCALEPADX() is always right.                                       	*/
/*                                                                      */
/* I can make                                                           */
/* - FEMOFFSET() with only x?a:b branching                              */
/* - DSCALEPX() even more compact and faster                            */
/* but this would reduce readability. I'll have mercy ... 		*/
/* Code below variable recycle ...                                      */
/*                                                                      */

/* True always */
#define SCALEPADX(px)        ( (px) >= WORDLEN ? 1:0)

/* Fri Jun 23 10:37:55 EDT 2000 : Data seems to disagree with arm number*/
/* but only for Pc1 (Pc3 is apparently correct)                         */
#define FIXARM(a)            ( PadFEM_g.pcnum==0?(arm==0?1:0):arm)

/* BTW : This means that the layed out convention and lengthy discussion*/
/* we had a year ago about changing this code was both erroneous and    */
/* missleading. Only the extra Melissa's words needed to be applied ... */
/* Waht a waste of time !!!                                             */



/* Function type declaration 						*/
/* We will fix the missing/unspecified type cast later ...		*/
short	PadFEM_gimexy(int *, int *, int *, int *, int *, int *)		;
short	PadUNP_padxy(int *, int *,int *, int *, int *, int *, int *)	;

int	PadFEM_FEMnum(int *, int *, int *, int *)			;
void	PadFEM_DecodeFEMnum(int *, int *, int *, int *, int *);

int	PadFEM_ADR2FEE(unsigned long *)						;
long	PadFEM_gimeaddr(int *, int *, int *)				;
long	PadFEM_FEE2ADR(int *)						;

void	PadFEMinit();
void	PadFEMprintb(unsigned long *,int,int)				;
void	PadPrintb(FILE *,unsigned long *,int,int)			;


/*
   The  intent  of  the  next  function  was  to  use it within a "return
   padFEMfree(status)" instead of "return status".  Don't ask me why  but
   even  with  all type being correct Staf does not like it on ...  (wild
   guess) Linux platform !!!

long	PadFEMfree(long)						;

  So, we replaced it by a void and now use 2 instructions ...
*/
void	PadFEMfree();





/* Global variables and/or structures					*/
struct mPadFEMparam {
  FILE	*fo		; /* Some output to a file may be performed	*/
  short debug		; /* Copied debug level from dPadFEMPar		*/
  short	mode		; /* Unpack or pack				*/
  int	pcnum		; /* Pc number 0,1,2				*/
  int	rpcnm		; /* RealPc number or pcname 1,2,3		*/
  int	numsect		; /* Number of sectors per arm			*/
  int	nsubsect	; /* Number of sub-sectors per arm		*/
  int	fempaps		; /* Number of fem per side per arm		*/

  int	totfem		; /* Total number of FEM cards			*/
};


/*
struct mPadFEMparam *PadFEM_p						;
*/

/* Whenever malloc() will work on Linux platform, please change 	*/
/* PadFEM_g.xxx to PadFEM_p->xxx and comment the next line. malloc()	*/
/* free() instructions are still in place in PadFEM.c			*/
/* Understand the "_" as p=pointer g=global (unfortunatly...)		*/

struct mPadFEMparam PadFEM_g						;



#endif


