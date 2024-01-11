/* -------------------------------------------------------------------- */
/* mPadDCM module. Generated first on Thu Aug 20 12:51:47 EDT 1998	*/
/* J.Lauret								*/
/* REvision history at the end						*/
/* -------------------------------------------------------------------- */



#include <mPadDCM.h>
#include <PadFEM.h>
#include <emlLib.h>

#include <stdlib.h>

/* JTM mod 10-21-98 -> pio is not ready for this, so placing it here for now */
#define IDPC_DCM0 0
#define IDPC_DCM1 1
/* end JTM mod */

/* J.Lauret 17-Dec-1999 . Same remark as above 	  */
#define IDPC_DCM2 2	/* Who knows what that is */
#define IDPC_DCM3 3


/* Constants definitions						*/

/* Macros definitions							*/

/* Routine declaration							*/

/* Global variable and/or structures					*/



/* -------------------------------------------------------------------- */
/* Module section      							*/
/* -------------------------------------------------------------------- */

long mPadDCM_(
  TABLE_HEAD_ST        *dPadFEM_h,        DPADFEM_ST          *dPadFEM	,
  TABLE_HEAD_ST     *dPadDCMPar_h,     DPADDCMPAR_ST       *dPadDCMPar	,
  TABLE_HEAD_ST        *dPadDCM_h,        DPADDCM_ST          *dPadDCM )
{

int	i,j,scheme							;
int	pos								;
unsigned long	addr								;
FILE	*fo								;


  /* ------------------------------------------------------------------ */
  /* Pre-test scheme value						*/
  /* No need to go further if it is incorrect				*/
  /* ------------------------------------------------------------------ */
  scheme= dPadDCMPar[0].scheme						;
  if(dPadDCMPar[0].debug != 0){
    printf("mPadDCM :: debug level = %d scheme=%d (%s)\n",dPadDCMPar[0].debug,scheme,(scheme==IDPC_DCM0?"PassThrough":"ZeroSuppress"));
  }

  switch (scheme)
  {
    case IDPC_DCM3:
    case IDPC_DCM0:
      if(dPadDCMPar[0].debug>1)
	printf("mPadDCM :: Mode is pass-through (scheme=%d)\n",scheme)	;
      break								;
    
    case IDPC_DCM1:
      if(dPadDCMPar[0].debug>1)
	printf("mPadDCM :: Mode is zero supression (scheme=%d)\n ",scheme);
      break								;
    
    default:
      if(dPadDCMPar[0].debug>1)
	printf("mPadDCM :: Do not know what to do for scheme=%d\n",scheme);
      return STAFCV_BAD						        ;
      break								;
  }


  /* ------------------------------------------------------------------ */
  /* Debugging FILE * open() statement					*/
  /* We will do it simple this time					*/
  /* Reminder : output file used for debug level > 2 			*/
  /* ------------------------------------------------------------------ */
  if(dPadDCMPar[0].fout == 1){
    char	fname[80]						;

    sprintf(fname,"paddcm_%d.tmp",dPadDCMPar[0].idx)			;
    fo = fopen(fname,"w")						;
    if(fo == NULL){
      if(dPadDCMPar[0].debug>0)
	printf("mPadDCM :: could not open %s\n",fname)			;
      fo	= stdout						;
    } else {
      if(dPadDCMPar[0].debug>0)
	printf("mPadDCM :: some info in %s\n",fname)			;
    }
  } else {
    fo	= stdout							;
  }




  /* ------------------------------------------------------------------ */
  /* Filling ...							*/
  /* Common parts are the header and trailer words. Get rid of that now	*/
  /* Note that the 32 bits words contains extra info at bit 20, 28 and 31*/
  /* Bit shits below follows Nagle and Chi document from 4-22-98	*/
  /* ------------------------------------------------------------------ */
  for(i=0; i < dPadFEM_h->nok ; i++){
    /* Note : there are no CAV1 (000FFFFF) word to mark DCM begin	*/
    /* Not sure this is what's expected for Bcounter			*/
    dPadDCM[i].Word[0]	= dPadFEM[i].det	| BITS(31,1) | BITS(20,0);
    dPadDCM[i].Word[1]	= dPadFEM[i].Ecounter	| BITS(31,1) | BITS(20,1);
    dPadDCM[i].Word[2]	= dPadFEM[i].adr	| BITS(31,1) | BITS(20,2);
    dPadDCM[i].Word[3]	= dPadFEM[i].Flag	| BITS(31,1) | BITS(20,3);
    dPadDCM[i].Word[4]	= dPadFEM[i].Bcounter	| BITS(31,1) | BITS(20,4);

    pos			= DCMHEADER-1					;


    /* Data words : has bit 31 at 0					*/
    /* It is better to do the test once and then loop over MAXNUMWORDS	*/
    /* than to test MAXNUMWORDS times the switch statement. Also, would	*/
    /* be nce to move the statement outside the for(i;;) loop but for	*/
    /* readability, I think it will be fine here for now ...		*/

    /* CFM November 29, 2001: initialize the value of j as a safety     */
    j = -1;

    switch (scheme)
    {
     case IDPC_DCM0:
       /* Pass through mode. That's easy				*/
       /* The 32nd bit is at 0, the 20th one (and next) counts words	*/
       for(j=0; j < MAXNUMWORDS; j++){
	 dPadDCM[i].Word[pos]	= dPadFEM[i].Word[j] | BITS(20,j+1)	;

	 /* The next block is just for debugging purposes		*/
	 if( (dPadDCMPar[0].debug > 3)  ){
	   /* Thu Aug 27 98 -- We know this works fine.			*/
	   if( dPadDCMPar[0].debug > 4 ){
	     addr = dPadFEM[i].Word[j]					;
	     fprintf(fo,"mPadDCM :: FEM word (32 bits mode) ")		;
	     PadPrintb(fo,&addr,32,1)					;
	   }

	   addr = dPadDCM[i].Word[pos]					;
	   fprintf(fo,"mPadDCM :: DCM word %3d            ",j)		;
	   PadPrintb(fo,&addr,32,1)					;
	 }

	 /* Position on next word					*/
	 pos++								;
       }
       break								;


     case IDPC_DCM1:
       /* See bottom for details					*/
       printf("mPadDCM :: Zero supress mode not yet implemented ...\n") ;
       break								;


     default:
       printf("mPadDCM :: We should NOT have reached this part of the code\n");
       printf("           Critical logic error !! Aborting NOW\n")	;
       abort()								;
       break								;

    }

    /* In pass-though mode, we'd better find pos = num-of-header+108 = 113 */
    
    /* CFM November 29, 2001: the j below originally was possibly uninitialized ?? */
    if(dPadDCMPar[0].debug > 3 && j!=-1){
      fprintf(fo,"mPadDCM :: Now at position %d, num-header+words=%d"	,
	      pos,DCMHEADER+j-1)					;
      fprintf(fo,(pos==DCMHEADER+j-1)?" -- Checks out OK\n":" -- ARGHHHH !!! Bug !\n");
    }


    /* Get back to index position where we are				*/
    j	= pos								;
    /* User words							*/
    dPadDCM[i].Word[j++]= dPadFEM[i].usr1	| BITS(31,1) | BITS(20,14) | BITS(28,1);
    dPadDCM[i].Word[j++]= dPadFEM[i].usr2	| BITS(31,1) | BITS(20,14) | BITS(28,2);
    dPadDCM[i].Word[j++]= dPadFEM[i].usr3	| BITS(31,1) | BITS(20,14) | BITS(28,3);
    dPadDCM[i].Word[j++]= dPadFEM[i].usr4	| BITS(31,1) | BITS(20,14) | BITS(28,4);
    dPadDCM[i].Word[j++]= dPadFEM[i].usr5	| BITS(31,1) | BITS(20,14) | BITS(28,5);
    dPadDCM[i].Word[j++]= dPadFEM[i].usr6	| BITS(31,1) | BITS(20,14) | BITS(28,6);
    dPadDCM[i].Word[j++]= dPadFEM[i].usr7	| BITS(31,1) | BITS(20,14) | BITS(28,7);
    dPadDCM[i].Word[j++]= dPadFEM[i].usr8	| BITS(31,1) | BITS(20,14) | BITS(28,8);

    /* Parity and LASTWORD now						*/
    /* Although we may use the LASTWORD #define(d), we will copy CAV2 	*/
    /* and test in the Unpacker that we really got LASTWORD in there	*/
    dPadDCM[i].Word[j++]= dPadFEM[i].parity	| BITS(31,1) | BITS(20,15) | BITS(28,1);
    dPadDCM[i].Word[j++]= dPadFEM[i].CAV2	| BITS(31,1) | BITS(20,15) | BITS(28,2);

    dPadDCM_h->nok++							;
  }



  /* ------------------------------------------------------------------ */
  /* Done. Close file if needed, return STAFV_OK			*/
  /* ------------------------------------------------------------------ */
  if(fo != stdout) fclose(fo)						;
  return STAFCV_OK							;
}










/*
 We did declare a structure containing a unique array of maximum
 108+5+8+2= 123 words of 32 bits length. It does not seem to me that
 this choice makes any differences. The unpacker algo has to count
 on bit 31 going back to 1 (while 0 on data words). But, the fact is
 that the real world will assume a variable length array with the CAV2
 word at the end as the recognition that the DCM data is done ...

 Previous version did declare the following :

  unsigned long det;               Detector #
  unsigned long Ecounter;          Event counter
  unsigned long adr;               Module address
  unsigned long Flag;              Flags
  unsigned long clkC;              CLK count (clock count?)

     Maximum words in pass-through mode is 108 as in FEM
     Zero suppression (or noise supression) will save less
  unsigned long Word[108];         DCM words, 32 bits 31st bit=0


     Strangely enough, the DCM table looks like FEM at the end
     We copied/pasted the next block from dPadFEM.idl, changing
     long into unsigned long
  unsigned long usr1;            1st user word loaded from ArcNet
  unsigned long usr2;            2nd user word loaded from ArcNet
  unsigned long usr3;            3rd user word - DC FEM status word
  unsigned long usr4;            4th user word - all 0's
  unsigned long usr5							;
  unsigned long usr6							;
  unsigned long usr7							;
  unsigned long usr8							;
  unsigned long parity;          Parity word
  unsigned long CAV2;            End of transmission (00000000)
*/

#if 0
       /* In Zero supression mode, the data packet is NOT of fixed	*/
       /* length. Some words are eliminated (based on ORing the closest	*/
       /* words), header and trailer remains the same ...		*/
       /* There are 3 types of words : The 0(3) 1(3) and 2(3)		*/
       /* Anders calls them type "A" "B" and "C" and explains 		*/
       /* the algo which needs to be applied in his document  "Notes 	*/
       /* about Zero supression of data from the Pad Chambers"		*/
       /* (Doc from June 8th 1998)					*/

       /* It should follow a core like ...				*/
       for(j=0; j < MAXNUMWORDS; j++){
	 /* Get neighbour word and main word from the same word		*/
	 nwrd = mwrd = dPadFEM[i].Word[j]				;
	 /* Simple way to test type "A" "B" or "C"			*/
	 switch (j%3){
	 case 0:
	   break							;
	 case 1:
	   break							;
	 case 2:
	   break							;
	 }

       }
#endif
