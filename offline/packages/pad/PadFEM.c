/* General purpose PadFEM module functions 				*/
#include <mPadFEM.h>
#include <PadFEM.h>

#include <stdlib.h>




/* -------------------------------------------------------------------- */
/*  get x,y from padx,padz                                              */
/* -------------------------------------------------------------------- */
short PadFEM_gimexy(int *padx, int *padz, int *arm, int *side, int *x, int *y)
{

  /* Small issue : because Word 107 is in fact padz=0 and because	*/
  /* padx=0 is the MSB, there is an extra inversion we need to perform	*/
  /* comparing to preceding version ...					*/

#if 1
  /* Melissa has added one word every 12 starting at word 0 so		*/
  /* translation between padz and y is no longer 1 to 1 ...		*/
  *y	= (MAXNUMDWRD - 1) - *padz					;
  *y	= (*y) + (int) (*y)/XFREQ + 1					;
#else
  /* Xie Wei changed his SlowSim to produce result in Pad Space		*/
  *y	= *padz							        ;
#endif



  if(PadFEM_g.pcnum != 0){
    *x    = *padx - SCALEPADX(*padx)*WORDLEN				;
  } else {
    *x    = *padx							;
  }
  /* padx=0 is MSB 							*/
  *x	=  (WORDLEN - 1) - *x						;



  /* Be sure not to return something funky ::
     0 --> WORDLEN bits 	(currently 20 values/rows)
     0 --> MAXNUMWORDS words	(currently 117)				*/

  if(*x >= WORDLEN || *y >= MAXNUMWORDS || *x <0 || *y < 0){
    printf(" PadFEM :: PadUnp_gimexy : [x=%3d,y=%3d] (Max=[%3d,%3d]) ???\n",
	   *x,*y,WORDLEN,MAXNUMWORDS)					;
    return FALSE							;
  }

  return TRUE								;
}



/* -------------------------------------------------------------------- */
/* This is supposed to return the lowest number in the 3 bit grouping	*/
/* corresponding to the backward transformation x,y -> padx,padz	*/
/* -------------------------------------------------------------------- */
short PadUNP_padxy(int *padx, int *padz				        ,
                   int *FEMnum, int *arm, int *side			,
		   int *x, int *y)
{
  int offset								;

  /* Sanity check							*/
  if(*x >= WORDLEN || *y >= MAXNUMWORDS || *x <0 || *y < 0){
    printf(" PadFEM :: PadUnp_padxy : [x=%3d,y=%3d] (Max=[%3d,%3d]) ???\n",
	   *x,*y,WORDLEN,MAXNUMWORDS)					;
    return FALSE							;
  }


#if 1
  /* See comment above							*/
  *padz	= *y - 1 - (int) (*y)/(XFREQ+1)				        ;
  *padz = (MAXNUMDWRD -1) - *padz					;

  if(PadFEM_g.pcnum != 0){
    /* i.e. Pc2 and Pc3                                                 */
    /* Changed on Fri Jun 23 2000 pcnum == to pcnum != ... Initial      */
    /* information at last code modification was erroneous.             */
    offset = (PadFEM_g.pcnum != 2)?
      ((((*arm) == (*side)) && ((*FEMnum)%2 == 0)) || 
       (((*arm) != (*side)) && ((*FEMnum)%2 != 0))):
      ((((*arm) != (*side)) && ((*FEMnum)%2 == 0)) || 
       (((*arm) == (*side)) && ((*FEMnum)%2 != 0)));
    if( offset ){
      offset = 1;
    } else {
      offset = 0;
    }
  } else {
    offset = 0								;
  }

  *padx = (WORDLEN - 1) - *x						;
  *padx = *padx + offset*WORDLEN					;

#else
  *padz	= *y								;
  *padx	= (*x)
#endif


  return TRUE								;
}






/* -------------------------------------------------------------------- */
/* Returns one of the FEM number. At maximum 8*2*2. Using param 	*/
/* fempaps (fem per arm per side) 		        		*/
/* Late addition had to include padx because there is a factor 2 in	*/
/* that directrion for Pc2/3						*/
/* -------------------------------------------------------------------- */
int PadFEM_FEMnum(int *arm,int *sector,int *side,int *padx)
{
  int	retval								;
  int	offset								;

  /*     (*sector)+(*side)*(*fempaps)					*/
  /*     ^____________________________^ unique number for arm		*/
  /*     += (*arm)*(*fempaps)*2						*/
  /*        ^_________________________  arm w/e 0/1			*/

  /* For Pc2/3, we have an offset because there are 2 FEM per sector	*/
  /* on each side ...							*/
  if(PadFEM_g.pcnum == 0){
    retval = (*sector)+(*side)*(PadFEM_g.fempaps)+
      (*arm)*(PadFEM_g.fempaps)*2;
  } else {
    /* See PadUNP_padxy for note on logic inversion == 2 -> != 2        */
    offset = (PadFEM_g.pcnum != 2)?((*arm)==(*side)):((*arm)!=(*side))  ;
    if( offset ){
      offset = ((*padx) >= WORDLEN ? 0:1)                               ;
    } else {
      offset = ((*padx) >= WORDLEN ? 1:0)                               ;
    }
    retval = (*sector*2+offset)+(*side)*(PadFEM_g.fempaps)+
      (*arm)*(PadFEM_g.fempaps)*2;
  }


  /* Check will be done here now					*/
  if( retval >= MAXNUMFEM || retval < 0){
    if(PadFEM_g.debug > 0){
      printf(" PadFEM :: PadFEM_FEMnum : \n")				;
      printf("           Internal logic error or parameter inconsistency\n");
      printf("           indexing arm=%d sector=%d side=%d padx=%d\n"	,
	                 *arm,*sector,*side,*padx)			;
      printf("           leads to value %d (Max=%d) ????\n",retval,MAXNUMFEM);
    }
    retval = MAXNUMFEM - 1						;
  }

  return retval							        ;
}


/* -------------------------------------------------------------------- */
/* Debugging purposes. Decode back the FEMnumber.                       */
/* -------------------------------------------------------------------- */
void	PadFEM_DecodeFEMnum(int *feen,int *arm,int *side,int *sector,int *FEMoffset)
{
  int	tmp								;

  /* Decode arm/side/sector from ROCnumber				*/
  *arm	= *feen/(PadFEM_g.fempaps*2)					;
  tmp	= *feen - (*arm)*(PadFEM_g.fempaps*2)				;
  *side	= tmp/(PadFEM_g.fempaps)					;
  *sector= tmp - (*side)*(PadFEM_g.fempaps)				;



  /* the FEM number was modified to reflect the 2 FEM/sector in Pc2/3	*/
  /* Real sector number for those is the above value/2			*/
  if(PadFEM_g.pcnum != 0){
    *FEMoffset	= *sector % 2						;
    *sector	/= 2							;
  } else {
    *FEMoffset	= 0							;
  }
  return								;
}



/* -------------------------------------------------------------------- */
/* This is definitly going to return a 20 bit long number and is an	*/
/* arbitrary coding choice.						*/
/* May be a macro later in a definit stage				*/
/* -------------------------------------------------------------------- */
long PadFEM_gimeaddr(int *arm,int *sector,int *side)
{
  return BASEADDR | (*arm)*0x1000 | (*side)*0x100 |  (*sector)*0x01;
}




/* ==================================================================== */
/* The 2 next functions are "artificial" i.e. we planned the module	*/
/* Address as a coding of arm/side/sector and provide a way to go	*/
/* from one to another							*/
/* ==================================================================== */

/* -------------------------------------------------------------------- */
/* Transform FEE number to Module Address				*/
/* -------------------------------------------------------------------- */
long PadFEM_FEE2ADR(int *feen)
{
  int	arm								;
  int	side								;
  int	sector								;
  int	offset								;


  PadFEM_DecodeFEMnum(feen,&arm,&side,&sector,&offset)			;

  if(PadFEM_g.debug > 3)
    fprintf(PadFEM_g.fo,"feen=%2d -> arm=%d side=%d sector=%d offset=%d\n",
	    *feen,arm,side,sector,offset)				;

  return PadFEM_gimeaddr(&arm,&sector,&side)				;
}



/* -------------------------------------------------------------------- */
/* Transform Module Address to FEE number				*/
/* THIS ROUTINE IS __NOT__ the oposite transform of the above one	*/
/* because the sector coded in gimeaddr() is the real sector number	*/
/* while we offset the FEM number for Pc2/3 (see gimexy() and above 	*/
/* FEE2ADR()								*/
/* Consistency/debugging routine which should not have any usage in	*/
/* final version							*/
/* -------------------------------------------------------------------- */
int PadFEM_ADR2FEE(unsigned long *adr)
{
  int	arm								;
  int	side								;
  int	sector								;
  int	padx=0								;

  /* sector is coded up to 16=0x10, *0x01 -> 0x10 maximum      		*/
  /* We need to bitmask it with 111=0x07 , others need only one bit mask*/
  /* Initial idea was to code with 0x10 but would overlap with side	*/
  /* With such coding, sector does not need to be shifted		*/
  arm	= ((int) *adr & 0x1000) >> 12					;
  side	= ((int) *adr & 0x100)  >> 8					;
  sector= ((int) *adr & 0x07)						;

  /* Debugged and fine							*/
  if(PadFEM_g.debug > 3)
    fprintf(PadFEM_g.fo,"adr=%lx -> arm=%d side=%d sector=%d\n"		,
	    *adr,arm,side,sector)					;

  /* WARNING :: padx is not assigned here 				*/
  /* We will NOT be able to recover the proper num (in principle) BUT	*/
  /* since we work it out modulo 2 close, we will be back on our feet	*/
  return PadFEM_FEMnum(&arm,&sector,&side,&padx)			;
}



/* ==================================================================== */
/* Print number in binary format of len bits				*/
/* ==================================================================== */


/* -------------------------------------------------------------------- */
/* WARNING :: Carefull with the type. We will force it in .h		*/
/* Aug 20  :: make this routine usable by other module not having an	*/
/* access to PadFEM_p pointer because we need it for DCM debugging	*/
/* Note : we use unsigned long args for addr but we can pass a shorter	*/
/* int(kind=x) variable since the unsigned long is the biggest possible.*/
/* Note also that newl > 32 would be stupid but we did not add any test	*/
/* for it ... developpers are supposed to know about it ...		*/
/* -------------------------------------------------------------------- */
void	PadFEMprintb(unsigned long *addr,int len,int newl)
{
  PadPrintb(PadFEM_g.fo,addr,len,newl)					;
  return								;
}

/* Above is interface routine, this is the real one			*/
void	PadPrintb(FILE *fo,unsigned long *addr,int len,int newl)
{
  int	i								;

  for( i=len-1 ; i >= 0 ; i--){
    fprintf(fo,"%lu", RBIT(i,0x1,*addr) )				;
  }
  if(newl == 1){
    fprintf(fo,"\n")							;
  } else {
    fprintf(fo," ")							;
  }
}



/* -------------------------------------------------------------------- */
/* Global structure initialization					*/
/* and cleanup								*/
/* -------------------------------------------------------------------- */
void	PadFEMinit(DPADGEOM_ST *dPadGeom,DPADFEMPAR_ST *dPadFEMPar)
{
  char	fname[80]							;

  /* Try to allocate memory, abort otherwise				*/
  /* Guess what ? Linux node does not like malloc()  (???) -- Workaround
     --> private static structure.
  PadFEM_p = (struct mPadFEMparam *) malloc(sizeof(struct mPadFEMparam));
  if(PadFEM_p == 0){
    printf(" PadFEM :: PadFEMinit() : Cannot allocate memory -- Abort\n");
    abort()								;
  }
  */


  PadFEM_g.debug	= dPadFEMPar[0].debug				;
  PadFEM_g.pcnum	= dPadFEMPar[0].pcnumber			;
  PadFEM_g.mode		= dPadFEMPar[0].mode				;


  /* This line looks stupid and useless BUT it IS NOT !!! 		*/
  /* See mPadUnpack for more information. 28-JAN-199 			*/
  dPadFEMPar[0].last	= LASTDCMWORD					;


  /* Allow user to forget this one					*/
  if( dPadFEMPar[0].skipg != 0 &&  dPadFEMPar[0].skipg != 1){
    dPadFEMPar[0].skipg = 0						;
  }


  /* Consistency check */
  if( PadFEM_g.pcnum >= NUMPCDET){
    printf("mPadFEM :: PadFEMinit() : Invalid pcnum=%d. MUST be < %d -- Abort\n",
	   PadFEM_g.pcnum,NUMPCDET)					;
    abort()								;
  }
  if( XFREQ <= 0 || XFREQ >= MAXNUMWORDS){
    printf("mPadFEM :: PadFEMinit() : Internal invalid XFREQ value -- Abort\n");
    abort()								;
  }

  PadFEM_g.rpcnm	= dPadFEMPar[0].pcnumber+1			;
  PadFEM_g.numsect	= dPadGeom[0].sectperarm[PadFEM_g.pcnum]	;


  /* There are twice the number of sectors sub-sectors for pc1		*/
  /* For Pc2/3, the number of sub-sectors equates the number of sectors	*/
  PadFEM_g.nsubsect	= PadFEM_g.numsect *((PadFEM_g.pcnum == 0) ? 2 : 1);


  /* fempaps reflects the number of FEM cards per arm per side. We will	*/
  /* loop over that number *4 due to symetrie and may remove one 	*/
  /* variable level later.						*/
  PadFEM_g.fempaps	= PadFEM_g.numsect* ((PadFEM_g.pcnum == 0) ? 1:2);
  PadFEM_g.totfem	= PadFEM_g.fempaps*4				;


  /* code file name with pcnum						*/
  if(PadFEM_g.mode == 0){
    sprintf(fname,"padfem_%d.tmp",PadFEM_g.pcnum)			;
  } else {
    sprintf(fname,"padunpack_%d.tmp",PadFEM_g.pcnum)			;
  }


  if(dPadFEMPar[0].fout == 1){
    PadFEM_g.fo	= fopen(fname,"w")					;
    if(PadFEM_g.fo == NULL){
      if(PadFEM_g.debug > 0)
	printf(" PadFEM :: PadFEMinit() : could not open %s\n",fname)	;
      PadFEM_g.fo	= stdout					;
    } else {
      if(PadFEM_g.debug > 0)
	printf(" PadFEM :: PadFEMinit() : some info in %s\n",fname)	;
    }
  } else {
    PadFEM_g.fo	= stdout						;
  }
}

/* -------------------------------------------------------------------- */
/* See coment in .h file ...                                            */
/* long	PadFEMfree(long status)                                         */
/* -------------------------------------------------------------------- */
void	PadFEMfree()
{
  /* free(PadFEM_p)							; */
  if(PadFEM_g.fo != stdout)	fclose(PadFEM_g.fo)			;
  return								;
}



