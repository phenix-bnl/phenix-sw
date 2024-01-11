/* -------------------------------------------------------------------- */
/* mPadFEM.c Written J.Lauret Aug 1998					*/
/* All debug>2 may be removed in a final stage for speed issue		*/
/* Other debug cases (especially the >0) should remain 			*/
/*									*/
/* -------------------------------------------------------------------- */
#include <mPadFEM.h>
#include <PadFEM.h>
#include <emlLib.h>
#include <limits.h>

static int COUNTER[3]={0,0,0};


long mPadFEM_(
  TABLE_HEAD_ST        *dPadRaw_h,        DPADRAW_ST          *dPadRaw	,
  TABLE_HEAD_ST    *dPadGhitRaw_h,    DPADGHITRAW_ST      *dPadGhitRaw	,
  TABLE_HEAD_ST       *dPadGeom_h,       DPADGEOM_ST         *dPadGeom	,
  TABLE_HEAD_ST     *dPadFEMPar_h,     DPADFEMPAR_ST       *dPadFEMPar	,
  TABLE_HEAD_ST        *dPadFEM_h,        DPADFEM_ST          *dPadFEM	,
  TABLE_HEAD_ST *dPadNibbleGhit_h, DPADNIBBLEGHIT_ST   *dPadNibbleGhit )
{
int	padx,padz							;
int	arm,side,sector						        ;
int	index								;

unsigned long	addr								;

int	i,j,k=0							        ;
int	x,y								;
int	maxx[2]={0,0},maxy[2]={0,0}					;
int	minx[2]={INT_MAX,INT_MAX},miny[2]={INT_MAX,INT_MAX}		;
int	Gidx=0								;
int	Ghits=0							        ;



   /* ----------------------------------------------------------------- */
   /* Init a bunch of global variable via a structure			*/
   /* ----------------------------------------------------------------- */
   PadFEMinit(dPadGeom,dPadFEMPar)					;
   if(PadFEM_g.debug != 0)
     printf("mPadFEM :: debug level = %d\n",PadFEM_g.debug)		;

   COUNTER[PadFEM_g.pcnum]++;
   

   /* Report as 1 2 3 							*/
   if(PadFEM_g.debug > 1){
      printf("mPadFEM :: Pcnumber = %d (i.e. Pc%d)\n"			,
	     PadFEM_g.pcnum						,
	     PadFEM_g.rpcnm)						;
   }
   if( (PadFEM_g.debug > 0) && (SKIPGEANT)){
     printf("mPadFEM :: Module has been setup to skip Geant associations\n");
   }



   /* ----------------------------------------------------------------- */
   /* Report the number of sectors. Will be used for FEM indexing 	*/
   /* ----------------------------------------------------------------- */
   if(PadFEM_g.debug>1){
     printf("mPadFEM :: # of sectors/arm  %d\n",PadFEM_g.numsect)	;
     printf("mPadFEM :: # sub-sectors/arm %d\n",PadFEM_g.nsubsect)	;
     printf("mPadFEM :: # fem pa ps       %d\n",PadFEM_g.fempaps)	;
     printf("mPadFEM :: Total num of FEM  %d\n",PadFEM_g.totfem)	;
   }


   /* Small sanity check */
   if(dPadRaw_h->nok <= 0){
     if(PadFEM_g.debug > 0)
       printf("mPadFEM :: Empty PadRaw received. Sorry ... \n")	        ;
     return STAFCV_BAD							;
   }






   /* ----------------------------------------------------------------- */
   /* Initialization							*/
   /* ----------------------------------------------------------------- */
   if(PadFEM_g.debug > 1)
     printf("mPadFEM :: Initializing %d FEM cards\n",PadFEM_g.totfem)	;

   for(j=0 ; j < PadFEM_g.totfem ; j++){
     /* Just in case							*/
     if(j >= MAXNUMWORDS){
       if(PadFEM_g.debug > 0)
	 printf("mPadFEM :: Error in loop j=%d >= %d\n",j,MAXNUMWORDS)	;
       PadFEMfree()							;
       return STAFCV_BAD						;
     }

     addr	       	= PadFEM_FEE2ADR(&j)				;

     /* Consistency check of our ADDR coding choice		 	*/
     index		= PadFEM_ADR2FEE(&addr)			        ;

     /* Aug 23								*/
     /* The Pc2/Pc3 treatment we have chosen makes the ADR2ROC 		*/
     /* non-surjective.	It now goes modulo 2. See PadFEM for more info	*/
     /* Aug 25 : the next test is obsolete. It should be the same now	*/
     /* but we won't be picky ...					*/
     if( (index-j) > 1 && PadFEM_g.debug > 0){
       printf("mPadFEM :: BUG in ADR vs FEMnum coding routine !!!\n")	;
       printf("mPadFEM :: card %d addr %x ? %d\n\n",j,(unsigned int)addr,index)	;
     }


     /* Those should be fine providing we change the .h constants 	*/
     dPadFEM[j].CAV1	= 0xFFFFF					;
     dPadFEM[j].det	= PADID					        ;
     dPadFEM[j].adr	= addr						;
     dPadFEM[j].Flag	= PADFLAG					;
     dPadFEM[j].CAV2	= LASTWORD					;

     /* Next are taken from Julia (supposed to be arcnet related 	*/
     dPadFEM[j].usr1	= 0xFF555					;
     dPadFEM[j].usr2	= 0xFF444					;

     /* Now totally arbitrary						*/
     dPadFEM[j].Ecounter= 0x2						;
     dPadFEM[j].Bcounter= 0x00055					;
     dPadFEM[j].usr3	= 0x1						;
     dPadFEM[j].usr4	= 0x1						;
     dPadFEM[j].usr5	= 0x1						;
     dPadFEM[j].usr6	= 0x1						;
     dPadFEM[j].usr7	= 0x1						;
     dPadFEM[j].usr8	= 0x1						;

     /* This may be an XOR of the previous user words			*/
     /* Since previous are arbitrary, never mind			*/
     dPadFEM[j].parity	= 0x0						;


     for(i=0 ; i < MAXNUMWORDS ; i++){
       if( i%(XFREQ+1) == 0 ){
	 if(PadFEM_g.debug > 2)
	   fprintf(PadFEM_g.fo,"mPadFEM :: Init with 0xF0F at Word %d\n",i);

	 dPadFEM[j].Word[i]	= 0xF0F				        ;
       } else {
	 /* All the other words at 0					*/
	 dPadFEM[j].Word[i]	= 0x0					;
       }

       if(PadFEM_g.debug > 4){
	 /* All word debugging. Kind of useless. We will leave it with	*/
	 /* debug level 4 ...						*/
	 addr = dPadFEM[j].Word[i]					;
	 fprintf(PadFEM_g.fo,"%3d %3d\t",j,i)				;
	 PadFEMprintb(&addr,WORDLEN,1)					;
       }
     }

     dPadFEM_h->nok++							;
   }




   /* ----------------------------------------------------------------- */
   /* Redundant Initialization check					*/
   /* ----------------------------------------------------------------- */
   if(PadFEM_g.debug > 2){
     for(j=0 ; j < PadFEM_g.totfem ; j++){
       fprintf(PadFEM_g.fo,"mPadFEM :: Checking index=%3d addr=%x\n"	,
	       j,(unsigned int)dPadFEM[j].adr)					;


       for(i=0;i < MAXNUMWORDS; i++){
	 if(dPadFEM[j].Word[i] != 0x0 ){
	   if( i%(XFREQ+1) == 0 ){
	     fprintf(PadFEM_g.fo,"\t Word %d has a special info\n",i)	;
	   } else {
	     fprintf(PadFEM_g.fo,"\t Error in init Word %d <> 0 : %x\n" ,
	             i,(unsigned int)dPadFEM[j].Word[i])				;
	   }
	 }
       }
     }
   }


   /* All FEM cards have zeroed Word[], and the other fields 		*/
   /* initialized ... Now, we need to fill it. Rather easy since 	*/
   /* it is just a {1|0} "|" operation ... 		 		*/
   if(PadFEM_g.debug > 1)
     printf("mPadFEM :: PadFEM tables should be initialized by now\n")	;


   /* ----------------------------------------------------------------- */
   /* Fill the FEM table now ...					*/
   /* ----------------------------------------------------------------- */
   for(j=0 ; j < dPadRaw_h->nok ; j++){
     padx	= dPadRaw[j].padx					;
     padz	= dPadRaw[j].padz					;
     arm	= dPadRaw[j].arm					;
     side	= dPadRaw[j].side					;
     sector	= dPadRaw[j].sector					;

     /* A problem with Pc1 Data made us to apply a fix internal to      */
     /* mPadUnpack. Although not the best, we will make this better     */
     /* later on ...                                                    */
     arm        = FIXARM(arm);
 

     if(PadFEM_g.debug > 3){
       fprintf(PadFEM_g.fo,"mPadFEM :: init = %4d %4d %4d %4d %4d\n"	,
	 arm,sector,side,padx,padz)					;
     }



     /* Index is where it should be i.e. dPadFEM[index] 		*/
     index	= PadFEM_FEMnum(&arm,&sector,&side,&padx)		;


     /* First check is if this returned value is reversible		*/
     if(PadFEM_g.debug > 2){
       int	armn,sectorn,siden					;
       int	FEMoffset						;

       PadFEM_DecodeFEMnum(&index,&armn,&siden,&sectorn,&FEMoffset)	;
       if(armn != arm || siden != side || sectorn != sector){
	 printf("mPadFEM :: Non bijective transformation FEEnum<->Location\n");
	 printf("           Arm=%d->%d  Side=%d->%d  Sector=%d->%d (offset=%d)\n",
	                    arm,armn,side,siden,sector,sectorn,FEMoffset);
       }
     }


     /* Now that this is checked, we can fill it			*/
     /* Get coordinates							*/
     if (PadFEM_gimexy(&padx,&padz,&arm,&side,&x,&y)){

       /* -- Processing --						*/
       /* Statistics. May be removed with some debugging print-out	*/
       maxx[0]	= MAX(padx,maxx[0])					;
       maxy[0]	= MAX(padz,maxy[0])					;
       minx[0]	= MIN(padx,minx[0])					;
       miny[0]	= MIN(padz,miny[0])					;
       maxx[1]	= MAX(x,maxx[1])					;
       maxy[1]	= MAX(y,maxy[1])					;
       minx[1]	= MIN(x,minx[1])					;
       miny[1]	= MIN(y,miny[1])					;


       /* Show debug info						*/
       if(PadFEM_g.debug > 3){
	 fprintf(PadFEM_g.fo,"mPadFEM :: index=%3d YWord %3d (XPos=%3d)\n",
		 index,y,x)						;

	 addr	= dPadFEM[index].Word[y]				;
	 fprintf(PadFEM_g.fo,"\t\t w ")				        ;
	 PadFEMprintb(&addr,WORDLEN,1)					;

	 addr	= BITS(x,0x01)						;
	 fprintf(PadFEM_g.fo,"\t\t | ")				        ;
	 PadFEMprintb(&addr,WORDLEN,1)					;
       }



       /* ------------------------------------------------------------- */
       /* Fill dPadFEM[].Word[]						*/
       dPadFEM[index].Word[y]	= dPadFEM[index].Word[y] | BITS(x,0x01) ;

       /* Display the word 						*/
       if(PadFEM_g.debug > 3){
	 addr	= dPadFEM[index].Word[y]				;
	 fprintf(PadFEM_g.fo,"\t\t = ")				        ;
	 PadFEMprintb(&addr,WORDLEN,0)					;
	 fprintf(PadFEM_g.fo," @ %d %d\n",index,y)			;
       }


       if( DOGEANT ){
	 /* ----------------------------------------------------------- */
	 /* Fill NibbleGhit here as well ...				*/
	 /* We will assume that the Gidx are sorted in ascending order	*/
	 /* We start at j=0 and Gidx=0 and increments by Gidx++ if the	*/
	 /* rawid is not equivalent to j ... If sorted, we should not have*/
	 /* to start from Gidx=0 each time ...				*/
	 while((j != dPadGhitRaw[Gidx].rawid) && (Gidx < dPadGhitRaw_h->nok)){
	   Gidx++							;
	 }
	 if(j != dPadGhitRaw[Gidx].rawid){
	   if(PadFEM_g.debug > 0){
	     printf("mPadFEM :: NibbleGhit ERROR. Could not find a dPadGhitRaw[].rawid for index %d\n",j);
	   }
	   /* Next scan will attempt to find it from start ... User will*/
	   /* have lots of message ... We will NOT return _BAD status	*/
	   Gidx	= 0							;
	   dPadNibbleGhit[Ghits].ghitid	= -1				;
	   dPadNibbleGhit[Ghits].rawid	= -1				;
	   dPadNibbleGhit[Ghits].Card	= -1				;
	   dPadNibbleGhit[Ghits].padx	= -1				;
	   dPadNibbleGhit[Ghits].padz	= -1				;
	 } else {
	   dPadNibbleGhit[Ghits].ghitid	= dPadGhitRaw[Gidx].ghitid	;
	   dPadNibbleGhit[Ghits].rawid	= dPadGhitRaw[Gidx].rawid	;
	   dPadNibbleGhit[Ghits].Card	= index			        ;
	   dPadNibbleGhit[Ghits].padx	= padx				;
	   dPadNibbleGhit[Ghits].padz	= padz				;
	   if(PadFEM_g.debug > 3){
	     fprintf(PadFEM_g.fo,"             Nibble Saving %4d %4d %4d\n",
	       dPadNibbleGhit[Ghits].Card				,
	       dPadNibbleGhit[Ghits].padx				,
	       dPadNibbleGhit[Ghits].padz)				;
	   }
	 }



	 /* We will globally associate dPadNibbleGhit_h->nok at the end	*/
	 /* so we do not have to carry along a too long index name 	*/
	 Ghits++							;

       } /* DOGEANT ...							*/


     } else {
       if(PadFEM_g.debug > 2){
	 printf("mPadFEM :: gimexy returned error for padx=%d padz=%d\n",
	        padx,padz)						;
       }
       k++								;
     }


     if(PadFEM_g.debug > 3){
#if 0
       fprintf(PadFEM_g.fo,"             %5d {%2d %2d %2d} %3d %4d -> %3d %3d\n",
	       dPadRaw[j].id						,
	       arm,sector,side						,
	       padx,padz,x,y)						;
#else
       /* New debug format for easier online/offline debugging          */    
       fprintf(PadFEM_g.fo,"[%d-%d] %d %d %d %d %d (%8d %8d)\n",
	       COUNTER[PadFEM_g.pcnum],dPadRaw[j].id,
	       arm,side,sector,padz,padx,y,x);
#endif
     }
   }



   /* ----------------------------------------------------------------- */
   /* Done ! Last sanity check and debug message, file closing etc..	*/
   /* leave the module returning STAFCV_OK				*/
   /* ----------------------------------------------------------------- */

   if(PadFEM_g.debug > 1){
     if( Ghits+k != dPadRaw_h->nok && DOGEANT){
       printf("mPadFEM :: ERROR : We missed some entries in dPadRaw (%d!=%ld)\n",
	      Ghits+k,dPadRaw_h->nok)					;
     }

     printf("mPadFEM :: Raw   [%3d,%3d] [%3d,%3d]\n",minx[0],maxx[0],miny[0],maxy[0]);
     printf("           Trans [%3d,%3d] [%3d,%3d]\n",minx[1],maxx[1],miny[1],maxy[1]);
     if((WORDLEN-maxx[1]) > 5 || (MAXNUMWORDS-maxy[1]) > 10){
       /* and we are rather flexible with arbitrary 5 and 10 ...	*/
       /* those have the meaning of "if I missed by 5 (10) units, I am	*/
       /* probably doing something wrong ...				*/
       printf("mPadFEM :: WARNING Suspicious conversion SlowSim->FEMcoord\n");
     }
   }


   if(PadFEM_g.debug > 3){
     /* Dump the table before leaving					*/
     fprintf(PadFEM_g.fo,"Dumping the complete table\n")		;

     for(j=0 ; j < PadFEM_g.totfem ; j++){
       for(i=0 ; i < MAXNUMWORDS ; i++){
	 addr = dPadFEM[j].Word[i]					;
	 fprintf(PadFEM_g.fo,"%3d %3d\t",j,i)				;
	 PadFEMprintb(&addr,WORDLEN,1)					;
       }
     }
   }


   dPadNibbleGhit_h->nok = Ghits					;
   if(PadFEM_g.debug > 3){
     printf("mPadFEM :: Number of hits %ld %d\n",dPadNibbleGhit_h->nok,Ghits);
   }

   PadFEMfree()							        ;
   return STAFCV_OK							;
}



