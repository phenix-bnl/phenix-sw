/* -------------------------------------------------------------------------- */
/* Created J.Lauret Mon Aug 24 1998 - PadChamber Unpacker module	      */
/* -------------------------------------------------------------------------- */

#include <mPadUnpack.h>
#include <PadFEM.h>
#include <emlLib.h>

#include <stdlib.h>


#define TOTNUMWORD  ( DCMHEADER+MAXNUMWORDS+DCMTRAILER )

/* Alpha Osf1 and cc combination problem. Solved it this way		*/
#ifdef LASTDCMWORD
#undef LASTDCMWORD
#endif
#define LASTDCMWORD  dPadFEMPar[0].last
static int COUNTER[3]={0,0,0};

long mPadUnpack_(
  TABLE_HEAD_ST        *dPadDCM_h,        DPADDCM_ST          *dPadDCM	,
  TABLE_HEAD_ST       *dPadGeom_h,       DPADGEOM_ST         *dPadGeom	,
  TABLE_HEAD_ST *dPadNibbleGhit_h, DPADNIBBLEGHIT_ST   *dPadNibbleGhit	,
  TABLE_HEAD_ST     *dPadFEMPar_h,     DPADFEMPAR_ST       *dPadFEMPar	,
  TABLE_HEAD_ST        *dPadRaw_h,        DPADRAW_ST          *dPadRaw	,
  TABLE_HEAD_ST    *dPadGhitRaw_h,    DPADGHITRAW_ST      *dPadGhitRaw )
{
int	debug								;
int	i,j,k								;
int	x,y								;
int	padx,padz							;
int	check								;
int	arm,side,sector,FEMoffset					;
int	saved_id							;
int	Ghits=0							        ;
short	*tindex = 0							;
short   *ptindex = 0                                                    ;
int	index=-1							;
short	mindex=0							;
short	DCMnum=0							;
short	b20,b31							        ;
unsigned long addr							;



  /* ------------------------------------------------------------------ */
  /* Initialisation 							*/
  /* We will use the same info for FEM module and Unpacker module	*/
  /* ------------------------------------------------------------------ */
  PadFEMinit(dPadGeom,dPadFEMPar)					;

  /* I am not going to carry along a variable name that long ...	*/
  debug = PadFEM_g.debug						;
  COUNTER[PadFEM_g.pcnum]++;

  if(debug > 0){
    printf("mPadUnp :: Will refer to mPadUnpack by mPadUnp. Debug level = %d\n",debug);
    printf("mPadUnp :: Number of entries %ld\n",dPadDCM_h->nok)		;
  }

   /* Small sanity check -- Added 1-DEC-1998 */
  if(dPadDCM_h->nok <= 0){
    if(debug > 0)
       printf("mPadUnp :: Empty dPadDCM received. Sorry ... \n")	;
    return STAFCV_BAD							;
  }

  if ( DOGEANT && (dPadNibbleGhit_h->nok > 0)){
    tindex = (short *) malloc(sizeof(short)*MAXNENTRIES)		;
    if( ptindex == NULL){
      printf("mPadUnp :: Cannot allocate memory for dPadRaw checking.\n");
      printf("           dPadRaw and dPadGhitRaw may not be properly associated\n");
    } else {
      for(i=0 ; i < MAXNENTRIES ; i++) tindex[i] = 0			;
    }
  } else {
    ptindex = NULL							;
    if(debug > 0 )
      printf("mPadUnp :: Geant Associations will not be done\n")	;
  }



  /* ------------------------------------------------------------------ */
  /* Main DCM table loop						*/
  /* ------------------------------------------------------------------ */
  j = 0								        ;
  for(i=0; i < dPadDCM_h->nok ; i++){
    /* Get first word, use while() loop until last word BUT do not	*/
    /* exceed the TOTNUMWORD expected in pass-through mode neither	*/
    j	= 0								;
    addr= dPadDCM[i].Word[j]						;


    while( (addr != LASTDCMWORD) && (j < TOTNUMWORD) ){
      /* Note that we do not need to extract a value greater than 0xF=15*/
      b31	= RBIT(31,0x1,addr)					;
      b20	= RBIT(20,0x7F,addr)					;


      /* Debugging block 1 was here. Moved at the EndOf this code	*/
      /* for historical/hysterical purposes only ...			*/


      if(b31 == 0){
	/* This is a data word						*/
	/* Now, easer we are in pass-through mode OR in zero-supress mode */
	/* Regardless of the mode, the above loop will stop at 		*/
	/* addr == LASTDCMWORD 						*/
	/* Index i has been 'decoded' already. Index j after the headers*/
	/* is related to our mPadFEM "y" coordinate. "x" needs to be	*/
	/* sorted out amongst the 20 bits long word			*/


	y	= b20 - 1						;

	if(debug > 3){
	  fprintf(PadFEM_g.fo,"mPadUnp :: Data word %3d ",y)		;
	  PadPrintb(PadFEM_g.fo,&addr,32,1)				;
	}

	if ( y%(XFREQ+1) != 0 ){
	  for(k=0 ; k < WORDLEN ; k++){
	    if( RBIT(k,0x1,addr) == 1){
	      /* We got an "x" coordinate				*/
	      x	= k							;

	      /* The i index can be decoded into arm side sector	*/
	      PadFEM_DecodeFEMnum(&i,&arm,&side,&sector,&FEMoffset)	;

	      if (PadUNP_padxy(&padx,&padz,&i,&arm,&side,&x,&y)){
		/* We got a good one and have padx,padz			*/
#if 0
		if(PadFEM_g.pcnum != 0 && (i%2) != 0){
		  /* New code in Pad Space				*/
		  padx	= padx + WORDLEN				;
		}
#else
		/* done internally now					*/
		/* Fri Jun 23 2000: Data disagree in regards of the arm */
		/* number ...                                           */
		arm = FIXARM(arm);
#endif



		/* This is similar to the debug output line in mPadFEM	*/
		if(debug > 3){
#if 0
		  fprintf(PadFEM_g.fo,"             %5d {%2d %2d %2d} %3d %4d <- %3d %3d\n",
			  dPadRaw_h->nok				,
			  arm,sector,side				,
			  padx,padz,x,y)				;
#else
		  /* New debug format for easier online/offline debugging*/    
		  fprintf(PadFEM_g.fo,"[%d.%ld] %d %d %d %d %d (%8d %8d)\n",
			  COUNTER[PadFEM_g.pcnum],dPadRaw_h->nok,
			  arm,side,sector,padz,padx,y,x);
#endif
		}



		/* Fill  dPadGhitRaw  table  from  dPadNibbleGhit and	*/
		/* check if OK ...  The tricky one is [].Card   	*/
		/* checked  through  DCMnum  which   is   incremented   */
		/* whenever  with  find  the LASTDCMWORD word (in DCM   */
		/* table, everything is sequential i.e.  only one row   */
		/* appears in that table).                              */
		saved_id = index					;
		if ( dPadNibbleGhit_h->nok <= 0 || SKIPGEANT ){
		  /* Well, this most probably mean that we are reading	*/
		  /* a DataFile	... I know, this suck (to have 2 	*/
		  /* separate treatement) but it's the only way to have	*/
		  /* it right and sorted in case of simulation at the	*/
		  /* time.						*/
		  index++						;
		  dPadRaw[index].padx	= padx				;
		  dPadRaw[index].padz	= padz				;
		  dPadRaw[index].arm	= arm				;
		  dPadRaw[index].side	= side				;
		  dPadRaw[index].sector	= sector			;
		  dPadRaw[index].id	= index			        ;
		  dPadRaw[index].padtype = 0                            ;

		  /* Free tindex() right away (at first fill)		*/
		  /* so we will avoid later adjustment loop		*/
		  if( index == 0){
		    if( ptindex != NULL){
		      free(tindex)					;
		      ptindex = NULL					;
		    }
		  }
		  mindex = index					;

		} else {
		  for( Ghits = 0 ; Ghits < dPadNibbleGhit_h->nok ; Ghits++){
		      if ( (dPadNibbleGhit[Ghits].padx == padx)	&&
			 (dPadNibbleGhit[Ghits].padz == padz)	&&
			 (dPadNibbleGhit[Ghits].Card == DCMnum )) {

			 /* Here  we  go  :   in  order  to  restore    */
			 /* dPadRaw in the same original order, what    */
			 /* we do is to blindly restore the original    */
			 /* dPadNibbleGhit[Ghits].rawid    as    the    */
			 /* dPadRaw  index,  record  the  max  index    */
			 /* mindex, and keeping track of the one  we    */
			 /* have   actually   assigned  in  tindex.     */
			 /* Before leaving, adjustements can be done    */
			 /* to compensate for non-filled rows.        	*/

			 index			= dPadNibbleGhit[Ghits].rawid;
			 dPadGhitRaw[dPadGhitRaw_h->nok].ghitid= dPadNibbleGhit[Ghits].ghitid;
			 dPadGhitRaw[dPadGhitRaw_h->nok].rawid = index	;
			 dPadRaw[index].padx	= padx			;
			 dPadRaw[index].padz	= padz			;
			 dPadRaw[index].arm	= arm			;
			 dPadRaw[index].side	= side			;
			 dPadRaw[index].sector	= sector		;
			 dPadRaw[index].id	= index		        ;
			 dPadRaw[index].padtype = 0                     ;

			 mindex			= MAX(mindex,index)	;
			 if( ptindex != NULL){
			   if( index < MAXNENTRIES ){
			     tindex[index]		= 1		;
			   } else {
			     if( debug > 0 ){
			       printf("mPadUnp :: cannot record rawid=%d > MAXNENTRIES=%d\n",index,MAXNENTRIES);
			     }
			   }
			 }

			 dPadGhitRaw_h->nok++				;


		      }	/* End of "this is matches" test		*/
		  }	/* End of NibbleGhit scan			*/
		}	/* End of Test if NibbleGhit is empty		*/



		/* Check if we found an association. BTW : this is	*/
		/* automatically skipped (by construction) in Data mode.*/
		if( saved_id == index){
		  if(debug > 0){
		    printf("mPadUnp :: Nothing matching Card=%d padx=%d padz=%d!!\n",
		           DCMnum,padx,padz)				;
		  }
		  /* Fill GhitRaw with one "empty"			*/
		  dPadGhitRaw[dPadGhitRaw_h->nok].ghitid= -1		;
		  dPadGhitRaw[dPadGhitRaw_h->nok].rawid = -1		;
		  dPadGhitRaw_h->nok++					;
		}


	      }	/* Was Unpacked into x/y space properly			*/
	    }	/* X coordinate was found				*/
	  }	/* End loop over word					*/
	} else {/* End test if that's really a valid DataWord		*/
	  if(debug > 3){
	    fprintf(PadFEM_g.fo,"mPadUnp :: Skipping it\n")		;
	  }
	}


      } else {
	/* NOT b31 == 0							*/
	/* This is easer a trailer or header				*/
	/* We will treat the header with more checks than trailer	*/
	/* but continue the precessing in ALL cases (even if wrong)	*/
	if(debug > 3){
	  fprintf(PadFEM_g.fo,"mPadUnp :: Hdr/Trail  ")		        ;
	  PadPrintb(PadFEM_g.fo,&addr,32,1)				;
	}


	if(j < DCMHEADER){
	  if(debug > 4){
	    fprintf(PadFEM_g.fo,"mPadUnp :: DCM %d Word sequence number %d\n",i,j);
	    fprintf(PadFEM_g.fo,"           Header --> ")		;
	    PadPrintb(PadFEM_g.fo,&addr,32,1)				;
	  }

	  /* True only for pass-through mode				*/
	  /* if( b20 != j){ 						*/
	  /* What's always true is that b20 will always be >= j		*/

	  if( b20 < j ){
	    if(debug > 1){
	      printf("mPadUnp :: Header %d out of sequence. Expected Bit20 at %d\n",j,b20);
	      fprintf(PadFEM_g.fo,"mPadUnp :: Header %d out of sequence. Expected Bit20 at %d\n",j,b20);
	    }
	  }
	} else {
	  /* It got to be a trailer word				*/
	  if(debug > 1){
	    /* Again, very mild warning. Processing will go ahead	*/
	    if(b20 != 14 && b20 != 15){
	      printf("mPadUnp :: Incorrect trailer word. bit20 at %d, expected 14 or 15\n",b20);
	      fprintf(PadFEM_g.fo,"mPadUnp :: Incorrect trailer word. bit20 at %d, expected 14 or 15\n",b20);
	    }
	  }
	}
      }
      /* b31 Test							*/

      /* Get to next word, read new address				*/
      j++								;
      addr	= dPadDCM[i].Word[j]					;

      /* This is required for NibbleGhit scan				*/
      if (addr == LASTDCMWORD) DCMnum++				        ;

    } /* EndWhile							*/



    /* EndWhile Condition debugging					*/
    if(debug > 3){
      fprintf(PadFEM_g.fo,"mPadUnp :: EndWhile  ")			;
      PadPrintb(PadFEM_g.fo,&addr,32,1)				        ;
      fprintf(PadFEM_g.fo,"mPadUnp :: EndWhile  j=%d < %d\n",j,TOTNUMWORD);
    }



    /* while() is done, go to the next card and start again		*/
    /* But before, do the last counter consistency check		*/
    /* check Must be less than MAXNUMWORDS 				*/
    check	= j-DCMHEADER-DCMTRAILER-1				;
    if(check >= MAXNUMWORDS){
      if(debug > 0)
	printf("mPadUnp :: ERROR -> Number of words in DCM %d is %d >= than %d\n",
	       i,check,MAXNUMWORDS)					;
    } else {
      if (check == 0){
	if(debug > 0)
	  printf("mPadUnp :: Hum, empty %d DCM card found. No Data Word scanned\n",i);
      } else {
	if(debug > 3)
	  fprintf(PadFEM_g.fo,"mPadUnp :: Number of words for DCM %d is %d (%d)\n",
		  i,check,j)						;
      }
    }
  }	/* dPadDCM_h loop						*/



  /* ------------------------------------------------------------------ */
  /* Last word sanity check. This checks only that the last word in the	*/
  /* DCM table is 0x0 (and change)					*/
  /* ------------------------------------------------------------------ */
  if( addr != LASTDCMWORD ){
    /* the last word after all DCM entries is NOT 0x0 (and change) ???	*/
    /* The entire scan went real wrong ...				*/
    if(debug > 0){
      printf("mPadUnp :: This DCM table is wrong. Last word is NOT correct\n");

      printf("mPadUnp :: Current indexes are i=%d j=%d\n",i,j)		;
      PadPrintb(stdout,&addr,32,1)					;
      addr = LASTDCMWORD						;
      PadPrintb(stdout,&addr,32,1)					;
    }
  }


  /* Adjustment loop.							*/
  /* Just in case, fill the empty table cells with "invalid values"	*/
  if( ptindex != NULL){
    for(i=0 ; i < mindex ; i++){
      if( tindex[i] == 0){
	dPadRaw[i].padx	= -1						;
	dPadRaw[i].padz = -1						;
	dPadRaw[i].arm	= -1						;
	dPadRaw[i].side = -1						;
	dPadRaw[i].sector= -1						;
	dPadRaw[i].id	= -1						;
	dPadRaw[i].padtype = -1                                         ;
      }
    }
    free(tindex)							;
  }


  /* And before leaving ...						*/
  dPadRaw_h->nok = mindex+1						;


  /* ------------------------------------------------------------------	*/
  /* Done ... Cleanup as usual and retunr status ....			*/
  /* ------------------------------------------------------------------	*/
  PadFEMfree()								;
  return STAFCV_OK							;
}



#if 0
#define LASTDCMWORD_TEST (LASTWORD      | BITS(31,1) | BITS(20,15) | BITS(28,2))
      /* 28-JAN-1999
         This is a debugging block. Regardless of what I tried, an
         unsigned long tmp=LASTDCMWORD with LASTDCMWORD as defined
         in PadFEM.h does NOT equate to unsigned long addr with
         addr = dPadDCM[i].Word[j] ; dPadDCM[i].Word[j] being an
         unsigned long  initialized to LASTDCMWORD (???). I refused
         to understand that dilema after hours and hours of debugging
         and used a new scheme involving mPadFEMPar[0].last initialized
         in mPadFEM and mPadUnpack modules.  Btw : This does not happen
         with gcc but happens on Osf1/cc.
         See also mPadDCM.c line 254 (or search on date) for extra test
         in a desperate attempt to force the LastWord to be correct.
         Nothing seems to do it ...                                     */

      if(debug>3){
        unsigned long tmp						;
        /* tmp = dPadFEMPar[0].last; */
        tmp     = LASTDCMWORD_TEST					;

        fprintf(PadFEM_g.fo,"mPadUnp :: %d %d %d %d %d %d %d %d %d %d\n",\
                addr,LASTDCMWORD,tmp,dPadFEMPar[0].last,\
                addr == LASTDCMWORD, addr == -1594884096,\
                tmp  == LASTDCMWORD, tmp  == -1594884096,\
                tmp  == addr,\
                LASTDCMWORD == -1594884096)				;
      }
#endif


/* ----------------------- :                                            */
/*   Date of Revision      :  3-DEC-1999                                */
/*   Change Author         : JLAURET                                    */
/*   Purpose of Revision   : Modified to work with Extra-Words.		*/
/*                         :                                            */
/* ----------------------- :                                            */
/*   Date of Revision      :  3-DEC-1999                                */
/*   Change Author         : JLAURET                                    */
/*   Purpose of Revision   : Sorted PadRaw as original + corrected	*/
/*                         : the NibbleGhit.				*/
