
/* ---------------------------------------------------------------------------- */
/* Stupid module to dump the Raw Hit tables					*/
/* I don't know whu but the ntuples does not Show up on sgi/RL97		*/
/* ---------------------------------------------------------------------------- */

#include <stdio.h>
#include "mPadDumpRaw.h"
#include "emlLib.h"

#define DIM 2

FILE	*fo1								;
FILE	*fo2								;
static  int save_mode=-1						;
static	int calls[DIM]							;



long mPadDumpRaw_(
  TABLE_HEAD_ST        *dPadRaw_h,        DPADRAW_ST          *dPadRaw ,
  TABLE_HEAD_ST    *dPadGhitRaw_h,    DPADGHITRAW_ST      *dPadGhitRaw ,
  TABLE_HEAD_ST     *dPadFEMPar_h,     DPADFEMPAR_ST       *dPadFEMPar )
{

int	i;
char	fname1[80],fname2[80];
int	mode;
int	debug;

  if(save_mode == -1){
    for(i=0;i<DIM;i++){
      calls[i] = 0;
    }
    save_mode = 0;
  }

  /* Get a call counter incremented. Ntuple Initialization should happen*/
  /* only once 								*/
  /* Control parameter is .mode and .fout 				*/
  mode	= dPadFEMPar[0].mode						;
  debug	= dPadFEMPar[0].debug						;

  sprintf(fname1,"padraw_%d.tmp",mode)					;
  sprintf(fname2,"padghitraw_%d.tmp",mode)				;


  if(mode >= DIM){
    printf("mPadDump:: Mode >= %d not understood\n",DIM)		;
    return STAFCV_BAD;
  }
  calls[mode]++								;


  if(calls[mode] == 1){
    if(dPadFEMPar[0].fout != 0){
      fo1 = fopen(fname1,"w")						;
      if(fo1 == NULL){
	printf("mPadDump:: Problemo !! Could not open file %s\n",fname1);
	return STAFCV_BAD						;
      } else {
	if(debug>0)printf("mPadDump:: File %s is opened\n",fname1)	;
      }

      fo2 = fopen(fname2,"w")						;
      if(fo2 == NULL){
	printf("mPadDump:: Problemo !! Could not open file %s\n",fname2);
	return STAFCV_BAD						;
      } else {
	if(debug>0)printf("mPadDump:: File %s is opened\n",fname2)	;
      }
    } else {
      if(debug>0)printf("mPadDump:: No hard copy output done\n")	;
    }
  } else {
    /* Open file in append mode						*/
    if(debug>0)printf("mPadDump:: Opening files in append mode\n")	;
    fo1	= fopen(fname1,"a")						;
    fo2 = fopen(fname2,"a")						;
  }


  fprintf(fo1,"%d %d ---------------------------------------\n",mode,calls[mode]);
  for(i=0; i < dPadRaw_h->nok ; i++){
    if(dPadFEMPar[0].fout != 0){
      fprintf(fo1,"%6d %6d %6d %6d %6d\n",
	      dPadRaw[i].arm,	    dPadRaw[i].side,	    dPadRaw[i].sector,
	      dPadRaw[i].padx,	    dPadRaw[i].padz);
    }
  }
  fclose(fo1)								;

  fprintf(fo2,"%d %d ---------------------------------------\n",mode,calls[mode]);
  for(i=0; i < dPadGhitRaw_h->nok ; i++){
    if(dPadFEMPar[0].fout != 0){
      fprintf(fo2,"%6d %6d\n",  dPadGhitRaw[i].ghitid,   dPadGhitRaw[i].rawid);
    }
  }
  fclose(fo2)								;

  return STAFCV_OK;
}

