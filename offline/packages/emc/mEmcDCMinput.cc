/*:>--------------------------------------------------------------------
**: compile with cc
**: FILE:       mEmcDCMoutput.cc
*/
#include "mEmcDCMinput.h"
#include "emlLib.h"


long mEmcDCMinput_(
  TABLE_HEAD_ST    *dEmcDCMData_h,    DEMCDCMDATA_ST      *dEmcDCMData )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    mEmcFEMToDCM_
**: DESCRIPTION: Physics Analysis Module ANSI C template.
**: AUTHOR:     Martin L Purschke
**: ARGUMENTS:
**:       OUT:
**:        dEmcDCMData    - PLEASE FILL IN DESCRIPTION HERE
**:       dEmcDCMData_h   - header Structure for dEmcDCMData
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

/*
  int i, status;

  i = 0;
  dEmcDCMData_h->nok = 0;
  int id = 8001;
  int nw;

  while (  ( status =pio_getpacketdata ( 
                       (int *) &dEmcDCMData[i].DCM[0],  // address
		       450,                            // length 
		       id,                              // id 
		       &nw,                             // so may words 
		       IDCRAW)                          // getit raw.
		       ) == 0)
    {
      dEmcDCMData_h->nok++;
      dEmcDCMData[i].scheme = pio_getpackethitformat(id);
      dEmcDCMData[i].nWords = nw;
      dEmcDCMData[i].packetID = id++;
      i++;
    }
*/

  return STAFCV_OK;
}



