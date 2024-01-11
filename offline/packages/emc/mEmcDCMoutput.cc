/*:>--------------------------------------------------------------------
**: compile with cc
**: FILE:       mEmcDCMoutput.cc
*/
#include "mEmcDCMoutput.h"
#include "emlLib.h"

long mEmcDCMoutput_(
  TABLE_HEAD_ST    *dEmcDCMData_h,    DEMCDCMDATA_ST      *dEmcDCMData )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    mEmcFEMToDCM_
**: DESCRIPTION: Physics Analysis Module ANSI C template.
**:             This is an ANSI C Physics Analysis Module template
**:             automatically generated by stic from mEmcFEMToDCM.idl.
**:             Please edit comments and code.
**: AUTHOR:     Martin L Purschke
**: ARGUMENTS:
**:       IN:
**:        dEmcDCMData    - PLEASE FILL IN DESCRIPTION HERE
**:       dEmcDCMData_h   - header Structure for dEmcDCMData
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

/*
  int i, status;

  for(i=0; i<dEmcDCMData_h->nok; i++)
    {
      
      if ( status =pio_addpacket ((PHDWORD *) &dEmcDCMData[i].DCM[0],  // address
			  dEmcDCMData[i].nWords,                     // length 
  			  dEmcDCMData[i].packetID,                   // id 
			  4,                                         // wordsize 
			  dEmcDCMData[i].scheme)                     // hitformat
	   )

	{
	  switch (status)
	    {
	    case  (PIO_NOCURRENTEVENT):
	      cout<<" NO current event to add packet "<< endl;
	      return STAFCV_BAD;
	      break;
	      
	    case  (PIO_NOSPACELEFT):
	      cout<<" Space exceeded in Event "<< endl;
	      return STAFCV_BAD;
	      break;
	    default:
	      return STAFCV_BAD;
	    }
	  
	}
    }
*/
   return STAFCV_OK;
}
