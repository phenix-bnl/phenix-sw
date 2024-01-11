/*:>--------------------------------------------------------------------
**: FILE:       memcgeaevent.c.template
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.1.1.1 1997/09/26 20:43:47 dave Exp  
**:<------------------------------------------------------------------*/
#include "mEmcGeaEvent.h"
#include "emlLib.h"

/** This module gets the actual vertex in the event when processing
    simulated data.  It uses "header" as an input.  The vertex info
    is needed in order to properly calculate TOF, position and PC3
    projection.  It produces the same output table as mEmcEvent for
    real events.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
*/
long mEmcGeaEvent_(
  TABLE_HEAD_ST         *header_h,         HEADER_ST           *header ,
  TABLE_HEAD_ST      *dEmcEvent_h,      DEMCEVENT_ST        *dEmcEvent )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    memcgeaevent_
**: DESCRIPTION: Physics Analysis Module ANSI C template.
**:             This is an ANSI C Physics Analysis Module template
**:             automatically generated by stic from memcgeaevent.idl.
**:             Please edit comments and code.
**: AUTHOR:     hpl - H.P. Lovecraft, hplovecraft@cthulhu.void
**: ARGUMENTS:
**:       IN:
**:             header    - PLEASE FILL IN DESCRIPTION HERE
**:            header_h   - header Structure for header
**:    INOUT:
**:      OUT:
**:          dEmcEvent    - PLEASE FILL IN DESCRIPTION HERE
**:         dEmcEvent_h   - header Structure for dEmcEvent
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/


    long i;

    dEmcEvent[0].id = 1;
    dEmcEvent[0].evtyp = 1;   /* GEANT */
    dEmcEvent[0].evno = header[0].event;
    dEmcEvent[0].runno = header[0].run;
    dEmcEvent[0].serialno = 1;
    dEmcEvent[0].impact = header[0].b;
    dEmcEvent[0].xyz[0] = header[0].vertex[0];
    dEmcEvent[0].xyz[1] = header[0].vertex[1];
    dEmcEvent[0].xyz[2] = header[0].vertex[2];
    dEmcEvent[0].twrmultlo = 0.0;
    dEmcEvent[0].twrmulthi = 0.0;
    for ( i = 0; i < 3; i++)
      {
	dEmcEvent[0].trigsum[i] = 0.0;
      }
    dEmcEvent[0].tote = 0.0;
    dEmcEvent[0].totet = 0.0;
    
    

    dEmcEvent_h->nok = 1;
    
   return STAFCV_OK;
}