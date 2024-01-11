#include "mEmcEvent.h"
#include "emlLib.h"

/** This module gets the actual vertex in the event when processing
    real data.  It uses dBbcOut as an input.  The vertex info is
    needed in order to properly calculate TOF, position and PC3
    projection.  In case of simulated data the mEmcGeaEvent module can
    be called instead (note, that both produce the same output table).
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov} @version 1.0
*/
long 
mEmcEvent_
(
 TABLE_HEAD_ST *dBbcOut_h, DBBCOUT_ST *dBbcOut ,
 TABLE_HEAD_ST *dEmcEvent_h, DEMCEVENT_ST *dEmcEvent 
 )
{
  /*
    ARGUMENTS:
    IN:
    dBbcOut    - PLEASE FILL IN DESCRIPTION HERE
    dBbcOut_h   - header Structure for dBbcOut
    OUT:
    dEmcEvent    - PLEASE FILL IN DESCRIPTION HERE
    dEmcEvent_h   - header Structure for dEmcEvent
    RETURNS:    STAF Condition Value
  */
 
  long i;
  static long ievno = 0;
  float vertex[3];

  ievno++;

  dEmcEvent[0].id = 1;
  dEmcEvent[0].evtyp = 1;   /* GEANT */
  dEmcEvent[0].evno = ievno;
  dEmcEvent[0].runno = 0;
  dEmcEvent[0].serialno = 1;
  dEmcEvent[0].impact = 0.0;

  for (i = 0; i < 3; i++)
    { 
       vertex[i] = 0.0;
    } 

  if (dBbcOut_h->nok > 0)
    {
      vertex[2] = dBbcOut[0].VertexPoint;
    }

#ifdef DEBUG
  printf("BBC vertex %f \n", dBbcOut[0].VertexPoint);
#endif

  if (vertex[2] > 100.0 || vertex[2] < -100.0)
    {
      vertex[2] = 0.0;
    }

  for ( i = 0; i < 3; i++)
    {
      dEmcEvent[0].xyz[i] = vertex[i];
    }

  dEmcEvent[0].twrmultlo = 0.0;
  dEmcEvent[0].twrmulthi = 0.0;
  for (i = 0; i < 3; i++)
    {
      dEmcEvent[0].trigsum[i] = 0.0;
    }
  dEmcEvent[0].tote = 0.0;
  dEmcEvent[0].totet = 0.0;
  dEmcEvent_h->nok = 1;

  return STAFCV_OK;
}
