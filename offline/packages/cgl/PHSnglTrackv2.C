#include <PHSnglTrackv2.h>

ClassImp(PHSnglTrackv2)

PHSnglTrackv2::PHSnglTrackv2()
{
  trackIndex= 0;
  for (short int i=0;i<3;i++)
    {
      projectionVtx[i] = -999999.9;
      projectionDch[i] = -999999.9;
      projectionTec[i] = -999999.9;
      projectionPc1[i] = -999999.9;
      projectionPc2[i] = -999999.9;
      projectionPc3[i] = -999999.9;
      projectionPbGl[i] = -999999.9;
      projectionPbSc[i] = -999999.9;
      projectionTof[i] = -999999.9;
      projectionCrk[i] = -999999.9;
      projectionTzr[i] = -999999.9;
      projectionPcr[i] = -999999.9;
      directionVtx[i] = -999999.9;
      directionDch[i] = -999999.9;
      directionPc1[i] = -999999.9;
      directionPc2[i] = -999999.9;
      directionPc3[i] = -999999.9;
      directionCrk[i] = -999999.9;
      directionTec[i] = -999999.9;
      directionTof[i] = -999999.9;
      directionPbSc[i] = -999999.9;
      directionPbGl[i] = -999999.9;
      directionTzr[i] = -999999.9;
      directionPcr[i] = -999999.9;
    }
  tofPathLength = -999999.9;
  emcPathLength = -999999.9;
  crkPathLength = -999999.9; 
  tzrPathLength = -999999.9; 
}





