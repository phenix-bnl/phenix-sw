
#include "AccpRawv1.h"

ClassImp(AccpRawv1)

AccpRawv1::AccpRawv1()
{ 
  Reset(); 
}

AccpRawv1::~AccpRawv1()
{
}

void
AccpRawv1::Reset()
{
  for ( int k = 0; k < ACCP_NCH; k++ )
    {
      adchpost[k]  = 0;
      adchpre[k]  = 0;
      adclpost[k]  = 0;
      adclpre[k]  = 0;
      tdc[k] = 0;
    }
}
