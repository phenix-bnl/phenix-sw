#include <iostream>
#include "PISAEvent.h"
#include "TofPISAHit.h"
#include <cstdlib>

void encodeRootEvntTof(int kentries, int i[], float f[], 
                       int isubevent, PISAEvent *pisaevent)
{

  assert( pisaevent->GetStartFlag() != 2 );
 
  Int_t mctrack = -1;  // Will change in Off-Line
  Int_t nfile = -1;

  Int_t p = 0;
  for (int k=0; k<kentries; k++)
  {
    
    Int_t subvol         = i[p++];
    Int_t panel          = i[p++];
    Int_t column         = i[p++];
    Int_t pslat          = i[p++];
    Int_t slat_seq       = i[p++];
    Int_t partl          = i[p++];
    Float_t xm           = f[p++];
    Float_t ym           = f[p++];
    Float_t zm           = f[p++];
    Float_t pos_hit_slat = f[p++];
    Float_t pxm          = f[p++];
    Float_t pym          = f[p++];
    Float_t pzm          = f[p++];
    Float_t tof          = f[p++];
    Float_t dele         = f[p++];
    Int_t track          = i[p++];

    pisaevent->AddTofHit(subvol, panel, column, pslat, slat_seq, partl, xm, ym, zm,
      pos_hit_slat, pxm, pym, pzm, tof, dele, track, mctrack,
      isubevent, nfile);

  }  // loop over all track entries

  return;
}
