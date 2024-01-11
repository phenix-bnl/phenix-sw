// ==================
// FILE: SvxClusterInfo.C
// ==================

#include <SvxClusterInfo.h>

ClassImp(SvxClusterInfo)

int SvxClusterInfo::get_sublayer()
{
  if ( getLayer()<2 ) {
    return getLayer();
  } else if ( getLayer()==2 ) {
    if ( getLadder()<8 ) {
      if( getLadder()%3==1 )       { return 2; }
      else if ( getLadder()%3==0 ) { return 3; }
      else                         { return 4; }
    } else {
      if( getLadder()%3==2 )       { return 2; }
      else if ( getLadder()%3==0 ) { return 3; }
      else                         { return 4; }
    }
  } else {
    if ( getLadder()<12 ) {
      if( getLadder()%3==0 )       { return 5; }
      else if ( getLadder()%3==1 ) { return 6; }
      else                         { return 7; }
    } else {
      if( getLadder()%3==2 )       { return 5; }
      else if ( getLadder()%3==1 ) { return 6; }
      else                         { return 7; }
    }
  }
}

