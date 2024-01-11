#include "AccHitMapEntry.h"
#include <iostream>

ClassImp(AccHitMapEntry)

using namespace std;

AccHitMapEntry::AccHitMapEntry()
{
  //id = -1;
  hitid=-1;
  hitconfig=-1;
  for(int ibox=0;ibox<4;ibox++)
    {
      ph1[ibox]=-9998.0;
      ph2[ibox]=-9998.0;
      t1[ibox]=-9998.0;
      t2[ibox]=-9998.0;
    }
  return;
}

void
AccHitMapEntry::identify(ostream &os) const
{
  os << "AccHitMapEntry: " 
     << " hitid: " << hitid
     << ", hitconfig: " << hitconfig;
  for(int i=0;i<4;i++)
    {
      os << ", ibox: " << i
	 << ", ph1: " << ph1[i]
	 << ", ph2: " << ph2[i]
	 << ", t1: " << t1[i]
	 << ", t2: " << t2[i];
	}
  os << endl;
  
  return;
}
