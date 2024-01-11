#include "TofPISAHit.h"
#include "tofghitWrapper.h"
//INCLUDECHECKER: Removed this line: #include "TofGetGEA.h"

#include "getClass.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

//INCLUDECHECKER: Removed this line: #include <iostream>

using namespace std;

long
TofGetGEA(PHCompositeNode* topNode)
{
  
  TofPISAHit *event = TofPISAHit::GetTofHitEvt();
  Int_t tofRows = TofPISAHit::GetTofCount();    // variable number of rows

  tofghitWrapper* w = findNode::getClass<tofghitWrapper>(topNode,"tofghit");
  w->SetMaxRowCount(tofRows);

  TOFGHIT_ST* tofghit = w->TableData();

  for(int i=0; i<tofRows; i++) {
   
    tofghit[i].subvol = event[i].GetSubvol();
    tofghit[i].panel = event[i].GetPanel();
    tofghit[i].column = event[i].GetColumn();
    tofghit[i].pslat = event[i].GetPslat();
    tofghit[i].slat_seq = event[i].GetSlat_seq();
    tofghit[i].partl = event[i].GetPartl();
    tofghit[i].pos_m[0] = event[i].GetXm(); 
    tofghit[i].pos_m[1] = event[i].GetYm();
    tofghit[i].pos_m[2] = event[i].GetZm();
    tofghit[i].p_m[0] = event[i].GetPxm();
    tofghit[i].p_m[1] = event[i].GetPym();
    tofghit[i].p_m[2] = event[i].GetPzm();
    tofghit[i].pos_hit_slat = event[i].GetPos_hit_slat();
    tofghit[i].tof = event[i].GetTof();
    tofghit[i].dele = event[i].GetDele();
    tofghit[i].mctrack = event[i].GetMctrack();
          
  }
  w->SetRowCount(tofRows);

 return 0;
}

