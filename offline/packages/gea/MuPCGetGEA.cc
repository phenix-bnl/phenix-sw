#include "MuPCPISAHit.h"
#include "mupcghitWrapper.h"
//INCLUDECHECKER: Removed this line: #include "MuPCGetGEA.h"

#include "getClass.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

//INCLUDECHECKER: Removed this line: #include <iostream>

using namespace std;

long
MuPCGetGEA(PHCompositeNode* topNode)
{
  MuPCPISAHit *event = MuPCPISAHit::GetMuPC1HitEvt();
  Int_t mupc1Rows = MuPCPISAHit::GetMuPC1Count();

  mupcghitWrapper* w = findNode::getClass<mupcghitWrapper>(topNode,"mupc1ghit");
  w->SetMaxRowCount(mupc1Rows);

  MUPCGHIT_ST* mupc1ghit = w->TableData();

  for(int i=0; i<mupc1Rows; i++) {
    mupc1ghit[i].xyzinloc[0]  = event[i].GetXin();
    mupc1ghit[i].xyzinloc[1]  = event[i].GetYin();
    mupc1ghit[i].xyzinloc[2]  = event[i].GetZin();
    mupc1ghit[i].xyzoutloc[0] = event[i].GetXout();
    mupc1ghit[i].xyzoutloc[1] = event[i].GetYout();
    mupc1ghit[i].xyzoutloc[2] = event[i].GetZout();
    mupc1ghit[i].xyzinglo[0]  = event[i].GetXing();
    mupc1ghit[i].xyzinglo[1]  = event[i].GetYing();
    mupc1ghit[i].xyzinglo[2]  = event[i].GetZing();
    mupc1ghit[i].dedx         = event[i].GetDedx();
    mupc1ghit[i].tof          = event[i].GetTof();
    mupc1ghit[i].pathLength   = event[i].GetPathLength();
    mupc1ghit[i].id           = i;
    mupc1ghit[i].arm          = event[i].GetIarm();
    mupc1ghit[i].mctrack      = event[i].GetMctrack();    
  }
  w->SetRowCount(mupc1Rows);

  event = MuPCPISAHit::GetMuPC2HitEvt();
  Int_t mupc2Rows = MuPCPISAHit::GetMuPC2Count();

  w = findNode::getClass<mupcghitWrapper>(topNode,"mupc2ghit");
  w->SetMaxRowCount(mupc2Rows);

  MUPCGHIT_ST* mupc2ghit = w->TableData();

  for(int i=0; i<mupc2Rows; i++) {
    mupc2ghit[i].xyzinloc[0]  = event[i].GetXin();
    mupc2ghit[i].xyzinloc[1]  = event[i].GetYin();
    mupc2ghit[i].xyzinloc[2]  = event[i].GetZin();
    mupc2ghit[i].xyzoutloc[0] = event[i].GetXout();
    mupc2ghit[i].xyzoutloc[1] = event[i].GetYout();
    mupc2ghit[i].xyzoutloc[2] = event[i].GetZout();
    mupc2ghit[i].xyzinglo[0]  = event[i].GetXing();
    mupc2ghit[i].xyzinglo[1]  = event[i].GetYing();
    mupc2ghit[i].xyzinglo[2]  = event[i].GetZing();
    mupc2ghit[i].dedx         = event[i].GetDedx();
    mupc2ghit[i].tof          = event[i].GetTof();
    mupc2ghit[i].pathLength   = event[i].GetPathLength();
    mupc2ghit[i].id           = i;
    mupc2ghit[i].arm          = event[i].GetIarm();
    mupc2ghit[i].mctrack      = event[i].GetMctrack();    
  }
  w->SetRowCount(mupc2Rows);

  event = MuPCPISAHit::GetMuPC3HitEvt();
  Int_t mupc3Rows = MuPCPISAHit::GetMuPC3Count();

  w = findNode::getClass<mupcghitWrapper>(topNode,"mupc3ghit");
  w->SetMaxRowCount(mupc3Rows);

  MUPCGHIT_ST* mupc3ghit = w->TableData();

  for(int i=0; i<mupc3Rows; i++) {
    mupc3ghit[i].xyzinloc[0]  = event[i].GetXin();
    mupc3ghit[i].xyzinloc[1]  = event[i].GetYin();
    mupc3ghit[i].xyzinloc[2]  = event[i].GetZin();
    mupc3ghit[i].xyzoutloc[0] = event[i].GetXout();
    mupc3ghit[i].xyzoutloc[1] = event[i].GetYout();
    mupc3ghit[i].xyzoutloc[2] = event[i].GetZout();
    mupc3ghit[i].xyzinglo[0]  = event[i].GetXing();
    mupc3ghit[i].xyzinglo[1]  = event[i].GetYing();
    mupc3ghit[i].xyzinglo[2]  = event[i].GetZing();
    mupc3ghit[i].dedx         = event[i].GetDedx();
    mupc3ghit[i].tof          = event[i].GetTof();
    mupc3ghit[i].pathLength   = event[i].GetPathLength();
    mupc3ghit[i].id           = i;
    mupc3ghit[i].arm          = event[i].GetIarm();
    mupc3ghit[i].mctrack      = event[i].GetMctrack();    
  }
  w->SetRowCount(mupc3Rows);

 return 0;
}

