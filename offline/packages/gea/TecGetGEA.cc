#include <TecPISAHit.h>
#include <tecghitWrapper.h>

#include <getClass.h>

#include <cstdlib>
#include <iostream>

using namespace std;

long
TecGetGEA(PHCompositeNode* topNode)
{

  TecPISAHit *event = TecPISAHit::GetTecHitEvt();
  Int_t tecRows = TecPISAHit::GetTecCount();    // variable number of rows

  tecghitWrapper* w = findNode::getClass<tecghitWrapper>(topNode,"tecghit");
    if (!w) {
      cerr << "\n TecGetGEA<E>: unable to find tecghitWrapper pointer; " ;
      cerr << " program is exiting at this point " << endl;
      exit(1);
    }
    TECGHIT_ST* tecghit = w->TableData();

  for(int i=0; i<tecRows; i++) {
    tecghit[i].xyzinloc[0]  = event[i].GetXin();
    tecghit[i].xyzinloc[1]  = event[i].GetYin();
    tecghit[i].xyzinloc[2]  = event[i].GetZin();
    tecghit[i].tof          = event[i].GetTof();
    tecghit[i].xyzoutloc[0] = event[i].GetXout();
    tecghit[i].xyzoutloc[1] = event[i].GetYout();
    tecghit[i].xyzoutloc[2] = event[i].GetZout();
    tecghit[i].arm          = event[i].GetIarm();
    tecghit[i].id           = i;  // December 30, 1999 change for multi-file input
    //
    // October 4, 1999: PISA output has planes counted from 1
    //
    tecghit[i].plane        = event[i].GetPlane() - 1;  // count from 0 in PHOOL
    tecghit[i].mctrack      = event[i].GetMctrack();
    tecghit[i].sector       = event[i].GetSector();
    tecghit[i].dedx         = event[i].GetDeDx();
    tecghit[i].xyzinglo[0]  = event[i].GetXing();
    tecghit[i].xyzinglo[1]  = event[i].GetYing();
    tecghit[i].xyzinglo[2]  = event[i].GetZing();
  }
  w->SetRowCount(tecRows);

 return 0;
}
