#include "TfwPISAHit.h"
#include "tfwghitWrapper.h"

#include "getClass.h"

using namespace std;

long
TfwGetGEA(PHCompositeNode* topNode)
{
  
  TfwPISAHit *event = TfwPISAHit::GetTfwHitEvt();
  Int_t tfwRows = TfwPISAHit::GetTfwCount();    // variable number of rows

  tfwghitWrapper* w = findNode::getClass<tfwghitWrapper>(topNode,"tfwghit");
  w->SetMaxRowCount(tfwRows);

  TFWGHIT_ST* tfwghit = w->TableData();

  for(int i=0; i<tfwRows; i++) {

    for(int j=0; j<3; j++) {
      tfwghit[i].xyzinglo[j] = event[i].get_xyzinglo(j);
      tfwghit[i].xyzinloc[j] = event[i].get_xyzinloc(j);
      tfwghit[i].xyzoutloc[j] = event[i].get_xyzoutloc(j);
    }
    tfwghit[i].tof = event[i].GetTof();
    tfwghit[i].pathLength = event[i].GetPathLength();
    tfwghit[i].dele = event[i].GetDedx();
    tfwghit[i].panel = event[i].GetPanel();
    tfwghit[i].idpart = event[i].GetId();
    tfwghit[i].mctrack = event[i].GetMctrack();
    tfwghit[i].nFile = event[i].GetNfile();
    tfwghit[i].track = event[i].GetNtrack();   // GEANT's track-in-subevent number
    tfwghit[i].isubevent = event[i].GetIsubevent();
 
  }
  w->SetRowCount(tfwRows);

 return 0;
}

