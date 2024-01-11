#include "CrkPISAHit.h"
#include "crkghitWrapper.h"
//INCLUDECHECKER: Removed this line: #include "CrkGetGEA.h"

#include "getClass.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

//INCLUDECHECKER: Removed this line: #include <iostream>
using namespace std;

long
CrkGetGEA(PHCompositeNode* topNode)
{

  CrkPISAHit *event = CrkPISAHit::GetCrkHitEvt();
  Int_t crkRows = CrkPISAHit::GetCrkCount();    // variable number of rows

  // Instantiate the GEA table for this subsystem, and attach it to
  // the GEA "directory" node.
  crkghitWrapper* w = findNode::getClass<crkghitWrapper>(topNode,"crkghit");
  w->SetMaxRowCount(crkRows);

  CRKGHIT_ST* crkghit = w->TableData();

  for(int i=0; i<crkRows; i++) {
    crkghit[i].x      = event[i].GetX();
    crkghit[i].y      = event[i].GetY();
    crkghit[i].z      = event[i].GetZ();
    crkghit[i].px     = event[i].GetPx();
    crkghit[i].py     = event[i].GetPy();
    crkghit[i].pz     = event[i].GetPz();
    crkghit[i].tof    = event[i].GetTof();
    crkghit[i].bp1    = event[i].GetBp1(); 
    crkghit[i].bp2    = event[i].GetBp2();
    crkghit[i].pid    = event[i].GetPid();
    crkghit[i].parent = event[i].GetParent();
    crkghit[i].pmt    = event[i].GetPmt();
    crkghit[i].tra    = event[i].GetNtrack();
    crkghit[i].nbf    = event[i].GetNbf(); 
    crkghit[i].bi1    = event[i].GetBi1();    
    crkghit[i].bi2    = event[i].GetBi2(); 
  }
  w->SetRowCount(crkRows);

 return 0;
}
