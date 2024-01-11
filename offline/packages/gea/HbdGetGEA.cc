#include <iostream>
#include <stdlib.h>
#include "PHIODataNode.h"
#include "HbdPISAHit.h"
#include "hbdghitWrapper.h"

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

long
HbdGetGEA(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode), *j;
  PHCompositeNode* geaNode;
  PHNode *n2;                   // HbdGetDCM also defined n1 pointer
  TableNode_t *d;

  //
  // Find the GEA "directory" if it exists; otherwise exit on error
  //
  n2 = iter.findFirst("PHCompositeNode", "GEA");
  if (!n2) {
    cerr << "\n HbdGetGEA <E> unable to find GEA node; program exiting " << endl;
    exit(1);
  }
  else {
    geaNode = static_cast<PHCompositeNode*>(n2);
  }

  HbdPISAHit *event = HbdPISAHit::GetHbdHitEvt();
  Int_t hbdRows = HbdPISAHit::GetHbdCount();    // variable number of rows

  //
  // Instantiate the GEA table for this subsystem, and attach it to
  // the GEA "directory" node.
  //
  hbdghitWrapper* w;
  j = new PHNodeIterator(geaNode);
  d = static_cast<TableNode_t*>(j->findFirst("PHIODataNode","hbdghit"));
  if (!d) {
    cerr << "\n HbdGetGEA<E>: unable to find hbdghit STAF Table; " ;
    cerr << " program is exiting at this point " << endl;
    exit(1);
  }
  else {
    w = static_cast<hbdghitWrapper*>(d->getData());
    if (!w) {
      cerr << "\n HbdGetGEA<E>: unable to find HBD hbdghitWrapper pointer; " ;
      cerr << " program is exiting at this point " << endl;
      exit(1);
    }
    w->SetMaxRowCount(hbdRows);
  }
  delete j;

  HBDGHIT_ST* hbdghit = w->TableData();

  for(int i=0; i<hbdRows; i++) {
    hbdghit[i].xyzinloc[0]  = event[i].GetXin();
    hbdghit[i].xyzinloc[1]  = event[i].GetYin();
    hbdghit[i].xyzinloc[2]  = event[i].GetZin();
    hbdghit[i].xyzoutloc[0] = event[i].GetXout();
    hbdghit[i].xyzoutloc[1] = event[i].GetYout();
    hbdghit[i].xyzoutloc[2] = event[i].GetZout();
    hbdghit[i].tof          = event[i].GetTOF();
    hbdghit[i].id           = i;
    hbdghit[i].sector       = event[i].GetSect();
    hbdghit[i].detflag       = event[i].GetDetFlag();
    hbdghit[i].idpart       = event[i].GetIdPart();
    hbdghit[i].mctrack      = event[i].GetMctrack();    
  }
  w->SetRowCount(hbdRows);

 return 0;
}

