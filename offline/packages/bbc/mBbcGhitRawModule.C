#include "mBbcGhitRawModule.h"
//INCLUDECHECKER: Removed this line: #include "dBbcGhitRawParWrapper.h"
//INCLUDECHECKER: Removed this line: #include "dBbcGeoWrapper.h"
//INCLUDECHECKER: Removed this line: #include "dBbcUcalWrapper.h"
//INCLUDECHECKER: Removed this line: #include "bbcghitWrapper.h"
//INCLUDECHECKER: Removed this line: #include "dBbcGhitRawWrapper.h"
//INCLUDECHECKER: Removed this line: #include "dBbcRawWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNode.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
#include "PHNodeIterator.h"
//INCLUDECHECKER: Removed this line: #include "PHPointerList.h"

#include <iostream>

using namespace std;

PHBoolean
mBbcGhitRawModule::event(PHCompositeNode *root)
{
  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root);
  PHNode *n;

  // Insert code here to navigate node hierarchy and find
  // or create specific nodes to pass to physics module...

  n = i.findFirst("PHIODataNode", "dBbcGhitRawPar");
  if (!n)
    {
      cout << PHWHERE << " Cannot find dBbcGhitRawPar Node" << endl;
    }
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dBbcGeo");
  if (!n)
    {
      cout << PHWHERE << " Cannot find dBbcGeo table" << endl;
    }
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dBbcUcal");
  if (!n)
    {
      cout << PHWHERE << " Cannot find dBbcUcal table" << endl;
    }
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "bbcghit");
  if (!n)
    {
      cout << PHWHERE << "Cannot find bbcghit table" << endl;
    }
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dBbcGhitRaw");
  if (!n)
    {
      cout << "Cannot find dBbcGhitRaw table" << endl;
    }
  nodes.append(n);

  n = i.findFirst("PHIODataNode","dBbcRaw");
  if (!n)
    {
      cout << "Cannot find dBbcRaw table" << endl;
    }
  nodes.append(n);

  return callPAM(nodes);
}
