#include <ZdcGeaHits.h>

#include <PHIODataNode.h>

#include <cstdlib>
#include <iostream>

using namespace std;

long ZdcGetGEA(PHCompositeNode* topNode)
{
  //
  // Find the GEA Node if it exists; otherwise exit on error
  //
  PHNodeIterator iter(topNode);
  PHCompositeNode *geaNode =
      static_cast<PHCompositeNode*>( iter.findFirst("PHCompositeNode", "GEA") );
  if (!geaNode) {
    cerr << PHWHERE << "unable to find GEA node; program exiting " << endl;
    exit(1);
  }
 
  //
  // Instantiate the GEA table for this subsystem, and attach it to
  // the GEA "directory" node.
  ZdcGeaHits *zdcgea = 0;
  PHTypedNodeIterator<ZdcGeaHits> zdcgeaiter(geaNode);
  PHIODataNode<ZdcGeaHits> *ZdcGeaNode = zdcgeaiter.find("ZdcGeaHits");
  if (ZdcGeaNode)
    {
      zdcgea = ZdcGeaNode->getData();
    }
  if (!zdcgea)
    {
      cerr << PHWHERE << "unable to find ZdcGeaHits object" ;
      return -1;
    }

  //
  // Get the Zdc Hit Event pointer directly
  //
  zdcgea->set_nhits( ZdcPISAHit::GetZdcCount() );
  zdcgea->set_ZdcPISAHits( ZdcPISAHit::GetZdcHitEvt() );

  return 0;
}

