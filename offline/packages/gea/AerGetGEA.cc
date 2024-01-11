#include <AerGeaHits.h>
#include <PHIODataNode.h>
#include <PHTable.hh>

#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

long AerGetGEA(PHCompositeNode* topNode)
{
  
  // Find the GEA Node if it exists; otherwise exit on error
  PHNodeIterator iter(topNode);
  PHCompositeNode *geaNode =
      static_cast<PHCompositeNode*>( iter.findFirst("PHCompositeNode", "GEA") );
  if (!geaNode) {
    cerr << PHWHERE << "unable to find GEA node; program exiting " << endl;
    exit(1);
  }
 
  // Instantiate the GEA table for this subsystem, and attach it to
  // the GEA "directory" node.
  AerGeaHits *aergea = 0;
  PHTypedNodeIterator<AerGeaHits> iGEA(geaNode);
  PHIODataNode<AerGeaHits> *AerGeaNode = iGEA.find("AerGeaHits");
  if (AerGeaNode)
  {
    aergea = AerGeaNode->getData();
  }
  if (!aergea)
  {
    cerr << PHWHERE << "unable to find AerGeaHits object" ;
    return -1;
  }

  // Get the Aer Hit Event pointer directly
  aergea->set_nhits( AerPISAHit::GetAerCount() );
  aergea->set_AerPISAHits( AerPISAHit::GetAerHitEvt() );

  return 0;
}

