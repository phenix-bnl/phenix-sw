#include <Fun4AllReturnCodes.h>
#include "mEmcGeaPatchSimulatedCluster.h"
#include "emcNodeHelper.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include <iostream>
#include <cassert>
#include "phool.h"

using std::cerr;
using std::cout;
using std::endl;

//_____________________________________________________________________________
mEmcGeaPatchSimulatedCluster::mEmcGeaPatchSimulatedCluster(): SubsysReco("mEmcGeaPatchSimulatedCluster")
{
}

//_____________________________________________________________________________
mEmcGeaPatchSimulatedCluster::~mEmcGeaPatchSimulatedCluster()
{
}

//_____________________________________________________________________________
int
mEmcGeaPatchSimulatedCluster::process_event(PHCompositeNode* topNode)
{
  emcNodeHelper nh;

  PHCompositeNode* dstNode = emcNodeHelper::findCompositeNode(topNode,"DST");

  if (!dstNode)
    {
      cerr << PHWHERE << " Could not find DST node !" << endl;
      return ABORTRUN;
    }

  emcClusterContainer* clusters = 
    nh.getObject<emcClusterContainer>("emcClusterContainer",dstNode);

  if (!clusters)
    {
      cerr << PHWHERE << " Could not find emcClusterContainer !" << endl;
      return ABORTRUN;
    }
  
  for ( size_t i = 0; i < clusters->size(); ++i )
    {
      emcClusterContent* c = clusters->getCluster(i);
      assert(c!=0);
      // indicate that clusters are simulated, i.e. Fraction of simulated
      // energy = 1 (100%)
      c->set_simfrac(1);    
    }

  return EVENT_OK;
}
