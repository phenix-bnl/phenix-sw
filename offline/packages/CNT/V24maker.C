#include "V24maker.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHTypedNodeIterator.h"
#include "PHIODataNode.h"
#include "Fun4AllReturnCodes.h"

#include "PHInclusiveNanoCuts.h"
#include "PHCentralTrack.h"
#include "PHCentralTrackv24.h"
#include "PHSnglCentralTrack.h"

#include "getClass.h"

#include <cassert>
#include <iostream>

using namespace std;
using namespace findNode;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHCentralTrack> PHParticleNode_t;

V24maker::V24maker(const char* aNodeName, const char* bNodeName): SubsysReco("V24")
{
  InputName = aNodeName;
  OutputName = bNodeName;
  return ;
}

V24maker::~V24maker()
{
  return ;
}

int V24maker::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  cout << "Initialize the " << ThisName << " Track Version Upgrade routine..." << endl;

  // First find the original "uncut" PHParticle object...
  PHCentralTrack *particle = getClass<PHCentralTrack>(topNode, InputName.c_str());
  if (!particle) {
    cout << ThisName << "Reco::Init()  Failed to find original (uncut) PHParticle object" << endl;
    return ABORTEVENT;
  }

  // Ask the uncut particle container for a clone of itself and place that in the node tree...
  // This latter one will hold particles selected as being electrons.
  PHCentralTrackv24 *WGparticles   = new PHCentralTrackv24(); // This is the output place...
  PHObjectNode_t *WGParticleNode   = new PHIODataNode<PHObject>(WGparticles,OutputName.c_str(),"PHObject");
  dstNode->addNode(WGParticleNode);

 return 0;

}

int V24maker::process_event(PHCompositeNode *topNode)
{

  // First, grab pointers to both the input and output particle lists:
  PHCentralTrack *particle   = getClass<PHCentralTrack>(topNode,  InputName.c_str());
  PHCentralTrack *WGparticle = getClass<PHCentralTrack>(topNode, OutputName.c_str());
  if (!particle)   { cout << ThisName << "Reco::process_event()  Original Particle not found" << endl; }
  if (!WGparticle) { cout << ThisName << "Reco::process_event()  Output Particle not found" << endl;   }

  // OK...now loop through the input particle lists and Add any particle that passes
  // the cuts to the output particle list...
  WGparticle->Reset();
  for (unsigned int in=0; in<particle->get_npart(); in++) 
    {
      PHSnglCentralTrack* copy = WGparticle->AddPHParticle(*(particle->get_track(in)) );
      assert(copy!=0);
    }      

  if (verbosity)
    {
      cout << " Original Particle Count: "<<   particle->get_npart() << endl;
      cout << "      V24 Particle Count: "<< WGparticle->get_npart() << endl;

      //  Test the validity of the copy
      for (unsigned int itr=0; itr<particle->get_npart(); itr++)
	{
	  cout << " mom = " << particle->get_mom(itr) <<", ";
	  cout <<            WGparticle->get_mom(itr) <<", diff=";
	  cout << particle->get_mom(itr)-WGparticle->get_mom(itr)<<endl;

	  cout << " the0 = " << particle->get_the0(itr) <<", ";
	  cout <<            WGparticle->get_the0(itr) <<", diff=";
	  cout << particle->get_the0(itr)-WGparticle->get_the0(itr)<<endl;

	  cout << " phi0 = " << particle->get_phi0(itr) <<", ";
	  cout <<            WGparticle->get_phi0(itr) <<", diff=";
	  cout << particle->get_phi0(itr)-WGparticle->get_phi0(itr)<<endl;

	}
    }

  return 0;
}

int V24maker::ResetEvent(PHCompositeNode *topNode)
{
  // We are resetting just prior to refilling...
  return 0;
}




