#include "mMpcExCreateNodeTree.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHIODataNode.h"
#include "Fun4AllReturnCodes.h"
#include "recoConsts.h"
#include "getClass.h"
#include "MpcExRawHit.h"
#include "MpcExRawHitv1.h"
#include "MpcExEventHeader.h"
#include "MpcExEventHeaderv1.h"
#include "TMpcExHitContainer.h"
#include "TMpcExCalibContainer.h"
#include "TMpcExGeaHitContainer.h"
#include "MpcExPISAEventHeader.h"
#include "MpcExPISAEventHeaderv1.h"
#include "MpcExPISAEventHeaderv2.h"

mMpcExCreateNodeTree::mMpcExCreateNodeTree() : SubsysReco("MMPCEXCREATENODETREE") {
}

mMpcExCreateNodeTree::~mMpcExCreateNodeTree(){
}

int mMpcExCreateNodeTree::InitRun(PHCompositeNode *topNode){

  //Create the DST NODE if it doesn't exist.
  PHNodeIterator nodeIter(topNode);
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode","DST"));
  if (dstNode == NULL)
    {
      dstNode = new PHCompositeNode("DST");
      topNode->addNode(dstNode);
    }

  // Instantiate nodes for mpcex containers
  PHCompositeNode *mpcexNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "MPCEX"));
  if(mpcexNode == NULL)
    {
      mpcexNode = new PHCompositeNode("MPCEX");
      dstNode->addNode(mpcexNode);
    }

  //these are the minipad-by-minipad calibrations
  TMpcExCalibContainer *calibs = new TMpcExCalibContainer();
  PHIODataNode<TMpcExCalibContainer>* calibNode = new PHIODataNode<TMpcExCalibContainer>(calibs,"TMpcExCalibContainer","PHObject");
  mpcexNode->addNode(calibNode);

  //these are the hits that people will use
  //the TMpcExHit object knows what "kind" it is
  //i.e. what level of calibration there is
  TMpcExHitContainer *hits = new TMpcExHitContainer();
  PHIODataNode<TMpcExHitContainer>* hitNode = new PHIODataNode<TMpcExHitContainer>(hits,"TMpcExHitContainer","PHObject");
  mpcexNode->addNode(hitNode);

  //if this is simulation, we will read in the NCCPisaHit node
  //into the key business and store it on TMpcExGeaHitContainer
  //we will then digitize this into the MpcExRawHitNode and
  //create a fake MpcExEventHeader so that the simulated data 
  //looks as much like real data as possible.
  recoConsts *flags = recoConsts::instance();
  if(flags->FlagExist("SIMULATIONFLAG")) 
    {
      TMpcExGeaHitContainer *geahits = findNode::getClass<TMpcExGeaHitContainer>(topNode,"TMpcExGeaHitContainer");
      if(geahits == NULL)
	{
	  geahits = new TMpcExGeaHitContainer();
	  PHIODataNode<TMpcExGeaHitContainer>* geahitNode = new PHIODataNode<TMpcExGeaHitContainer>(geahits,"TMpcExGeaHitContainer","PHObject");
	  dstNode->addNode(geahitNode);
	}

      MpcExRawHit *rawhits = findNode::getClass<MpcExRawHit>(topNode,"MpcExRawHit");
      if(rawhits == NULL)
	{
	  rawhits = new MpcExRawHitv1();
	  PHIODataNode<MpcExRawHit>* rawhitNode = new PHIODataNode<MpcExRawHit>(rawhits,"MpcExRawHit","PHObject");
	  dstNode->addNode(rawhitNode);
      }

      MpcExEventHeader *header = findNode::getClass<MpcExEventHeader>(topNode,"MpcExEventHeader");
      if(header == NULL)
	{
	  header = new MpcExEventHeaderv1();
	  PHIODataNode<MpcExEventHeader>* headerNode = new PHIODataNode<MpcExEventHeader>(header,"MpcExEventHeader","PHObject");
	  dstNode->addNode(headerNode);
	}

      MpcExPISAEventHeader *PISAheader = findNode::getClass<MpcExPISAEventHeader>(topNode,"MpcExPISAEventHeader");
      if(PISAheader == NULL)
	{
	  //PISAheader = new MpcExPISAEventHeaderv1();
	  PISAheader = new MpcExPISAEventHeaderv2();
	  PHIODataNode<MpcExPISAEventHeader>* PISAheaderNode = new PHIODataNode<MpcExPISAEventHeader>(PISAheader,"MpcExPISAEventHeader","PHObject");
	  dstNode->addNode(PISAheaderNode);
	}


    }//if this is a simulated event

  return EVENT_OK;
}
