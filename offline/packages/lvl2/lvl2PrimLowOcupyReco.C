#include <cassert>
#include <PHCompositeNode.h>
#include <lvl2PrimLowOcupyReco.h>
#include <PHIODataNode.h>
#include <TClonesArray.h>
#include <Lvl2OutArray.h>
#include <Lvl2Event.h>

#include <L2ElecCanddtLowOcupyMicrov1.h>
#include <L2ElecInvMassLowOcupyMicroV1.h>
#include <L2TrackPC3LowOcupyMicroV1.h>
#include <L2TrackEMCLowOcupyMicroV1.h>
#include <L2PC1PC3TrackEMCAssocMicroV1.h>

#include <L2ElecCanddtLowOcupy.h>
#include <L2ElecInvMassLowOcupy.h>
#include <L2TwoPointTrackerEMC.h>
#include <L2TwoPointTrackerPC3.h>
#include <L2EMCAssociator.h>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<Lvl2OutArray> Lvl2OutArrayNode_t;

typedef PHIODataNode<L2ElecCanddtLowOcupyDST> MicroL2ElecCanddtLowOcupyNode_t;
typedef PHIODataNode<L2TrackEMCLowOcupyDST> MicroL2TrackEMCLowOcupyNode_t;
typedef PHIODataNode<L2TrackPC3LowOcupyDST> MicroL2TrackPC3LowOcupyNode_t;
typedef PHIODataNode<L2ElecInvMassLowOcupyDST> MicroL2ElecInvMassLowOcupyNode_t;
typedef PHIODataNode<L2PC1PC3TrackEMCAssocDST> MicroL2PC1PC3TrackEMCAssocNode_t;

static Lvl2OutArray *lvl2outarray = 0;

//___________________________________
lvl2PrimLowOcupyReco::lvl2PrimLowOcupyReco()
{
  ThisName = "lvl2PrimLowOcupy";
  return ;
}

//___________________________________
int lvl2PrimLowOcupyReco::Init(PHCompositeNode *topNode)
{
  cout << "Initialize the lvl2PrimitiveLowOcupy..." << endl;


  // Put these guys on the DST node so that they get written to the file

  // First find the dstNode  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "lvl2PrimLowOcupyReco::Init: no DST node, create one! " << endl;
      dstNode = new PHCompositeNode("DST");
      topNode->addNode(dstNode);
    }

  //  L2ElecCanddtLowOcupy
  //
  L2ElecCanddtLowOcupyDST* L2SnglElec = new L2ElecCanddtLowOcupyMicrov1();
  PHObjectNode_t *L2SnglElecNode = new PHIODataNode<PHObject>(L2SnglElec, "L2ElecCanddtLowOcupyNano", "PHObject");
  dstNode->addNode(L2SnglElecNode);

  //  L2ElecInvMassLowOcupy
  //
  L2ElecInvMassLowOcupyDST* L2ElecPair = new L2ElecInvMassLowOcupyMicroV1();
  PHObjectNode_t *L2ElecPairNode = new PHIODataNode<PHObject>(L2ElecPair, "L2ElecInvMassLowOcupyNano", "PHObject");
  dstNode->addNode(L2ElecPairNode);

  // L2TwoPointTrackerEMC
  //
  L2TrackEMCLowOcupyDST* L2TrackEMC = new L2TrackEMCLowOcupyMicroV1();
  PHObjectNode_t *L2TrackEMCNode = new PHIODataNode<PHObject>(L2TrackEMC, "L2TrackEMCLowOcupyNano", "PHObject");
  dstNode->addNode(L2TrackEMCNode);

  // L2TwoPointTrackerPC3
  //
  L2TrackPC3LowOcupyDST* L2TrackPC3 = new L2TrackPC3LowOcupyMicroV1();
  PHObjectNode_t *L2TrackPC3Node = new PHIODataNode<PHObject>(L2TrackPC3, "L2TrackPC3LowOcupyNano", "PHObject");
  dstNode->addNode(L2TrackPC3Node);

  // L2PC1PC3TrackEMCAssociator
  //
  L2PC1PC3TrackEMCAssocDST* L2PC1PC3TrackEMCAssoc = new L2PC1PC3TrackEMCAssocMicroV1();
  PHObjectNode_t *L2PC1PC3TrackEMCAssocNode = new PHIODataNode<PHObject>(L2PC1PC3TrackEMCAssoc, "L2PC1PC3TrackEMCAssocNano", "PHObject");
  dstNode->addNode(L2PC1PC3TrackEMCAssocNode);

 return 0;
}

//___________________________________
int lvl2PrimLowOcupyReco::InitRun(PHCompositeNode *topNode)
{
  return 0;
}

//___________________________________________________
// do lvl2 read back for all lvl2 trigger primitives
//
int lvl2PrimLowOcupyReco::process_lvl2ReadBackLowOcupy(PHCompositeNode* topNode)
{
  // Find the Lvl2OutArray on the dstNode

  // First find the dstNode  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "lvl2PrimLowOcupyReco::process_lvl2ReadBackLowOcupy: Did not find dstNode - do nothing and return" << endl; 
    }

  PHTypedNodeIterator<Lvl2OutArray> l2iter(dstNode);
  Lvl2OutArrayNode_t *lvl2node = l2iter.find("Lvl2OutArray");

  if (!lvl2node)
  {
      cout << "process_lvl2ReadBack: no Lvl2OutArray Node" << endl;
      return -1;
  }

  lvl2outarray = lvl2node->getData();

  if ( !lvl2outarray )
  {
      cout << "process_lvl2ReadBack: no Lvl2OutArray object" << endl;
      return -1;
  }

  return 0;
}

//________________________________________________________________
// copy L2TrackEMCLowOcupy from lvl2 readback to nDST/uDST class 
//
int lvl2PrimLowOcupyReco::process_L2TrackEMCLowOcupy(PHCompositeNode* topNode)
{
  // First find the dstNode  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "lvl2PrimLowOcupyReco::process_L2TrackEMCLowOcupy: Did not find dstNode - do nothing and return" << endl; 
    }

  L2TrackEMCLowOcupyDST* L2TrackEMC = NULL;
  PHTypedNodeIterator<L2TrackEMCLowOcupyDST> diter(dstNode);
  MicroL2TrackEMCLowOcupyNode_t* L2TrackEMCMicroNode = diter.find("L2TrackEMCLowOcupyNano");
  if (L2TrackEMCMicroNode)
  {
      L2TrackEMC = L2TrackEMCMicroNode->getData();
  } else {
      cout << PHWHERE << "ERROR: Node L2TrackEMCLowOcupyNano not found"<<endl;
      return -1;
  }

  if (!lvl2outarray->GetPrimitive( "L2TwoPointTrackerEMC"))
  {
      cout<<" no primitive L2TwoPointTrackerEMC"<<endl;
      return -1;
  }

  L2TwoPointTrackerEMCRBP Tracklvl2(0);

  unsigned int ncandidates = Tracklvl2->tracks.size();

  L2TrackEMC->set_TClonesArraySize(ncandidates);
  L2TrackEMC->set_NumTracks(ncandidates);

  for (unsigned int icand = 0; icand < ncandidates; icand++)
  { 
      L2TrackEMC->AddL2TrackEMC(icand);

      L2TrackEMC->set_arm(icand, Tracklvl2->tracks[icand].arm);
      L2TrackEMC->set_alpha(icand, Tracklvl2->tracks[icand].alpha);
      L2TrackEMC->set_beta(icand, Tracklvl2->tracks[icand].beta);
      L2TrackEMC->set_p(icand, Tracklvl2->tracks[icand].p);
      L2TrackEMC->set_pxdet(icand, Tracklvl2->tracks[icand].pxdet);
      L2TrackEMC->set_pydet(icand, Tracklvl2->tracks[icand].pydet);
      L2TrackEMC->set_pzdet(icand, Tracklvl2->tracks[icand].pzdet);
      L2TrackEMC->set_pxphys(icand, Tracklvl2->tracks[icand].pxphys);
      L2TrackEMC->set_pyphys(icand, Tracklvl2->tracks[icand].pyphys);
      L2TrackEMC->set_pzphys(icand, Tracklvl2->tracks[icand].pzphys);
      L2TrackEMC->set_zvertex(icand, Tracklvl2->tracks[icand].zvertex);
      L2TrackEMC->set_xdet(icand, Tracklvl2->tracks[icand].xdet);
      L2TrackEMC->set_ydet(icand, Tracklvl2->tracks[icand].ydet);
      L2TrackEMC->set_ydet(icand, Tracklvl2->tracks[icand].ydet);
      L2TrackEMC->set_zdet(icand, Tracklvl2->tracks[icand].zdet);
      L2TrackEMC->set_xdetout(icand, Tracklvl2->tracks[icand].xdetout);
      L2TrackEMC->set_ydetout(icand, Tracklvl2->tracks[icand].ydetout);
      L2TrackEMC->set_zdetout(icand, Tracklvl2->tracks[icand].zdetout);
      L2TrackEMC->set_theta0(icand, Tracklvl2->tracks[icand].theta0);
      L2TrackEMC->set_phi0(icand, Tracklvl2->tracks[icand].phi0);
      L2TrackEMC->set_charge(icand, Tracklvl2->tracks[icand].charge);
      L2TrackEMC->set_tileID(icand, Tracklvl2->tracks[icand].tileID);
      L2TrackEMC->set_energy(icand, Tracklvl2->tracks[icand].energy);
  }

  return 0;
}

//________________________________________________________________
// copy L2TrackPC3LowOcupy from lvl2 readback to nDST/uDST class 
//
int lvl2PrimLowOcupyReco::process_L2TrackPC3LowOcupy(PHCompositeNode* topNode)
{
  // First find the dstNode  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "lvl2PrimLowOcupyReco::process_L2TrackEMCLowOcupy: Did not find dstNode - do nothing and return" << endl; 
    }

  L2TrackPC3LowOcupyDST* L2TrackPC3 = NULL;
  //PHTypedNodeIterator<L2TrackPC3LowOcupyDST> diter(topNode);
  PHTypedNodeIterator<L2TrackPC3LowOcupyDST> diter(dstNode);
  MicroL2TrackPC3LowOcupyNode_t* L2TrackPC3MicroNode = diter.find("L2TrackPC3LowOcupyNano");
  if (L2TrackPC3MicroNode)
  {
      L2TrackPC3 = L2TrackPC3MicroNode->getData();
  } else {
      cout << PHWHERE << "ERROR: Node L2TrackPC3LowOcupyNano not found"<<endl;
      return -1;
  }

  if (!lvl2outarray->GetPrimitive("L2TwoPointTrackerPC3"))
  {
      cout<<" no primitive L2TwoPointTrackerPC3"<<endl;
      return -1;
  }

  L2TwoPointTrackerPC3RBP Tracklvl2(0);

  unsigned int ncandidates = Tracklvl2->tracks.size();

  L2TrackPC3->set_TClonesArraySize(ncandidates);
  L2TrackPC3->set_NumTracks(ncandidates);

  for (unsigned int icand = 0; icand < ncandidates; icand++)
  { 
      L2TrackPC3->AddL2TrackPC3(icand);

      L2TrackPC3->set_arm(icand, Tracklvl2->tracks[icand].arm);
      L2TrackPC3->set_alpha(icand, Tracklvl2->tracks[icand].alpha);
      L2TrackPC3->set_beta(icand, Tracklvl2->tracks[icand].beta);
      L2TrackPC3->set_p(icand, Tracklvl2->tracks[icand].p);
      L2TrackPC3->set_pxdet(icand, Tracklvl2->tracks[icand].pxdet);
      L2TrackPC3->set_pydet(icand, Tracklvl2->tracks[icand].pydet);
      L2TrackPC3->set_pzdet(icand, Tracklvl2->tracks[icand].pzdet);
      L2TrackPC3->set_pxphys(icand, Tracklvl2->tracks[icand].pxphys);
      L2TrackPC3->set_pyphys(icand, Tracklvl2->tracks[icand].pyphys);
      L2TrackPC3->set_pzphys(icand, Tracklvl2->tracks[icand].pzphys);
      L2TrackPC3->set_zvertex(icand, Tracklvl2->tracks[icand].zvertex);
      L2TrackPC3->set_xdet(icand, Tracklvl2->tracks[icand].xdet);
      L2TrackPC3->set_ydet(icand, Tracklvl2->tracks[icand].ydet);
      L2TrackPC3->set_ydet(icand, Tracklvl2->tracks[icand].ydet);
      L2TrackPC3->set_zdet(icand, Tracklvl2->tracks[icand].zdet);
      L2TrackPC3->set_xdetout(icand, Tracklvl2->tracks[icand].xdetout);
      L2TrackPC3->set_ydetout(icand, Tracklvl2->tracks[icand].ydetout);
      L2TrackPC3->set_zdetout(icand, Tracklvl2->tracks[icand].zdetout);
      L2TrackPC3->set_theta0(icand, Tracklvl2->tracks[icand].theta0);
      L2TrackPC3->set_phi0(icand, Tracklvl2->tracks[icand].phi0);
      L2TrackPC3->set_charge(icand, Tracklvl2->tracks[icand].charge);
      L2TrackPC3->set_regionPC3(icand, Tracklvl2->tracks[icand].regionPC3);
      L2TrackPC3->set_clustIDPC3(icand, Tracklvl2->tracks[icand].clustIDPC3);
  }


  return 0;
}

//________________________________________________________________
// copy L2ElecCanddtLowOcupy from lvl2 readback to nDST/uDST class 
//
int lvl2PrimLowOcupyReco::process_L2ElecCanddtLowOcupy(PHCompositeNode* topNode)
{
  // First find the dstNode  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "lvl2PrimLowOcupyReco::process_L2TrackEMCLowOcupy: Did not find dstNode - do nothing and return" << endl; 
    }

  L2ElecCanddtLowOcupyDST* L2SnglElec = NULL;
  //PHTypedNodeIterator<L2ElecCanddtLowOcupyDST> diter(topNode);
  PHTypedNodeIterator<L2ElecCanddtLowOcupyDST> diter(dstNode);
  MicroL2ElecCanddtLowOcupyNode_t* L2ElecCandMicroNode = diter.find("L2ElecCanddtLowOcupyNano");
  if (L2ElecCandMicroNode)
  {
      L2SnglElec = L2ElecCandMicroNode->getData();
  } else {
      cout << PHWHERE << "ERROR: Node L2ElecCanddtLowOcupyNano not found"<<endl;
      return -1;
  }

  if (!lvl2outarray->GetPrimitive( "L2ElecCanddtLowOcupy"))
  {
      cout<<" no primitive L2ElecCanddtLowOcupy"<<endl;
      return -1;
  }

  L2ElecCanddtLowOcupyRBP elvl2(0);

  unsigned int ncandidates = elvl2->Candidate.size();

  L2SnglElec->set_TClonesArraySize(ncandidates);
  L2SnglElec->set_NumCandidate(ncandidates);

  for (unsigned int icand = 0; icand < ncandidates; icand++)
  { 
      L2SnglElec->AddL2ElecCandidate(icand);

      L2SnglElec->set_charge(icand, elvl2->Candidate[icand].charge);
      L2SnglElec->set_theta0(icand, elvl2->Candidate[icand].theta0);
      L2SnglElec->set_phi0(icand, elvl2->Candidate[icand].phi0);
      L2SnglElec->set_ptot(icand, elvl2->Candidate[icand].ptot);
      L2SnglElec->set_xrich(icand, elvl2->Candidate[icand].xrich);
      L2SnglElec->set_yrich(icand, elvl2->Candidate[icand].yrich);
      L2SnglElec->set_zrich(icand, elvl2->Candidate[icand].zrich);
      L2SnglElec->set_pmtCent(icand, elvl2->Candidate[icand].pmtCent);
      L2SnglElec->set_npmt(icand, elvl2->Candidate[icand].npmt);
      L2SnglElec->set_npe(icand, elvl2->Candidate[icand].npe);
      L2SnglElec->set_TrkRingDist(icand, elvl2->Candidate[icand].TrkRingDist);
      L2SnglElec->set_xemc(icand, elvl2->Candidate[icand].xemc);
      L2SnglElec->set_yemc(icand, elvl2->Candidate[icand].yemc);
      L2SnglElec->set_zemc(icand, elvl2->Candidate[icand].zemc);
      L2SnglElec->set_energy(icand, elvl2->Candidate[icand].energy);
      L2SnglElec->set_xpc1(icand, elvl2->Candidate[icand].xpc1);
      L2SnglElec->set_ypc1(icand, elvl2->Candidate[icand].ypc1);
      L2SnglElec->set_zpc1(icand, elvl2->Candidate[icand].zpc1);
  }
  return 0;
}

//________________________________________________________________
// copy L2ElecInvMassLowOcupy from lvl2 readback to nDST/uDST class 
//
int lvl2PrimLowOcupyReco::process_L2ElecInvMassLowOcupy(PHCompositeNode* topNode)
{
  // First find the dstNode  
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "lvl2PrimLowOcupyReco::process_L2TrackEMCLowOcupy: Did not find dstNode - do nothing and return" << endl; 
    }

  L2ElecInvMassLowOcupyDST* L2ElecPair = NULL;
  //PHTypedNodeIterator<L2ElecInvMassLowOcupyDST> diter(topNode);
  PHTypedNodeIterator<L2ElecInvMassLowOcupyDST> diter(dstNode);
  MicroL2ElecInvMassLowOcupyNode_t* L2ElecPairMicroNode = diter.find("L2ElecInvMassLowOcupyNano");
  if (L2ElecPairMicroNode)
  {
     L2ElecPair = L2ElecPairMicroNode->getData();
  } else {
     cout << PHWHERE << "ERROR: Node L2ElecInvMassLowOcupyNano not found"<<endl;
     return -1;
  }

  if (!lvl2outarray->GetPrimitive("L2ElecInvMassLowOcupy"))
  {
      cout<<" no primitive L2ElecInvMassLowOcupy"<<endl;
      return -1;
  }

  L2ElecInvMassLowOcupyRBP epairlvl2(0);

  unsigned int ncandidates = epairlvl2->ElectronPair.size();

  L2ElecPair->set_TClonesArraySize(ncandidates);
  L2ElecPair->set_NumCandidate(ncandidates);

  for (unsigned int icand = 0; icand < ncandidates; icand++)
  { 
      L2ElecPair->AddL2ElecCandidate(icand);

      L2ElecPair->set_KilledByChargeID(icand, epairlvl2->ElectronPair[icand].KilledByChargeID);
      L2ElecPair->set_Mass(icand, epairlvl2->ElectronPair[icand].Mass);
      L2ElecPair->set_Pt(icand, epairlvl2->ElectronPair[icand].Pt);
      L2ElecPair->set_candID0(icand, epairlvl2->ElectronPair[icand].candID0);
      L2ElecPair->set_candID1(icand, epairlvl2->ElectronPair[icand].candID1);
  }
  return 0;
}

//________________________________________________________________
// copy L2PC1PC3TrackEMC from lvl2 readback to nDST/uDST class 
//
int lvl2PrimLowOcupyReco::process_L2PC1PC3TrackEMCAssoc(PHCompositeNode* topNode)
{
  // First find the dstNode  
  //
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  assert(dstNode);

  L2PC1PC3TrackEMCAssocDST* L2TrackEMCAssoc_ptr = 0;
  PHTypedNodeIterator<L2PC1PC3TrackEMCAssocDST> diter(dstNode);
  MicroL2PC1PC3TrackEMCAssocNode_t* L2TrackEMCAssocMicroNode = diter.find("L2PC1PC3TrackEMCAssocNano");
  assert(L2TrackEMCAssocMicroNode != 0);

  L2TrackEMCAssoc_ptr = L2TrackEMCAssocMicroNode->getData();
  if (!lvl2outarray->GetPrimitive("L2EMCAssociator"))
  {
      cout<<" no primitive L2EMCAssociate"<<endl;
      return -1;
  }

  L2EMCAssociatorRBP primitive(0);

  //  The cast is really ugly but I don't off hand know any way around it.
  //     The void* interface is necessary for ROOT purposes but void* doesn't
  //     preserve the const. However in the implementation we static_cast back
  //     to a const ... *
  //
  //  (and yes I am too proud to put in a reinterpret_cast)
  //
  L2TrackEMCAssoc_ptr->storeLvl2Primitives((void*) primitive.operator->());

  return 0;
}



//_______________________________________________________________
// copy lvl2 primitive into their corresponding nDST/uDST class 
//
int lvl2PrimLowOcupyReco::process_event(PHCompositeNode *topNode)
{
  if ( process_lvl2ReadBackLowOcupy(topNode) != 0 ) 
    {
      cout << "process_lvl2Primitive: failed to get Lvl2OutArray object" << endl;
      return -1;
    }

  process_L2ElecCanddtLowOcupy(topNode);
  process_L2ElecInvMassLowOcupy(topNode);
  process_L2TrackEMCLowOcupy(topNode);
  process_L2TrackPC3LowOcupy(topNode);
  process_L2PC1PC3TrackEMCAssoc(topNode);

  return 0;
}




