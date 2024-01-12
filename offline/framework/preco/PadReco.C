#include "PadReco.h"

#include <padEvtToRaw.hh>
#include <padInclBad.hh>
#include <PadRecModule.hh>
#include <dPadRawWrapper.h>
#include <dPadClusterWrapper.h>
#include <dPadRawClusWrapper.h>
#include <PadRawv1.h>
#include <PadClusterv2.h>

#include <Fun4AllReturnCodes.h>

#include <RawDataCheck.h>

#include <recoConsts.h>
#include <getClass.h>
#include <Event.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeReset.h>
#include <PHTable.hh>
#include <PHObject.h>

#include <sstream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;

PadReco::PadReco(const string &name): SubsysReco(name)
{
  padSplitMode = 0;
  PadEvtToRaw = 0;
  PadInclBad = 0;
  mPadDetGeo = 0;
  Pc1Rec = 0;
  Pc2Rec = 0;
  Pc3Rec = 0;
  norawdatacheck = 0;
  return ;
}

PadReco::~PadReco()
{
  delete PadEvtToRaw;
  delete PadInclBad;
  delete Pc1Rec;
  delete Pc2Rec;
  delete Pc3Rec;
  return;
}

int PadReco::Init(PHCompositeNode *topNode)
{
  PadEvtToRaw = new padEvtToRaw();
  PadInclBad = new padInclBad();
  mPadDetGeo = new padDetectorGeo();
  Pc1Rec = new PadRecModule(0);
  Pc2Rec = new PadRecModule(1);
  Pc3Rec = new PadRecModule(2);
  int iret = CreateNodeTree(topNode);

  return iret;
}

int PadReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int iret = 0;
  mPadDetGeo->FetchFromDatabase(rc->get_TimeStamp());
  PHTimeStamp TimeStp = rc->get_TimeStamp();
  PadInclBad->FetchCalDataFromObjy(TimeStp);
  PadEvtToRaw->FetchCalDataFromPdbCal(TimeStp);
  return iret;
}

int PadReco::CreateNodeTree(PHCompositeNode *topNode)
{
  // first test if neccessary nodes have been created, if not bail out
  enum {DSTNODE, DCMNODE, PARNODE, LAST};
  const char *NName[] = {
    "DST",
    "DCM",
    "PAR"};
  PHNodeIterator iter(topNode);
  PHCompositeNode *outNode[LAST];
  for (short int i = 0;i < LAST;i++)
    {
      outNode[i] = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", NName[i]));
      if (!outNode[i])
        {
          cout << PHWHERE << NName[i] << " Node is missing doing nothing" << endl;
          return -1;
        }
    }
  // create the PAD node if it does not exist yet
  PHCompositeNode* padNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAD"));
  if (! padNode)
    {
      padNode = new PHCompositeNode("PAD");
      topNode->addNode(padNode);
    }
  // set up tables
  ostringstream tmpstream;
  int maxrows = 60000;
  PHTableNode_t *PHTableIONode;
  PHObjectNode_t *PHObjectIONode;
  dPadRawWrapper *dPadRaw;
  for (short int j = 1; j <= 3 ; j++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "dPc" << j << "Raw";
      string NodeName = tmpstream.str();
      dPadRaw = findNode::getClass<dPadRawWrapper>(topNode,NodeName.c_str());
      if (!dPadRaw)
        {
          dPadRaw = new dPadRawWrapper(NodeName.c_str(), maxrows);
          PHTableIONode = new PHTableNode_t(dPadRaw, NodeName.c_str());
          outNode[DSTNODE]->addNode(PHTableIONode);
        }
    }

  dPadClusterWrapper *dPadCluster;
  for (short int j = 1; j <= 3 ; j++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "dPc" << j << "Cluster";
      string NodeName = tmpstream.str();
      dPadCluster = findNode::getClass<dPadClusterWrapper>(topNode,NodeName.c_str());
      if (!dPadCluster)
        {
          dPadCluster = new dPadClusterWrapper(NodeName.c_str(), maxrows);
          PHTableIONode = new PHTableNode_t(dPadCluster, NodeName.c_str());
          outNode[DSTNODE]->addNode(PHTableIONode);
        }
    }

  PadRaw *padraw;
  for (short int j = 1; j <= 3 ; j++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "Pc" << j << "Raw";
      string NodeName = tmpstream.str();
      padraw = findNode::getClass<PadRaw>(topNode,NodeName.c_str());
      if (!padraw)
        {
          padraw = new PadRawv1();
          PHObjectIONode = new PHObjectNode_t(padraw, NodeName.c_str(), "PHObject");
          outNode[DSTNODE]->addNode(PHObjectIONode);
        }
    }

  PadCluster *padcluster;
  for (short int j = 1; j <= 3 ; j++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "Pc" << j << "Cluster";
      string NodeName = tmpstream.str();
      padcluster = findNode::getClass<PadCluster>(topNode,NodeName.c_str());
      if (!padcluster)
        {
          padcluster = new PadClusterv2();
          PHObjectIONode = new PHObjectNode_t(padcluster, NodeName.c_str(), "PHObject");
          outNode[DSTNODE]->addNode(PHObjectIONode);
        }
    }

  dPadRawClusWrapper *dPadRawClus;
  for (short int j = 1; j <= 3 ; j++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "dPc" << j << "RawClus";
      string NodeName = tmpstream.str();
      dPadRawClus = findNode::getClass<dPadRawClusWrapper>(topNode,NodeName.c_str());
      if (!dPadRawClus)
        {
          dPadRawClus = new dPadRawClusWrapper(NodeName.c_str(), maxrows);
          PHTableIONode = new PHTableNode_t(dPadRawClus, NodeName.c_str());
          padNode->addNode(PHTableIONode);
        }
    }
   PHDataNode<padDetectorGeo> *mPadDetGeoNode = new PHDataNode<padDetectorGeo>(mPadDetGeo, "mPadDetGeo");
  outNode[PARNODE]->addNode(mPadDetGeoNode);

  // Setting padInclBad parameters
  PadInclBad->setDebugLevel(0);
  PadInclBad->doNotInclBadChs();
  PadInclBad->doInclBadROCs();
  PadInclBad->RemoveHotROCs();
  PadInclBad->RemoveUnSynchROCs();
  PadInclBad->doNotAddHotROCs();
  PadInclBad->doNotAddInactiveROCs();
  PadInclBad->doNotAddUnSynchROCs();

  // Setting mPadDetGeo Parameters
  double pc1Radius = 248.891;
  double pc2Radius = 419.173;
  double pc3Radius = 492.012;
  double ThetaArm[2] = { -0.589049, 2.15984};
  mPadDetGeo->set_pc1Radius(pc1Radius);
  mPadDetGeo->set_pc2Radius(pc2Radius);
  mPadDetGeo->set_pc3Radius(pc3Radius);
  mPadDetGeo->set_Theta0(ThetaArm);

  // Setting PadRecModule parameters
  short level = 0;              // Debug level 0: no debug information
  Pc1Rec->setDebugLevel(level); // Debug level 1: print method calls
  Pc2Rec->setDebugLevel(level); // Debug level 2: show reconstruction
  //                statistics
  Pc3Rec->setDebugLevel(level); // Debug level 3: show dPadCluster fillings
  // Debug level 4: Save hit information in a
  //                text file -> ./padrec.dat
  // Debug level 5: Save hit information for
  //                Birdseye event display
  //                -> ./birdseye.dat
  Pc1Rec->setSplitMode(padSplitMode); // Split mode 0: Do not split large clusters
  Pc2Rec->setSplitMode(padSplitMode); // Split mode 1: Split large clusters
  Pc3Rec->setSplitMode(padSplitMode);

  // The maximum allowed cluster size (number of cells)
  short splitMax = 100;
  Pc1Rec->setSplitMax(splitMax);
  Pc2Rec->setSplitMax(splitMax);
  Pc3Rec->setSplitMax(splitMax);

  short limit = 2; // Demand two (default) out of three good pads per cell
  Pc1Rec->setPadTypeLimit(limit);
  Pc2Rec->setPadTypeLimit(limit);
  Pc3Rec->setPadTypeLimit(limit);

  // The idea behind the reconstruction code is to place an imaginative box
  // around each cluster and to measure its size. These values set the maximum
  // size of the box that covers the cluster created by a single particle.
  short oneW = 4; // Box height
  short oneZ = 4; // Box width
  short oneL = 11; // Maximum number of allowed cells within a one particle box
  short twoW = 8;
  short twoZ = 8;
  short twoL = 16;
  short threeW = 100;
  short threeZ = 100;
  short threeL = 100;
  Pc1Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);
  Pc2Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);
  Pc3Rec->setClusterSizes(oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL);
  return 0;
}

int PadReco::process_event(PHCompositeNode *topNode)
{
  Event *evt = findNode::getClass<Event>(topNode,"PRDF");
  if (!evt)
    {
      cout << PHWHERE << "No PRDF Node or no Event" << endl;
      return 0;
    }

  if (PadEvtToRaw->event(topNode, evt))
    {
      if (!norawdatacheck)
	{
	  RawDataCheck *raw = RawDataCheck::instance();
	  raw->AddToList(evt, "PADOCCUPANCY");
	}
      return ABORTEVENT; // set iret to ABORTEVENT if PadEvtToRaw fails (all pads in packet are lit)
    }

  PadInclBad->event(topNode);

  Pc1Rec->event(0, mPadDetGeo, topNode);
  Pc2Rec->event(1, mPadDetGeo, topNode);
  Pc3Rec->event(2, mPadDetGeo, topNode);

  copyWrapper(topNode);
  return 0;
}

int PadReco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("PAD"))
    {
      mainIter.forEach(reset);
      mainIter.cd();
    }
  return 0;
}

int PadReco::copyWrapper(PHCompositeNode *topNode)
{
  int iret = 0;
  ostringstream WrapperNodeName, PhobjectNodeName;

  for (short int j = 1; j <= 3; j++)
    {
      WrapperNodeName.str("");
      PhobjectNodeName.str("");
      WrapperNodeName << "dPc" << j << "Cluster";
      dPadClusterWrapper *dpadcluster = findNode::getClass<dPadClusterWrapper>(topNode,WrapperNodeName.str().c_str());

      PhobjectNodeName << "Pc" << j << "Cluster";
      PadCluster *padcluster = findNode::getClass<PadCluster>(topNode,PhobjectNodeName.str().c_str());
      if (dpadcluster && padcluster)
        {
          if (!padcluster->isValid() && dpadcluster->RowCount())
            {
              padcluster->FillFromWrapper(dpadcluster);
            }
        }
    }

  for (short int j = 1; j <= 3; j++)
    {
      WrapperNodeName.str("");
      PhobjectNodeName.str("");
      WrapperNodeName << "dPc" << j << "Raw";
      dPadRawWrapper *dpadraw = findNode::getClass<dPadRawWrapper>(topNode,WrapperNodeName.str().c_str());

      PhobjectNodeName << "Pc" << j << "Raw";
      PadRaw *padraw = findNode::getClass<PadRaw>(topNode,PhobjectNodeName.str().c_str());
      if (dpadraw && padraw)
        {
          if (!padraw->isValid() && dpadraw->RowCount())
            {
              padraw->FillFromWrapper(dpadraw);
            }
        }
    }
  return iret;
}
