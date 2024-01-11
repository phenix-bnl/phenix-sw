#include <MpcReco.h>
#include <MpcEvent.h>
#include <MpcMap.h>
#include <MpcCalib.h>
#include <mpcSampleContainerV1.h>
#include <mpcRawContainerV2.h>
#include <mpcRawContainerV3.h>
#include <mpcRawContainerV4.h>
#include <mpcRawContainerV5.h>
#include <mpcTowerContainerV1.h>
#include <mpcSimTowerContainerV1.h>
#include <mpcClusterContainerV1.h>
#include <mpcClusterContainerV2.h>
#include <mpcGeaTowerContainerV1.h>
#include <RunHeader.h>
#include <recoConsts.h>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <TrigLvl1.h>

#include <PHIODataNode.h>
#include <PHObject.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

MpcReco::MpcReco(const std::string &name): SubsysReco(name)
{
  mMpcEvent = 0;
  mpcmap = 0;
  mpccalib = 0;
  return ;
}

MpcReco::~MpcReco()
{
  delete mMpcEvent;
}

int MpcReco::Init(PHCompositeNode *topNode)
{
cout << "NEW MPCRECO INIT" << endl;

  int iret = CreateNodeTree(topNode);
  mMpcEvent = new MpcEvent(topNode);	// this module does all the reconstruction work
  mMpcEvent->SetEventNumber(0);
  return iret;
}

int MpcReco::EndRun(const int runnumber)
{
  mMpcEvent->Save();
  return 0;
}

int MpcReco::InitRun(PHCompositeNode *topNode)
{
  // Set the energy scale based on run
  int run_number = -999999;
  RunHeader *runheader = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if ( runheader!=0 )
    {
      run_number = runheader->get_RunNumber();
    }

  if ( run_number == -999999 )
    {
      // Didn't get good run number from run header,
      // try again using recoConst
      recoConsts *rc = recoConsts::instance();
      run_number = rc->get_IntFlag("RUNNUMBER");
    } 

  cout << "MpcReco RUNNUMBER " << run_number << endl;

  // Need to add in text database here, if MPC_DATABASE exists

  if ( mpcmap != 0 ) mpcmap->Download_Maps(topNode);
  if ( mpccalib != 0 ) mpccalib->Download_All(topNode);

  if ( run_number > 246000 && run_number < 261000 )	// Run08 d+Au and p+p
    {
      recoConsts *rc = recoConsts::instance();
      rc->set_FloatFlag("MPCSCALE_NORTH",1.05/1.08);
      rc->set_FloatFlag("MPCSCALE_SOUTH",1.05/1.03);

      cout << "MPC: Setting North scale to 1.05/1.08 and South to 1.05/1.03" << endl;
      cout << "     This is only applied to the real data, not simulation" << endl;
    }

  return 0;
}

int MpcReco::process_event(PHCompositeNode *topNode)
{
/*
  TrigLvl1 *trigl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  unsigned int trigscaled = trigl1->get_lvl1_trigscaled();
  if ( (trigscaled&0x12)==0 ) return ABORTEVENT;
*/

  //mMpcEvent->Clear();			// reset to default values
  mMpcEvent->ProcessOneEvent(topNode);	// reconstruct and put in MpcOutNode

  /*
    // Dump out tower info
    mpcTowerContainer *mpctower = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
    int ntow = mpctower->size();
    for (int itow=0;itow<ntow;itow++)
      {
        mpcTowerContent *tow = mpctower->getTower(itow);
        //cout << "xxx " << tow->get_ch() << "\t" << tow->get_energy() << endl;
      }
  */

  return 0;
}

int MpcReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHCompositeNode *dstNode;
  PHNodeIterator iter(topNode);
  dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cout << PHWHERE << "DST Node missing doing nothing" << endl;
      return -1;
    }

  // here recoConsts should be used to determine mpc reco mode
  // or should i automatically figure this out from the presence of the
  // mpc nodes? (bad b/c sometimes we want to override the reco setting
  // by default mpcreco should do all things, but each process (raw or cluster)
  // should have it's own flag

  // MPC_RECO_MODE: 0x1 = make MpcRaw node,
  //                0x2 = make MpcTower node
  //                0x4 = make MpcCluster node
  //                0x7 = all
  //
  // This also implies that we will want to create the objects that go into those
  // nodes.

  recoConsts *rc = recoConsts::instance();
  int mpc_reco_mode = rc->get_IntFlag("MPC_RECO_MODE",0x7);
  cout << "MPC_RECO_MODE\t0x" << hex << mpc_reco_mode << dec << endl;
  if ( rc->FlagExist("SIMULATIONFLAG") && rc->get_IntFlag("SIMULATIONFLAG")==2 )
    {
      cout << "MpcReco, creating mpcSimTowers\t" << mpc_reco_mode << endl;
    }

  //-* contains raw values (TDCs, ADCs, etc)
  if ( (mpc_reco_mode&0x1)==0x1 )
    {
      // Original MpcRaw Container
      mpcRawContainer *mpcraw = new mpcRawContainerV2();
      PHObjectNode_t *MpcRawNode = new PHObjectNode_t(mpcraw, "MpcRaw", "PHObject");
      dstNode->addNode(MpcRawNode);

      // Container for all ADC samples, new MPC Electronics
      mpcSampleContainer *mpcsamples = new mpcSampleContainerV1();
      PHObjectNode_t *mpcSampleContainerNode = new PHObjectNode_t(mpcsamples, "mpcSampleContainer", "PHObject");
      dstNode->addNode( mpcSampleContainerNode );
    }

  if ( (mpc_reco_mode&0x2)==0x2 )
    {
      if ( rc->FlagExist("SIMULATIONFLAG") && rc->get_IntFlag("SIMULATIONFLAG")==2 )
        { 
          mpcTowerContainer *mpctower = new mpcSimTowerContainerV1();
          PHObjectNode_t *MpcTowerNode = new PHObjectNode_t(mpctower, "mpcTowerContainer", "PHObject");
          dstNode->addNode(MpcTowerNode);

          // Should add if (SIMULATIONFLAG == 2) here
          mpcGeaTowerContainer *mpcgeatower = new mpcGeaTowerContainerV1();
          PHObjectNode_t *MpcGeaTowerNode = new PHObjectNode_t(mpcgeatower, "mpcGeaTowerContainer", "PHObject");
          dstNode->addNode(MpcGeaTowerNode);
        }
      else
        {
          // New MpcRaw Container, for new MPC Electronics
          if ( (mpc_reco_mode&0x10)==0x0 )
            {
              cout << "Making MpcRaw2" << endl;
              mpcRawContainer *mpcraw2 = new mpcRawContainerV5(); // Run-16 container to carry fit quality information 
              PHObjectNode_t *MpcRaw2Node = new PHObjectNode_t(mpcraw2, "MpcRaw2", "PHObject");
              dstNode->addNode(MpcRaw2Node);
            }

          mpcTowerContainer *mpctower = new mpcTowerContainerV1();
          PHObjectNode_t *MpcTowerNode = new PHObjectNode_t(mpctower, "mpcTowerContainer", "PHObject");
          dstNode->addNode(MpcTowerNode);
        }
    }

  //-* contains final physics products (energies, tof, etc)
  if ( (mpc_reco_mode&0x4)==0x4 )
    {
      mpcClusterContainer *mpcclus = findNode::getClass<mpcClusterContainer>(topNode, "mpcClusterContainer");
      if(mpcclus){
        cout << PHWHERE << "mpcClusterContainer already exists, it will not be replaced." << endl; 
      }
      else{
	int cluster_alg = rc->get_IntFlag("MPC_CLUSTER_ALG", 1);
	if ( cluster_alg == 0 )
	  {
            mpcclus = new mpcClusterContainerV1();
	  }
	else if ( cluster_alg == 1 )
	  {
            mpcclus = new mpcClusterContainerV2();
	  }
	PHObjectNode_t *MpcClusterNode = new PHObjectNode_t(mpcclus, "mpcClusterContainer", "PHObject");
	dstNode->addNode(MpcClusterNode);
      }

    }

  if ( (mpc_reco_mode&0x7)!=0 )
    {
      mpcmap = findNode::getClass<MpcMap>(topNode, "MpcMap");
      if ( mpcmap == 0 )
        {
          mpcmap = new MpcMap(topNode,0);
        }
      mpccalib = findNode::getClass<MpcCalib>(topNode, "MpcCalib");
      if ( mpccalib == 0 )
        {
          mpccalib = new MpcCalib(topNode,0);
        }
    }

  return 0;
}

