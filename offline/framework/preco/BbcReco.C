// $Id: BbcReco.C,v 1.23 2012/11/16 21:34:10 chiu Exp $

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <BbcOutv1.h>
#include <BbcRawv1.h>
#include <recoConsts.h>
#include <RunNumberRanges.h>
#include <mBbcUnpackModule.h>
#include <mBbcRawOutModule.h>
#include <BbcCalib.hh>
#include <dBbcRawHitParWrapper.h>
#include <dBbcRawWrapper.h>
#include <dBbcOutWrapper.h>
#include <BbcReco.h>
#include <RunHeader.h>
#include <Fun4AllReturnCodes.h>

#include <getClass.h>
#include <cmath>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHDataNode<BbcCalib> BbcCalibNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;

//_______________________________________________________________________
BbcReco::BbcReco(const char *name):
  SubsysReco(name),
  mBbcUnpack( 0 ),
  mBbcRawOut( 0 ),
  bbcout( 0 ),
  BbcVtxError( 0 )
{ return ; }

//_______________________________________________________________________
BbcReco::~BbcReco()
{
  if (mBbcUnpack) delete mBbcUnpack;
  if (mBbcRawOut) delete mBbcRawOut;
  return;
}

//_______________________________________________________________________
int BbcReco::Init(PHCompositeNode *topNode)
{
  // Skip creating node tree if doing PISA->DST.
  recoConsts *rc = recoConsts::instance();
  int simflag = 0;
  if ( rc->FlagExist("SIMULATIONFLAG") )
    {
      simflag = rc->get_IntFlag("SIMULATIONFLAG");
    }
  if ( simflag == 2 ) return EVENT_OK;

  int iret = CreateNodeTree(topNode);
  return iret;
}

//_______________________________________________________________________
int BbcReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int iret = 0;
  int runnumber = rc->get_IntFlag("RUNNUMBER");

  int simflag = 0;
  if ( rc->FlagExist("SIMULATIONFLAG") )
    {
      simflag = rc->get_IntFlag("SIMULATIONFLAG");
    }
  
  // create modules
  mBbcUnpack = new mBbcUnpackModule();
  mBbcRawOut = new mBbcRawOutModule();

  //Failure value...
  int currentCentral = -9999; 
  RunHeader *d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if (!d_runhdr && simflag!=2 && simflag!=3)
    {
      cout << "BbcReco InitRun::no RUN HEADER...can't determine magnet current!!!!" << endl;
      exit(1);
    }
  if ( simflag==2 )
    {
      currentCentral = 4000;
    }
  else
    {
      currentCentral = d_runhdr->get_currentCentral();
    }
  cout << "BbcReco InitRun::Central Magnet Current is " << currentCentral << endl;
  cout << "                 Thus, we conclude that the magnet is ";
  int icalibversion = 0;
  if (abs(currentCentral) < 100)
    {
      cout << "OFF" << endl;
      
      // Choose calib based upon run number...
      if (runnumber<BEGIN_OF_RUN3)                            icalibversion = 1001;
      if (runnumber>BEGIN_OF_RUN3 && runnumber<BEGIN_OF_RUN4) icalibversion = 1003;
      if (runnumber>BEGIN_OF_RUN4 && runnumber<BEGIN_OF_RUN5) icalibversion = 3005;
      if (runnumber>BEGIN_OF_RUN5)                            icalibversion = 4001;
    }
  else
    {
      cout << "ON" << endl;

      //  Choose calib based upon run number...
      if (runnumber<BEGIN_OF_RUN3)                            icalibversion = 1002;
      if (runnumber>BEGIN_OF_RUN3 && runnumber<BEGIN_OF_RUN4) icalibversion = 1004;
      if (runnumber>BEGIN_OF_RUN4 && runnumber<BEGIN_OF_RUN5) icalibversion = 3006;
      if (runnumber>BEGIN_OF_RUN5)                            icalibversion = 4002;

    }
    
  cout << "BbcReco::InitRun - run number: " << runnumber << endl;
  cout << "BbcReco::InitRun - calibration version:  " << icalibversion << endl;
  rc->set_IntFlag("BBCCALIBVERSION", icalibversion); 
  // Run3 Field ON.

  // Choose vertex error based upon run number,
  // if it was not manually set already
  if( !BbcVtxError )
  {
    cout << "BbcReco::InitRun - using run-number to set BbcVtxError" << endl;
    cout << " Note: this code is largelly unmaintained and incomplete." << endl;
    cout << " It is far from covering all PHENIX data taking periods and is thus" << endl;
    cout << " incorrect. For the time-being, it is recommanded to set the vertex " << endl;
    cout << " error to its correct value directly in the macro using the" << endl;
    cout << " \"setBbcVtxError()\" method." << endl;
    
    if (runnumber < BEGIN_OF_RUN3) BbcVtxError = 0.5;
    else if (runnumber < BEGIN_OF_RUN4) BbcVtxError = 2.0;
    else BbcVtxError = 0.5;
    
  }

  // print value
  cout << "BbcReco::InitRun - BbcVtxError: " << BbcVtxError << endl;
  
  BbcCalib *BbcCalibPar = findNode::getClass<BbcCalib>(topNode,"BbcCalibPar");
  if (BbcCalibPar)
  {
    PHTimeStamp TimeStp = rc->get_TimeStamp();
    int BBCCALIBVERSION = rc->get_IntFlag("BBCCALIBVERSION");
    cout << "BbcReco::InitRun - restored constants are for " << TimeStp << endl;
    BbcCalibPar->restore(TimeStp, BBCCALIBVERSION);
  }
  return iret;
}

//_______________________________________________________________________________________
int BbcReco::CreateNodeTree(PHCompositeNode *topNode)
{
  // first test if neccessary nodes have been created, if not bail out
  enum {DSTNODE, PARNODE, LAST}; // leave LAST at end - it is used for loops
  const char *NName[] = {
    "DST",
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
  PHCompositeNode* bbcNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "BBC"));
  if (! bbcNode)
    {
      bbcNode = new PHCompositeNode("BBC");
      topNode->addNode(bbcNode);
    }

  int mr, nrc;
  mr = 1;
  dBbcRawHitParWrapper* dBbcRawHitPar = new dBbcRawHitParWrapper("dBbcRawHitPar", mr);
  PHTableNode_t *dBbcRawHitParNode = new PHTableNode_t(dBbcRawHitPar, "dBbcRawHitPar");
  outNode[PARNODE]->addNode(dBbcRawHitParNode);

  // Setting dBbcRawHitPar Parameters
  nrc = 1;
  dBbcRawHitPar->SetRowCount(nrc);
  dBbcRawHitPar->set_MinAdc(0, 0);
  dBbcRawHitPar->set_MaxAdc(0, 4096);
  dBbcRawHitPar->set_MinTdc0(0, 10);
  dBbcRawHitPar->set_MaxTdc0(0, 4000);
  dBbcRawHitPar->set_MinTdc1(0, 10);
  dBbcRawHitPar->set_MaxTdc1(0, 4000);

  mr = 128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw", mr);
  PHTableNode_t *dBbcRawNode = new PHTableNode_t(dBbcRaw, "dBbcRaw");
  bbcNode->addNode(dBbcRawNode);

  BbcRaw *bbcraw = new BbcRawv1();
  PHObjectNode_t *BbcRawNode = new PHObjectNode_t(bbcraw, "BbcRaw", "PHObject");
  outNode[DSTNODE]->addNode(BbcRawNode);

  BbcCalib* BbcCalibPar = new BbcCalib();
  BbcCalibNode_t *BbcCalibParNode = new BbcCalibNode_t(BbcCalibPar, "BbcCalibPar");
  outNode[PARNODE]->addNode(BbcCalibParNode);

  mr = 1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut", mr);
  PHTableNode_t *dBbcOutNode = new PHTableNode_t(dBbcOut, "dBbcOut");
  bbcNode->addNode(dBbcOutNode);

  BbcOut* temp_bbcout = new BbcOutv1();
  PHObjectNode_t *BbcOutNode = new PHObjectNode_t(temp_bbcout, "BbcOut", "PHObject");
  outNode[DSTNODE]->addNode(BbcOutNode);
  return 0;
}

//_______________________________________________________________________________________
int BbcReco::process_event(PHCompositeNode *topNode)
{
  int iret = mBbcUnpack->event(topNode);
  if (!iret)
    {
      bbcout = findNode::getClass<BbcOut>(topNode,"BbcOut");

      mBbcRawOut->event(topNode);
      if (fabs(bbcout->get_dVertexPoint()) < 0.01)
	{
	  bbcout->set_dZVertex(BbcVtxError);
	}
    }
  return 0;
}

//_______________________________________________________________________________________
int BbcReco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("BBC"))
    {
      mainIter.forEach(reset);
    }
  return 0;
}
