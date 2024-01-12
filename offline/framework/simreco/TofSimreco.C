// $Id: TofSimreco.C,v 1.33 2010/07/13 20:49:37 hpereira Exp $
#include "TofSimreco.h"


#include "TofGeometryObject.hh"

#include "dTofGdigiRecWrapper.h"
#include "dTofGdigiWrapper.h"
#include "dTofGhitGdigiWrapper.h"
#include "dTofGhitRawWrapper.h"
#include "dTofRawRecWrapper.h"
#include "dTofRawWrapper.h"
#include "dTofReconstructedWrapper.h"
#include "tofghitWrapper.h"
#include "TofPISAHit.h"

#include "TofOutv2.h"

#include "TofGetGEA.h"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "RunNumberRanges.h"
#include "recoConsts.h"

#include "getClass.h"

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;
typedef PHIODataNode<TObject> TObjectNode_t;
typedef PHIODataNode <TofOut> TofOutNode_t;

//___________________________________________________
TofSimreco::TofSimreco(const string &name) :
  SubsysReco(name),
  TofGeometry(0)
{}

//___________________________________________________
int TofSimreco::Init(PHCompositeNode *topNode)
{

  // it is important that all objects
  // that are created here (using explicit "new" operator)
  // are deleted in the destructor

  // Setting TOF response chain parameters (new code)
  // (0 = not verbose, 1 = verbose)
  tofevent.setDebugLevel(0);

  // Set intrinsic timing resolution [ns]
  // : 100 ps with BBC t0_sigme = 40ps for QM02 analysis
  // Changed the default value from 100ps to 120 ps (T.Chujo, 2007.0330).
  tofevent.setTimingResolution(0.12);

  //tofevent.setAttenuationLength(128.0); // [cm]
  return 0;

}

//___________________________________________________
TofSimreco::~TofSimreco()
{ return; }

//___________________________________________________
int TofSimreco::InitRun(PHCompositeNode *topNode)
{
  enum {DCMNODE, DSTNODE, EVANODE, GEANODE, PARNODE, LAST};
  const char *NName[] = {
    "DCM",
    "DST",
    "EVA",
    "GEA",
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

  PHCompositeNode* tofNode = new PHCompositeNode("TOF");
  topNode->addNode(tofNode);

  dTofGhitRawWrapper* dTofGhitRaw = new dTofGhitRawWrapper("dTofGhitRaw", 3000);
  PHIODataNode<PHTable>* dTofGhitRawNode = new PHIODataNode<PHTable>(dTofGhitRaw, "dTofGhitRaw");
  outNode[EVANODE]->addNode(dTofGhitRawNode);

  tofghitWrapper* tofghit = new tofghitWrapper("tofghit", 4500);
  PHIODataNode<PHTable>* tofghitNode = new PHIODataNode<PHTable>(tofghit, "tofghit");
  outNode[GEANODE]->addNode(tofghitNode);

  dTofGdigiWrapper* dTofGdigi = new dTofGdigiWrapper("dTofGdigi", 1500);
  PHIODataNode<PHTable>* dTofGdigiNode = new PHIODataNode<PHTable>(dTofGdigi, "dTofGdigi");
  outNode[EVANODE]->addNode(dTofGdigiNode);

  dTofGhitGdigiWrapper* dTofGhitGdigi = new dTofGhitGdigiWrapper("dTofGhitGdigi", 1500);
  PHIODataNode<PHTable>* dTofGhitGdigiNode = new PHIODataNode<PHTable>(dTofGhitGdigi, "dTofGhitGdigi");
  outNode[EVANODE]->addNode(dTofGhitGdigiNode);

  dTofGdigiRecWrapper* dTofGdigiRec = new dTofGdigiRecWrapper("dTofGdigiRec", 1500);
  PHIODataNode<PHTable>* dTofGdigiRecNode = new PHIODataNode<PHTable>(dTofGdigiRec, "dTofGdigiRec");
  outNode[EVANODE]->addNode(dTofGdigiRecNode);
  tofNode->addNode(dTofGdigiRecNode);

  dTofRawRecWrapper* dTofRawRec = new dTofRawRecWrapper("dTofRawRec", 960);
  PHIODataNode<PHTable>* dTofRawRecNode = new PHIODataNode<PHTable>(dTofRawRec, "dTofRawRec");
  outNode[EVANODE]->addNode(dTofRawRecNode);

  dTofRawWrapper* dTofRaw = new dTofRawWrapper("dTofRaw", 960);
  PHIODataNode<PHTable>* dTofRawNode = new PHIODataNode<PHTable>(dTofRaw, "dTofRaw");
  outNode[DSTNODE]->addNode(dTofRawNode);

  dTofReconstructedWrapper *dTofReconstructed = findNode::getClass<dTofReconstructedWrapper>(topNode, "dTofReconstructed");
  if (!dTofReconstructed)
    {
      dTofReconstructed = new dTofReconstructedWrapper("dTofReconstructed", 960);
      PHIODataNode<PHTable> *NewNode = new PHIODataNode<PHTable>(dTofReconstructed, "dTofReconstructed");
      outNode[DSTNODE]->addNode(NewNode);
    }

  TofOut *tofout = findNode::getClass<TofOut>(topNode, "TofOut");
  if (!tofout)
    {
      tofout = new TofOutv2();
      PHIODataNode<PHObject> *NewNode =	new PHIODataNode<PHObject>(tofout, "TofOut", "PHObject");
      outNode[DSTNODE]->addNode(NewNode);
    }

  //===============================================================
  // simulation parameters stored in DB
  // last update : Oct. 18, 2007, by T.Chujo
  //
  //   Run1 : 2001.1.1.0.0.0 - 2001.1.1.23.59.59
  //   Run2 : 2002.2.1.0.0.0 - 2002.2.1.23.59.59
  //   Run3 : (used Run2 time stamp)
  //   Run4 : 2004.4.4.0.0.0 - 2004.4.4.23.59.59
  //   Run5 : 2005.1.1.0.0.0 - 2005.1.1.23.59.59
  //   Run6 : (used Run5 time stamp, will be updated)
  //   Run7 : (used Run5 time stamp, will be updated)
  //   Run8 : (used Run5 time stamp, will be updated)
  //============================================================

  PHTimeStamp TimeStp;

  // Get run number and set timeStamp accordingly
  recoConsts *rc = recoConsts::instance();
  int run_number = fabs( rc->get_IntFlag( "RUNNUMBER", 0 ) );
  if( run_number == 0 )
  {
    cout << "TofSimReco::InitRun - missing Run number, needed to decide ";
    cout << "TofSimReco::InitRun - calibration time stamp. Exiting" << endl;
    exit(1);
  }

  // Run2 and Run3 setup
  if( run_number < BEGIN_OF_RUN4 ) TimeStp = PHTimeStamp(2002, 2, 1, 12, 0, 0);

  // Run 4 setup
  else if( run_number < BEGIN_OF_RUN5 ) TimeStp = PHTimeStamp(2004, 4, 4, 12, 0, 0);

  // Run 5 setup and later
  else TimeStp = PHTimeStamp(2005, 1, 1, 12, 0, 0);

  cout << "TofSimreco::InitRun - using run number: " << run_number << endl;
  cout << "TofSimreco::InitRun - time stamp for MC parameters : " << TimeStp << "\n" << endl;

  TofGeometry    = new TofGeometryObject();
  PHDataNode<TofGeometryObject>* TofDetGeoNode = new PHDataNode<TofGeometryObject>(TofGeometry, "TofGeometry");
  outNode[PARNODE]->addNode(TofDetGeoNode);

  TofAddress.setTimeStamp(TimeStp);
  TofAddress.fetch();
  TofGeometry->setTimeStamp(TimeStp);
  TofGeometry->fetch();
  TofCalib.setTimeStamp(TimeStp);
  TofCalib.fetch();

  return 0;
}

//________________________________________________________________________
int TofSimreco::process_event(PHCompositeNode *topNode)
{

  TofGetGEA(topNode);
  tofevent.GeaToRaw(topNode, &TofAddress, TofGeometry, &TofCalib);
  tofevent.RawToDst(topNode, &TofAddress, TofGeometry, &TofCalib);

  // Add for TOF evaluator, AK Dec 21, 2000
  mTofGhitGdigi.event(topNode);
  mTofEvaluate.event(topNode);

  copyWrapper(topNode);
  return 0;
}

int TofSimreco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("TOF"))
    {
      mainIter.forEach(reset);
    }

  return 0;
}

int
TofSimreco::copyWrapper(PHCompositeNode *topNode)
{
  dTofReconstructedWrapper *dtofout = findNode::getClass<dTofReconstructedWrapper>(topNode, "dTofReconstructed");
  if (dtofout)
    {
      TofOut *tofout = findNode::getClass<TofOut>(topNode, "TofOut");

      if (tofout)
        {
          if (!tofout->isValid() && dtofout->RowCount())
            {
              tofout->FillFromWrapper(dtofout);
            }
          if (verbosity > 0)
            {
              tofout->identify();
            }
        }
    }
  return 0;
}
