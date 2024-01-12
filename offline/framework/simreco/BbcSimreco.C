#include "BbcSimreco.h"

#include <BbcEvent.hh>
#include <BbcGeo.hh>
#include <BbcGetDCM.h>

#include <bbcghitWrapper.h>
#include <BbcOutv1.h>
#include <BbcRawv1.h>
#include <dBbcCalWrapper.h>
#include <dBbcDCMWrapper.h>
#include <dBbcFEMWrapper.h>
#include <dBbcGeoWrapper.h>
#include <dBbcGhitRawParWrapper.h>
#include <dBbcGhitRawWrapper.h>
#include <dBbcOutWrapper.h>
#include <dBbcRawHitParWrapper.h>
#include <dBbcRawWrapper.h>
#include <dBbcUcalWrapper.h>

#include <PISAEventHeader.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHDataNode.h>
#include <PHNodeIterator.h>
#include <PHNodeReset.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>

#include <getClass.h>
#include <RunToTime.hh>
#include <RunHeader.h>

#include <gsl/gsl_randist.h>

#include <iostream>
#include <cmath>

using namespace std;

typedef PHIODataNode <BbcOut> BbcOutNode_t;
typedef PHDataNode<BbcCalib> BbcCalibNode_t;
typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<PHTable> PHTableNode_t;

long BbcPutDCM(PHCompositeNode* topNode);
long BbcPutDCMReCal(PHCompositeNode* topNode);

//_____________________________________________________________
BbcSimreco::BbcSimreco(const string &name): SubsysReco(name)
{return;}

//_____________________________________________________________
int BbcSimreco::InitRun(PHCompositeNode *topNode)
{


  enum {DCMNODE, DSTNODE, EVANODE, GEANODE, PARNODE, LAST};
  const char *NName[] = {
    "DCM",
    "DST",
    "EVA",
    "GEA",
    "PAR"
  };

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


  recoConsts *rc = recoConsts::instance();
  unsigned int mr, nrc;

  int runnumber = rc->get_IntFlag("RUNNUMBER");

  // Get magnetic field information if availbale,
  // otherwise simulation is dedicated for B-on case
  RunHeader *d_runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");
  int currentCentral = 4000;	// field-on is the default
  if(!d_runhdr){
	static int counter = 0;
        if ( counter<10 )
	{
		cout<<PHWHERE<<"No RunHeader Object"<<endl;
		counter++;
	}
  }
  else
  {
	currentCentral = d_runhdr->get_currentCentral();
  }

  int VersionOffset;
  if (abs(currentCentral) < 100)
  {
    VersionOffset = 1; // B-off case
  }
  else
  {
    VersionOffset = 0; // B-on case. Default is B-on version
  }

  // set calibration version
  int CalibrationVersion;
  if (runnumber < BEGIN_OF_RUN3)
  {

    // Either B-on or B-off depending on run number during Run2
    CalibrationVersion = 3002;

  } else if (runnumber < BEGIN_OF_RUN4) {

    // Run3
    CalibrationVersion = 3004 - VersionOffset;

  } else if (runnumber <  BEGIN_OF_RUN5) {

    // Run4
    CalibrationVersion = 3006 - VersionOffset;

  } else {

    // Run5
    CalibrationVersion = 4002 - VersionOffset;

  }

  rc->set_IntFlag("BBCCALIBVERSION", CalibrationVersion);
  cout << "BBCCALIBVERSION = " << CalibrationVersion << endl;


  //Get time stamp for a given run number
  RunToTime *runtime = RunToTime::instance();
  PHTimeStamp *ts = runtime->getBeginTime(runnumber);
  PHTimeStamp TimeStp = *ts;
  delete ts;

  mr = 128;
  dBbcRawWrapper* dBbcRawReCal = new dBbcRawWrapper("dBbcRawReCal", mr);
  PHTableNode_t* dBbcRawReCalNode = new PHTableNode_t(dBbcRawReCal, "dBbcRawReCal");
  bbcNode->addNode(dBbcRawReCalNode);

  mr = 1;
  dBbcDCMWrapper* dBbcDCMReCal = new dBbcDCMWrapper("dBbcDCMReCal", mr);
  PHTableNode_t* dBbcDCMReCalNode = new PHTableNode_t(dBbcDCMReCal, "dBbcDCMReCal");
  outNode[DCMNODE]->addNode(dBbcDCMReCalNode);

  mr = 1;
  dBbcGeoWrapper* dBbcGeo = new dBbcGeoWrapper("dBbcGeo", mr);
  PHTableNode_t* dBbcGeoNode = new PHTableNode_t(dBbcGeo, "dBbcGeo");
  outNode[PARNODE]->addNode(dBbcGeoNode);
  mBbcSetGeo.event(topNode);

  dBbcGhitRawParWrapper* dBbcGhitRawPar = new dBbcGhitRawParWrapper("dBbcGhitRawPar", mr);
  PHTableNode_t* dBbcGhitRawParNode = new PHTableNode_t(dBbcGhitRawPar, "dBbcGhitRawPar");
  outNode[PARNODE]->addNode(dBbcGhitRawParNode);

  mr = 10000;
  dBbcGhitRawWrapper* dBbcGhitRaw = new dBbcGhitRawWrapper("dBbcGhitPar", mr);
  PHTableNode_t* dBbcGhitRawNode = new PHTableNode_t(dBbcGhitRaw, "dBbcGhitRaw");
  outNode[EVANODE]->addNode(dBbcGhitRawNode);

  mr = 128;
  dBbcUcalWrapper* dBbcUcal = new dBbcUcalWrapper("dBbcUcal", mr);
  PHTableNode_t* dBbcUcalNode = new PHTableNode_t(dBbcUcal, "dBbcUcal");
  bbcNode->addNode(dBbcUcalNode);

  // Setting dBbcGeo Parameters
  nrc = 1;
  dBbcGeo->SetRowCount(nrc);
  dBbcGeo->set_MaxPmtNo(0, 128);


  // Setting dBbcGhitRawPar Parameters
  dBbcGhitRawPar->SetRowCount(nrc);
  // K.Homma
  //dBbcGhitRawPar->set_SimFlag(0, 2000); // No DB access for Run1
  //dBbcGhitRawPar->set_SimFlag(0, 2001); // Ascii DB access for Run2
  dBbcGhitRawPar->set_SimFlag(0, 3000);   // Above 2001 specifies PG DB access with version 3000 or later.
  dBbcGhitRawPar->set_RunNumber(0, runnumber);
  dBbcGhitRawPar->set_CalibVersion(0, rc->get_IntFlag("BBCCALIBVERSION"));

  mr = 1;
  dBbcRawHitParWrapper* dBbcRawHitPar = new dBbcRawHitParWrapper("dBbcRawHitPar", mr);
  PHTableNode_t* dBbcRawHitParNode = new PHTableNode_t(dBbcRawHitPar, "dBbcRawHitPar");
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
  dBbcRawWrapper* dBbcRaw = findNode::getClass<dBbcRawWrapper>(topNode,"dBbcRaw");
  if ( !dBbcRaw )
    {
      dBbcRaw = new dBbcRawWrapper("dBbcRaw", mr);
      PHTableNode_t* dBbcRawNode = new PHTableNode_t(dBbcRaw, "dBbcRaw");
      bbcNode->addNode(dBbcRawNode);
    }

  BbcRaw *bbcraw = findNode::getClass<BbcRaw>(topNode,"BbcRaw");
  if ( ! bbcraw )
    {
      BbcRaw *bbcraw = new BbcRawv1();
      PHObjectNode_t* BbcRawNode = new PHObjectNode_t(bbcraw, "BbcRaw", "PHObject");
      outNode[DSTNODE]->addNode(BbcRawNode);
    }

  BbcCalib* BbcCalibPar = findNode::getClass<BbcCalib>(topNode,"BbcCalibPar");
  if ( ! BbcCalibPar )
    {
      BbcCalibPar = new BbcCalib();
      BbcCalibNode_t *BbcCalibParNode = new BbcCalibNode_t(BbcCalibPar, "BbcCalibPar");
      outNode[PARNODE]->addNode(BbcCalibParNode);
    }

  // pmt mapping different in sims
  BbcCalibPar->setSimulation(2);


  //K.Homma
  cout << "BbcSimreco> restored constants are for " << TimeStp << " ; parameters dump has been suppressed " << endl;
  BbcCalibPar->restore(TimeStp, rc->get_IntFlag("BBCCALIBVERSION"));
  if (verbosity > 0) BbcCalibPar->showParameters();
  //K.Homma

  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut", 1);
  PHTableNode_t* dBbcOutNode = new PHTableNode_t(dBbcOut, "dBbcOut");
  bbcNode->addNode(dBbcOutNode);

  BbcOut *bbc = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (!bbc)
    {
      BbcOut* bbcout = new BbcOutv1();
      PHObjectNode_t* BbcOutNode = new PHObjectNode_t(bbcout, "BbcOut", "PHObject");
      outNode[DSTNODE]->addNode(BbcOutNode);
    }

  dBbcDCMWrapper* dBbcDCM = new dBbcDCMWrapper("dBbcDCM", 1);
  PHTableNode_t* dBbcDCMNode = new PHTableNode_t(dBbcDCM, "dBbcDCM");
  outNode[DCMNODE]->addNode(dBbcDCMNode);

  dBbcFEMWrapper *dBbcFEM = new dBbcFEMWrapper("dBbcFEM",1);
  PHTableNode_t* dBbcFEMNode = new PHTableNode_t(dBbcFEM, "dBbcFEM");
  bbcNode->addNode(dBbcFEMNode);

  mBbcSetUcal.event(topNode);

  return 0;
}

//_______________________________________________________
int BbcSimreco::process_event(PHCompositeNode *topNode)
{
  if (verbosity>0)
    cout << PHWHERE << "mBbcGhitRaw" << endl;
  mBbcGhitRaw.event(topNode);

  if (verbosity>0)
    cout << PHWHERE << "mBbcFEM" << endl;
  mBbcFEM.event(topNode);

  if (verbosity>0)
    cout << PHWHERE << "mBbcDCM" << endl;
  mBbcDCM.event(topNode);

  if (verbosity>0)
    cout << PHWHERE << "BbcPutDCM" << endl;
  BbcPutDCM(topNode);

  if (verbosity>0)
    cout << PHWHERE << "mBbcRawOUt" << endl;
  mBbcRawOut.event(topNode);

  return 0;
}

//_______________________________________________________
int BbcSimreco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("BBC")) mainIter.forEach(reset);
  return 0;
}
