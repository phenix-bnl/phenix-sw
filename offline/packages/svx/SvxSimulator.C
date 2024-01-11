// ===============
// FILE: SvxSimulator.C
// ===============

// ******************************************************
//
// Class: SvxSimulator implementation
//
// Purpose: Reads PISA output and creates raw hits for
//          Silicon Vertex Detector.
//
// Author: Sasha Lebedev (lebedev@iastate.edu
//
// Revisions: December 2009 - initial version
//
// ***************************************************************************

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <cstdio>

#include <SvxCommon.h>
#include <SvxSimulator.h>
#include <SvxGhitv1.h>
#include <SvxGhitListv1.h>
#include <SvxRawhitv4.h>
#include <SvxRawhitListv1.h>
#include <SvxRawhitListv4.h>
#include <SvxGhitRawhitv1.h>
#include <SvxGhitRawhitListv1.h>
#include <SvxPixel1v1.h>
#include <SvxStrip11v2.h>
#include <SvxPisaHitv1.h>
#include <SvxGetGEA.h>
#include <svxDetectorGeo.hh>
//#include <svxAddress.hh>

#include <RunHeader.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <getClass.h>

#include <PdbCalBank.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCoordinate.hh>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <TFile.h>


using namespace std;

// Helpers for scanning Node Tree...
typedef PHIODataNode <PHObject>             PHObjectNode_t;
typedef PHIODataNode <SvxGhitList>          SvxGhitListNode_t;
typedef PHIODataNode <SvxRawhitList>        SvxRawhitListNode_t;
typedef PHIODataNode <SvxGhitRawhitList>    SvxGhitRawhitListNode_t;
typedef PHIODataNode <SvxPisaHit> SvxNode_t;

typedef PHIODataNode<RunHeader> RunHeaderNode_t;

// SVX geometry definitions: sensor types in the barrel layers
const unsigned int SvxSimulator::barSenType[] = {1, 1, 11, 11}; // sensor type/layer

//-----------------------------------------------------------------------------------------

SvxSimulator::SvxSimulator(const string &name) :
  SubsysReco(name),
  _timer(PHTimeServer::get()->insert_new(name))
{
  barSensor = NULL;

  // Default parameters for stripixel
  m_stripixel_sAQ                = 0.109; // based on beam test
  m_stripixel_sNOISE             = 10.2; // based on measurement (2008 June)
  m_stripixel_adcthre_zs         = 31; // 3 sigma of sNOISE
  m_stripixel_adcthre_rawhit     = 31; // 3 sigma of sNOISE
  m_stripixel_adcthre_rawhit_sum = 50; // a speculated value
}

SvxSimulator::~SvxSimulator() {
//  delete svxGeometry;
  if (barSensor != NULL) delete [] barSensor;
}

//-----------------------------------------------------------------------------------------

void SvxSimulator::set_ChargeAsymXUWidth( float val ) {
  cout << PHWHERE << "Charge asymmetry width for XU sharing is changed from " << m_stripixel_sAQ << " to " << val << endl;
  m_stripixel_sAQ = val;
}

void SvxSimulator::set_StripixelNoise( float val ) {
  cout << PHWHERE << "1 sigma of stripixel noise is changed from " << m_stripixel_sNOISE << " to " << val << endl;
  m_stripixel_sNOISE = val;
}

void SvxSimulator::set_StripixelZeroSup( int val ) {
  cout << PHWHERE << "Zero suppression of stripixel is changed from " << m_stripixel_adcthre_zs << " to " << val << endl;
  m_stripixel_adcthre_zs = val;
}

void SvxSimulator::set_StripixelAdcThreshold(int val) {
  cout << PHWHERE << "Threshold of rawhit ADC for clustering is changed from " << m_stripixel_adcthre_rawhit << " to " << val << endl;
  m_stripixel_adcthre_rawhit = val;
}

void SvxSimulator::set_StripixelAdcSumThreshold(int val) {
  cout << PHWHERE << "Threshold of rawhit summed ADC for clustering is changed from " << m_stripixel_adcthre_rawhit_sum << " to " << val << endl;
  m_stripixel_adcthre_rawhit_sum = val;
}

//--------------------------------------------------------------------------------------

// Run-independent initialization
int SvxSimulator::Init(PHCompositeNode *topNode)
{
  if (verbosity > 0) cout << "SvxSimulator::Init-I: Execution started." << endl;

  // Reading SVX parameters from svxPISA.par file or the database

  ///////////////////////
  // detectorGeo
  // added by T.Hachiya 2011.08.14
  svxDetectorGeo* svxGeometry = findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if ( svxGeometry == NULL) {
    if (verbosity > 0) { cout << PHWHERE << "Can't find svxDetectorGeo. " << endl; }
    return ABORTRUN;
  }


  barSensor = new SvxSensor *[SVXLAYERNUMBER][SVXLADDERNUMBER][SVXSENSORNUMBER];

  cout << "LayerNum: " << SVXLAYERNUMBER << endl;
  for (int i = 0; i < SVXLAYERNUMBER; i++) {
    nBarLadder[i] = svxGeometry->get_nBarLadder(i);
    nBarSensor[i] = svxGeometry->get_nBarSensor(i);
    cout << "SxvSimulator_Init : " << i << " " << nBarLadder[i] << " " << nBarSensor[i] << endl;
  }
  svxSecNlayers[0] = (unsigned int)SVXLAYERNUMBER;

  if (verbosity > 0) cout << "SvxSimulator::Init-I: Creating sensor objects. Strip noise sigma = " << m_stripixel_sNOISE << endl;
  for (unsigned int ilr = 0; ilr < SVXLAYERNUMBER; ilr++)  {
    for (unsigned int ild = 0; ild < nBarLadder[ilr]; ild++) {
      for (unsigned int isn = 0; isn < nBarSensor[ilr]; isn++) {
        barSensor[ilr][ild][isn] = svxGeometry->GetSensorPtr(ilr, ild, isn);
        // set parameters for stripixel
        if (ilr > 1) {
          barSensor[ilr][ild][isn] -> set_sAQ               (m_stripixel_sAQ);
          barSensor[ilr][ild][isn] -> set_sNOISE            (m_stripixel_sNOISE);
          barSensor[ilr][ild][isn] -> set_adcthre_zs        (m_stripixel_adcthre_zs);
          barSensor[ilr][ild][isn] -> set_adcthre_rawhit    (m_stripixel_adcthre_rawhit);
          barSensor[ilr][ild][isn] -> set_adcthre_rawhit_sum(m_stripixel_adcthre_rawhit_sum);
        }
      }
    }
  }


  if (verbosity > 0) cout << "SvxSimulator::Init-I: Execution completed." << endl;

  return EVENT_OK;

}

//--------------------------------------------------------------------------------------

// Run-dependent initialization
int SvxSimulator::InitRun(PHCompositeNode *topNode)
{

  cout << PHWHERE << " myrun" << endl;

  if (verbosity > 0) cout << "SvxSimulator::InitRun-I: Started." << endl;

  // Find the Run Header
  //RunHeader *d_runhdr=0;
  //PHTypedNodeIterator<RunHeader> iRUN(topNode);
  //RunHeaderNode_t *RUN = iRUN.find("RunHeader");
  //if (RUN) d_runhdr = RUN->getData();
  //if (!d_runhdr)
  //  {
  //    cerr << PHWHERE << "SvxSimulator::InitRun ERROR RunHeader not in Node Tree" << endl;
  //    return 1;
  //  }
  //int runNumber = d_runhdr->get_RunNumber();

  // Set up the node tree
  int i = CreateNodeTree(topNode);
  if (verbosity > 0) cout << "SvxSimulator::InitRun-I: CreateNodeTree returned " << i << endl;
  if (!(i == EVENT_OK)) {return EVENT_OK;}

  ///////////////////////
  // added by T.Hachiya 2011.06.17
  // check if the svxAddress exist in the nodetree.
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if (verbosity > 0) { cout << PHWHERE << "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  //SvxAddressObject = address;
  ///////////////////////



  if (verbosity > 0) cout << "SvxSimulator::InitRun-I: Finished." << endl;

  return EVENT_OK;

}

//-------------------------------------------------------------------------------------------

int SvxSimulator::process_event(PHCompositeNode *topNode)
{
  _timer.get()->restart();

  if (verbosity > 0) cout << "SvxSimulator::process_event-I: Execution started..." << endl;

  int iError;
//  if(verbosity>0)  { cout << "SvxSimulator::process_event-I: topNode:"<< endl; topNode->print(); }

  // Initialization
  nSvxGhits          = 0;
  nSvxRawhits        = 0;

  if (verbosity > 0) cout << "SvxSimulator::process_event-I: Getting nodes." << endl;
  GetNodes(topNode);

  /////////
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if (verbosity > 0) { cout << PHWHERE << "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  svxAddress& SvxAddressObject = *address;
  /////////




  if (verbosity > 0) cout << "SvxSimulator::process_event-I: resetting data nodes..." << endl;
  // Reset data nodes
  if ( d_ghit == NULL || d_rawhit == NULL || d_ghit2rawhit == NULL ) {
    cerr << "SvxSimulator::process_event == no Ghit or Rawhit or Ghit2Rawhit" << endl;
    return EVENT_OK;
  }

  d_ghit          ->Reset() ;
  d_rawhit        ->Reset() ;
  d_ghit2rawhit   ->Reset() ;

  // Reset SVX sensors
  // """""""""""""""""
  for ( unsigned int ilr = 0; ilr < svxSecNlayers[0]; ilr++ )          {
    for ( unsigned int ild = 0; ild < nBarLadder[ilr]; ild++ )   {
      for ( unsigned int isn = 0; isn < nBarSensor[ilr]; isn++ ) {
        barSensor[ilr][ild][isn]->Reset();
      } // isn
    } // ild
  } // ilr


  if (verbosity > 0) cout << "SvxSimulator::process_event-I: starting SvxGetGEA..." << endl;
// copy svx hits from SvxPISAHit to SvxPisaHit
  long int tmp = SvxGetGEA(topNode);
  if (verbosity > 0) cout << "SvxSimulator::process_event-I: SvxGetGEA returned " << tmp << endl;

  // Find svx pisa hits node

  SvxPisaHit *svx = 0;
  PHTypedNodeIterator<SvxPisaHit> svxiter(topNode);
  SvxNode_t *SvxNode = svxiter.find("SvxPisaHit");
  if (SvxNode) {
    svx = SvxNode->getData();
  } else {
    cerr << PHWHERE << " ERROR: SvxPisaHit Node missing. Aborting." << endl; return EVENT_OK;
  }

  if (verbosity > 0) cout << "SvxSimulator::process_event-I: Svx count: " << svx->GetnHit() << endl;
  if (verbosity > 0) cout << "SvxSimulator::process_event-I: filling GEANT hits." << endl;

  if ( ( iError = fillGhitList(svx) ) ) { cerr << PHWHERE << " ERROR: fillGhitList() = " << iError << endl; }

  if (verbosity > 0) cout << "SvxSimulator::process_event-I: filling raw hits." << endl;
  if ( ( iError = fillRawhitList(SvxAddressObject) ) ) {
    cerr << PHWHERE << " ERROR: fillRawhitList() = " << iError << endl;
  }

  if (verbosity > 0) cout << "SvxSimulator::process_event-I: Event processed." << endl;

  _timer.get()->stop();
  return EVENT_OK;
}

//-------------------------------------------------------------------------------------------

// Create the data
int SvxSimulator::CreateNodeTree(PHCompositeNode *topNode)
{

  if (verbosity > 0) cout << "SvxSimulator::CreateNodeTree-I: Execution started." << endl;

  PHNodeIterator iter(topNode);

  // Looking for the DST node
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
  {
    cerr << PHWHERE << "DST Node missing, doing nothing." << endl; return EVENT_OK;
  }

  // Looking for the GEA node
  PHCompositeNode *geaNode;
  geaNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "GEA"));
  if (!geaNode)
  {
    cerr << PHWHERE << "GEA Node missing, doing nothing." << endl; return EVENT_OK;
  }

// Create INPUT SvxPisaHit node
  PHObjectNode_t *SvxInputNode =  static_cast<PHObjectNode_t*>(iter.findFirst("PHIODataNode", "SvxPisaHit"));
  if (!SvxInputNode) {
    SvxPisaHit *svxin = new SvxPisaHitv1();
    SvxInputNode = new PHObjectNode_t(svxin, "SvxPisaHit", "PHObject");
    geaNode->addNode(SvxInputNode);
  }

  // Create the SVX node.
  PHCompositeNode* svxNode = dynamic_cast<PHCompositeNode*>
                             (iter.findFirst("PHCompositeNode", "SVX"));
  if (! svxNode)
  {
    svxNode = new PHCompositeNode("SVX"); dstNode->addNode(svxNode);
  }

  PHIODataNode<PHObject>* SvxGhitListNode = NULL;
  SvxGhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxGhitList");
  if (!SvxGhitListNode)
  {
    SvxGhitList* svxghits = new SvxGhitListv1();
    SvxGhitListNode =
      new PHIODataNode<PHObject>(svxghits, "SvxGhitList", "PHObject");
    svxNode->addNode(SvxGhitListNode);
  }

  PHIODataNode<PHObject>* SvxRawhitListNode = NULL;
  SvxRawhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxRawhitList");
  if (!SvxRawhitListNode)
  {
    SvxRawhitList* svxrawhits = new SvxRawhitListv4();
    SvxRawhitListNode =
      new PHIODataNode<PHObject>(svxrawhits, "SvxRawhitList", "PHObject");
    svxNode->addNode(SvxRawhitListNode);
  }

  PHIODataNode<PHObject>* SvxGhitRawhitListNode = NULL;
  SvxGhitRawhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxGhitRawhitList");
  if (!SvxGhitRawhitListNode)
  {
    SvxGhitRawhitList* svxghit2rawhits = new SvxGhitRawhitListv1();
    SvxGhitRawhitListNode =
      new PHIODataNode<PHObject>(svxghit2rawhits, "SvxGhitRawhitList",
                                 "PHObject");
    svxNode->addNode(SvxGhitRawhitListNode);
  }

  if (verbosity > 0) cout << "SvxSimulator::CreateNodeTree-I: Execution completed." << endl;

  return EVENT_OK;

}

// Grab the data
void SvxSimulator::GetNodes(PHCompositeNode *topNode)
{

  if (verbosity > 0) cout << "SvxSimulator::GetNodes-I: Execution started." << endl;

  // Set all pointers to null...
  d_ghit           = NULL;
  d_rawhit         = NULL;
  d_ghit2rawhit    = NULL;

  // Search out the nodes from the node tree...

  PHTypedNodeIterator<SvxGhitList> iGHIT(topNode);
  SvxGhitListNode_t *GHIT = iGHIT.find("SvxGhitList");
  if (GHIT) d_ghit = GHIT->getData();
  if (!d_ghit) cerr << PHWHERE << "SvxSimulator:: ghit data not in Node Tree" << endl;

  PHTypedNodeIterator<SvxRawhitList> iRAWHIT(topNode);
  SvxRawhitListNode_t *RAWHIT = iRAWHIT.find("SvxRawhitList");
  if (RAWHIT) d_rawhit = RAWHIT->getData();
  if (!d_rawhit) cerr << PHWHERE << "SvxSimulator:: rawhit data not in Node Tree" << endl;

  PHTypedNodeIterator<SvxGhitRawhitList> iGHIT2RAWHIT(topNode);
  SvxGhitRawhitListNode_t
  *GHIT2RAWHIT = iGHIT2RAWHIT.find("SvxGhitRawhitList");
  if (GHIT2RAWHIT) d_ghit2rawhit = GHIT2RAWHIT->getData();
  if (!d_ghit2rawhit) cerr << PHWHERE << "SvxSimulator:: ghit2rawhit data not in Node Tree" << endl;

  if (verbosity > 0) cout << "SvxSimulator::GetNodes-I: Execution completed." << endl;

  return;

}

//--------------------------------------------------------------------------------------

int SvxSimulator::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

//------------------------------------------------------------------------------------

int SvxSimulator::generate_ghits()
{
  SvxGhit* ghit;

  if (verbosity > 0) cout << "SvxSimulator::generate_ghits-I: Execution started" << endl;

  // Generate 10 tracks with 3 hits each for testing
  for (int iTrack = 0; iTrack < 10; iTrack++) {
    for (int iHit = 0; iHit < 3; iHit++) {

      // Update the ghit list
      ghit = d_ghit->addGhit();

      // Update the individual ghit
      ghit->set_svxSection(SVXBARRELINDEX);
      ghit->set_layer(iHit);
      ghit->set_ladder(2);
      ghit->set_sensor(2);
      ghit->set_isubevent(0);
      ghit->set_nfile(0);
      ghit->set_mctrack(iTrack);
      ghit->set_idPart(8);
      ghit->set_track(iTrack);
      ghit->set_dele(0.0);
      ghit->set_tof(0.0);
      for (int i = 0; i < 3; i++) {
        ghit->set_xyzglobal  (i, 1.0 * iHit * iTrack);
        ghit->set_xyzlocalin (i, 2.0 * iHit * iTrack);
        ghit->set_xyzlocalout(i, 3.0 * iHit * iTrack);
        ghit->set_pmomxyz    (i, 10.0 * iHit * iTrack);
      }  // i
    }  // iHit
  }  // iTrack

  if (verbosity > 0) cout << "SvxSimulator::GetNodes-I: Execution completed" << endl;

  return EVENT_OK;  // no error
}

//------------------------------------------------------------------------------------

// *********************************************************
// Copy GEANT hit from the PISAEvent.root file into SvxGhitList
// *********************************************************
int SvxSimulator::fillGhitList(SvxPisaHit* svx)
{
  int      itemp, itrack;
  int      iTest, nTest;
  float    xtemp, ytemp, ztemp;
  SvxGhit* ghit = NULL;

  if (verbosity > 0) cout << "SvxSimulator::fillGhitList-I: Execution started." << endl;

  if (verbosity > 0) cout << "SvxSimulator::fillGhitList-I: Processing " << svx->GetnHit() << " hits." << endl;

  for (int khit = 0; khit < svx->GetnHit(); khit++) {

    // Find svxSection (0-Barrel, 1-North, 2-South), layer, ladder and sensor
    itemp = (int) (svx->GetHit(khit))->GetLayer();
    iTest = 0;
    int i;
    for (i = 0; (itemp > (nTest = iTest + svxSecNlayers[i])) && (i < NSVXSECTIONS); i++)
    {
      iTest = nTest;
    }

    // Set SVX section & layer
    int isection = i;
    itemp -= (iTest + 1);
    int ilayer = itemp;
    int iladder = -999;
    int isensor = -999;

    //Modified the following piece of code to run PISA with an abitrary geometry file following Hubert's instructions
    //J. Orjuela-Koop 08.03.16
    if ( i == SVXBARRELINDEX ) {                             // Barrel

      itemp = (int) (svx->GetHit(khit))->GetHitVolume(3);      // HvH: ladder index (was 3)
      iladder = itemp - 1;
      if (iladder != -1) {                                     // 3 was OK, so pre-2016 geometry
        itemp = (int) (svx->GetHit(khit))->GetHitVolume(4);    // sensor on the ladder (was 4)
        isensor = itemp - 1;
      }
      else {                                                   // 3 is the wrong index, so
        itemp = (int) (svx->GetHit(khit))->GetHitVolume(4);    // new ladder index
        iladder = itemp - 1;
        itemp = (int) (svx->GetHit(khit))->GetHitVolume(6);    // new sensor index
        isensor = itemp - 1;
      }
      if (isensor < 0) continue;
    } else {                                                // Endcap
      iladder = 0;
      isensor = 0;
      if (verbosity > 9) { cerr << PHWHERE << " ERROR:  EndCaps Hit !!! " << endl; }
    }

    /*
    if ( i == SVXBARRELINDEX ) {                             // Barrel
      itemp = (int) (svx->GetHit(khit))->GetHitVolume(3);
      iladder = itemp-1;
      itemp = (int) (svx->GetHit(khit))->GetHitVolume(4);
      isensor = itemp-1;
      if (isensor < 0) continue;
    } else {                                                // Endcap
       iladder = 0;
       isensor = 0;
      if(verbosity>9) { cerr << PHWHERE << " ERROR:  EndCaps Hit !!! " << endl; }
    }
    */

    // Make a ghit object and set other parameters
    ghit = d_ghit->addGhit();
    ghit->set_svxSection(isection);
    ghit->set_layer(ilayer);
    ghit->set_ladder(iladder);
    ghit->set_sensor(isensor);

    itrack = (int) (svx->GetHit(khit))->GetMctrack();
    ghit->set_mctrack(itrack);

    int iidpart = (int) (svx->GetHit(khit))->GetIdPart();
    ghit->set_idPart(iidpart);

    itemp = (int) (svx->GetHit(khit))->GetTrack();
    ghit->set_track(itemp);

    float xdele = (float) (svx->GetHit(khit))->GetDele();
    ghit->set_dele(xdele);

    float xtof = (float) (svx->GetHit(khit))->GetTof();
    ghit->set_tof(xtof);

    itemp = (int) (svx->GetHit(khit))->GetIsubevent();
    ghit->set_isubevent(itemp);

    itemp = (int) (svx->GetHit(khit))->GetNfile();
    ghit->set_nfile(itemp);

    float xglob = (float) (svx->GetHit(khit))->GetXGlobal();
    float yglob = (float) (svx->GetHit(khit))->GetYGlobal();
    float zglob = (float) (svx->GetHit(khit))->GetZGlobal();
    ghit->set_xyzglobal(0, xglob);
    ghit->set_xyzglobal(1, yglob);
    ghit->set_xyzglobal(2, zglob);

    xtemp = (float) (svx->GetHit(khit))->GetXLocalIn();
    ytemp = (float) (svx->GetHit(khit))->GetYLocalIn();
    ztemp = (float) (svx->GetHit(khit))->GetZLocalIn();
    ghit->set_xyzlocalin(0, xtemp);
    ghit->set_xyzlocalin(1, ytemp);
    ghit->set_xyzlocalin(2, ztemp);

    xtemp = (float) (svx->GetHit(khit))->GetXLocalOut();
    ytemp = (float) (svx->GetHit(khit))->GetYLocalOut();
    ztemp = (float) (svx->GetHit(khit))->GetZLocalOut();
    ghit->set_xyzlocalout(0, xtemp);
    ghit->set_xyzlocalout(1, ytemp);
    ghit->set_xyzlocalout(2, ztemp);

    xtemp = (float) (svx->GetHit(khit))->GetPmomX();
    ytemp = (float) (svx->GetHit(khit))->GetPmomY();
    ztemp = (float) (svx->GetHit(khit))->GetPmomZ();
    ghit->set_pmomxyz(0, xtemp);
    ghit->set_pmomxyz(1, ytemp);
    ghit->set_pmomxyz(2, ztemp);

    if (verbosity > 9 && isection == 0) {
      cout << "   layer,ladder,sensor,xyzr: " << ilayer << " " << iladder << " " << isensor << " " << xglob << " " << yglob
           << " " << zglob << " " << sqrt(xglob * xglob + yglob * yglob) << endl;
    }

    nSvxGhits++;

  }  // end loop over khit

  int EXIT_STATUS = ( nSvxGhits == d_ghit->get_nGhits() ) ? 0 : -1;

  if (verbosity > 0) cout << "SvxSimulator::fillGhitList-I: Filled " << nSvxGhits << " GEANT hits in the event." << endl;

  if (verbosity > 0) cout << "SvxSimulator::fillGhitList-I: Execution completed." << endl;

  return EXIT_STATUS;  // 0 if no error

}

// ***************************************************************************
// Compute raw hits from SvxGhitList *d_ghit,
// fill out SvxRawhitList *d_rawhit and SvxGhitRawhitList *d_ghit2rawhit
// ***************************************************************************
int SvxSimulator::fillRawhitList(svxAddress& SvxAddressObject)
{
  if (verbosity > 0) cout << "SvxSimulator::fillRawhitList-I: Execution started." << endl;

  if (verbosity > 0) cout << "SvxSimulator::fillRawhitList-I: Processing " << nSvxGhits << " GEANT hits." << endl;

  // Make raw hits and fill global lists d_rawhit & d_ghit2rawhit
  // """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  d_ghit->sort_sensorID();

  int nhit_real_sum = 0, nhit_noise_sum = 0;
  for ( unsigned int ilr = 0; ilr < svxSecNlayers[0]; ilr++ )    {
    for ( unsigned int ild = 0; ild < nBarLadder[ilr]; ild++ )   {
      for ( unsigned int isn = 0; isn < nBarSensor[ilr]; isn++ ) {
        int ng2rhit = d_ghit2rawhit->get_nGhitRawhits();

        // Add noise for stripixel by 2008/06/01 M.Togawa
        // Moved from the orginal line by K. Nakano 2008/07/03
        int nhit_real = barSensor[ilr][ild][isn]->
                        makeRawhits(d_ghit, d_rawhit, d_ghit2rawhit, &SvxAddressObject);
        int nhit_noise = barSensor[ilr][ild][isn]->
                         AddNoise(nSvxRawhits, nSvxRawhits + nhit_real, d_rawhit, d_ghit2rawhit, ng2rhit);

        nSvxRawhits += nhit_real + nhit_noise;
        nhit_real_sum  += nhit_real;
        nhit_noise_sum += nhit_noise;
      } // isn
    } // ild
  } // ilr

  if (verbosity > 0) cout << "   " << PHWHERE << " Nhit (real, noise) = " << nhit_real_sum + nhit_noise_sum
                            << " (" << nhit_real_sum << " " << nhit_noise_sum << ")" << endl;

  // renumber to make consistency between idx of rawlist and rawhitid
  renumberRawhitID();

  //{ //-- debug print
  //  for(int ihit=0; ihit<d_rawhit->get_nRawhits(); ihit++){
  //    SvxRawhit *raw = d_rawhit->get_Rawhit(ihit);
  //    if(raw==NULL){
  //      cout<<ihit<<" raw is NULL"<<endl;
  //      continue;
  //    }
  //    bool issame = (ihit==raw->get_hitID());
  //    cout<<ihit<<" raw "<<raw->get_hitID()<<" "<<((issame)?"same":"different")<<endl;
  //  }
  //}

  // Remove holes (from removed hits) in the hit lists
  //  d_rawhit->Compress();
  //  d_ghit2rawhit->Compress();

  if (verbosity > 0) cout << "SvxSimulator::fillRawhitList-I: nSvxRawhits = " << nSvxRawhits << endl;
  int EXIT_STATUS = ( nSvxRawhits == d_rawhit->get_nRawhits() ) ? 0 : -1;

  if (verbosity > 0) cout << "SvxSimulator::fillRawhitList-I: Filled " << nSvxRawhits << " Rawhits in the event." << endl;
  if (verbosity > 0) cout << "SvxSimulator::fillRawhitList-I: Execution completed." << endl;

  return EXIT_STATUS; // 0 if no errors
}

void SvxSimulator::renumberRawhitID()
{
  // make conversion table and put idx to rawhitID
  map<int, int> vIdMap; // int: original rawid, int: new rawid
  for (int ihit = 0; ihit < d_rawhit->get_nRawhits(); ihit++) {
    SvxRawhit *raw = d_rawhit->get_Rawhit(ihit);
    if (raw == NULL) {
      cout << ihit << " raw is NULL" << endl;
      continue;
    }

    int org_id = raw->get_hitID();

    map<int, int>::iterator itr = vIdMap.find(org_id);
    if (itr == vIdMap.end()) { // org_id is not in the map, then add

      raw->set_hitID(ihit); // ihit is new id
      vIdMap.insert(pair<int, int>(org_id, ihit));
    }
    else {
      if (verbosity > 0) cout << ihit << " raw is already in the map : " << org_id << endl;
    }
  }

  // renumber g2r by using vIdMap
  for (int ig2r = 0; ig2r < d_ghit2rawhit->get_nGhitRawhits(); ig2r++) {
    SvxGhitRawhit* g2raw = d_ghit2rawhit->get_GhitRawhit(ig2r);
    if (g2raw == NULL) {
      cout << ig2r << " g2raw is NULL" << endl;
      continue;
    }

    int rawnewid = -1;

    int rawoldid = g2raw->get_rawhitID();
    map<int, int>::iterator itr = vIdMap.find(rawoldid);
    if (itr != vIdMap.end()) {
      rawnewid = itr->second;
    }
    else {
      if (verbosity > 0) cout << "orgid " << rawoldid << " is removed : put -1" << endl;
    }

    g2raw->set_rawhitID(rawnewid);
  }
}
