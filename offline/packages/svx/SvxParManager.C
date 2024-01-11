// ===============
// FILE: SvxParManager.C
// ===============

// ******************************************************
//
// Class: SvxParManager implementation
//
// Author:  Takashi Hachiya (hachiya@rcf.rhic.bnl.gov)
//
// Revisions: August 2011 - initial version
//
// ***************************************************************************


#include "SvxParManager.h"
#include "svxAddress.hh"
#include "svxDetectorGeo.hh"
#include "SvxPixelHotDeadMap.h"
#include "SvxPixelHotDeadMapv2.h"
#include "SvxDeadMap.h"
#include "SvxBeamCenterPar.h"
#include "SvxStripThreshold.h"
#include "SvxDaqErrorMap.h"

#include <Fun4AllReturnCodes.h>
#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <recoConsts.h>
#include <getClass.h>
#include <RunToTime.hh>

#include <cstdlib>
#include <iostream>
#include <iomanip>

using namespace std;

//----------------------------------------------------------------------------------------------------

SvxParManager::SvxParManager(const string &name):
SubsysReco(name),
m_readGeofile(0),
m_readRefDiffPixelMapFiles(0),  // New ref/diff format
m_useRefDiffPixelMap(true),     // New ref/diff format now default, D. McGlinchey 10/25/2013
m_readPixelMapfile(0),          // To be deprecated
m_usePixelMap(false),
m_readStripHotDeadFromFile(false),
m_PixChipFile(""),
m_RefPixChannelFile(""),
m_DifPixChannelFile(""),
m_StripHotDeadFileName("svxStripDeadMap.txt"),
m_StripHotDeadHybridsFileName("svxStripHybridsDeadMap.txt"),
m_StripHotDeadReadoutsFileName("svxStripReadoutsDeadMap.txt"),
m_GeometryFileName("svxPISA.par"),
m_beamcenterFromDB(true),
m_offsetFromDB(true),
m_useProductionGeoFlag(false),
m_stripThresholdFileName("threshold.h"),
m_useStripThresholdDatabase(true),
m_useOldStripThresholdFile (true),
m_useDaqErrorMapDatabase(true),
m_daqErrorMapFileName("svxDaqErrorMap.txt"),
m_oldhotdead(false)
{
  m_address  = new svxAddress();
  m_geometry = new svxDetectorGeo();
  m_pixelhotdead = new SvxPixelHotDeadMap();
  m_pixelhotdead2 = new SvxPixelHotDeadMapv2();
  m_striphotdead = new SvxDeadMap();
  m_beamcenter = new SvxBeamCenterPar();
  m_stripthreshold = new SvxStripThreshold(m_address);
  m_daqerr = new SvxDaqErrorMap();
  m_timeStamp = PHTimeStamp();
  m_timeStamp.setToSystemTime();
}

SvxParManager::~SvxParManager()
{
  //  cout<<"SvxParManager::~SvxParManager"<<endl;
  //  svxAddress will be deleted in Fun4All node tree.
  //  Here, I do not delete this.
  //  if (m_address!=NULL) {
  //    delete m_address;
  //    m_address=NULL;
  //  }
  //  if (m_geometry!=NULL) {
  //    delete m_geometry;
  //    m_geometry=NULL;
  //  }
  //  if (m_pixelhotdead!=NULL) {
  //    delete m_pixelhotdead;
  //    m_pixelhotdead=NULL;
  //  }
}


//------------------------------------------------------------------------------------------------------

// Run-independent initialization
int SvxParManager::Init(PHCompositeNode *topNode)
{
  if (verbosity>0) cout << "SvxParManager::Init() Execution started." << endl;

  // svxAddress initialize
  m_address->Initialize();

  int i = CreateNodeTree(topNode);
  if (verbosity>0) cout << "SvxParManager::Init() CreateNodeTree() returned " << i << endl;
  if (!(i==EVENT_OK)) {return EVENT_OK;}


  if (verbosity>0) cout << "SvxParManager::Init() Execution completed." << endl;

  return EVENT_OK;
}

//----------------------------------------------------------------------------------------------------

// Run-dependent initialization
int SvxParManager::InitRun(PHCompositeNode *topNode)
{

  if (verbosity>0) cout << "SvxParManager::InitRun() Execution started.." << endl;

  recoConsts *rc = recoConsts::instance();
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  if (verbosity>0) cout<<"SvxParManager::InitRun runnumber= "<<runnumber<<endl;

  // Use new pixel hot/dead database format starting with Run 14 He+Au
  if(runnumber>=415751)
    m_oldhotdead = false;

  RunToTime *rt = RunToTime::instance();
  PHTimeStamp* tStamp = rt->getBeginTime(runnumber);
  m_timeStamp = *(tStamp);
  delete tStamp;


  // Initialize svxAddress
  if ( runnumber<340200 ) {
    if (verbosity>0) { cout << "Use old map for pixel" << endl; }
    m_address->set_useoldmap(true);
    m_address->Initialize();
  }

  // Initialize SvxPixelHotDeadMap
  if (verbosity>0) {
    m_pixelhotdead->Verbosity(verbosity);
    m_pixelhotdead2->Verbosity(verbosity);
  }

  // ----------------------- Original code (to be deprecated) --------------------------------
  if (m_usePixelMap)
  {
    if (m_readPixelMapfile==0) { // read from DB
      if (verbosity>0){cout << "SvxParManager::InitRun: Reading Chip Hot_Dead Map from database." << endl;}
      if (( m_oldhotdead && !m_pixelhotdead->readChipMapFromDatabase(runnumber)) ||
          (!m_oldhotdead && !m_pixelhotdead2->readChipMapFromDatabase(runnumber)) ){
            cerr << PHWHERE << "ERROR reading Chip HotDead Map from database." << endl;
      }
      
      if (verbosity>0)
        cout << "SvxParManager::InitRun: Reading Hot_Dead Map from database." << endl;
      
      if (( m_oldhotdead && !m_pixelhotdead->readFromDatabase(runnumber)) ||
          (!m_oldhotdead && !m_pixelhotdead2->readFromDatabase(runnumber))){
        cerr << PHWHERE << "ERROR reading VtxPixel HotDead Map from database." << endl;
      }

    } else { // read from file
      if (verbosity>0)
        cout << "SvxParManager::InitRun: Reading Hot_Dead Map from svxPISA.par." << endl;
      cout<<"SvxParManager: readPixelFromFile is not implement yet "<<endl;

      int run, run1, run2;
      if (!m_pixelhotdead->readChipMapFromFile(run, run, m_PixelHotDeadChipFileName)) {
        cerr << PHWHERE << "ERROR reading VtxPixel Chip HotDead Map from database." << endl;
      }
      if (!m_pixelhotdead->readPixelMapFromFile(run1, run2, m_PixelHotDeadPixelFileName)) {
        cerr << PHWHERE << "ERROR reading VtxPixel Pixel HotDead Map from database." << endl;
      }
    }
  }
  // ------------------------------------------------------------------------------------------

  // ----------------------- New ref/diff format ----------------------------------------------
  if (m_useRefDiffPixelMap)
  {
    bool chipok = false;
    bool chanok = false;
    if (m_readRefDiffPixelMapFiles==0) {  // Get deadmaps from DB
      if (verbosity>0) cout << "SvxParManager::InitRun: Reading Chip Hot_Dead Map from database." << endl;
      if(m_oldhotdead) {
        chipok = m_pixelhotdead->readChipMapFromDatabase(runnumber);
        chanok = m_pixelhotdead->readFromDatabase(runnumber, true);
      } else {
        chipok = m_pixelhotdead2->readChipMapFromDatabase(runnumber);
        chanok = m_pixelhotdead2->readFromDatabase(runnumber, true);
      }

      if (!chipok) cout << PHWHERE << "ERROR reading Chip HotDead Map from database." << endl;
      if (!chanok) cout << PHWHERE << "ERROR reading VtxPixel HotDead Map from database." << endl;
    }
    else {  // Or text files
      if (verbosity>0) cout << PHWHERE << "Reading Map from text files." << endl;
      int run1, run2;

      if(m_oldhotdead) {
        chipok = m_pixelhotdead->readChipMapFromFile(run1, run2,
                                                     m_PixChipFile);
        chanok = m_pixelhotdead->readPixelMapsFromFile(run1, run2,
                                                       m_RefPixChannelFile, m_DifPixChannelFile);
      } else {
        chipok = m_pixelhotdead2->readChipMapFromFile(m_PixChipFile);
        chanok = m_pixelhotdead2->readPixelMapsFromFile(run1, run2,
                                                        m_RefPixChannelFile, m_DifPixChannelFile);
      }

      if (!chipok) cout << PHWHERE << "Problem with readChipMapFromFile()" << endl;
      if (!chanok) cout << PHWHERE << "Problem with readPixelMapsFromFile()" << endl;
    }

  }
  // ------------------------------------------------------------------------------------------


  // fill the % good pixels in each tile, regardless of which pixel deadmap is used.
  if(m_oldhotdead) m_pixelhotdead->setTileMap();
  else             m_pixelhotdead2->setTileMap();



  // Initialize Svx(Strip)HotDeadMap
  if (m_readStripHotDeadFromFile) {
    if (verbosity>0) cout << "SvxParManager::InitRun: Reading Strip Hot/Dead Map from ASCII File." << endl;
    bool read_readout_sucess = m_striphotdead->readReadoutsFromReadoutsFile(m_StripHotDeadReadoutsFileName.c_str());
    bool read_sucess = m_striphotdead->readFromFile(m_StripHotDeadFileName.c_str());
    if(!read_readout_sucess) {std::cerr << PHWHERE << "Can't open input file " << m_StripHotDeadReadoutsFileName.c_str() << std::endl; return 1;}
    if(!read_sucess) {std::cerr << PHWHERE << "Can't open input file " << m_StripHotDeadFileName.c_str() << std::endl; return 1;}
  }
  else {
    if (verbosity>0) cout << "SvxParManager::InitRun: Reading Strip Hot/Dead Map from Database." << endl;
    bool read_readout_sucess = m_striphotdead->readReadoutsFromDatabase(&m_timeStamp);
    bool read_sucess = m_striphotdead->readFromDatabase(&m_timeStamp);
    if(!read_readout_sucess) {std::cerr << PHWHERE << "Can't find strip readout map in DB for run:  " << runnumber << std::endl; return 1;}
    if(!read_sucess) {std::cerr << PHWHERE << "Can't find strip channel map in DB for run:  " << runnumber << std::endl; return 1;}
  }

  // Beam Center
  if (m_beamcenterFromDB) {
    if (verbosity>0) cout << "SvxParManager::InitRun: Reading BeamCenter from DB." << endl;
    m_beamcenter->fetchFromDB(runnumber);
  }
  if (verbosity>0) {
    cout<<"SvxParManager Beam Center"<<endl;
    m_beamcenter->print();
  }

  // Coordinate Offset
  if (m_offsetFromDB) {
    if (verbosity>0) cout << "SvxParManager::InitRun: Reading Coordinate Offset from DB." << endl;

    if(m_useProductionGeoFlag) setOffsetBankIdForOldProduction(runnumber);

    m_geometry->Fetch_coordinateOffset(runnumber);
  }
  if (verbosity>0) {
    cout<<"SvxParManager Coordinate Offset"<<endl;
    m_geometry->printCoordinateOffset();
  }

  // svxDetectorGeo initialize
  if (m_readGeofile) {
    cout << PHWHERE << "- Reading geometry from " << m_GeometryFileName << endl;
    if (!m_geometry->Read_svxPISApar(m_GeometryFileName)) {
      cerr << PHWHERE << "ERROR reading svxPISA.par file." << endl;
      exit(1);
    }
  } else {
    cout << PHWHERE << "- Reading geometry from database." << endl;
    if(m_useProductionGeoFlag) setGeoBankIdForOldProduction(runnumber);

    if (!m_geometry->Fetch_svxPISApar(&m_timeStamp)) {
      cerr << PHWHERE << "ERROR reading VTX geometry from database." << endl;
    }
  }

  //////////////////////////
  // StripThreshold //  set threshold, by CHC
  // copy from SvxReco::set_Threshold()
  {
    if (m_useStripThresholdDatabase) {
      bool success = m_stripthreshold->readFromDatabase(&m_timeStamp);
      if (success) {cout << PHWHERE << " Successfully read SVX Strip Thresholds from Database." << endl;}
      else        {cerr << PHWHERE << " ERROR: Failed to read SVX Strip Thresholds from Database." << endl; return 1;}
    }
    else {
      if (m_useOldStripThresholdFile) {
        bool success = m_stripthreshold->readFromThresholdFile(m_stripThresholdFileName);
        if (success) {cout << PHWHERE << " Successfully read SVX Strip Thresholds from OLD ASCII file." << endl;}
        else        {cerr << PHWHERE << " ERROR: Failed to read SVX Strip Thresholds from OLD ASCII file." << endl; return 1;}
      } else {
        bool success = m_stripthreshold->readFromFile(m_stripThresholdFileName);
        if (success) {cout << PHWHERE << " Successfully read SVX Strip Thresholds from ASCII file." << endl;}
        else        {cerr << PHWHERE << " ERROR: Failed to read SVX Strip Thresholds from ASCII file." << endl; return 1;}
      }
    }
    //m_stripthreshold->print();
  }
  //////////////////////////

  // DaqErrorMap
  {
    if (m_useDaqErrorMapDatabase) {
      bool success = m_daqerr->readFromDatabase(runnumber);
      if (success) {cout << PHWHERE << " Successfully read SVX Daq Error from Database." << endl;}
      else        {cerr << PHWHERE << " ERROR: Failed to read SVX Daq Error from Database. Default is used" << endl;}
    }
    else {
      bool success = m_daqerr->readFromFile(m_daqErrorMapFileName.c_str());
      if (success) {cout << PHWHERE << " Successfully read SVX DaqErrors from ASCII file." << endl;}
      else        {cerr << PHWHERE << " ERROR: Failed to read DaqErrors from ASCII file. Default is used" << endl;}
    }
    m_daqerr->printMaskModule();
  }


  if (verbosity>0) cout << "SvxParManager::InitRun() Node tree created." << endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------
void SvxParManager::setGeoBankIdForOldProduction(int runnumber)
{
 if(m_geometry==NULL){
   cerr<<"SvxParManager::setGeoBankIdForOldProduction no geomtry object"<<endl;
   cerr<<"               failed to setBankId for old production"<<endl;
   return;
 }

 if( (415370<=runnumber&&runnumber<=416893)|| // Run14 HeAu running. Runnumbers here are taken from run-control-logbook
     (421707<=runnumber&&runnumber<=438422)   // all the run15 data p+p, p+Au, p+Al
   )
 {  
   const int geo_bank_id = 1;
   m_geometry->setGeoBankIDForOldProduction(geo_bank_id);
 }
 else if  (364574<=runnumber&&runnumber<=368798)   // run12pp510
 {  
   const int geo_bank_id = 2;
   m_geometry->setGeoBankIDForOldProduction(geo_bank_id);
 }
 else{
   cout<<"------------------------"<<endl;
   cout<<" SvxParManager"<<endl;
   cout<<"   set_UseProductionGeo is only available for run14 HeAu dataset (pro102) and run15pp, pAu, pAl(pro104), run12pp510"<<endl;
   cout<<"   If you want to use this flag for other dataset, "<<endl;
   cout<<"   please contact Takashi Hachiya (hachiya@rcf.rhic.bnl.gov), or VTX experts"<<endl;
   cout<<"------------------------"<<endl;
 }
  
}

void SvxParManager::setOffsetBankIdForOldProduction(int runnumber)
{
 if(m_geometry==NULL){
   cerr<<"SvxParManager::setOffsetBankIdForOldProduction no geomtry object"<<endl;
   cerr<<"               failed to setBankId for old production"<<endl;
   return;
 }

 if( 
     (364574<=runnumber&&runnumber<=368798)   // run12pp510
   )
 {  
   const int geo_bank_id = 2;
   m_geometry->setOffsetBankIDForOldProduction(geo_bank_id);
 }
 else{
   cout<<"   bankID for coordinateOffset is set to the old"<<endl;
 }
  
}


//---------------------------------------------------------------------------------------------

int SvxParManager::process_event(PHCompositeNode *topNode)
{

  if (verbosity>0) cout << "SvxParManager::process_event() Execution started..." <<endl;
  if (verbosity>0) {cout << "SvxParManager::process_event() Event processed." <<endl;}
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------------------

// Create the data
int SvxParManager::CreateNodeTree(PHCompositeNode *topNode)
{

  if (verbosity>0) cout << "SvxParManager::CreateNodeTree() Execution started." << endl;

  PHNodeIterator iter(topNode);


  // Look for the PAR node
  PHCompositeNode *parNode;
  parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) {
    cerr << PHWHERE << "PAR node missing, doing nothing." << endl;
    return EVENT_OK;
  }

  PHDataNode<svxAddress>* SvxAddressNode = NULL;
  SvxAddressNode = (PHIODataNode<svxAddress>*)iter.findFirst("PHDataNode", "svxAddress");
  if (!SvxAddressNode)
  {
    SvxAddressNode =
      new PHDataNode<svxAddress>(m_address, "svxAddress");
    parNode->addNode(SvxAddressNode);
  }

  PHDataNode<svxDetectorGeo>* SvxGeometryNode = NULL;
  SvxGeometryNode = (PHIODataNode<svxDetectorGeo>*)iter.findFirst("PHDataNode", "svxDetectorGeo");
  if (!SvxGeometryNode)
  {
    SvxGeometryNode =
      new PHDataNode<svxDetectorGeo>(m_geometry, "svxDetectorGeo");
    parNode->addNode(SvxGeometryNode);
  }

  PHDataNode<SvxPixelHotDeadMapv2>* SvxPixelHotDeadMapNode2 = NULL;
  SvxPixelHotDeadMapNode2 = (PHIODataNode<SvxPixelHotDeadMapv2>*)iter.findFirst("PHDataNode", "SvxPixelHotDeadMapv2");
  if (!SvxPixelHotDeadMapNode2)
  {
    SvxPixelHotDeadMapNode2 =
      new PHDataNode<SvxPixelHotDeadMapv2>(m_pixelhotdead2, "SvxPixelHotDeadMapv2");
    parNode->addNode(SvxPixelHotDeadMapNode2);
  }

  PHDataNode<SvxPixelHotDeadMap>* SvxPixelHotDeadMapNode = NULL;
  SvxPixelHotDeadMapNode = (PHIODataNode<SvxPixelHotDeadMap>*)iter.findFirst("PHDataNode", "SvxPixelHotDeadMap");
  if (!SvxPixelHotDeadMapNode)
  {
    SvxPixelHotDeadMapNode =
      new PHDataNode<SvxPixelHotDeadMap>(m_pixelhotdead, "SvxPixelHotDeadMap");
    parNode->addNode(SvxPixelHotDeadMapNode);
  }

  PHDataNode<SvxDeadMap>* SvxDeadMapNode = NULL;
  SvxDeadMapNode = (PHIODataNode<SvxDeadMap>*)iter.findFirst("PHDataNode", "SvxStripHotDeadMap");
  if (!SvxDeadMapNode)
  {
    SvxDeadMapNode =
      new PHDataNode<SvxDeadMap>(m_striphotdead, "SvxStripHotDeadMap");
    parNode->addNode(SvxDeadMapNode);
  }


  PHDataNode<SvxBeamCenterPar>* SvxBeamCenterParNode = NULL;
  SvxBeamCenterParNode = (PHIODataNode<SvxBeamCenterPar>*)iter.findFirst("PHDataNode", "SvxBeamCenterPar");
  if (!SvxBeamCenterParNode)
  {
    SvxBeamCenterParNode =
      new PHDataNode<SvxBeamCenterPar>(m_beamcenter, "SvxBeamCenterPar");
    parNode->addNode(SvxBeamCenterParNode);
  }

  PHDataNode<SvxStripThreshold>* SvxStripThresholdNode = NULL;
  SvxStripThresholdNode = (PHIODataNode<SvxStripThreshold>*)iter.findFirst("PHDataNode", "SvxStripThreshold");
  if (!SvxStripThresholdNode)
  {
    SvxStripThresholdNode =
      new PHDataNode<SvxStripThreshold>(m_stripthreshold, "SvxStripThreshold");
    parNode->addNode(SvxStripThresholdNode);
  }

  PHDataNode<SvxDaqErrorMap>* SvxDaqErrorMapNode = NULL;
  SvxDaqErrorMapNode = (PHIODataNode<SvxDaqErrorMap>*)iter.findFirst("PHDataNode", "SvxDaqErrorMap");
  if (!SvxDaqErrorMapNode)
  {
    SvxDaqErrorMapNode =
      new PHDataNode<SvxDaqErrorMap>(m_daqerr, "SvxDaqErrorMap");
    parNode->addNode(SvxDaqErrorMapNode);
  }

  if (verbosity>0) parNode->print();


  return EVENT_OK;
}

int SvxParManager::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

void SvxParManager::set_ReadAddrParFromDB(int a) // call address->set_usedatabase
{
  m_address->set_usedatabase(a);
}

void SvxParManager::set_OffsetVtxToCnt(float x, float y, float z)
{
  m_geometry->setOffsetVtxToCnt(x, y, z);
  m_offsetFromDB=false;
}
void SvxParManager::set_OffsetEastToWest(float x, float y, float z)
{
  m_geometry->setOffsetEastToWest(x, y, z);
  m_offsetFromDB=false;
}

void SvxParManager::set_BeamCenter(float x, float y) {
  m_beamcenter->setBeamCenter(x, y);
  m_beamcenterFromDB=false;
}

void SvxParManager::set_RefDiffPixelMapFiles(const std::string &chanref,
                                             const std::string &chandif,
                                             const std::string &chipmap)
{
  m_RefPixChannelFile = chanref;
  m_DifPixChannelFile = chandif;
  m_PixChipFile = chipmap;
}
