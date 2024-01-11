/*
  INFORMATION OF FLAGS
  
  ///////////////////////////////////////
  ///  SvxParManager
  ///////////////////////////////////////

  =========================
  Geometrical configuration
  =========================

  - set_OffsetVtxToCnt(xoffset, yoffset, zoffset)
  Set the coordinate offset between VTX and central arm.
  If you don't call the function, the offset is obtained from database.

  - set_OffsetEastToWest(xoffset, yoffset, zoffset)
  Set the coordinate offset between west and east arms of VTX.
  If you don't call the function, the offset is obtained from database.
  
  - set_BeamCenter(beam_x, beam_y)
  Set rough beam center position
  If you don't call the function, the beam center is obtained from database.

  - set_ReadGeoParFromFile(flag)
  Set a flag to define how to get geometrical information of VTX.
    * flag  = 0 : get from database. (default)
    * flag != 0 : get from a file.

  - set_GeometryFileName(filename)
  Set name of file containing geometrical information of VTX.
  Default file name is "svxPISA.par"

  ================
  Hot/dead channel
  ================

  - set_ReadPixelMapFromFile(flag)
  Set a flag to define how to get information of hot/dead channels of pixel layers.
    * flag  = 0 : get from database. (default)
    * flag != 0 : get from a file.
  
  - set_ReadStripHotDeadFromFile(flag)
  Set a flag to define how to get information of hot/dead channels of pixel layers.
    * flag = true  : get from a file.
    * flag = false : get from database. (default)
   
  - set_PixelHotDeadChipFileName(filename);
  Set name of file containing information of hot/dead chips of pixel layers.
  Default file name is "pixelchipmap.txt"
  
  - set_PixelHotDeadPixelFileName(filename);
  Set name of file containing information of hot/dead pixels of pixel layers.
  Default file name is "pixelpixelmap.txt"
  
  - set_StripHotDeadHybridsFileName(filename);
  Set name of file containing information of hot/dead hybrids of stripixel layers.
  Default file name is "svxStripHybridsDeadMap.txt"

  - set_StripHotDeadFileName(filename);
  Set name of file containing information of hot/dead channels of stripixel layers.
  Default file name is "svxStripDeadMap.txt"


  ================
  Strip Threshold
  ================

  - set_UseStripThresholdDatbase(flag)
  Set flag to define how to get stripixel threshold information.
    * flag = true  : get from database. (default)
    * flag = false : get from a file.

  - Load_ThresholdFile(filename)
  Set file name containing threshold information.
  No default, so you have to set the file name if you set
  useStripThresholdDatabase = false.

  ///////////////////////////////////////
  ///  SvxDecode
  ///////////////////////////////////////

  - includePixel(bool)
  Set flag to decide whether to include pixel layers in reconstruction.
    * flag = true  : include pixel layers. (default)
    * flag = false : not include pixel layers.

  - includeStripixel(bool)
  Set flag to decide whether to include stripixel layers in reconstruction.
    * flag = true  : include stripixel layers. (default)
    * flag = false : not include pixel layers.
  
  - setAdcOffset(offset)
  Set ADC offset of stripixel.
  Pedestal of stripixel ADC values are corrected in order that the mean of
  the pedestal distribution would be the offset value.
  At the SvxDecode module, all ADC values is subtracted by the offset.
  Default value is 0.
  During RUN11, this offset is set to be 24.
  
  - setAdcCutoff(cut)
  Set cut parameter for stripixel ADC.
  If ADC of a raw hit of stripixel after offset subtraction is less than
  the cut parameter, the raw hit is not stored in SvxRawhitList.
  Default value is 0.
  

  ///////////////////////////////////////
  ///  SvxReco
  ///////////////////////////////////////
  

  ///////////////////////////////////////
  ///  SvxStandAloneReco
  ///////////////////////////////////////

  - setVertexRecoFlag(flag)
  Set flag to define how to get collision vertex information which is used
  input of track search.
    * flag != 2 : x & y vertex positions are values set by 
                  SvxParManager::set_BeamCenter().
		  z vertex position is fixed at 0.
    * flag  = 2 : vertex information is get from VtxOut node. (default)

  - setWindowScale(scale)
  Set scale factor of hit search window at track finding.
  Default is 1.

  - setProjectionFlag(flag)
  If you set false, the collision vertex is not used for the projection at
  hit search at track finding.
  Default is true.

  - setBbcChargeCut(cut)
  If BBC charge (north+south) is less than the "cut", search window parameter
  for low multiplicity events are used.
  Default value of the cut is 200.

  - setPPFlag(flag)
  If you analyze p+p collision events, please set flag=true.
  If you set flag=true, the collision vertex is not used for the projection at
  hit search at track finding (this can be done by setProjectionFlag(false).),
  and search window parameter for low multiplicity events are used.
  Default is false.

  - setSwapFlag(flag)
  If you want to evaluate fake background with hit position swap method,
  you have to set this flag.
    * flag = 1 : all hits on layer2 are converted to fake hits.
    * flag = 2 : all hits on layer3 are converted to fake hits.
    * flag = 3 : all hits on both layer2 and layer3 are converted to fake hits.

*/

#include <string>
#include <sstream>
#include <iostream>

using namespace std;

void Reco_prdf_Run11
(
 int nEvents=100000, 
 char *inputfile="/direct/phenix+scratch/shavera/EVENTDATA_P00-0000349369-0000.PRDFF",
 char* outputfile="/phenix/scratch/shavera/temphold/tPRDF_DST_349369.root"
)
///
/// INPUT
///   nEvents    : Number of events which you want to reconstruct.
///                If you set 0, all events are reconstructed.
///   inputfile  : input PRDF file
///   outputfile : output file name
///
{
  ///////////////////////////////////////////
  // Load Libraries
  //////////////////////////////////////////
  // gSystem->Load("libsvx");
  /// If you want to use a private svx library, put the pass the library here.
  /// Otherwise you don't have to load libsvx.so.
  /// It is loaded at libfun4all.so
  gSystem->Load("libfun4all.so");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libcompactCNT.so");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("SVXACTIVE", 1);

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  Fun4AllServer *se = Fun4AllServer::instance(); 
  se->Verbosity(0);
  
  ///////////////////////////////////////////
  // Make the Synchronization Object
  ///////////////////////////////////////////
  SubsysReco *sync = new SyncReco();
 
  //////////////////////////////////////////
  // Central arms
  //////////////////////////////////////////
  SubsysReco *head    = new HeadReco();
  SubsysReco *trig    = new TrigReco();
  TrigSelect *ppg     = new TrigSelect("PPG");
  SubsysReco *peve    = new PreviousEventReco();
  SubsysReco *bbc     = new BbcReco();
  /// if you want to put event cut, edit here.
  VtxReject *vrej     = new VtxReject("VTXREJ");
  //  vrej->SetVertexRange(-10,10);
  //  vrej->SetBbcChargeRange(10,100);  
  SubsysReco *ert     = new ErtReco();
  SubsysReco *zdc     = new ZdcReco();
  SubsysReco *t0      = new T0Reco();
  SubsysReco *pad     = new PadReco();
  SubsysReco *vtx     = new VtxReco();
  SubsysReco *emc     = new EmcReco3();
  SubsysReco *emcres  = new EmcClusterContainerResurrector();
  SubsysReco *padvtx  = new PadVtxReco();
  SubsysReco *tof     = new TofReco();
  SubsysReco *aero    = new AccReco();
  SubsysReco *dch     = new DchReco();
  SubsysReco *crk     = new CrkReco();
  CglReco *cgl     = new CglReco();
  cgl->set_SvxUseAsciiFile(true);
  SubsysReco *aerocl  = new AccclusterReco();
  SubsysReco *ring    = new RingReco();
  SubsysReco *tofw    = new TofwReco();
  SubsysReco *global  = new GlobalReco(); 
  SubsysReco *global_central  = new GlobalReco_central(); 
  SubsysReco *mpc     = new MpcReco();
  SubsysReco *rp      = new RpSumXYReco();
  
  ////////////////////////////////// 
  // Register SubSystems 
  ////////////////////////////////// 
  se->registerSubsystem(head);
  se->registerSubsystem(sync);
  se->registerSubsystem(trig);
  // Register the PPG veto trigger to get rid of those events early
  se->registerSubsystem(ppg);
  se->registerSubsystem(peve);
  se->registerSubsystem(bbc);
  se->registerSubsystem(vrej);
  se->registerSubsystem(zdc);
  se->registerSubsystem(t0);
  se->registerSubsystem(pad);
  se->registerSubsystem(vtx);
  se->registerSubsystem(emc);
  se->registerSubsystem(emcres);
  se->registerSubsystem(padvtx);
  se->registerSubsystem(ert);
  se->registerSubsystem(tof);
  se->registerSubsystem(aero);
  se->registerSubsystem(dch);
  se->registerSubsystem(crk);
  se->registerSubsystem(tofw);

  SvxParManager *svxpar = new SvxParManager();
  svxpar->Verbosity(0);
  /*
   *  Coordinate offsets, beam center, geometry information, and hot/dead mask
   *  are obtained from database.
   *  If you want to set them by hand, please remove comment out for the
   *  corresponding lines.
   */
  /*** setting for coordinate offset and beam center ***/
  //  svxpar->set_OffsetVtxToCnt(0.,0.,0.);
  //  svxpar->set_OffsetEastToWest(0.,0.,0.);
  //  svxpar->set_BeamCenter(0.,0.);
  /*** setting for geometry file ***/
  //  svxpar->set_ReadGeoParFromFile(1);
  //  svxpar->set_GeometryFileName("svxPISA.par");
  /*** setting for masking of hot/dead channels ***/
  //  svxpar->set_ReadPixelMapFromFile(1);
  //  svxpar->set_ReadStripHotDeadFromFile(1);
  //  svxpar->set_PixelHotDeadChipFileName("pixelhotdeadchipmap.txt");
  //  svxpar->set_PixelHotDeadPixelFileName("pixelhotdeadpixelmap.txt");
  //  svxpar->set_StripHotDeadHybridsFileName("BadStripHybrids_347129.txt");
  //  svxpar->set_StripHotDeadFileName("striphotdeadmap.txt");
  /*** load threshold file ***/
  svxpar->set_UseStripThresholdDatbase(false);
  svxpar->Load_ThresholdFile("threshold.h");
  se->registerSubsystem(svxpar);

  SvxDecode *svxdecode = new SvxDecode();
  svxdecode->Verbosity(0);
  svxdecode->includePixel(true);
  svxdecode->includeStripixel(true);
  svxdecode->setAdcOffset(24);
  svxdecode->setAdcCutoff(-24);
  se->registerSubsystem(svxdecode);

  SvxApplyHotDead *svxhotdead = new SvxApplyHotDead();
  svxhotdead->Verbosity(0);
  se->registerSubsystem(svxhotdead);

  SvxReco *svxrec = new SvxReco();
  svxrec->Verbosity(0);
  //  svxrec->Load_ThresholdFile("threshold.h");
  se->registerSubsystem(svxrec);

  SvxPriVertexSeedFinder *svxvtxseedfinder = new SvxPriVertexSeedFinder();
  svxvtxseedfinder->Verbosity(0);
  se->registerSubsystem(svxvtxseedfinder);

  SvxStandAloneReco *svxstandalone = new SvxStandAloneReco();
  svxstandalone->Verbosity(0);
  svxstandalone->setVertexRecoFlag(2);
  se->registerSubsystem(svxstandalone);

  SvxPrimVertexFinder *svxprimvtxfinder = new SvxPrimVertexFinder();
  svxprimvtxfinder->Verbosity(0);
  se->registerSubsystem(svxprimvtxfinder);

  // cgl must register after svx standalone tracking.
  se->registerSubsystem(cgl);
  se->registerSubsystem(aerocl);
  se->registerSubsystem(ring);
  se->registerSubsystem(global);
  se->registerSubsystem(global_central);
  se->registerSubsystem(mpc);
  se->registerSubsystem(rp);  
  
  //=========================================
  // These fill the compactCNT storage nodes
  //=========================================
  SubsysReco *fillprojections = new FillTrackProjections();
  SubsysReco *filllineprojections = new FillTrackLineProjections();
  SubsysReco *fillpl = new FillTrackPathLengths();
  SubsysReco *filltrkhits = new FillTrackHits();
  SubsysReco *fillpadhits = new FillPadHits();
  SubsysReco *filldchits = new FillDchHits();
  SubsysReco *filltofehits = new FillTofeHits();
  SubsysReco *filltofwhits = new FillTofwHits();
  SubsysReco *fillcrkhits = new FillCrkHits();
  SubsysReco *fillacchits = new FillAccHits();
  SubsysReco *fillemchits = new FillEmcHits();

  // svx
  SubsysReco *fillsvxhits = new FillSvxHits();
  fillsvxhits->Verbosity(0);

  se->registerSubsystem(fillprojections);
  se->registerSubsystem(filllineprojections);
  se->registerSubsystem(fillpl);
  se->registerSubsystem(filltrkhits);
  se->registerSubsystem(filldchits);
  se->registerSubsystem(fillpadhits);
  se->registerSubsystem(filltofehits);
  se->registerSubsystem(filltofwhits);
  se->registerSubsystem(fillcrkhits);
  se->registerSubsystem(fillacchits);

  // This one requires that EmcClusterContainer is already on the node tree
  se->registerSubsystem(fillemchits);

  //==============================================
  // These modules read the compactCNT nodes
  // and create hits objects for each subsystem
  //================================================
  se->registerSubsystem(new RecoverTrackProjections());
  se->registerSubsystem(new RecoverTrackLineProjections());
  se->registerSubsystem(new RecoverTrackPathLengths());
  se->registerSubsystem(new RecoverTrackHits());
  se->registerSubsystem(new RecoverDchHits());
  se->registerSubsystem(new RecoverPadHits());
  se->registerSubsystem(new RecoverTofeHits());
  se->registerSubsystem(new RecoverTofwHits());
  se->registerSubsystem(new RecoverCrkHits());
  se->registerSubsystem(new RecoverAccHits());
  se->registerSubsystem(new RecoverEmcHits());
  //svx
  SubsysReco* recoverSvx = new RecoverSvxHits();
  recoverSvx->Verbosity(0);
  se->registerSubsystem(recoverSvx);
  
  //========================
  // Creates PHCentralTrack
  //========================

  se->registerSubsystem(new CreateCNT());
  
  //=================================================
  // These modules re-associate hits with tracks and
  // fill the PHCentralTrack fields
  //==================================================
  se->registerSubsystem(new FillCNT_TrackProjections());
  se->registerSubsystem(new FillCNT_TrackPathLengths());
  se->registerSubsystem(new FillCNT_TrackHits());
  se->registerSubsystem(new FillCNT_DchHits());
  se->registerSubsystem(new FillCNT_TofeHits());
  se->registerSubsystem(new FillCNT_TofwHits());
  se->registerSubsystem(new FillCNT_PadHits());
  se->registerSubsystem(new FillCNT_CrkHits());
  se->registerSubsystem(new FillCNT_AccHits());
  // This one needs EmcClusterContainer also
  se->registerSubsystem(new FillCNT_EmcHits());

  //SubsysReco* fillCNT_SVX = new FillCNT_SvxHits();
  //fillCNT_SVX->Verbosity(0);
  //se->registerSubsystem(fillCNT_SVX);
  
  /// SvxCentralTrackReco should be called after PHCentralTrack is reconstructed.
  SubsysReco* svxcentraltrack = new SvxCentralTrackReco();
  se->registerSubsystem(svxcentraltrack);
  
  //////////////////////////////////
  // Define the triggers
  //////////////////////////////////
  ppg->AddVetoTrigger("PPG(Laser)");
  ppg->AddVetoTrigger("PPG(Pedestal)");
  ppg->AddVetoTrigger("PPG(Test Pulse)");
  ppg->SetReturnCode("ABORT");

  //////////////////////////////////
  // Add nodes in node tree
  //////////////////////////////////
  Fun4AllDstOutputManager *manager = new Fun4AllDstOutputManager("DST_SVX",outputfile);
  manager->AddNode("EventHeader");
  manager->AddNode("Sync");
  manager->AddNode("TrigLvl1");
  manager->AddNode("PreviousEvent");
  manager->AddNode("BbcOut");
  manager->AddNode("VtxOut");
  manager->AddNode("SvxEventInfo");
  manager->AddNode("SvxRawhitList");
  manager->AddNode("SvxRawhitClusterList");
  manager->AddNode("SvxClusterList");
  manager->AddNode("SvxSegmentList");
  manager->AddNode("SvxCentralTrackList");
  manager->AddNode("BbcRaw");
  manager->AddNode("mpcTowerContainer");
  manager->AddNode("PHGlobal");
  /*
  manager->AddNode("PHGlobal_CENTRAL");
  manager->AddNode("RpSumXYObject");
  manager->AddNode("DchHit_VarArray");
  manager->AddNode("EmcHit_VarArray");
  manager->AddNode("Pc1Hit_VarArray");
  manager->AddNode("Pc2Hit_VarArray");
  manager->AddNode("Pc3Hit_VarArray");

  manager->AddNode("TofeHit_VarArray");
  //  manager->AddNode("TofwHit_VarArray");
  manager->AddNode("CrkHit_VarArray");
  manager->AddNode("AccHit_VarArray");
  manager->AddNode("CglTrackHits_VarArray");
  manager->AddNode("CglTrackBackHits_VarArray");
  manager->AddNode("TrackProjection_VarArray");
  manager->AddNode("TrackLineProjection_VarArray");
  manager->AddNode("TrackPathLength_VarArray");
  manager->AddNode("emcHitContainer");
  */
  // DST
  manager->AddNode("PHCentralTrack");
  
  manager->AddNode("PHTrackOut");
  manager->AddNode("CglTrack");
  manager->AddNode("DchTrack");
  //manager->AddNode("dDchHit");
  //manager->AddNode("dDchTracks");
  manager->AddNode("Pc1Cluster");
  manager->AddNode("Pc2Cluster");
  manager->AddNode("Pc3Cluster");
  manager->AddNode("emcClusterContainer");
  manager->AddNode("CrkRing");
  se->registerOutputManager(manager);

  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  se->registerInputManager(in);
  in->fileopen(inputfile);
  se->run(nEvents);

  se->End();
  
  gSystem->Exec("ps -o sid,ppid,pid,user,comm,vsize,rssize,time");

  cout << "Successfully Completed Analysis." << endl;

  delete se;
  //  gSystem->Exit(0);
}
