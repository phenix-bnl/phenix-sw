{
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libphgeo.so"); 
  gSystem->Load("libRawDataCheck.so");
 
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");
  gSystem->Load("libheader.so");

  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc.so");
  gSystem->Load("libmvd.so");
  gSystem->Load("libEG.so");
  gSystem->Load("libemcCalib.so");
  gSystem->Load("libemc.so");
  gSystem->Load("libtof.so");
  gSystem->Load("libtec.so");
  gSystem->Load("libpad.so");

  gSystem->Load("libdgo.so");
  gSystem->Load("libdch.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libcrk.so");
  gSystem->Load("libmom.so");
  gSystem->Load("libcge.so");
  gSystem->Load("liblv1.so");
  gSystem->Load("libvtx.so");
  gSystem->Load("/direct/phenix+data07/jjia/mixing2/install/lib/libembed.so");

//gSystem->Load("/ccj/w/r02/janebh/install/lib/libphdisplay.so");
//gSystem->Load("/ccj/w/r02/janebh/install/lib/libphgui.so");
//gSystem->Load("/ccj/w/r02/janebh/install/lib/libdchdisplay.so");
//gSystem->Load("/ccj/w/r02/janebh/install/lib/libpaddisplay.so");
//gSystem->Load("/ccj/w/r02/janebh/install/lib/libtecdisplay.so");  

  PHCompositeNode* topNode     = new PHCompositeNode("MERGED");
  PHCompositeNode* node1       = new PHCompositeNode("SINGLE");
  PHCompositeNode* node2       = new PHCompositeNode("REAL");
  PHNodeIOManager* singleIn    = 0;
  PHNodeIOManager* dstIn       = 0;
  PHNodeIOManager* mergedOut   = 0;


  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* doNode = new PHCompositeNode("DO");
  topNode->addNode(doNode);
 PHCompositeNode* geaNode = new PHCompositeNode("GEA");
  topNode->addNode(geaNode);
  PHCompositeNode* prdfNode = new PHCompositeNode("PRDF");
  topNode->addNode(prdfNode);  
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
  PHCompositeNode* zdcNode = new PHCompositeNode("ZDC");
  topNode->addNode(zdcNode);
  PHCompositeNode* mvdNode = new PHCompositeNode("MVD");
  topNode->addNode(mvdNode);
  PHCompositeNode* padNode = new PHCompositeNode("PAD");
  topNode->addNode(padNode);
  PHCompositeNode* crkNode = new PHCompositeNode("CRK");
  topNode->addNode(crkNode);
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);
  PHCompositeNode* emcNode = new PHCompositeNode("EMC");
  topNode->addNode(emcNode);
  PHCompositeNode* tofNode = new PHCompositeNode("TOF");
  topNode->addNode(tofNode);
  PHCompositeNode* cglNode = new PHCompositeNode("CGL");
  topNode->addNode(cglNode);
  PHCompositeNode* dchNode = new PHCompositeNode("DCH");
  topNode->addNode(dchNode);
  PHCompositeNode* momNode = new PHCompositeNode("MOM");
  topNode->addNode(momNode);
  PHCompositeNode* cgeNode = new PHCompositeNode("CGE");
  topNode->addNode(cgeNode);
  PHCompositeNode* mixNode = new PHCompositeNode("MIX");//for event display
  topNode->addNode(mixNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  VtxOut* vtxout = new VtxOutv1();
  setVtxOutNode(vtxout, dstNode); 
  
  PHEmbedInitializer::instance()->init(node1,node2,topNode);
  PHEmbedMixer *               mixer = new PHEmbedMixer;
// PHEmbedRealEvaluator*        Realevaluator = new PHEmbedRealEvaluator;
  PHEmbedMCEvaluator*          MCevaluator   = new PHEmbedMCEvaluator;
  PHEmbedHistogrammer*         histogrammer = PHEmbedHistogrammer::instance();
//PHEmbededTrackBuilder*       eTrackBuilder = new PHEmbededTrackBuilder;
  mixer->setVerbose(12);
//Realevaluator->setVerbose(12);
  MCevaluator->setVerbose(12);
  MCevaluator->setEvaluationMode(Mode);
// PHString parInFile = "/phenix/data07/jjia/mixing2/6.12/reco/rawpar.root";
//PHNodeIOManager *parIn = new PHNodeIOManager(parInFile,PHReadOnly);
//parIn->read(parNode);
  int runNumber =9416;
  int verbose =12;

  /****************************************Module Declearation**************************************/
  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);

  // Set up the modules
  BbcEvent* bbcevent = new BbcEvent();
  BbcGeo* bbcgeo = new BbcGeo();
  mBbcSetGeoModule* mBbcSetGeo = new mBbcSetGeoModule;
  mBbcUnpackModule* mBbcUnpack = new mBbcUnpackModule;
  mBbcSetUcalModule* mBbcSetUcal = new mBbcSetUcalModule;
  mBbcGhitRawModule* mBbcGhitRaw = new mBbcGhitRawModule;
  mBbcRawOutModule* mBbcRawOut = new mBbcRawOutModule;
  mMvdResp* mMvdResp = new mMvdResp;
cout<<"HIHIH"<<endl;
  mNewDchInitializer*          mDchInitializer = new mNewDchInitializer(1,1,0);
  mDchInitializer->setGeometryFileNames("/phenix/data07/jjia/mixing2/6.12/reco/DchGeometry.info","/phenix/data07/jjia/mixing2/6.12/reco/DchGeometry.wire00.moved_phiandalpha.3","/phenix/data07/jjia/mixing2/6.12/reco/DchGeometry.frameMc");
  mDchInitializer->setNoiseFileName("/phenix/data07/jjia/mixing2/6.12/reco/DchNoise.Real0000010629","DchEfficiency.Real");
mDchInitializer->setCalibrationFileName("/phenix/data07/jjia/mixing2/6.12/reco/DchCalibration.Real0000010629");
 mNewDchCandidatory*          mDchCandidatory = new mNewDchCandidatory;
  mNewDchCalibrator* mDchCalibrator = new mNewDchCalibrator;
  mNewDchFastSimulator* mNewDchFastSim = new mNewDchFastSimulator;
  mNewDchPerfectTracker* mNewDchPerfect = new mNewDchPerfectTracker;
  mNewDchEvaluator* mNewDchEvaluate = new mNewDchEvaluator;

 /*mDchCandidatory->clearAnalysisOptions();
  mDchCandidatory->useX1X2HoughTransform();
  mDchCandidatory->useX1HoughTransform();
  mDchCandidatory->useX2HoughTransform();
  mDchCandidatory->useFitToXProjection();
 */
int numofruns = 268;
char *dstFileName[] ={
"/phenix/data11/dst_data/DST_v03_Stream01-0000009724-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009729-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009739-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009741-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009746-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009750-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009752-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009757-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009759-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009770-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009839-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009847-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009851-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009858-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009862-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009889-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009890-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009923-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009939-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009942-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009944-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009948-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009972-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009978-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009979-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009979-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009981-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009987-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000009988-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000009992-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010000-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010003-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010008-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010014-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010017-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010025-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010028-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010030-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010039-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010042-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010047-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010102-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010108-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010111-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010113-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010115-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010126-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010146-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010152-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010156-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010162-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010164-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010167-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010169-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010171-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010205-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010213-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010215-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010218-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010221-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010226-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010232-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010238-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010243-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010245-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010248-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010251-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010254-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010761-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010763-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010765-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010767-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010902-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010902-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010902-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010902-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010902-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010907-0000.proot",
//"/phenix/data11/dst_data/DST_v03_Stream01-0000010909-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010927-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010927-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010927-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000010927-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011020-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011020-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011020-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011022-0008.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0008.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0010.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0011.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011024-0012.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011589-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011589-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011591-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011591-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011591-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011591-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011591-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011591-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011591-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011591-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0008.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0009.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0010.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0011.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0012.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0013.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0014.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0015.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0016.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0017.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0018.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0019.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011597-0020.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0008.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0009.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0010.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0011.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0012.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0013.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0014.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0015.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0016.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0017.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0018.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0019.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0020.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0021.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0022.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0023.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0024.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0025.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0026.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011610-0027.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0008.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0009.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0010.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0012.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0013.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0014.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0015.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0016.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0017.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0018.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0019.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0020.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0021.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0022.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0023.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0024.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0025.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0026.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0027.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0028.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0029.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0030.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011613-0031.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0008.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0009.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011615-0010.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0009.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0010.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0011.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0012.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0013.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011618-0014.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0000.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0001.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0002.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0003.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0004.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0005.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0006.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0007.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0008.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0009.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0010.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0011.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0012.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0013.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0014.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0015.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0016.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0017.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0018.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0019.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0020.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0021.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0022.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0023.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0024.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0025.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0026.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0027.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0028.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0029.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0030.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0031.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0032.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0033.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0034.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0035.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0036.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0037.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0038.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0039.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0040.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0041.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0042.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0043.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0044.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0045.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0046.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0047.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0048.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0049.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0050.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0051.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0052.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0053.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0054.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0055.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0056.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0057.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0058.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0059.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0060.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0061.proot",
"/phenix/data11/dst_data/DST_v03_Stream01-0000011679-0062.proot"
};
  
  //TString dstFileName = "/phenix/data04/phnxreco/v02b/dsts/DST_v02_Stream01-0000009666-0000.proot";
  int geomFlag  = 1;//0 is retracted, 1 is standard
  Int_t bFieldFlag = 1;  // 0 is off, 1 is on
  PHBoolean padStatCh;
  PHBoolean padStatROC;

  PHBoolean padInclStat;
  padInclBad* padInclBad = new padInclBad();
  PadCalibrationObject* PadCalibration = new PadCalibrationObject();
  PHpadDetectorGeo* mPadDetGeo = new PHpadDetectorGeo();
  PadRecModule* Pc1Rec = new PadRecModule;
  PadRecModule* Pc2Rec = new PadRecModule;
  PadRecModule* Pc3Rec = new PadRecModule;
  mPadEvaluateModule* mPc1Evaluate = new mPadEvaluateModule; //
  mPadEvaluateModule* mPc2Evaluate = new mPadEvaluateModule; //
  mPadEvaluateModule* mPc3Evaluate = new mPadEvaluateModule; //
  
  TecAddressObject* TecAddress = new TecAddressObject();
  TecAddress->setTimeStamp(TimeStamp);
  TecGeometryObject* TecGeometry = new TecGeometryObject();
  TecGeometry->setTimeStamp(TimeStamp);
  TecCalibrationObject* TecCalibration = new TecCalibrationObject();
  TecCalibration->setTimeStamp(TimeStamp);
  mTecAlignModule* mTecAlign = new mTecAlignModule;
  mTecSlowSimModule* mTecSlowSim = new mTecSlowSimModule;
  mTecRemapModule* mTecRemap = new mTecRemapModule;
  mTecCalibModule* mTecCalib = new mTecCalibModule;
  mTecHoughTrackModule* mTecHoughTrack = new mTecHoughTrackModule;
  mTofSetGeoModule* mTofSetGeo = new mTofSetGeoModule;
  mTofSetUcalModule* mTofSetUcal = new mTofSetUcalModule;
  mTofSetCalModule* mTofSetCal = new mTofSetCalModule;
  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofCalibObject* TofCalib = new TofCalibObject();

  // Set up the modules
  mEmcGeaParamsModule* mEmcGeaParams = new mEmcGeaParamsModule;
  mEmcGeaMakeRawModule* mEmcGeaMakeRaw = new mEmcGeaMakeRawModule;
  mEmcDefGeomModule* mEmcDefGeom = new mEmcDefGeomModule;
  mEmcRawToFEMModule* mEmcRawToFEM = new mEmcRawToFEMModule;
  mEmcFEMToDCMModule* mEmcFEMToDCM = new mEmcFEMToDCMModule;
  mEmcDCMToRawModule* mEmcDCMToRaw = new mEmcDCMToRawModule;
  mEmcEventModule* mEmcEvent = new mEmcEventModule;
  mEmcCalibTowerModule* mEmcCalibTower = new mEmcCalibTowerModule;
//mEmcClusterLocalModule* mEmcClusterLocal = new mEmcClusterLocalModule;
  mEmcGeaTrackModule* mEmcGeaTrack = new mEmcGeaTrackModule;
//mEmcClusterLocalChi2Module* mEmcClusterLocalChi2 = new mEmcClusterLocalChi2Module;
  mEmcGeaClusterEvalModule* mEmcGeaClusterEval = new mEmcGeaClusterEvalModule;
//mEmcNtupleModule* mEmcNtuple = new mEmcNtupleModule;
//mEmcNtupleSaveModule* mEmcNtupleSave = new mEmcNtupleSaveModule;
  mEmcEventModule* mEmcEvent = new mEmcEventModule;
  mEmcGeaEventModule* mEmcGeaEvent = new mEmcGeaEventModule;

  mEmcGeometryModule* mEmcGeometry = new mEmcGeometryModule();
  mEmcClusterNewModule* mEmcClusterNew = new mEmcClusterNewModule(mEmcGeometry);

  PHcglDetectorGeo* cglDetectorGeo = new PHcglDetectorGeo();
  mPHLineTrack* cglLineTrack = new mPHLineTrack();
  cglOffsetTrack* cglOffsetTrack = new cglOffsetTrack();
  mPHDchTrackModel* dchTrackModel = new mPHDchTrackModel;
  cglHitAssociate* cglHitAssociate = new cglHitAssociate();

  // look for DAO for crk --> constructor takes crk_cabling_vrdc.txt
  CrkDAO* mCrkDAO = new CrkDAO("crk_cabling_vrdc.txt");
  mCrkSetGeoModule* mCrkSetGeo = new mCrkSetGeoModule;
  mCrkSetUcalModule* mCrkSetUcal = new mCrkSetUcalModule;
  mCrkSetCalModule* mCrkSetCal = new mCrkSetCalModule;
  mCrkGhitRawModule* mCrkGhitRaw = new mCrkGhitRawModule;
  mCrkRawHitModule* mCrkRawHit = new mCrkRawHitModule;
  mCrkProjPidModule* mCrkProjPid = new mCrkProjPidModule;

  mTecPIDModule* mTecPID = new mTecPIDModule;
  mCglProjectionModule* mCglProjection = new mCglProjectionModule;
  mCglPidModule* mCglPid = new mCglPidModule;
  mTofGhitGdigiModule* mTofGhitGdigi = new mTofGhitGdigiModule;
  mTofGhitRawModule* mTofGhitRaw = new mTofGhitRawModule;
  mTofRawRecModule* mTofRawRec = new mTofRawRecModule;
  // Add by A.K. Dec 21, 2000
  mTofEvaluateModule* mTofEvaluate = new mTofEvaluateModule;
  cgeEvalTrack* cgeEvalTrack = new cgeEvalTrack();
  
  /********************************************INIT Tables****************************************/
  cout << "1----------------------------"<< endl;
  size_t mr=1;
  dBbcGeoWrapper* dBbcGeo = new dBbcGeoWrapper("dBbcGeo",mr);
  PHIODataNode<PHTable>* dBbcGeoNode = new PHIODataNode<PHTable>(dBbcGeo,"dBbcGeo");
  parNode->addNode(dBbcGeoNode);

  size_t mr=1;
  dBbcGhitRawParWrapper* dBbcGhitRawPar = new dBbcGhitRawParWrapper("dBbcGhitRawPar",mr);
  PHIODataNode<PHTable>* dBbcGhitRawParNode = new PHIODataNode<PHTable>(dBbcGhitRawPar,"dBbcGhitRawPar");
  parNode->addNode(dBbcGhitRawParNode);

  size_t mr=1;
  dBbcRawHitParWrapper* dBbcRawHitPar = new dBbcRawHitParWrapper("dBbcRawHitPar",mr);
  PHIODataNode<PHTable>* dBbcRawHitParNode = new PHIODataNode<PHTable>(dBbcRawHitPar,"dBbcRawHitPar");
  parNode->addNode(dBbcRawHitParNode);

  size_t mr=3000;
  bbcghitWrapper* bbcghit = new bbcghitWrapper("bbcghit",mr);
  PHIODataNode<PHTable>* bbcghitNode = new PHIODataNode<PHTable>(bbcghit,"bbcghit");
  geaNode->addNode(bbcghitNode);

  size_t mr=128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw",mr);
  PHIODataNode<PHTable>* dBbcRawNode = new PHIODataNode<PHTable>(dBbcRaw,"dBbcRaw");
  dstNode->addNode(dBbcRawNode);

  BbcCalib* BbcCalibPar = new BbcCalib();
  PHIODataNode<TObject>* BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  size_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);

  size_t mr=128;
  dBbcCalWrapper* dBbcCal = new dBbcCalWrapper("dBbcCal",mr);
  PHIODataNode<PHTable>* dBbcCalNode = new PHIODataNode<PHTable>(dBbcCal,"dBbcCal");
  parNode->addNode(dBbcCalNode);
  
  size_t mr=128;
  dBbcUcalWrapper* dBbcUcal = new dBbcUcalWrapper("dBbcUcal",mr);
  PHIODataNode<PHTable>* dBbcUcalNode = new PHIODataNode<PHTable>(dBbcUcal,"dBbcUcal");
  bbcNode->addNode(dBbcUcalNode);

  size_t mr=10000;
  dBbcGhitRawWrapper* dBbcGhitRaw = new dBbcGhitRawWrapper("dBbcGhitRaw",mr);
  PHIODataNode<PHTable>* dBbcGhitRawNode = new PHIODataNode<PHTable>(dBbcGhitRaw,"dBbcGhitRaw");
  evaNode->addNode(dBbcGhitRawNode);
  cout << "2----------------------------"<< endl;

  size_t mr=1;
  dMvdGeoWrapper* dMvdGeo = new dMvdGeoWrapper("dMvdGeo",mr);
  PHIODataNode<PHTable>* dMvdGeoNode = new PHIODataNode<PHTable>(dMvdGeo,"dMvdGeo");
  parNode->addNode(dMvdGeoNode);

  size_t mr=30000;
  dMvbRawWrapper* dMvbRaw = new dMvbRawWrapper("dMvbRaw",mr);
  PHIODataNode<PHTable>* dMvbRawNode = new PHIODataNode<PHTable>(dMvbRaw,"dMvbRaw");
  dstNode->addNode(dMvbRawNode);

  size_t mr=9000;
  dMvcRawWrapper* dMvcRaw = new dMvcRawWrapper("dMvcRaw",mr);
  PHIODataNode<PHTable>* dMvcRawNode = new PHIODataNode<PHTable>(dMvcRaw,"dMvcRaw");
  dstNode->addNode(dMvcRawNode);

  size_t mr=10500;
  dMvdClmpWrapper* dMvdClmp = new dMvdClmpWrapper("dMvdClmp",mr);
  PHIODataNode<PHTable>* dMvdClmpNode = new PHIODataNode<PHTable>(dMvdClmp,"dMvdClmp");
  mvdNode->addNode(dMvdClmpNode);

  size_t mr=200;
  dMvdFEMWrapper* dMvdFEM = new dMvdFEMWrapper("dMvdFEM",mr);
  PHIODataNode<PHTable>* dMvdFEMNode = new PHIODataNode<PHTable>(dMvdFEM,"dMvdFEM");
  mvdNode->addNode(dMvdFEMNode);

  size_t mr=200;
  dMvdDCMWrapper* dMvdDCM = new dMvdDCMWrapper("dMvdDCM",mr);
  PHIODataNode<PHTable>* dMvdDCMNode = new PHIODataNode<PHTable>(dMvdDCM,"dMvdDCM");
  dcmNode->addNode(dMvdDCMNode);

  size_t mr=900;
  dMvddNdEtaOutWrapper* dMvddNdEtaOut = new dMvddNdEtaOutWrapper("dMvddNdEtaOut",mr);
  PHIODataNode<PHTable>* dMvddNdEtaOutNode = new PHIODataNode<PHTable>(dMvddNdEtaOut,"dMvddNdEtaOut");
  dstNode->addNode(dMvddNdEtaOutNode);

  size_t mr=1;
  dMvdMultOutWrapper* dMvdMultOut = new dMvdMultOutWrapper("dMvdMultOut",mr);
  PHIODataNode<PHTable>* dMvdMultOutNode = new PHIODataNode<PHTable>(dMvdMultOut,"dMvdMultOut");
  dstNode->addNode(dMvdMultOutNode);

  size_t mr=3;
  dMvdVertexOutWrapper* dMvdVertexOut = new dMvdVertexOutWrapper("dMvdVertexOut",mr);
  PHIODataNode<PHTable>* dMvdVertexOutNode = new PHIODataNode<PHTable>(dMvdVertexOut,"dMvdVertexOut");
  dstNode->addNode(dMvdVertexOutNode);
  cout << "3----------------------------"<< endl;

  size_t mr=15000;
  verghitWrapper* verghit = new verghitWrapper("verghit",mr);
  PHIODataNode<PHTable>* verghitNode = new PHIODataNode<PHTable>(verghit,"verghit");
  geaNode->addNode(verghitNode);

  size_t mr=1;
  dDchGeomWrapper* dDchGeom = new dDchGeomWrapper("dDchGeom",mr);
  PHIODataNode<PHTable>* dDchGeomNode = new PHIODataNode<PHTable>(dDchGeom,"dDchGeom");
  parNode->addNode(dDchGeomNode);

  size_t mr=1;
  dDchFastSimParWrapper* dDchFastSimPar = new dDchFastSimParWrapper("dDchFastSimPar",mr);
  PHIODataNode<PHTable>* dDchFastSimParNode = new PHIODataNode<PHTable>(dDchFastSimPar,"dDchFastSimPar");
  parNode->addNode(dDchFastSimParNode);

  size_t mr = 1;
  dDchPerfParWrapper* dDchPerfPar = new dDchPerfParWrapper("dDchPerfPar",mr);
  dDchPerfPar->set_verbose(0,0);
  dDchPerfPar->set_localStudy(0,0);
  PHIODataNode<PHTable>* dDchPerfParNode = new PHIODataNode<PHTable>(dDchPerfPar,"dDchPerfPar");
  dchNode->addNode(dDchPerfParNode);

  size_t mr=1;
  dDchEvalParWrapper* dDchEvalPar = new dDchEvalParWrapper("dDchEvalPar",mr);
  dDchEvalPar->set_main(0,1);
  PHIODataNode<PHTable>* dDchEvalParNode = new PHIODataNode<PHTable>(dDchEvalPar,"dDchEvalPar");
  parNode->addNode(dDchEvalParNode);

  size_t mr=60000;
  dDchRawWrapper* dDchRaw = new dDchRawWrapper("dDchRaw",mr);
  PHIODataNode<PHTable>* dDchRawNode = new PHIODataNode<PHTable>(dDchRaw,"dDchRaw");
  dchNode->addNode(dDchRawNode);

  size_t mr=1;
  dDchReconstructionParWrapper* dDchRecoPar = 
  new dDchReconstructionParWrapper("dDchRecoPar",mr);
  PHIODataNode<PHTable>* dDchRecoParNode = 
  new PHIODataNode<PHTable>(dDchRecoPar,"dDchRecoPar");
  parNode->addNode(dDchRecoParNode);

  size_t mr=60000;
  dDchHitWrapper* dDchHit = new dDchHitWrapper("dDchHit",mr);
  PHIODataNode<PHTable>* dDchHitNode = new PHIODataNode<PHTable>(dDchHit,"dDchHit");
  dstNode->addNode(dDchHitNode);

  size_t mr=1500;
  dDchTracksWrapper* dDchTracks = new dDchTracksWrapper("dDchTracks",mr);
  PHIODataNode<PHTable>* dDchTracksNode = new PHIODataNode<PHTable>(dDchTracks,"dDchTracks");
  dstNode->addNode(dDchTracksNode);

  size_t mr=1500;
  dDchTracksWrapper* dDchTracksPerf = new dDchTracksWrapper("dDchTracksPerf",mr);
  PHIODataNode<PHTable>* dDchTracksPerfNode = new PHIODataNode<PHTable>(dDchTracksPerf,"dDchTracksPerf");
  dstNode->addNode(dDchTracksPerfNode);


  size_t mr=60000;
  dDchGhitRawWrapper* dDchGhitRaw = new dDchGhitRawWrapper("dDchGhitRaw",mr);
  PHIODataNode<PHTable>* dDchGhitRawNode = new PHIODataNode<PHTable>(dDchGhitRaw,"dDchGhitRaw");
  dchNode->addNode(dDchGhitRawNode);
  dstNode->addNode(dDchGhitRawNode);

  size_t mr=60000;
  dDchGhitHitsWrapper* dDchGhitHits = new dDchGhitHitsWrapper("dDchGhitHits",mr);
  PHIODataNode<PHTable>* dDchGhitHitsNode = new PHIODataNode<PHTable>(dDchGhitHits,"dDchGhitHits");
  dchNode->addNode(dDchGhitHitsNode);
  dstNode->addNode(dDchGhitHitsNode);

  size_t mr=45000;
  dcghitWrapper* dcghit = new dcghitWrapper("dcghit",mr);
  PHIODataNode<PHTable>* dcghitNode = new PHIODataNode<PHTable>(dcghit,"dcghit");
  geaNode->addNode(dcghitNode);
  dstNode->addNode(dcghitNode); 
  cout << "3----------------------------"<< endl;

  size_t mr = 10;
  dPadEvalParWrapper* dPadEvalPar = new dPadEvalParWrapper("dPadEvalPar",mr);
  PHIODataNode<PHTable>* dPadEvalParNode = new PHIODataNode<PHTable>(dPadEvalPar,"dPadEvalPar");
  parNode->addNode(dPadEvalParNode);

  size_t mr =1000;
  dPadEvalWrapper* dPc1Eval = new dPadEvalWrapper("dPc1Eval",mr);
  PHIODataNode<PHTable>* dPc1EvalNode = new PHIODataNode<PHTable>(dPc1Eval,"dPc1Eval");
  padNode->addNode(dPc1EvalNode);

  size_t mr =1000;
  dPadEvalWrapper* dPc2Eval = new dPadEvalWrapper("dPc2Eval",mr);
  PHIODataNode<PHTable>* dPc2EvalNode = new PHIODataNode<PHTable>(dPc2Eval,"dPc2Eval");
  padNode->addNode(dPc2EvalNode);

  size_t mr =1000;
  dPadEvalWrapper* dPc3Eval = new dPadEvalWrapper("dPc3Eval",mr);
  PHIODataNode<PHTable>* dPc3EvalNode = new PHIODataNode<PHTable>(dPc3Eval,"dPc3Eval");
  padNode->addNode(dPc3EvalNode);

  size_t mr=1;
  dPad23ParWrapper* dPad23Par = new dPad23ParWrapper("dPad23Par",mr);
  PHIODataNode<PHTable>* dPad23ParNode = new PHIODataNode<PHTable>(dPad23Par,"dPad23Par");
  parNode->addNode(dPad23ParNode);

  size_t mr=1;
  dPadGeomWrapper* dPadGeom = new dPadGeomWrapper("dPadGeom",mr);
  PHIODataNode<PHTable>* dPadGeomNode = new PHIODataNode<PHTable>(dPadGeom,"dPadGeom");
  parNode->addNode(dPadGeomNode);

  size_t mr=1;
  dPadSlowSimParWrapper* dPadSlowSimPar = new dPadSlowSimParWrapper("dPadSlowSimPar",mr);
  PHIODataNode<PHTable>* dPadSlowSimParNode = new PHIODataNode<PHTable>(dPadSlowSimPar,"dPadSlowSimPar");
  parNode->addNode(dPadSlowSimParNode);

  size_t mr=1;
  dPadRecParWrapper* dPadRecPar = new dPadRecParWrapper("dPadRecPar",mr);
  PHIODataNode<PHTable>* dPadRecParNode = new PHIODataNode<PHTable>(dPadRecPar,"dPadRecPar");
  parNode->addNode(dPadRecParNode);

  size_t mr=12000;
  dPadRawWrapper* dPc1Raw = new dPadRawWrapper("dPc1Raw",mr);
  PHIODataNode<PHTable>* dPc1RawNode = new PHIODataNode<PHTable>(dPc1Raw,"dPc1Raw");
  dstNode->addNode(dPc1RawNode);

  size_t mr=12000;
  dPadRawWrapper* dPc2Raw = new dPadRawWrapper("dPc2Raw",mr);
  PHIODataNode<PHTable>* dPc2RawNode = new PHIODataNode<PHTable>(dPc2Raw,"dPc2Raw");
  dstNode->addNode(dPc2RawNode);

  size_t mr=12000;
  dPadRawWrapper* dPc3Raw = new dPadRawWrapper("dPc3Raw",mr);
  PHIODataNode<PHTable>* dPc3RawNode = new PHIODataNode<PHTable>(dPc3Raw,"dPc3Raw");
  dstNode->addNode(dPc3RawNode);

  size_t mr=1500;
  dPadClusterWrapper* dPc1Cluster = new dPadClusterWrapper("dPc1Cluster",mr);
  PHIODataNode<PHTable>* dPc1ClusterNode = new PHIODataNode<PHTable>(dPc1Cluster,"dPc1Cluster");
  dstNode->addNode(dPc1ClusterNode);

  size_t mr=1500;
  dPadClusterWrapper* dPc2Cluster = new dPadClusterWrapper("dPc2Cluster",mr);
  PHIODataNode<PHTable>* dPc2ClusterNode = new PHIODataNode<PHTable>(dPc2Cluster,"dPc2Cluster");
  dstNode->addNode(dPc2ClusterNode);

  size_t mr=1500;
  dPadClusterWrapper* dPc3Cluster = new dPadClusterWrapper("dPc3Cluster",mr);
  PHIODataNode<PHTable>* dPc3ClusterNode = new PHIODataNode<PHTable>(dPc3Cluster,"dPc3Cluster");
  dstNode->addNode(dPc3ClusterNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc1GhitRaw = new dPadGhitRawWrapper("dPc1GhitRaw",mr);
  PHIODataNode<PHTable>* dPc1GhitRawNode = new PHIODataNode<PHTable>(dPc1GhitRaw,"dPc1GhitRaw");
  evaNode->addNode(dPc1GhitRawNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc2GhitRaw = new dPadGhitRawWrapper("dPc2GhitRaw",mr);
  PHIODataNode<PHTable>* dPc2GhitRawNode = new PHIODataNode<PHTable>(dPc2GhitRaw,"dPc2GhitRaw");
  evaNode->addNode(dPc2GhitRawNode);

  size_t mr=6000;
  dPadGhitRawWrapper* dPc3GhitRaw = new dPadGhitRawWrapper("dPc3GhitRaw",mr);
  PHIODataNode<PHTable>* dPc3GhitRawNode = new PHIODataNode<PHTable>(dPc3GhitRaw,"dPc3GhitRaw");
  evaNode->addNode(dPc3GhitRawNode);

  size_t mr=3000;
  dPadRawClusWrapper* dPc1RawClus = new dPadRawClusWrapper("dPc1RawClus",mr);
  PHIODataNode<PHTable>* dPc1RawClusNode = new PHIODataNode<PHTable>(dPc1RawClus,"dPc1RawClus");
  padNode->addNode(dPc1RawClusNode);
  dstNode->addNode(dPc1RawClusNode);

  size_t mr=3000;
  dPadRawClusWrapper* dPc2RawClus = new dPadRawClusWrapper("dPc2RawClus",mr);
  PHIODataNode<PHTable>* dPc2RawClusNode = new PHIODataNode<PHTable>(dPc2RawClus,"dPc2RawClus");
  padNode->addNode(dPc2RawClusNode);
  dstNode->addNode(dPc2RawClusNode);

  size_t mr=3000;
  dPadRawClusWrapper* dPc3RawClus = new dPadRawClusWrapper("dPc3RawClus",mr);
  PHIODataNode<PHTable>* dPc3RawClusNode = new PHIODataNode<PHTable>(dPc3RawClus,"dPc3RawClus");
  padNode->addNode(dPc3RawClusNode);
  dstNode->addNode(dPc3RawClusNode);

  size_t mr=6000;
  dPadGhitClusWrapper* dPc1GhitClus = new dPadGhitClusWrapper("dPc1GhitClus",mr);
  PHIODataNode<PHTable>* dPc1GhitClusNode = new PHIODataNode<PHTable>(dPc1GhitClus,"dPc1GhitClus");
  padNode->addNode(dPc1GhitClusNode);
  dstNode->addNode(dPc1GhitClusNode);

  size_t mr=6000;
  dPadGhitClusWrapper* dPc2GhitClus = new dPadGhitClusWrapper("dPc2GhitClus",mr);
  PHIODataNode<PHTable>* dPc2GhitClusNode = new PHIODataNode<PHTable>(dPc2GhitClus,"dPc2GhitClus");
  padNode->addNode(dPc2GhitClusNode);
  dstNode->addNode(dPc2GhitClusNode);

  size_t mr=6000;
  dPadGhitClusWrapper* dPc3GhitClus = new dPadGhitClusWrapper("dPc3GhitClus",mr);
  PHIODataNode<PHTable>* dPc3GhitClusNode = new PHIODataNode<PHTable>(dPc3GhitClus,"dPc3GhitClus");
  padNode->addNode(dPc3GhitClusNode);
  dstNode->addNode(dPc3GhitClusNode);

  size_t mr=1500;
  pcghitWrapper* pc1ghit = new pcghitWrapper("pc1ghit",mr);
  PHIODataNode<PHTable>* pc1ghitNode = new PHIODataNode<PHTable>(pc1ghit,"pc1ghit");
  geaNode->addNode(pc1ghitNode);
  dstNode->addNode(pc1ghitNode);

  size_t mr=1500;
  pcghitWrapper* pc2ghit = new pcghitWrapper("pc2ghit",mr);
  PHIODataNode<PHTable>* pc2ghitNode = new PHIODataNode<PHTable>(pc2ghit,"pc2ghit");
  geaNode->addNode(pc2ghitNode);
  dstNode->addNode(pc2ghitNode);

  size_t mr=1500;
  pcghitWrapper* pc3ghit = new pcghitWrapper("pc3ghit",mr);
  PHIODataNode<PHTable>* pc3ghitNode = new PHIODataNode<PHTable>(pc3ghit,"pc3ghit");
  geaNode->addNode(pc3ghitNode);
  dstNode->addNode(pc3ghitNode);

  PHIODataNode<TObject>* mPadDetGeoNode = new PHIODataNode<TObject>(mPadDetGeo,"mPadDetGeo");
  parNode->addNode(mPadDetGeoNode);
  cout << "4----------------------------"<< endl;

  size_t mr=1;
  dCrkGeoWrapper* dCrkGeo = new dCrkGeoWrapper("dCrkGeo",mr);
  PHIODataNode<PHTable>* dCrkGeoNode = new PHIODataNode<PHTable>(dCrkGeo,"dCrkGeo");
  parNode->addNode(dCrkGeoNode);
 
  size_t mr=1;
  dCrkGhitRawParWrapper* dCrkGhitRawPar = new dCrkGhitRawParWrapper("dCrkGhitRawPar",mr);
  PHIODataNode<PHTable>* dCrkGhitRawParNode = new PHIODataNode<PHTable>(dCrkGhitRawPar,"dCrkGhitRawPar");
  parNode->addNode(dCrkGhitRawParNode);

  size_t mr=1;
  dCrkRawHitParWrapper* dCrkRawHitPar = new dCrkRawHitParWrapper("dCrkRawHitPar",mr);
  PHIODataNode<PHTable>* dCrkRawHitParNode = new PHIODataNode<PHTable>(dCrkRawHitPar,"dCrkRawHitPar");
  parNode->addNode(dCrkRawHitParNode);

  size_t mr=1;
  dCrkProjPidParWrapper* dCrkProjPidPar = new dCrkProjPidParWrapper("dCrkProjPidPar",mr);
  PHIODataNode<PHTable>* dCrkProjPidParNode = new PHIODataNode<PHTable>(dCrkProjPidPar,"dCrkProjPidPar");
  parNode->addNode(dCrkProjPidParNode);

  size_t mr=5120;
  dCrkCalWrapper* dCrkCal = new dCrkCalWrapper("dCrkCal",mr);
  PHIODataNode<PHTable>* dCrkCalNode = new PHIODataNode<PHTable>(dCrkCal,"dCrkCal");
  parNode->addNode(dCrkCalNode);
  dstNode->addNode(dCrkCalNode);

  size_t mr=5120;
  dCrkUcalWrapper* dCrkUcal = new dCrkUcalWrapper("dCrkUcal",mr);
  PHIODataNode<PHTable>* dCrkUcalNode = new PHIODataNode<PHTable>(dCrkUcal,"dCrkUcal");
  parNode->addNode(dCrkUcalNode);
  dstNode->addNode(dCrkUcalNode);

  size_t mr=5120;
  dCrkRawWrapper* dCrkRaw = new dCrkRawWrapper("dCrkRaw",mr);
  PHIODataNode<PHTable>* dCrkRawNode = new PHIODataNode<PHTable>(dCrkRaw,"dCrkRaw");
  dstNode->addNode(dCrkRawNode);

  size_t mr=5120;
  dCrkHitWrapper* dCrkHit = new dCrkHitWrapper("dCrkHit",mr);
  PHIODataNode<PHTable>* dCrkHitNode = new PHIODataNode<PHTable>(dCrkHit,"dCrkHit");
  dstNode->addNode(dCrkHitNode);

  size_t mr=1500;
  dCrkPidWrapper* dCrkPid = new dCrkPidWrapper("dCrkPid",mr);
  PHIODataNode<PHTable>* dCrkPidNode = new PHIODataNode<PHTable>(dCrkPid,"dCrkPid");
  dstNode->addNode(dCrkPidNode);

  size_t mr=5120;
  dCrkRel2sWrapper* dCrkRel2s = new dCrkRel2sWrapper("dCrkRel2s",mr);
  PHIODataNode<PHTable>* dCrkRel2sNode = new PHIODataNode<PHTable>(dCrkRel2s,"dCrkRel2s");
  evaNode->addNode(dCrkRel2sNode);
  dstNode->addNode(dCrkRel2sNode);

  size_t mr=2250000;
  crkghitWrapper* crkghit = new crkghitWrapper("crkghit",mr);
  PHIODataNode<PHTable>* crkghitNode = new PHIODataNode<PHTable>(crkghit,"crkghit");
  geaNode->addNode(crkghitNode);
  dstNode->addNode(crkghitNode);

  cout << "5 ----------------------------- "<< endl;

/*size_t mr=1;
  dTecControlWrapper* dTecControl = new dTecControlWrapper("dTecControl",mr);
  PHIODataNode<PHTable>* dTecControlNode = new PHIODataNode<PHTable>(dTecControl,"dTecControl");
  parNode->addNode(dTecControlNode);
*/
  size_t mr=1;
  dTecGeomWrapper* dTecGeom = new dTecGeomWrapper("dTecGeom",mr);
  PHIODataNode<PHTable>* dTecGeomNode = new PHIODataNode<PHTable>(dTecGeom,"dTecGeom");
  parNode->addNode(dTecGeomNode);

/*  size_t mr=1;
  dTecSlowSimParWrapper* dTecSlowSimPar = new dTecSlowSimParWrapper("dTecSlowSimPar",mr);
  PHIODataNode<PHTable>* dTecSlowSimParNode = new PHIODataNode<PHTable>(dTecSlowSimPar,"dTecSlowSimPar");
  parNode->addNode(dTecSlowSimParNode);
*/
  size_t mr=7500;
  dTecRawWrapper* dTecRaw = new dTecRawWrapper("dTecRaw",mr);
  PHIODataNode<PHTable>* dTecRawNode = new PHIODataNode<PHTable>(dTecRaw,"dTecRaw");
  tecNode->addNode(dTecRawNode);

  size_t mr=7500;
  dTecVectorWrapper* dTecVector = new dTecVectorWrapper("dTecVector",mr);
  PHIODataNode<PHTable>* dTecVectorNode = new PHIODataNode<PHTable>(dTecVector,"dTecVector");
  tecNode->addNode(dTecVectorNode);

  size_t mr=3000;
  dTecTrackWrapper* dTecTrack = new dTecTrackWrapper("dTecTrack",mr);
  PHIODataNode<PHTable>* dTecTrackNode = new PHIODataNode<PHTable>(dTecTrack,"dTecTrack");
  dstNode->addNode(dTecTrackNode);

  size_t mr=150000;
  dTecGhitRawWrapper* dTecGhitRaw = new dTecGhitRawWrapper("dTecGhitRaw",mr);
  PHIODataNode<PHTable>* dTecGhitRawNode = new PHIODataNode<PHTable>(dTecGhitRaw,"dTecGhitRaw");
  evaNode->addNode(dTecGhitRawNode);

  size_t mr=7500;
  dTecRawVectWrapper* dTecRawVect = new dTecRawVectWrapper("dTecRawVect",mr);
  PHIODataNode<PHTable>* dTecRawVectNode = new PHIODataNode<PHTable>(dTecRawVect,"dTecRawVect");
  tecNode->addNode(dTecRawVectNode);

  size_t mr=3000;
  dTecVectTrackWrapper* dTecVectTrack = new dTecVectTrackWrapper("dTecVectTrack",mr);
  PHIODataNode<PHTable>* dTecVectTrackNode = new PHIODataNode<PHTable>(dTecVectTrack,"dTecVectTrack");
  tecNode->addNode(dTecVectTrackNode);

  size_t mr=7500;
  dTecCalibWrapper* dTecCalib = new dTecCalibWrapper("dTecCalib",mr);
  PHIODataNode<PHTable>* dTecCalibNode = new PHIODataNode<PHTable>(dTecCalib,"dTecCalib");
  dstNode->addNode(dTecCalibNode);

  size_t mr=45120;
  dTecWireWrapper* dTecWire = new dTecWireWrapper("dTecWire",mr);
  PHIODataNode<PHTable>* dTecWireNode = new PHIODataNode<PHTable>(dTecWire,"dTecWire");
  parNode->addNode(dTecWireNode);

  size_t mr=1500;
  dTecPIDWrapper* dTecPID = new dTecPIDWrapper("dTecPID",mr);
  PHIODataNode<PHTable>* dTecPIDNode = new PHIODataNode<PHTable>(dTecPID,"dTecPID");
  dstNode->addNode(dTecPIDNode);

  size_t mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  size_t mr=150000;
  dTecRawTrackWrapper* dTecRawTrack = new dTecRawTrackWrapper("dTecRawTrack",mr);
  PHIODataNode<PHTable>* dTecRawTrackNode = new PHIODataNode<PHTable>(dTecRawTrack,"dTecRawTrack");
  tecNode->addNode(dTecRawTrackNode);

  size_t mr=4500;
  tecghitWrapper* tecghit = new tecghitWrapper("tecghit",mr);
  PHIODataNode<PHTable>* tecghitNode = new PHIODataNode<PHTable>(tecghit,"tecghit");
  geaNode->addNode(tecghitNode);
  cout << "6 ----------------------------- "<< endl;

// added by julia 05/30/01
  PHIODataNode<TObject>* TofDetGeoNode = new PHIODataNode<TObject>(TofGeometry,"TofGeometry");
  parNode->addNode(TofDetGeoNode);
//
  size_t mr=1;
  dTofGeoParWrapper* dTofGeoPar = new dTofGeoParWrapper("dTofGeoPar",mr);
  PHIODataNode<PHTable>* dTofGeoParNode = new PHIODataNode<PHTable>(dTofGeoPar,"dTofGeoPar");
  parNode->addNode(dTofGeoParNode);

  size_t mr=1500;
  dTofGeoWrapper* dTofGeo = new dTofGeoWrapper("dTofGeo",mr);
  PHIODataNode<PHTable>* dTofGeoNode = new PHIODataNode<PHTable>(dTofGeo,"dTofGeo");
  parNode->addNode(dTofGeoNode);

  size_t mr=1500;
  dTofCalWrapper* dTofCal = new dTofCalWrapper("dTofCal",mr);
  PHIODataNode<PHTable>* dTofCalNode = new PHIODataNode<PHTable>(dTofCal,"dTofCal");
  parNode->addNode(dTofCalNode);

  size_t mr=1500;
  dTofUcalWrapper* dTofUcal = new dTofUcalWrapper("dTofUcal",mr);
  PHIODataNode<PHTable>* dTofUcalNode = new PHIODataNode<PHTable>(dTofUcal,"dTofUcal");
  parNode->addNode(dTofUcalNode);

  size_t mr=1500;
  dTofGdigiWrapper* dTofGdigi = new dTofGdigiWrapper("dTofGdigi",mr);
  PHIODataNode<PHTable>* dTofGdigiNode = new PHIODataNode<PHTable>(dTofGdigi,"dTofGdigi");
  tofNode->addNode(dTofGdigiNode);
  evaNode->addNode(dTofGdigiNode);
  dstNode->addNode(dTofGdigiNode);

  size_t mr=300;
  dTofRawWrapper* dTofRaw = new dTofRawWrapper("dTofRaw",mr);
  PHIODataNode<PHTable>* dTofRawNode = new PHIODataNode<PHTable>(dTofRaw,"dTofRaw");
  tofNode->addNode(dTofRawNode);

  size_t mr=1500;
  dTofReconstructedWrapper* dTofReconstructed = new dTofReconstructedWrapper("dTofReconstructed",mr);
  PHIODataNode<PHTable>* dTofReconstructedNode = new PHIODataNode<PHTable>(dTofReconstructed,"dTofReconstructed");
  dstNode->addNode(dTofReconstructedNode);

  cout << "6a-------------------------"<< endl;

  size_t mr=1500;
  dTofGhitGdigiWrapper* dTofGhitGdigi = new dTofGhitGdigiWrapper("dTofGhitGdigi",mr);
  PHIODataNode<PHTable>* dTofGhitGdigiNode = new PHIODataNode<PHTable>(dTofGhitGdigi,"dTofGhitGdigi");
  tofNode->addNode(dTofGhitGdigiNode);
  evaNode->addNode(dTofGhitGdigiNode);
  dstNode->addNode(dTofGhitGdigiNode);

  size_t mr=1500;
  dTofGdigiRecWrapper* dTofGdigiRec = new dTofGdigiRecWrapper("dTofGdigiRec",mr);
  PHIODataNode<PHTable>* dTofGdigiRecNode = new PHIODataNode<PHTable>(dTofGdigiRec,"dTofGdigiRec");
  tofNode->addNode(dTofGdigiRecNode);
  evaNode->addNode(dTofGdigiRecNode);
  dstNode->addNode(dTofGdigiRecNode);

  size_t mr=3000;
  dTofGhitRawWrapper* dTofGhitRaw = new dTofGhitRawWrapper("dTofGhitRaw",mr);
  PHIODataNode<PHTable>* dTofGhitRawNode = new PHIODataNode<PHTable>(dTofGhitRaw,"dTofGhitRaw");
  evaNode->addNode(dTofGhitRawNode);

  size_t mr=150;
  dTofRawRecWrapper* dTofRawRec = new dTofRawRecWrapper("dTofRawRec",mr);
  PHIODataNode<PHTable>* dTofRawRecNode = new PHIODataNode<PHTable>(dTofRawRec,"dTofRawRec");
  tofNode->addNode(dTofRawRecNode);
  dstNode->addNode(dTofRawRecNode);

  size_t mr=1;
  dTofPerfParWrapper* dTofPerfPar = new dTofPerfParWrapper("dTofPerfPar",mr);
  PHIODataNode<PHTable>* dTofPerfParNode = new PHIODataNode<PHTable>(dTofPerfPar,"dTofPerfPar");
  parNode->addNode(dTofPerfParNode);

  cout << "6b------------------------"<< endl;
  size_t mr=1;
  dTofGhitRawParWrapper* dTofGhitRawPar = new dTofGhitRawParWrapper("dTofGhitRawPar",mr);
  PHIODataNode<PHTable>* dTofGhitRawParNode = new PHIODataNode<PHTable>(dTofGhitRawPar,"dTofGhitRawPar");
  parNode->addNode(dTofGhitRawParNode);

  size_t mr=1;
  dTofRawRecParWrapper* dTofRawRecPar = new dTofRawRecParWrapper("dTofRawRecPar",mr);
  PHIODataNode<PHTable>* dTofRawRecParNode = new PHIODataNode<PHTable>(dTofRawRecPar,"dTofRawRecPar");
  parNode->addNode(dTofRawRecParNode);

  size_t mr=1;
  dTofCalParWrapper* dTofCalPar = new dTofCalParWrapper("dTofCalPar",mr);
  PHIODataNode<PHTable>* dTofCalParNode = new PHIODataNode<PHTable>(dTofCalPar,"dTofCalPar");
  parNode->addNode(dTofCalParNode);

  size_t mr=1;
  dTofUcalParWrapper* dTofUcalPar = new dTofUcalParWrapper("dTofUcalPar",mr);
  PHIODataNode<PHTable>* dTofUcalParNode = new PHIODataNode<PHTable>(dTofUcalPar,"dTofUcalPar");
  parNode->addNode(dTofUcalParNode);

  size_t mr=4500;
  tofghitWrapper* tofghit = new tofghitWrapper("tofghit",mr);
  PHIODataNode<PHTable>* tofghitNode = new PHIODataNode<PHTable>(tofghit,"tofghit");
  geaNode->addNode(tofghitNode);
  dstNode->addNode(tofghitNode);
  cout << "7------------------------------"<< endl;

  size_t mr=8;
  dEmcGeaParamsWrapper* dEmcGeaParams = new dEmcGeaParamsWrapper("dEmcGeaParams",mr);
  PHIODataNode<PHTable>* dEmcGeaParamsNode = new PHIODataNode<PHTable>(dEmcGeaParams,"dEmcGeaParams");
  parNode->addNode(dEmcGeaParamsNode);

  size_t mr=30000;
  dEmcGeometryWrapper* dEmcGeometry = new dEmcGeometryWrapper("dEmcGeometry",mr);
  PHIODataNode<PHTable>* dEmcGeometryNode = new PHIODataNode<PHTable>(dEmcGeometry,"dEmcGeometry");
  parNode->addNode(dEmcGeometryNode);

  size_t mr=525000;
  dEmcGeaHitWrapper* dEmcGeaHit = new dEmcGeaHitWrapper("dEmcGeaHit",mr);
  PHIODataNode<PHTable>* dEmcGeaHitNode = new PHIODataNode<PHTable>(dEmcGeaHit,"dEmcGeaHit");
  geaNode->addNode(dEmcGeaHitNode);
//dstNode->addNode(dEmcGeaHitNode);

  size_t mr=15000;
  dEmcRawDataWrapper* dEmcRawData = new dEmcRawDataWrapper("dEmcRawData",mr);
  PHIODataNode<PHTable>* dEmcRawDataNode = new PHIODataNode<PHTable>(dEmcRawData,"dEmcRawData");
  emcNode->addNode(dEmcRawDataNode);

  size_t mr=15000;
  dEmcFEMDataWrapper* dEmcFEMData = new dEmcFEMDataWrapper("dEmcFEMData",mr);
  PHIODataNode<PHTable>* dEmcFEMDataNode = new PHIODataNode<PHTable>(dEmcFEMData,"dEmcFEMData");
  emcNode->addNode(dEmcFEMDataNode);

  size_t mr=500;
  dEmcDCMDataWrapper* dEmcDCMData = new dEmcDCMDataWrapper("dEmcDCMData",mr);
  PHIODataNode<PHTable>* dEmcDCMDataNode = new PHIODataNode<PHTable>(dEmcDCMData,"dEmcDCMData");
  dcmNode->addNode(dEmcDCMDataNode);

  size_t mr=10000;
  dEmcCalibTowerWrapper* dEmcCalibTower = new dEmcCalibTowerWrapper("dEmcCalibTower",mr);
  PHIODataNode<PHTable>* dEmcCalibTowerNode = new PHIODataNode<PHTable>(dEmcCalibTower,"dEmcCalibTower");
  emcNode->addNode(dEmcCalibTowerNode);

  size_t mr=6000;
  dEmcClusterLocalWrapper* dEmcClusterLocal = new dEmcClusterLocalWrapper("dEmcClusterLocal",mr);
  PHIODataNode<PHTable>* dEmcClusterLocalNode = new PHIODataNode<PHTable>(dEmcClusterLocal,"dEmcClusterLocal");
  dstNode->addNode(dEmcClusterLocalNode);

  size_t mr=1;
  dEmcEventWrapper* dEmcEvent = new dEmcEventWrapper("dEmcEvent",mr);
  PHIODataNode<PHTable>* dEmcEventNode = new PHIODataNode<PHTable>(dEmcEvent,"dEmcEvent");
  emcNode->addNode(dEmcEventNode);

  size_t mr=7500;
  dEmcGeaTrackTowerWrapper* dEmcGeaTrackTower = new dEmcGeaTrackTowerWrapper("dEmcGeaTrackTower",mr);
  PHIODataNode<PHTable>* dEmcGeaTrackTowerNode = new PHIODataNode<PHTable>(dEmcGeaTrackTower,"dEmcGeaTrackTower");
  evaNode->addNode(dEmcGeaTrackTowerNode);
  dstNode->addNode(dEmcGeaTrackTowerNode);

  size_t mr=6000;
  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = new dEmcClusterLocalExtWrapper("dEmcClusterLocalExt",mr);
  PHIODataNode<PHTable>* dEmcClusterLocalExtNode = new PHIODataNode<PHTable>(dEmcClusterLocalExt,"dEmcClusterLocalExt");
  dstNode->addNode(dEmcClusterLocalExtNode);

  size_t mr=6000;
  dEmcGeaClusterTrackWrapper* dEmcGeaClusterTrack = new dEmcGeaClusterTrackWrapper("dEmcGeaClusterTrack",mr);
  PHIODataNode<PHTable>* dEmcGeaClusterTrackNode = new PHIODataNode<PHTable>(dEmcGeaClusterTrack,"dEmcGeaClusterTrack");
  evaNode->addNode(dEmcGeaClusterTrackNode);
  dstNode->addNode(dEmcGeaClusterTrackNode);

  size_t mr=15000;
  dEmcGeaTrackClusterWrapper* dEmcGeaTrackCluster = new dEmcGeaTrackClusterWrapper("dEmcGeaTrackCluster",mr);
  PHIODataNode<PHTable>* dEmcGeaTrackClusterNode = new PHIODataNode<PHTable>(dEmcGeaTrackCluster,"dEmcGeaTrackCluster");
  evaNode->addNode(dEmcGeaTrackClusterNode);
  dstNode->addNode(dEmcGeaTrackClusterNode);

  size_t mr=15000;
  dEmcGeaTowerTrackWrapper* dEmcGeaTowerTrack = new dEmcGeaTowerTrackWrapper("dEmcGeaTowerTrack",mr);
  PHIODataNode<PHTable>* dEmcGeaTowerTrackNode = new PHIODataNode<PHTable>(dEmcGeaTowerTrack,"dEmcGeaTowerTrack");
  evaNode->addNode(dEmcGeaTowerTrackNode);
  dstNode->addNode(dEmcGeaTowerTrackNode);

  size_t mr=7500;
  dEmcGeaTrackWrapper* dEmcGeaTrack = new dEmcGeaTrackWrapper("dEmcGeaTrack",mr);
  PHIODataNode<PHTable>* dEmcGeaTrackNode = new PHIODataNode<PHTable>(dEmcGeaTrack,"dEmcGeaTrack");
  evaNode->addNode(dEmcGeaTrackNode);
  dstNode->addNode(dEmcGeaTrackNode);

  size_t mr=1;
  dEmcRespParWrapper* dEmcRespPar = new dEmcRespParWrapper("dEmcRespPar",mr);
  PHIODataNode<PHTable>* dEmcRespParNode = new PHIODataNode<PHTable>(dEmcRespPar,"dEmcRespPar");
  parNode->addNode(dEmcRespParNode);

/*size_t mr=1;
  dEmcNtupleParWrapper* dEmcNtuplePar = new dEmcNtupleParWrapper("dEmcNtuplePar",mr);
  PHIODataNode<PHTable>* dEmcNtupleParNode = new PHIODataNode<PHTable>(dEmcNtuplePar,"dEmcNtuplePar");
  parNode->addNode(dEmcNtupleParNode);
*/
  size_t mr=8;
  emcparWrapper* emcpar = new emcparWrapper("emcpar",mr);
  PHIODataNode<PHTable>* emcparNode = new PHIODataNode<PHTable>(emcpar,"emcpar");
  parNode->addNode(emcparNode);

  size_t mr=1;
  dEmcRecoParWrapper* dEmcRecoPar = new dEmcRecoParWrapper("dEmcRecoPar",mr);
  PHIODataNode<PHTable>* dEmcRecoParNode = new PHIODataNode<PHTable>(dEmcRecoPar,"dEmcRecoPar");
  parNode->addNode(dEmcRecoParNode);

  cout << "8---------------------------"<< endl;

  size_t mr=1;
  dCglPidParWrapper* dCglPidPar = new dCglPidParWrapper("dCglPidPar",mr);
  PHIODataNode<PHTable>* dCglPidParNode = new PHIODataNode<PHTable>(dCglPidPar,"dCglPidPar");
  parNode->addNode(dCglPidParNode);

  PHIODataNode<TObject>* cglDetectorGeoNode = new PHIODataNode<TObject>(cglDetectorGeo,"cglDetectorGeo");
  parNode->addNode(cglDetectorGeoNode);

  size_t mr=1;
  dCglProjParWrapper* dCglProjPar = new dCglProjParWrapper("dCglProjPar",mr);
  PHIODataNode<PHTable>* dCglProjParNode = new PHIODataNode<PHTable>(dCglProjPar,"dCglProjPar");
  parNode->addNode(dCglProjParNode);

  size_t mr=1500;
  dCglTrackWrapper* dCglTrack = new dCglTrackWrapper("dCglTrack",mr);
  PHIODataNode<PHTable>* dCglTrackNode = new PHIODataNode<PHTable>(dCglTrack,"dCglTrack");
  dstNode->addNode(dCglTrackNode);

  size_t mr=1500;
  dCglParticleWrapper* dCglParticle = new dCglParticleWrapper("dCglParticle",mr);
  PHIODataNode<PHTable>* dCglParticleNode = new PHIODataNode<PHTable>(dCglParticle,"dCglParticle");
  dstNode->addNode(dCglParticleNode);

  size_t mr=1;
  dCglSortedWrapper* dCglSortedArm0 = new dCglSortedWrapper("dCglSortedArm0",mr);
  PHIODataNode<PHTable>* dCglSortedArm0Node = new PHIODataNode<PHTable>(dCglSortedArm0,"dCglSortedArm0");
  cglNode->addNode(dCglSortedArm0Node);

  size_t mr=1;
  dCglSortedWrapper* dCglSortedArm1 = new dCglSortedWrapper("dCglSortedArm1",mr);
  PHIODataNode<PHTable>* dCglSortedArm1Node = new PHIODataNode<PHTable>(dCglSortedArm1,"dCglSortedArm1");
  cglNode->addNode(dCglSortedArm1Node);

  size_t mr=1500;
  dCglPidWrapper* dCglPid = new dCglPidWrapper("dCglPid",mr);
  PHIODataNode<PHTable>* dCglPidNode = new PHIODataNode<PHTable>(dCglPid,"dCglPid");
  dstNode->addNode(dCglPidNode);

  size_t mr=1500;
  dCglProjectionWrapper* dCglProjection = new dCglProjectionWrapper("dCglProjection",mr);
  PHIODataNode<PHTable>* dCglProjectionNode = new PHIODataNode<PHTable>(dCglProjection,"dCglProjection");
  cglNode->addNode(dCglProjectionNode);

  size_t mr=1;
  dCgeMCRelParWrapper* dCgeMCRelPar = new dCgeMCRelParWrapper("dCgeMCRelPar",mr);
  PHIODataNode<PHTable>* dCgeMCRelParNode = new PHIODataNode<PHTable>(dCgeMCRelPar,"dCgeMCRelPar");
  parNode->addNode(dCgeMCRelParNode);

  size_t mr=6000;
  dCgeMCBuildWrapper* dCgeMCBuild = new dCgeMCBuildWrapper("dCgeMCBuild",mr);
  PHIODataNode<PHTable>* dCgeMCBuildNode = new PHIODataNode<PHTable>(dCgeMCBuild,"dCgeMCBuild");
  cgeNode->addNode(dCgeMCBuildNode);

  size_t mr=1;
  dCgeGeantParWrapper* dCgeGeantPar = new dCgeGeantParWrapper("dCgeGeantPar",mr);
  PHIODataNode<PHTable>* dCgeGeantParNode = new PHIODataNode<PHTable>(dCgeGeantPar,"dCgeGeantPar");
  parNode->addNode(dCgeGeantParNode);

  cout << "XXXXXXXXXXXXXXXXXXXXXXXXX"<< endl;

  PhRootHistogramFactory::buildFactory();
  gRandom->SetSeed(0);



  gROOT->Macro("embedParMc.C");
}
