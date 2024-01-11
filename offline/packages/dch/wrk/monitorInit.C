//************************************************************
// Initialization macro written by the Pyrite GUI for PHOOL   
//************************************************************

{
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea_tables.so");
  gSystem->Load("libgea.so");

  // Loading subsystem libraries
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("/home/phnxdc/lib0720/install/lib/libdgo.so");
  gSystem->Load("/home/phnxdc/lib0720/install/lib/libdch_tables.so");
  gSystem->Load("/home/phnxdc/lib0720/install/lib/libdch.so");
 
 
  //
  // modify input file here:
  //

  printf("Set up input files\n");
 
  FILE* a = fopen("tmp.txt","w");
 
  PHString prdfInFile = "/export/bigdisk/dcm_data/built_evt/rc-0006537.prdf";



//************************************************************
//
// do not change code below   
//
//************************************************************

   

  Eventiterator *eventIter = new fileEventiterator(prdfInFile.getString());

  Int_t goodEvents= 0;
  Int_t verbose = 12;


  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHCompositeNode* dchNode = new PHCompositeNode("DCH");
  topNode->addNode(dchNode);
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
  PHCompositeNode* zdcNode = new PHCompositeNode("ZDC");
  topNode->addNode(zdcNode);
 
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
 

  Int_t first = 1;
  Int_t eventNumber= 0;

  Event *thisEvent = 0;
  PHDataNode<Event>* prdfEventNode = new  PHDataNode<Event>(thisEvent, "PRDF");
  topNode->addNode(prdfEventNode);

  printf("Set up the modules for the analysis \n");
  PHDchHistogrammer* histogrammer;
  mNewDchInitializer* mDchInitializer = new mNewDchInitializer(0,1,0);//
  mNewDchNoiseAnalyzer* mDchNoiseAnalyzer = new mNewDchNoiseAnalyzer(0,0); // 
  mNewDchUnpacker*    mDchUnpacker    = new mNewDchUnpacker;//
  mNewDchCalibrator*  mDchCalibrator  = new mNewDchCalibrator;//
  mNewDchTracker*     mDchTracker     = new mNewDchTracker;//
  mNewDchCandidatory* mDchCandidatory = new mNewDchCandidatory;
  mNewDchBuilder* mDchBuilder = new mNewDchBuilder;
  mNewDchAnalyzer* mDchAnalyzer = new mNewDchAnalyzer;
  mNewDchDriftVelocityCalibrator* mDchDVCalibrator = new mNewDchDriftVelocityCalibrator;



  printf("Set up the Vertex Fitter \n");
  mDchVertexFit* dchVertexFit = new mDchVertexFit;
  dchVertexFit->initialize();

  printf(" Initialize the tables \n");
  
  size_t mr=60000;
  dDchRawWrapper* dDchRaw = new dDchRawWrapper("dDchRaw",mr);
  PHIODataNode<PHTable>* dDchRawNode = new PHIODataNode<PHTable>(dDchRaw,"dDchRaw");
  dchNode->addNode(dDchRawNode);

  size_t mr=60000;
  dDchHitWrapper* dDchHit = new dDchHitWrapper("dDchHit",mr);
  PHIODataNode<PHTable>* dDchHitNode = new PHIODataNode<PHTable>(dDchHit,"dDchHit");
  dstNode->addNode(dDchHitNode);

  size_t mr=1500;
  dDchTracksWrapper* dDchTracks = new dDchTracksWrapper("dDchTracks",mr);
  PHIODataNode<PHTable>* dDchTracksNode = new PHIODataNode<PHTable>(dDchTracks,"dDchTracks");
  dstNode->addNode(dDchTracksNode);

  size_t mr=60000;
  dDchGhitRawWrapper* dDchGhitRaw = new dDchGhitRawWrapper("dDchGhitRaw",mr);
  PHIODataNode<PHTable>* dDchGhitRawNode = new PHIODataNode<PHTable>(dDchGhitRaw,"dDchGhitRaw");
  dchNode->addNode(dDchGhitRawNode);

  size_t mr=60000;
  dDchGhitHitsWrapper* dDchGhitHits = new dDchGhitHitsWrapper("dDchGhitHits",mr);
  PHIODataNode<PHTable>* dDchGhitHitsNode = new PHIODataNode<PHTable>(dDchGhitHits,"dDchGhitHits");
  dchNode->addNode(dDchGhitHitsNode);

  size_t mr=160;
  dDchDCMWrapper* dDchDCM = new dDchDCMWrapper("dDchDCM",mr);
  PHIODataNode<PHTable>* dDchDCMNode = new PHIODataNode<PHTable>(dDchDCM,"dDchDCM");
  dcmNode->addNode(dDchDCMNode);

  PHIODataNode<PHTable>* dDchNibbleGhitNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dDchNibbleGhit");
  if (!dDchNibbleGhitNode) {
    //cout << "Eva Read: Could not find data node dDchNibbleGhit" << endl;
  } else {
    dDchNibbleGhitWrapper* dDchNibbleGhit = (dDchNibbleGhitWrapper*)dDchNibbleGhitNode->getData();
  }


  size_t mr=1;
  dDchReconstructionParWrapper* dDchRecoPar = 
    new dDchReconstructionParWrapper("dDchRecoPar",mr);
  PHIODataNode<PHTable>* dDchRecoParNode = 
    new PHIODataNode<PHTable>(dDchRecoPar,"dDchRecoPar");
  parNode->addNode(dDchRecoParNode);

  size_t mr=1;
  dDchUnpackParWrapper* dDchUnpackPar = new dDchUnpackParWrapper("dDchUnpackPar",mr);
  PHIODataNode<PHTable>* dDchUnpackParNode = new PHIODataNode<PHTable>(dDchUnpackPar,"dDchUnpackPar");
  parNode->addNode(dDchUnpackParNode);


  

  PhRootHistogramFactory::buildFactory();
 
}











