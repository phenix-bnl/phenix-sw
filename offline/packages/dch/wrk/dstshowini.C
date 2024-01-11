
{
  gSystem->Load("libpreco.so");
  Int_t verbose = 12;
  Int_t runNumber = 0;
  Int_t runDate = 0;
  Int_t runTime = 0;

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);

  mNewDchInitializer* mDchInitializer;
  mNewDchCalibrator* mDchCalibrator = new mNewDchCalibrator;
  mNewDchCandidatory* mDchCandidatory = new mNewDchCandidatory();
  mNewDchPerfectTracker* mNewDchPerfect = new mNewDchPerfectTracker;
  mNewDchEvaluator* mNewDchEvaluate = new mNewDchEvaluator;
  mDchEventFilter * Filter = new mDchEventFilter;
  mDchCandidatory->setVerbose(5);
  PHpadDetectorGeo* mPadDetGeo = new PHpadDetectorGeo;
 
  PHNodeIOManager* dstIn;
}
