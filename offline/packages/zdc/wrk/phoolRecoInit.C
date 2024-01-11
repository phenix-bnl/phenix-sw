//************************************************************
// Initialization macro for PHOOL in reconstruction chain
//************************************************************

{  

  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libPdbCal.so");

  // Set up the general purpose node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* geaNode = new PHCompositeNode("GEA");
  topNode->addNode(geaNode);
  PHCompositeNode* evaNode = new PHCompositeNode("EVA");
  topNode->addNode(evaNode);
  PHNodeIterator mainIter(topNode);

  PhRootHistogramFactory::buildFactory();

}





