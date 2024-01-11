//************************************************************
// Initialization macro for PHOOL in response chain
//************************************************************

{  

  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libdgo.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("libPISARoot.so");
  gSystem->Load("libgea.so");

  gSystem->Load("libemcCalib.so") ;
  gSystem->Load("libemcOM.so") ;

  // Loading subsystem libraries
  gSystem->Load("libbbc.so");
  gSystem->Load("libzdc.so");
  gSystem->Load("libmvd.so");
  gSystem->Load("libpad.so");
  gSystem->Load("libemc.so");
  gSystem->Load("libtof.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libdch.so");
  gSystem->Load("libcrk.so");
  gSystem->Load("libtec.so");
  gSystem->Load("libheader.so");

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
  PHCompositeNode* prdfNode = new PHCompositeNode("PRDF");
  topNode->addNode(prdfNode);
  PHNodeIterator mainIter(topNode);

  PhRootHistogramFactory::buildFactory();

}





