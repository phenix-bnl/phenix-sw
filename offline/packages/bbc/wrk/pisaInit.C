//************************************************************
// Initialization macro for PISA interface to PHOOL  
//************************************************************

{  
  //
  // Load the PISA class libraries
  //
  gSystem->Load("libPISARoot.so");

  //
  // PISA interface GEA class libraries
  //
  gSystem->Load("libgea.so");

  size_t mr=30000;
  fkinWrapper* fkin = new fkinWrapper("fkin",mr);
  PHIODataNode<PHTable>* fkinNode = new PHIODataNode<PHTable>(fkin,"fkin");
  evaNode->addNode(fkinNode);

  size_t mr=30000;
  pythiaWrapper* pythia = new pythiaWrapper("pythia",mr);
  PHIODataNode<PHTable>* pythiaNode = new PHIODataNode<PHTable>(pythia,"pythia");
  evaNode->addNode(pythiaNode);

  size_t mr=30000;
  primaryWrapper* primary = new primaryWrapper("primary",mr);
  PHIODataNode<PHTable>* primaryNode = new PHIODataNode<PHTable>(primary,"primary");
  evaNode->addNode(primaryNode);

  size_t mr=10;
  headerWrapper* header = new headerWrapper("header",mr);
  PHIODataNode<PHTable>* headerNode = new PHIODataNode<PHTable>(header,"header");
  evaNode->addNode(headerNode);

}
