//************************************************************
// Test initialization macro for PRDF write (Bbc only version)  
//************************************************************

{  

  // Loading subsystem libraries
  gSystem->Load("libzdc_tables.so");
  gSystem->Load("libzdc.so");

  // Set up the node tree to have BBC tables
  PHCompositeNode* zdcNode = new PHCompositeNode("ZDC");
  topNode->addNode(zdcNode);

  // Set up the modules
  ZdcEvent* mZdcEvent = new ZdcEvent();

  // Set up ZDC tables
  size_t mr=8;
  dZdcUcalWrapper*  dZdcUcal = new dZdcUcalWrapper("dZdcUcal",mr);
  PHIODataNode<PHTable>* dZdcUcalNode = new PHIODataNode<PHTable>(dZdcUcal,"dZdcUcal");
  parNode->addNode(dZdcUcalNode);

  size_t mr=8;
  dZdcRawWrapper*  dZdcRaw = new dZdcRawWrapper("dZdcRaw",mr);
  PHIODataNode<PHTable>* dZdcRawNode = new PHIODataNode<PHTable>(dZdcRaw,"dZdcRaw");
  zdcNode->addNode(dZdcRawNode);

  size_t mr=1;
  dZdcFEMWrapper*  dZdcFEM = new dZdcFEMWrapper("dZdcFEM",mr);
  PHIODataNode<PHTable>* dZdcFEMNode = new PHIODataNode<PHTable>(dZdcFEM,"dZdcFEM");
  zdcNode->addNode(dZdcFEMNode);

  size_t mr=1;
  dZdcDCMWrapper*  dZdcDCM = new dZdcDCMWrapper("dZdcDCM",mr);
  PHIODataNode<PHTable>* dZdcDCMNode = new PHIODataNode<PHTable>(dZdcDCM,"dZdcDCM");
  dcmNode->addNode(dZdcDCMNode);

}
