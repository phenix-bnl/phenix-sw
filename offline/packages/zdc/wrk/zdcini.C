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

  size_t mr=8;
  dZdcRawWrapper*  dZdcRaw = new dZdcRawWrapper("dZdcRaw",mr);
  PHIODataNode<PHTable>* dZdcRawNode = new PHIODataNode<PHTable>(dZdcRaw,"dZdcRaw");
  zdcNode->addNode(dZdcRawNode);

  size_t mr=1;
  dZdcDCMWrapper*  dZdcDCM = new dZdcDCMWrapper("dZdcDCM",mr);
  PHIODataNode<PHTable>* dZdcDCMNode = new PHIODataNode<PHTable>(dZdcDCM,"dZdcDCM");
  dcmNode->addNode(dZdcDCMNode);

  size_t mr=1;
  dZdcOutWrapper*  dZdcOut = new dZdcOutWrapper("dZdcOut",mr);
  PHIODataNode<PHTable>* dZdcOutNode = new PHIODataNode<PHTable>(dZdcOut,"dZdcOut");
  dstNode->addNode(dZdcOutNode);

  size_t mr=1;
  ZdcCalib* ZdcCalibPar = new ZdcCalib();
  ZdcCalibPar->restore();
  PHIODataNode<TObject>*  ZdcCalibParNode = new PHIODataNode<TObject>(ZdcCalibPar,"ZdcCalibPar");
  parNode->addNode(ZdcCalibParNode);
  mZdcEvent->setCalibDataAll(ZdcCalibPar);  

} 
