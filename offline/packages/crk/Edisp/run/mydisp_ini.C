//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************
//void mydisp_ini(const char *dstIFile="centdst.root") {
{
  char *dstIFile = "centdst.root";
  Int_t eventNumber = 0;

  gSystem->Load("libEG.so");
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
  // Additions for EMCAL
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
  gSystem->Load("libmom.so");
  gSystem->Load("liblv1.so");
  gSystem->Load("libheader.so");

  // load browser
  gSystem->Load("libEdisp.so");

  // Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  // Set up input and output files
  PHString dstInFile = dstIFile;
  PHNodeIOManager* dstIn = new PHNodeIOManager(dstInFile,PHReadOnly);
  dstIn->read(dstNode);

  cout << "Initalization done"<<endl;
  topNode->print();

  TCanvas *c_N = new TCanvas("c_N","North",600,600);
  TCanvas *c_S = new TCanvas("c_S","South",600,600);
  //  TCanvas *c_R = new TCanvas("c_R","RICH",600,600);
  TCanvas *c_SW = new TCanvas("c_SW","South West",500,500); 
  TCanvas *c_NW = new TCanvas("c_NW","North West",500,500); 
  TCanvas *c_SE = new TCanvas("c_SE","South East",500,500); 
  TCanvas *c_NE = new TCanvas("c_NE","North East",500,500); 

  c_N->Range(-600,-600,600,600);
  c_S->Range(-600,-600,600,600);
  //  c_R->Range(-20,-70,540,470);
  c_SW->Range(-5.,-5.,405.,405.);
  c_NW->Range(-5.,-5.,405.,405.);
  c_SE->Range(-5.,-5.,405.,405.);
  c_NE->Range(-5.,-5.,405.,405.);

  Edisp *display = new Edisp(topNode);
}



