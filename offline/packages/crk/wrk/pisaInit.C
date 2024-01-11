//************************************************************
// Initialization macro for PISA interface to PHOOL  
//************************************************************

{  
  //
  // Load the PISA class libraries
  //
  gSystem->Load("/afs/rhic/phenix/software/simulation/pro/i386_linux2/lib/libPISARoot.so");

  //
  // PISA interface GEA class libraries
  //
  gSystem->Load("/afs/rhic/phenix/software/simulation/pro/i386_linux2/lib/libgea_tables.so");
  gSystem->Load("/afs/rhic/phenix/software/simulation/pro/i386_linux2/lib/libgea.so");

  size_t mr=30000;
  fkinWrapper* fkin = new fkinWrapper("fkin",mr);
  PHIODataNode<PHTable>* fkinNode = new PHIODataNode<PHTable>(fkin,"fkin");
  evaNode->addNode(fkinNode);

  size_t mr=10;
  headerWrapper* header = new headerWrapper("header",mr);
  PHIODataNode<PHTable>* headerNode = new PHIODataNode<PHTable>(header,"header");
  evaNode->addNode(headerNode);

}





