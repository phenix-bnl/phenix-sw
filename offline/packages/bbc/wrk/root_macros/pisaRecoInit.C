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
  gSystem->Load("libgea_tables.so");
  gSystem->Load("libgea.so");

}
