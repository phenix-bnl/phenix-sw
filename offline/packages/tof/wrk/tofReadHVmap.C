////////////////////////////////////////////////////////
//
//  Read from Hvmap by getSlatIDfromHV()
//  
//
/////////////////////////////////////////////////////////

void tofReadHVmap(Int_t icrate=0, Int_t iboard=0, Int_t ichannel=0){
  // Loading PHOOL libraries
  //gSystem->Load("libEvent.so");
  gSystem->Load("libtof.so");

  // Set up the modules
  TofAddressObject* TofAddress = new TofAddressObject();

  // Initialize the HV map
  //TofAddress->fetchFromFile("toffemmap.txt");
  TofAddress->fetchFromFile("toffemmap.txt","tofcablemap.txt","tofhvmap.txt");
  
  Int_t slatid;

  slatid = TofAddress->getSlatIDfromHV(icrate,iboard,ichannel);
  cout << "slatid = " << slatid << endl;
}
