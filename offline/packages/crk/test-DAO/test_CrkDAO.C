//************************************************************
// Initialization macro written by the Pyrite GUI for PHOOL   
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

  // Loading subsystem libraries
  gSystem->Load("libbbc.so");
  gSystem->Load("libmvd.so");
  gSystem->Load("libpad.so");
  gSystem->Load("libemc.so");
  gSystem->Load("libtof.so");
  gSystem->Load("libcgl.so");
  gSystem->Load("libdch.so");
  gSystem->Load("libcrk.so");
  gSystem->Load("libtec.so");
  //  gSystem->Load("libmom.so");

  //  CrkDAO *CrkDAO = new CrkDAO("crk_cabling.txt");
  CrkDAO *CrkDAO = new CrkDAO("crk_cabling_vrdc.txt"); 
  CrkDAO->print();

  while(1) {
    cout << "enter packet, channel"<<endl;
    int packet, channel;
    cin >> packet >> channel;
    if(packet < 0) break;

    cout << CrkDAO->get_PMTid(packet,channel) << endl;
  }
}

