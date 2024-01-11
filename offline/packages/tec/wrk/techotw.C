  //void techotw(const char*   prdfIFile="/data2/phnxtec/BigPartition/EVENTDATAxxx_P01-0000111743-0004.PRDFF") {
  //void techotw(const char*   prdfIFile="/data2/phnxtec/BigPartition/EVENTDATAxxx_P01-0000112666-0004.PRDFF") {
  //void techotw(const char*   prdfIFile="/data2/phnxtec/BigPartition/EVENTDATAxxx_P01-0000114102-0004.PRDFF") {
  void techotw(const char*   prdfIFile="/phenix/data23/lebedev/data/EVENTDATAxxx_P01-0000117297-0000.PRDFF") {

  int eventNumber=0;
  int RunNumber=0;
  int lastEvent=200;
  int index,isect,iplane,iside,iwire,ihit;
  int ibin;
  int list[48][491];
  int list4[48][491];

// Book histograms
  
  char run[11];
  run[0] = prdfIFile[40+9];
  run[1] = prdfIFile[41+9];
  run[2] = prdfIFile[42+9];
  run[3] = prdfIFile[43+9];
  run[4] = prdfIFile[44+9];
  run[5] = prdfIFile[45+9];
  run[6] = prdfIFile[46+9];
  run[7] = prdfIFile[47+9];
  run[8] = prdfIFile[48+9];
  run[9] = prdfIFile[49+9];
  run[10] = prdfIFile[50+9];
  cout << "RUN: " << run << endl;

  char hname[30],hfile[35];
  sprintf(hfile, "./hotw/hotwires_xx%s.root", run);
//  sprintf(hfile, "./hotw/hotwires_xxx%s.root", run);
//  sprintf(hfile, "./hotw/hotw_all_run%s.root", run);
  TFile *fileout;
  fileout = new TFile(hfile,"recreate");

  char hname[20], htitle[80]; 
  TProfile* hotw = new TProfile[48];
  TProfile* hotw4 = new TProfile[48];
  
  for(int i=0; i<4; i++) {        // sector
    for(int j=0; j<6; j++) {      // plane
      for(int k=0; k<2; k++) {    // side
	index=i*12+j*2+k;        
	    sprintf(hname,"hotwplane%d",index);
	    sprintf(htitle,"hot wires sector%d-plane%d-side%d",i,j,k);
	    hotw[index] = new TProfile(hname,htitle,491,-0.5,490.5);
	    sprintf(hname,"hotw4plane%d",index);
	    sprintf(htitle,"hot wires sector%d-plane%d-side%d",i,j,k);
	    hotw4[index] = new TProfile(hname,htitle,491,-0.5,490.5);
      } 
    }
  }
  
// Load libraries

//  gSystem->Load("libpreco.so");
//  cout << "libraries loaded..." << endl;

// Set up the node tree

  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  
// Create event iterator

  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfIFile);
  
// Set up the modules

  mTecUnpackModule* mTecUnpack = new mTecUnpackModule();
  mTecCalibModule* mTecCalib = new mTecCalibModule();

//  int mr = 8000;
//  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
//  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
//  tecNode->addNode(dTecFemDataNode);

  TecOutV1* tecout = new TecOutV1();  
  PHIODataNode<PHObject>* TecOutNode = new PHIODataNode<PHObject>((PHObject*)tecout,"TecOutV1","PHObject");
  tecNode->addNode(TecOutNode);

  TecAddressObject* TAO = new TecAddressObject();
  TecGeometryObject* TGO = new TecGeometryObject();
  TecCalibrationObject* TCO = new TecCalibrationObject();

  mainIter.cd();
  
//============================================================================

// Start loop over events

  printf("Entering event loop.\n");
  while ((thisEvent=eventIter->getNextEvent()) && eventNumber++ < lastEvent) {
    
  cout << eventNumber << " ";
  thisEvent->identify();

  if(eventNumber==1) {
    RunNumber = thisEvent->getRunNumber();
      cout << "Analyzing run number " << RunNumber << endl;

// Create TAO, TGO and TCO

    TAO->setRunNumber(RunNumber);  
//    TAO->FetchFromFile("tecmap_run3.txt");
    TAO->Fetch();
//    TGO->setRunNumber(RunNumber);  
    TGO->FetchFromFile();
//    TCO->setRunNumber(RunNumber);  
//    TCO->Fetch();
    TCO->FetchFromFile();
  
  }

// Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);
    
//    printf("Fetched event %d\n",eventNumber);
//    topNode->print();
    
      mTecUnpack->set_FadcCut(2);
      mTecUnpack->event(topNode,TAO);
      
      mTecCalib->set_ApplyGains(0);
      mTecCalib->event(topNode, TAO, TCO);
      
      cout << "TECOUT: " << tecout->getNTracks() << " " << tecout->getNHits() << endl;

	for(int iind=0; iind<48; iind++) 
	  for(int iw=0; iw<491; iw++) list[iind][iw]=0;
	for(int iind=0; iind<48; iind++) 
	  for(int iw=0; iw<491; iw++) list4[iind][iw]=0;


        for(int k=0; k<tecout->getNHits(); k++) {

          index = tecout->getHitIndex(k);
          iwire = tecout->getHitWire(k);

            if(tecout->getHitADC(k)>2) list[index][iwire]++;
            if(tecout->getHitADC(k)>6) list4[index][iwire]++;

        }

	for(int iind=0; iind<48; iind++) {
	  for(int iw=0; iw<491; iw++) {
	    hotw[iind]->Fill(float(iw),float(list[iind][iw]));
          }
        }

	for(int iind=0; iind<48; iind++) { 
	  for(int iw=0; iw<491; iw++) {
	    hotw4[iind]->Fill(float(iw),float(list4[iind][iw]));
          }
        }

// Reset all data for this event

      mainIter.cd();
      if (mainIter.cd("TEC")) {
	mainIter.forEach(reset);
	mainIter.cd();
      }
      
  } // end loop over events
  
  fileout->Write();
  fileout->Close();
  
}



