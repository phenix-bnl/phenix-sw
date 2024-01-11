
void teceff_realreal(Int_t firstEvent=1, Int_t lastEvent=1500, const char *prdfIFile2="/phenix/data05/evt_data/EVENTDATAxxx_P01-0000011970-0001.PRDFF", char *prdfIFile1="/phenix/data07/lebedev/filter/tecmult_17events11970-0000.prdfz") {

  Int_t eventNumber = 0;

// Book the ntuple
TFile *top = new TFile("trkeff.root","recreate");
float ntpart[19];
TNtuple *ntp0 = new TNtuple("ntp0","tracking efficiency","evt:mult:dxin:dxout:dyin:dyout:nhits:bbcn:bbcq:hitmult2s:hitmult4s:hitmult8s:hitmult2n:hitmult4n:hitmult8n:mult1s:mult1n:side:hitmult0");

// Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);
//BBC
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
//
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

// Create event iterator
  Event *thisEvent1 = 0;
  Event *thisEvent2 = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent1, "PRDF"));
  Eventiterator *eventIter1 = new fileEventiterator(prdfIFile1);
  Eventiterator *eventIter2 = new fileEventiterator(prdfIFile2);

// Set up the modules
  mTecAlignModule* mTecAlign = new mTecAlignModule;
  mTecDecodeModule* mTecDecode = new mTecDecodeModule;
  mTecRemapModule* mTecRemap = new mTecRemapModule;
  mTecCalibModule* mTecCalib = new mTecCalibModule;
  mTecHoughTrackModule* mTecHoughTrack = new mTecHoughTrackModule;

//BBC
  BbcEvent* bbcevent = new BbcEvent();
  BbcGeo* bbcgeo = new BbcGeo();
  mBbcSetGeoModule* mBbcSetGeo = new mBbcSetGeoModule;
  mBbcUnpackModule* mBbcUnpack = new mBbcUnpackModule;
  mBbcRawOutModule* mBbcRawOut = new mBbcRawOutModule;
//

  // Initialize the tables

//BBC

  Int_t mr=1;
  dBbcGeoWrapper* dBbcGeo = new dBbcGeoWrapper("dBbcGeo",mr);
  PHIODataNode<PHTable>* dBbcGeoNode = new PHIODataNode<PHTable>(dBbcGeo,"dBbcGeo");
  parNode->addNode(dBbcGeoNode);

  Int_t mr=1;
  dBbcRawHitParWrapper* dBbcRawHitPar = new dBbcRawHitParWrapper("dBbcRawHitPar",mr);
  PHIODataNode<PHTable>* dBbcRawHitParNode = new PHIODataNode<PHTable>(dBbcRawHitPar,"dBbcRawHitPar");
  parNode->addNode(dBbcRawHitParNode);

  Int_t mr=128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw",mr);
  PHIODataNode<PHTable>* dBbcRawNode = new PHIODataNode<PHTable>(dBbcRaw,"dBbcRaw");
  dstNode->addNode(dBbcRawNode);

  BbcCalib* BbcCalibPar = new BbcCalib();
  PHIODataNode<TObject>* BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  // Additions for BBC
  BbcCalibPar->restore("BbcCalib");
  bbcevent->setCalibDataAll(BbcCalibPar);

  Int_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);

  Int_t mr=128;
  dBbcCalWrapper* dBbcCal = new dBbcCalWrapper("dBbcCal",mr);
  PHIODataNode<PHTable>* dBbcCalNode = new PHIODataNode<PHTable>(dBbcCal,"dBbcCal");
  parNode->addNode(dBbcCalNode);

  Int_t mr=1;
  dBbcDCMWrapper* dBbcDCM = new dBbcDCMWrapper("dBbcDCM",mr);
  PHIODataNode<PHTable>* dBbcDCMNode = new PHIODataNode<PHTable>(dBbcDCM,"dBbcDCM");
  dcmNode->addNode(dBbcDCMNode);

// end BBC

// Initialize Tec tables

  Int_t mr=45120;
  dTecWireWrapper* dTecWire = new dTecWireWrapper("dTecWire",mr);
  PHIODataNode<PHTable>* dTecWireNode = new PHIODataNode<PHTable>(dTecWire,"dTecWire");
  parNode->addNode(dTecWireNode);

  Int_t mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  Int_t mr=8000;
  dTecRawWrapper* dTecRaw = new dTecRawWrapper("dTecRaw",mr);
  PHIODataNode<PHTable>* dTecRawNode = new PHIODataNode<PHTable>(dTecRaw,"dTecRaw");
  tecNode->addNode(dTecRawNode);

// Create second dTecRaw table for a test event
  Int_t mr=8000;
  dTecRawWrapper* dTecRaw2 = new dTecRawWrapper("dTecRaw2",mr);
  PHIODataNode<PHTable>* dTecRawNode2 = new PHIODataNode<PHTable>(dTecRaw2,"dTecRaw2");
//  tecNode->addNode(dTecRawNode2);
  parNode->addNode(dTecRawNode2);

  Int_t mr=8000;
  dTecCalibWrapper* dTecCalib = new dTecCalibWrapper("dTecCalib",mr);
  PHIODataNode<PHTable>* dTecCalibNode = new PHIODataNode<PHTable>(dTecCalib,"dTecCalib");
  tecNode->addNode(dTecCalibNode);

  Int_t mr=800;
  dTecTrackWrapper* dTecTrack = new dTecTrackWrapper("dTecTrack",mr);
  PHIODataNode<PHTable>* dTecTrackNode = new PHIODataNode<PHTable>(dTecTrack,"dTecTrack");
  tecNode->addNode(dTecTrackNode);

// Create second track table for the test event
  Int_t mr=800;
  dTecTrackWrapper* dTecTrack2 = new dTecTrackWrapper("dTecTrack2",mr);
  PHIODataNode<PHTable>* dTecTrackNode2 = new PHIODataNode<PHTable>(dTecTrack2,"dTecTrack2");
//  tecNode->addNode(dTecTrackNode2);
  parNode->addNode(dTecTrackNode2);

  Int_t mr=150000;
  dTecGhitRawWrapper* dTecGhitRaw = new dTecGhitRawWrapper("dTecGhitRaw",mr);
  PHIODataNode<PHTable>* dTecGhitRawNode = new PHIODataNode<PHTable>(dTecGhitRaw,"dTecGhitRaw");
  tecNode->addNode(dTecGhitRawNode);

  Int_t mr=150000;
  dTecRawTrackWrapper* dTecRawTrack = new dTecRawTrackWrapper("dTecRawTrack",mr);
  PHIODataNode<PHTable>* dTecRawTrackNode = new PHIODataNode<PHTable>(dTecRawTrack,"dTecRawTrack");
  tecNode->addNode(dTecRawTrackNode);

  Int_t mr=7500;
  dTecVectorWrapper* dTecVector = new dTecVectorWrapper("dTecVector",mr);
  PHIODataNode<PHTable>* dTecVectorNode = new PHIODataNode<PHTable>(dTecVector,"dTecVector");
  tecNode->addNode(dTecVectorNode);

  Int_t mr=3000;
  dTecVectTrackWrapper* dTecVectTrack = new dTecVectTrackWrapper("dTecVectTrack",mr);
  PHIODataNode<PHTable>* dTecVectTrackNode = new PHIODataNode<PHTable>(dTecVectTrack,"dTecVectTrack");
  tecNode->addNode(dTecVectTrackNode);


  mainIter.cd();

// Create TAO, TGO and TCO
  TecAddressObject* TecAddress = new TecAddressObject();
  TecAddressObject* TecAddress2 = new TecAddressObject();
  TecGeometryObject* TecGeometry = new TecGeometryObject();
  TecCalibrationObject* TecCalibration = new TecCalibrationObject();
  TecCalibrationObject* TecCalibration2 = new TecCalibrationObject();

// Set search time
   PHTimeStamp Tsearch(2000,8,20,0,0,0);
   TecAddress->setTimeStamp(Tsearch);
   TecAddress2->setTimeStamp(Tsearch);
   TecGeometry->setTimeStamp(Tsearch);
   TecCalibration->setTimeStamp(Tsearch);
   TecCalibration2->setTimeStamp(Tsearch);

// Set location
   char* locationAg; char* locationGg; char* locationCg;
   char* locationAs; char* locationGs; char* locationCs;
     locationAg = "map.tec.geant0";
     locationGg = "geom.tec.geant00";
     locationCg = "calib.tec.tecgain_geant0";
     locationAs = "map.tec.run00";
     locationGs = "geom.tec.run00";
     locationCs = "calib.tec.tecgain_run00";
   TecAddress->setCalibName(locationAs);
   TecAddress2->setCalibName(locationAs);
   TecGeometry->setCalibName(locationGs);
   TecCalibration->setCalibName(locationCs);
   TecCalibration2->setCalibName(locationCs);

// Get the data from Objy database
   TecAddress->Fetch();
   TecAddress2->Fetch();
   TecGeometry->Fetch();
   TecCalibration->Fetch();
   TecCalibration2->Fetch();

     PHIODataNode<TObject>* TecDetGeoNode = new PHIODataNode<TObject>(TecGeometry,"TecGeometry");
      parNode->addNode(TecDetGeoNode);

     PHIODataNode<TObject>* TecDetCalNode = new PHIODataNode<TObject>(TecCalibration,"TecCalibration");
      parNode->addNode(TecDetCalNode);

//  cout << "calling mTecAlign..." << endl;
//  mTecAlign->set_Verbose(0,0);
  mTecAlign->event(topNode, TecAddress, TecGeometry);

//BBC
        printf("Calling mBbcSetGeo\n");
        mBbcSetGeo->event(topNode);


// Start loop over events
  printf("Entering event loop.\n");

  while(eventNumber++ < lastEvent) {

// Read test event only once
//if(eventNumber==1) {
// Read TEST event every 50 events
if((eventNumber-1)%50 == 0) {

  thisEvent1=eventIter1->getNextEvent();
  mainIter.cd();

// Point the data node to the new event
  ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent1);

    printf("Fetched TEST event %d\n",eventNumber);
// See what tables are in the topNode
//    topNode->print();

// skip events
//      if(eventNumber>(firstEvent-1)) {

    mTecDecode->event(topNode,TecAddress);
cout << "TEST EVENT:" << endl;
    dTecFemData->Show();

//    printf("Calling mTecRemap...\n");
    mTecRemap->event(topNode, TecAddress);

     mTecCalib->event(topNode, TecAddress, TecCalibration);
cout << "TEST EVENT:" << endl;
     dTecCalib->Show();

// Run tracking for the first data file
//     mTecHoughTrack->set_Statistics(0,400000);
     mTecHoughTrack->event(topNode);
cout << "TEST EVENT:" << endl;
//     mTecHoughTrack->set_Verbose(0,0);
     dTecTrack->Show();
//       mTecHoughTrack->Draw(topNode,1,0,eventNumber,prdfIFile1);
//       mTecHoughTrack->Draw(topNode,1,1,eventNumber,prdfIFile1);
//       mTecHoughTrack->Draw(topNode,2,0,eventNumber,prdfIFile1);
//       mTecHoughTrack->Draw(topNode,2,1,eventNumber,prdfIFile1);

cout << "--------------------------------------------------------" << endl;

// Copy track table
  for(int i=0; i<dTecTrack->RowCount(); i++) {
    dTecTrack2->SetRowCount(i+1);
    dTecTrack2->set_id(i,i);
    dTecTrack2->set_xyzin(0,i,dTecTrack->get_xyzin(0,i));
    dTecTrack2->set_xyzin(1,i,dTecTrack->get_xyzin(1,i));
    dTecTrack2->set_xyzin(2,i,dTecTrack->get_xyzin(2,i));
    dTecTrack2->set_xyzout(0,i,dTecTrack->get_xyzout(0,i));
    dTecTrack2->set_xyzout(1,i,dTecTrack->get_xyzout(1,i));
    dTecTrack2->set_xyzout(2,i,dTecTrack->get_xyzout(2,i));
    dTecTrack2->set_dxyin(0,i,dTecTrack->get_dxyin(0,i));
    dTecTrack2->set_dxyin(1,i,dTecTrack->get_dxyin(1,i));
    dTecTrack2->set_dxyout(0,i,dTecTrack->get_dxyout(0,i));
    dTecTrack2->set_dxyout(1,i,dTecTrack->get_dxyout(1,i));
    dTecTrack2->set_quality(i,dTecTrack->get_quality(i));
    dTecTrack2->set_nhits(i,dTecTrack->get_nhits(i));
    dTecTrack2->set_ntime(i,dTecTrack->get_ntime(i));
    dTecTrack2->set_pid(i,dTecTrack->get_pid(i));
    dTecTrack2->set_pidqual(i,dTecTrack->get_pidqual(i));
  }
//     dTecTrack2->Show();

    float side0,hitmult0;
    if(dTecTrack2->RowCount()>0 && dTecTrack2->get_quality(0)==1) {
      side0 = (float)dTecTrack2->get_xyzin(2,0);
    }
    else { side0=0.; }
    hitmult0=(float)dTecRaw->RowCount();

    cout << "Resetting tables..." << endl;
    dTecFemData->SetRowCount(0);
//    cout << "Copying dTecRaw to dTecRaw2..." << endl;
    int k,kk;
    for(k=0; k<dTecRaw->RowCount(); k++) {
      dTecRaw2->SetRowCount(k+1);
      dTecRaw2->set_id(k,dTecRaw->get_id(k));
      dTecRaw2->set_arm(k,dTecRaw->get_arm(k));
      dTecRaw2->set_plane(k,dTecRaw->get_plane(k));
      dTecRaw2->set_sector(k,dTecRaw->get_sector(k));
      dTecRaw2->set_side(k,dTecRaw->get_side(k));
      dTecRaw2->set_wire(k,dTecRaw->get_wire(k));
      for(kk=0; kk<80; kk++)
        dTecRaw2->set_amplitude(kk,k,dTecRaw->get_amplitude(kk,k));
    }
    dTecRaw->SetRowCount(0);
    dTecCalib->SetRowCount(0);
    dTecTrack->SetRowCount(0);

} // End reading test event

    cout << "dTecRaw2 table: " << endl;
    dTecRaw2->Show();

    cout << "Readind second prdf file..." << endl;

    thisEvent2 = eventIter2->getNextEvent();
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent2);

//    cout << "Event from second file fetched." << endl;

// Trigger
  Packet* p;
  long trigScaled[8];
  int id = 14001;
  if ( (p = thisEvent2->getPacket(id)) != 0) {
    int board = 0;
    trigScaled[board] = p->iValue(board, "SCALEDTRIG");
    delete p;   // event->getPacket creates a packet object on the
                // heap, so delete it here!
  }  // p

//BBC
      printf("Calling BbcGetDCM\n");
      BbcGetDCM(topNode);

      printf("Calling mBbcUnpack\n");
      mBbcUnpack->event(topNode);

      printf("Calling mBbcRawOut\n");
      mBbcRawOut->event(topNode);

      // Additions for BBC
      bbcevent->Clear();
      bbcevent->DcmToRaw(topNode);
      bbcevent->calculate();

//        dBbcOut->Show();
      float bbcns = (float)dBbcOut->get_NhitPmtSouth(0);
      float bbcnn = (float)dBbcOut->get_NhitPmtNorth(0);
      float bbcqs = dBbcOut->get_ChargeSumSouth(0);
      float bbcqn = dBbcOut->get_ChargeSumNorth(0);
      float bbcvtx = dBbcOut->get_VertexPoint(0);
      cout << "TRIGGER: " << trigScaled[0] << " " << (trigScaled[0] & 0x5002) << endl;
      cout << "# BBC North: " << bbcnn << endl;
      cout << "# BBC South: " << bbcns << endl;
      cout << "Q BBC North: " << bbcqn << endl;
      cout << "Q BBC South: " << bbcqs << endl;
      cout << "BBC Vertex: " << bbcvtx << endl;

// Analyze this event only if it is good and central
   if(fabs(bbcvtx)<30. && (trigScaled[0] & 0x5002)!=0) {
// Analyse only central events
//   if((bbcqn+bbcqs)>4000.) {

//    cout << "Calling second mTecDecode..." << endl;
    mTecDecode->event(topNode,TecAddress2);
//    dTecFemData->Show();

//    printf("Calling second mTecRemap...\n");
     mTecRemap->event(topNode, TecAddress2);
    cout << "dTecRaw tables after reading from both files: " << endl;
    dTecRaw->Show();
    dTecRaw2->Show();

// Do mixing

//    printf("Calling mTecCalib...\n");
     mTecCalib->event(topNode, TecAddress, TecCalibration2, TecCalibration);
//  dTecCalib->Show();

     int hitmult2s=0;
     int hitmult4s=0;
     int hitmult8s=0;
     int hitmult2n=0;
     int hitmult4n=0;
     int hitmult8n=0;
     for(int i=0; i<dTecCalib->RowCount(); i++) {
       int sector = dTecCalib->get_sector(i);
       int side = dTecCalib->get_side(i);
       for(int j=0; j<80; j++) {
         if(dTecCalib->get_amplitude(j,i)>2. && sector==1 && side==0) hitmult2s++;
         if(dTecCalib->get_amplitude(j,i)>4. && sector==1 && side==0) hitmult4s++;
         if(dTecCalib->get_amplitude(j,i)>8. && sector==1 && side==0) hitmult8s++;
         if(dTecCalib->get_amplitude(j,i)>2. && sector==1 && side==1) hitmult2n++;
         if(dTecCalib->get_amplitude(j,i)>4. && sector==1 && side==1) hitmult4n++;
         if(dTecCalib->get_amplitude(j,i)>8. && sector==1 && side==1) hitmult8n++;
       }
     }

//    printf("Calling mTecHoughTrack for merged event...\n");
     mTecHoughTrack->event(topNode);

     dTecTrack->Show();
     dTecTrack2->Show();

     int ntrksect1s=0;
     int ntrksect1n=0;
     for(int i=0; i<dTecTrack->RowCount(); i++) {
       if(dTecTrack->get_quality(i)==1 && dTecTrack->get_xyzin(2,i)<0.) ntrksect1s++;
       if(dTecTrack->get_quality(i)==1 && dTecTrack->get_xyzin(2,i)>0.) ntrksect1n++;
     }

// Look for the test track in the mixed event
float xin,xout,yin,yout,xin2,yin2,xout2,yout2,dxin,dxout,dyin,dyout,dlim;
dlim=1.0;

// something was reconstructed
if(dTecTrack2->RowCount()>0) {
  dxin=999.; dyin=999.; dxout=999.; dyout=999.;
  for(int i=0; i<dTecTrack->RowCount(); i++) {
    float xin = dTecTrack->get_xyzin(0,i);
    float yin = dTecTrack->get_xyzin(1,i);
    float xout = dTecTrack->get_xyzout(0,i);
    float yout = dTecTrack->get_xyzout(1,i);
    float xin2 = dTecTrack2->get_xyzin(0,0);
    float yin2 = dTecTrack2->get_xyzin(1,0);
    float xout2 = dTecTrack2->get_xyzout(0,0);
    float yout2 = dTecTrack2->get_xyzout(1,0);
      if(fabs(xin-xin2)<dlim && fabs(yin-yin2)<dlim &&
         fabs(xout-xout2)<dlim && fabs(yout-yout2)<dlim) {
           dxin=xin-xin2; dxout=xout-xout2;
           dyin=yin-yin2; dyout=yout-yout2;
      }
//      cout << i << " " << xin-xin2 << " " << yin-yin2 << " " << xout-xout2
//      << " " << yout-yout2 << endl;
  }
    ntpart[0]=(float)eventNumber;
    ntpart[1]=(float)dTecTrack->RowCount();
    ntpart[2]=dxin;
    ntpart[3]=dxout;
    ntpart[4]=dyin;
    ntpart[5]=dyout;
    ntpart[6]=(float)dTecTrack2->get_ntime(0);
    ntpart[7]=bbcns+bbcnn;
    ntpart[8]=bbcqs+bbcqn;
    ntpart[9]=(float)hitmult2s;
    ntpart[10]=(float)hitmult4s;
    ntpart[11]=(float)hitmult8s;
    ntpart[12]=(float)hitmult2n;
    ntpart[13]=(float)hitmult4n;
    ntpart[14]=(float)hitmult8n;
    ntpart[15]=ntrksect1s;
    ntpart[16]=ntrksect1n;
    ntpart[17]=side0;
    ntpart[18]=hitmult0;
    ntp0->Fill(ntpart);
}

//       mTecHoughTrack->Draw(topNode,-1,0,eventNumber,prdfIFile1);
//       mTecHoughTrack->Draw(topNode,1,0,eventNumber,prdfIFile1);
//       mTecHoughTrack->Draw(topNode,1,1,eventNumber,prdfIFile1);
//       mTecHoughTrack->Draw(topNode,2,0,eventNumber,prdfIFile1);
//       mTecHoughTrack->Draw(topNode,2,1,eventNumber,prdfIFile1);
//       mTecHoughTrack->Draw(topNode,2,0,5);

cout << "******* finished with event # " << eventNumber << endl;

//    } // central events only 
    } // good event

//        } // skip some events

// Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DCM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TEC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
  }
  delete eventIter1;
  delete eventIter2;
  delete TecAddress;
  delete TecGeometry;
  delete TecCalibration;
  delete TecCalibration2;

  top->Write();
  top->Close();
  delete top;

}


