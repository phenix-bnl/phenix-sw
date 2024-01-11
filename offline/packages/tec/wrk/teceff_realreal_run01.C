
void teceff_realreal_run01(Int_t firstEvent=1, Int_t lastEvent=6000, const char *prdfIFile2="/phenix/data14/eventdata_2001/EVENTDATAxxx_P01-0000027197-0003.PRDFF", char *prdfIFile1="onetrackevents_27197-0002.prdfz") {

  int eventNumber = 0;
  int runNumber = 0;
  int runNumberTest = 0;
  int firstevent = 0;
  int firsttestevent = 0;

// Load libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libphgeo.so");
  gSystem->Load("/phenix/workarea/lebedev/offline/install/lib/libtec.so");
//  gSystem->Load("libtec.so");
  gSystem->Load("libbbc.so");

cout << "libraries loaded..." << endl;

// Book the ntuple
TFile *top = new TFile("trkeff27197-0003-0002.root","recreate");
float ntpart[38];
TNtuple *ntp0 = new TNtuple("ntp0","tracking efficiency","evt:mult:hitmult:hitmulthigh:bbcn:bbcq:dxin:dxout:dyin:dyout:nhits:sector:side:mult0:mult1:mult2:mult3:mult4:mult5:mult6:mult7:hitmult0:hitmult1:hitmult2:hitmult3:hitmult4:hitmult5:hitmult6:hitmult7:hitmulthigh0:hitmulthigh1:hitmulthigh2:hitmulthigh3:hitmulthigh4:hitmulthigh5:hitmulthigh6:hitmulthigh7:dlim");

  int mult[8],hitmult[8],hitmulthigh[8];

// Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);
  PHCompositeNode* bbcNode = new PHCompositeNode("BBC");
  topNode->addNode(bbcNode);
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
  mTecDecodeModule* mTecDecode = new mTecDecodeModule;
  mTecCalibModule* mTecCalib = new mTecCalibModule;
  mTecHoughTrackModule* mTecHoughTrack = new mTecHoughTrackModule;

//BBC
  mBbcUnpackModule* mBbcUnpack = new mBbcUnpackModule;
  mBbcRawOutModule* mBbcRawOut = new mBbcRawOutModule;

  // Initialize the tables

//BBC

  Int_t mr=1;
  dBbcRawHitParWrapper* dBbcRawHitPar = new dBbcRawHitParWrapper("dBbcRawHitPar",mr);
  PHIODataNode<PHTable>* dBbcRawHitParNode = new PHIODataNode<PHTable>(dBbcRawHitPar,"dBbcRawHitPar");
  parNode->addNode(dBbcRawHitParNode);

  // Setting dBbcRawHitPar Parameters
  int nrc=1;
  dBbcRawHitPar->SetRowCount(nrc);
  dBbcRawHitPar->set_MinAdc(0,0);
  dBbcRawHitPar->set_MaxAdc(0,4096);
  dBbcRawHitPar->set_MinTdc0(0,10);
  dBbcRawHitPar->set_MaxTdc0(0,4000);
  dBbcRawHitPar->set_MinTdc1(0,10);
  dBbcRawHitPar->set_MaxTdc1(0,4000);

  Int_t mr=128;
  dBbcRawWrapper* dBbcRaw = new dBbcRawWrapper("dBbcRaw",mr);
  PHIODataNode<PHTable>* dBbcRawNode = new PHIODataNode<PHTable>(dBbcRaw,"dBbcRaw");
  dstNode->addNode(dBbcRawNode);

  BbcRaw *bbcraw = new BbcRawv1();
  PHIODataNode<PHObject>* BbcRawNode = new 
    PHIODataNode<PHObject>((PHObject*)bbcraw,"BbcRaw","PHObject");
  dstNode->addNode(BbcRawNode);

  BbcCalib* BbcCalibPar = new BbcCalib();
  PHIODataNode<TObject>* BbcCalibParNode = new PHIODataNode<TObject>(BbcCalibPar,"BbcCalibPar");
  parNode->addNode(BbcCalibParNode);

  Int_t mr=1;
  dBbcOutWrapper* dBbcOut = new dBbcOutWrapper("dBbcOut",mr);
  PHIODataNode<PHTable>* dBbcOutNode = new PHIODataNode<PHTable>(dBbcOut,"dBbcOut");
  dstNode->addNode(dBbcOutNode);

  BbcOut* bbcout = new BbcOutv1();
  PHIODataNode<PHObject>* BbcOutNode = new 
    PHIODataNode<PHObject>((PHObject*)bbcout,"BbcOut","PHObject");
  dstNode->addNode(BbcOutNode);

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

  TecOutV1* tecout = new TecOutV1();
  PHIODataNode<PHObject>* TecOutNode = new
    PHIODataNode<PHObject>((PHObject*)tecout,"TecOutV1","PHObject");
  tecNode->addNode(TecOutNode);
  TecOutV1* tecouttest = new TecOutV1();
  PHIODataNode<PHObject>* TecOutNodeTest = new
     PHIODataNode<PHObject>((PHObject*)tecouttest,"TecOutV1TEST","PHObject");
  parNode->addNode(TecOutNodeTest);

  Int_t mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

  Int_t mr=800;
  dTecTrackWrapper* dTecTrack = new dTecTrackWrapper("dTecTrack",mr);
  PHIODataNode<PHTable>* dTecTrackNode = new PHIODataNode<PHTable>(dTecTrack,"dTecTrack");
  tecNode->addNode(dTecTrackNode);

  Int_t mr=150000;
  dTecRawTrackWrapper* dTecRawTrack = new dTecRawTrackWrapper("dTecRawTrack",mr);
  PHIODataNode<PHTable>* dTecRawTrackNode = new PHIODataNode<PHTable>(dTecRawTrack,"dTecRawTrack");
  tecNode->addNode(dTecRawTrackNode);

//------------------------------------------------------------------------

  mainIter.cd();

// Create TAO, TGO and TCO
  TecAddressObject* TecAddress = new TecAddressObject();
  TecGeometryObject* TecGeometry = new TecGeometryObject();
  TecCalibrationObject* TecCalibration = new TecCalibrationObject();
  TecCalibrationObject* TecCalibration2 = new TecCalibrationObject();

  PHIODataNode<TObject>* TecDetGeoNode = new 
    PHIODataNode<TObject>(TecGeometry,"TecGeometry");
  parNode->addNode(TecDetGeoNode);

  PHIODataNode<TObject>* TecDetCalNode = new 
    PHIODataNode<TObject>(TecCalibration,"TecCalibration");
  parNode->addNode(TecDetCalNode);

//--------------------------------------------------------------------------------

// Start loop over events
  printf("Entering event loop.\n");

  while(eventNumber++ < lastEvent) { // loop over events in data file

  float side0,sector0;
  
// Read TEST event every 25 events
if((eventNumber-1)%25 == 0) {

  thisEvent1=eventIter1->getNextEvent();
  mainIter.cd();

  tecouttest->Clear();

// Point the data node to the new event
  ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent1);

    printf("Fetched TEST event %d\n",eventNumber);
// See what tables are in the topNode
//    topNode->print();

    if(firsttestevent==0) {
      runNumberTest = thisEvent1->getRunNumber();
        TecAddress->setRunNumber(runNumberTest);
          PHTimeStamp timeStamp = TecAddress->getTimeStamp();
        TecGeometry->setTimeStamp(timeStamp);
        TecCalibration->setTimeStamp(timeStamp);
//        TecCalibration2->setTimeStamp(timeStamp); // same run number
        TecAddress->Fetch();
        TecGeometry->Fetch();
        TecCalibration->Fetch();
//          BbcCalibPar->restore(timeStamp);
      cout << "RUN NUMBER (TEST event) = " << runNumberTest << " " << timeStamp << endl;
      firsttestevent=1;
    }

    mTecDecode->event(topNode,TecAddress);
//    cout << "TEST EVENT: " << tecout->getNTracks() << " " << tecout->getNHits() << endl;

    mTecCalib->event(topNode, TecAddress, TecCalibration);

// Run tracking for the TEST event
//     mTecHoughTrack->set_Verbose(1);
     mTecHoughTrack->event(topNode);
     cout << "TEST EVENT: " << tecout->getNTracks() << " " << tecout->getNHits() << endl;


// Copy tecout to tecouttest

  float xy[2];
  int nok=0;
  float charge=0.;
  float xyz[2]; xyz[0]=0.; xyz[1]=0.;
  float xyzin[3],xyzout[3];
  int NHITS[6],Nwires[6];

  for(int i=0; i<tecout->getNHits(); i++) {
    int index = tecout->getHitIndex(i);
    int wire = tecout->getHitWire(i);
    int bin = tecout->getHitTimeBin(i);
    int adc = tecout->getHitADC(i);
    int trackid = tecout->getHitTrackID(i);
    float charge = tecout->getHitCharge(i);
    xy[0] = tecout->getHitX(i);
    xy[1] = tecout->getHitY(i);
      tecouttest->AddTecHit(index, wire, bin, adc, charge, xy, trackid);
  }

  for(int i=0; i<tecout->getNTracks(); i++) {
    xyzin[0]=tecout->getTrackXin(i);
    xyzin[1]=tecout->getTrackYin(i);
    if(tecout->getTrackSide(i)==0) {xyzin[2]=-1.;} else {xyzin[2]=1.;}
    xyzout[0]=tecout->getTrackXout(i);
    xyzout[1]=tecout->getTrackYout(i);
    if(tecout->getTrackSide(i)==0) {xyzout[2]=-1.;} else {xyzout[2]=1.;}
    int nhits = tecout->getTrackNhits(i);
    NHITS[0] = tecout->getTrackNhits(i,0);
    NHITS[1] = tecout->getTrackNhits(i,1);
    NHITS[2] = tecout->getTrackNhits(i,2);
    NHITS[3] = tecout->getTrackNhits(i,3);
    NHITS[4] = tecout->getTrackNhits(i,4);
    NHITS[5] = tecout->getTrackNhits(i,5);
    int index = tecout->getTrackIndex(i);
    Nwires[0] = tecout->getTrackNwires(i,0);
    Nwires[1] = tecout->getTrackNwires(i,1);
    Nwires[2] = tecout->getTrackNwires(i,2);
    Nwires[3] = tecout->getTrackNwires(i,3);
    Nwires[4] = tecout->getTrackNwires(i,4);
    Nwires[5] = tecout->getTrackNwires(i,5);
      tecouttest->AddTecTrack(xyzin, xyzout);
      tecouttest->setTrackNhits(nok,nhits);
      tecouttest->setTrackIndex(nok,index);
      tecouttest->setTrackNhits(nok,0,NHITS[0]);
      tecouttest->setTrackNhits(nok,1,NHITS[1]);
      tecouttest->setTrackNhits(nok,2,NHITS[2]);
      tecouttest->setTrackNhits(nok,3,NHITS[3]);
      tecouttest->setTrackNhits(nok,4,NHITS[4]);
      tecouttest->setTrackNhits(nok,5,NHITS[5]);
      tecouttest->setTrackNwires(nok,0,Nwires[0]);
      tecouttest->setTrackNwires(nok,1,Nwires[1]);
      tecouttest->setTrackNwires(nok,2,Nwires[2]);
      tecouttest->setTrackNwires(nok,3,Nwires[3]);
      tecouttest->setTrackNwires(nok,4,Nwires[4]);
      tecouttest->setTrackNwires(nok,5,Nwires[5]);
        nok++;
  }
     cout << "First (TEST) TecOut : " << tecout ->getNTracks() << " "
                               << tecout->getNHits() << endl;
     cout << "Second (TEST) TecOut : " << tecouttest ->getNTracks() << " "
                                << tecouttest->getNHits() << endl;


    cout << "Resetting tables after TEST event..." << endl;
    dTecFemData->SetRowCount(0);
    dTecTrack->SetRowCount(0);
    tecout->Clear();

     cout << "First (empty) TecOut : " << tecout ->getNTracks() << " "
                               << tecout->getNHits() << endl;
     cout << "Second (TEST) TecOut : " << tecouttest ->getNTracks() << " "
                                << tecouttest->getNHits() << endl;

     side0 = -1;
     if(tecouttest->getNTracks()==1) side0 = tecouttest->getTrackSide(0);
     sector0 = -1;
     if(tecouttest->getNTracks()==1) sector0 = tecouttest->getTrackSector(0);


} // End reading test event

    cout << "Readind second prdf file..." << endl;

    thisEvent2 = eventIter2->getNextEvent();
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent2);

    cout << "Event from second file fetched." << endl;

    if(firstevent==0) {
      runNumber = thisEvent2->getRunNumber();
        TecCalibration2->setRunNumber(runNumber);
        PHTimeStamp timeStamp = TecCalibration2->getTimeStamp();
        TecCalibration2->Fetch();
        BbcCalibPar->restore(timeStamp);
      cout << "RUN NUMBER = " << runNumber << " " << timeStamp << endl;
      firstevent=1;
    }

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
//      printf("Calling BbcGetDCM\n");
      BbcGetDCM(topNode);

//      printf("Calling mBbcUnpack\n");
      mBbcUnpack->event(topNode);

//      printf("Calling mBbcRawOut\n");
      mBbcRawOut->event(topNode);

//        dBbcOut->Show();
      float bbcns = (float)dBbcOut->get_NhitPmtSouth(0);
      float bbcnn = (float)dBbcOut->get_NhitPmtNorth(0);
      float bbcqs = dBbcOut->get_ChargeSumSouth(0);
      float bbcqn = dBbcOut->get_ChargeSumNorth(0);
      float bbcvtx = dBbcOut->get_VertexPoint(0);
      cout << "TRIGGER: " << trigScaled[0] << " " << (trigScaled[0] & 0x40) << endl;
//      cout << "# BBC North: " << bbcnn << endl;
//      cout << "# BBC South: " << bbcns << endl;
//      cout << "Q BBC North: " << bbcqn << endl;
//      cout << "Q BBC South: " << bbcqs << endl;
      cout << "BBC Vertex: " << bbcvtx << endl;

// Analyze this event only if it is good and central
   if(fabs(bbcvtx)<40. && (trigScaled[0] & 0x40)!=0) {
// Analyse only central events
//   if((bbcqn+bbcqs)>4000.) {

    cout << "Calling second mTecDecode..." << endl;
    mTecDecode->event(topNode,TecAddress);
//    dTecFemData->Show();

     cout << "      First TecOut : " << tecout ->getNTracks() << " "
                               << tecout->getNHits() << endl;
     cout << "      Second (TEST) TecOut : " << tecouttest ->getNTracks() << " "
                                << tecouttest->getNHits() << endl;

// Do mixing

    printf("Mixing...\n");
     mTecCalib->set_Verbose(1);
     mTecCalib->event(topNode, TecAddress, TecCalibration2, TecCalibration);

     cout << "Calling mTecHoughTrack for the MIXED event..." << endl;
     mTecHoughTrack->event(topNode);

     cout << "      First (mixed) TecOut : " << tecout ->getNTracks() << " "
                               << tecout->getNHits() << endl;
     cout << "      Second (TEST) TecOut : " << tecouttest ->getNTracks() << " "
                                << tecouttest->getNHits() << endl;

     for(int i=0; i<8; i++) {mult[i]=0; hitmult[i]=0; hitmulthigh[i]=0;}
     for(int i=0; i<tecout->getNHits(); i++) {
       int sector = tecout->getHitSector(i);
       int side = tecout->getHitSide(i);
         hitmult[sector*2+side]++;
         if(tecout->getHitADC(i)>4.) {
           hitmulthigh[sector*2+side]++;
         }
     }
     for(int i=0; i<tecout->getNTracks(); i++) {
       int sector = tecout->getTrackSector(i);
       int side = tecout->getTrackSide(i);
         mult[sector*2+side]++;
     }



// Look for the test track in the mixed event
float xin,xout,yin,yout,xin2,yin2,xout2,yout2,dxin,dxout,dyin,dyout,dlim;

// merged track was not reconstructed at all
dlim=999.;
if(tecouttest->getNTracks()==0) {
  dxin=999.; dyin=999.; dxout=999.; dyout=999.;
    ntpart[0]=(float)eventNumber;
    ntpart[1]=(float)tecout->getNTracks();
    ntpart[2]=(float)tecout->getNHits();
    ntpart[3]=(float)hitmulthigh;
    ntpart[4]=bbcns+bbcnn;
    ntpart[5]=bbcqs+bbcqn;
    ntpart[6]=dxin;
    ntpart[7]=dxout;
    ntpart[8]=dyin;
    ntpart[9]=dyout;
    ntpart[10]=0.;
    ntpart[11]=(float)sector0;
    ntpart[12]=(float)side0;
    ntpart[13]=(float)mult[0];
    ntpart[14]=(float)mult[1];
    ntpart[15]=(float)mult[2];
    ntpart[16]=(float)mult[3];
    ntpart[17]=(float)mult[4];
    ntpart[18]=(float)mult[5];
    ntpart[19]=(float)mult[6];
    ntpart[20]=(float)mult[7];
    ntpart[21]=(float)hitmult[0];
    ntpart[22]=(float)hitmult[1];
    ntpart[23]=(float)hitmult[2];
    ntpart[24]=(float)hitmult[3];
    ntpart[25]=(float)hitmult[4];
    ntpart[26]=(float)hitmult[5];
    ntpart[27]=(float)hitmult[6];
    ntpart[28]=(float)hitmult[7];
    ntpart[29]=(float)hitmulthigh[0];
    ntpart[30]=(float)hitmulthigh[1];
    ntpart[31]=(float)hitmulthigh[2];
    ntpart[32]=(float)hitmulthigh[3];
    ntpart[33]=(float)hitmulthigh[4];
    ntpart[34]=(float)hitmulthigh[5];
    ntpart[35]=(float)hitmulthigh[6];
    ntpart[36]=(float)hitmulthigh[7];
    ntpart[37]=dlim;
      ntp0->Fill(ntpart);
      cout << "RESULTS: " << dlim << " " << dxin << " " << dyin << endl;
}
// merged track was reconstructed
dlim=999.;
if(tecouttest->getNTracks()==1) {
  dxin=999.; dyin=999.; dxout=999.; dyout=999.;
  for(int i=0; i<tecout->getNTracks(); i++) {
    float xin = tecout->getTrackXin(i);
    float yin = tecout->getTrackYin(i);
    float xout = tecout->getTrackXout(i);
    float yout = tecout->getTrackYout(i);
    float xin2 = tecouttest->getTrackXin(0);
    float yin2 = tecouttest->getTrackYin(0);
    float xout2 = tecouttest->getTrackXout(0);
    float yout2 = tecouttest->getTrackYout(0);
      float dxintmp=xin-xin2; float dyintmp=yin-yin2;
      float dxouttmp=xout-xout2; float dyouttmp=yout-yout2;
      float mydist = sqrt(dxintmp*dxintmp+dyintmp*dyintmp+
                          dxouttmp*dxouttmp+dyouttmp*dyouttmp);
      if(mydist<dlim) {
        dlim = mydist;
        dxin = xin-xin2;
        dyin = yin-yin2;
        dxout = xout-xout2;
        dyout = yout-yout2;
      }
  }
        ntpart[0]=(float)eventNumber;
        ntpart[1]=(float)tecout->getNTracks();
        ntpart[2]=(float)tecout->getNHits();
        ntpart[3]=(float)hitmulthigh;
        ntpart[4]=bbcns+bbcnn;
        ntpart[5]=bbcqs+bbcqn;
        ntpart[6]=dxin;
        ntpart[7]=dxout;
        ntpart[8]=dyin;
        ntpart[9]=dyout;
        ntpart[10]=(float)tecouttest->getTrackNhits(0);
        ntpart[11]=(float)sector0;
        ntpart[12]=(float)side0;
        ntpart[13]=(float)mult[0];
        ntpart[14]=(float)mult[1];
        ntpart[15]=(float)mult[2];
        ntpart[16]=(float)mult[3];
        ntpart[17]=(float)mult[4];
        ntpart[18]=(float)mult[5];
        ntpart[19]=(float)mult[6];
        ntpart[20]=(float)mult[7];
        ntpart[21]=(float)hitmult[0];
        ntpart[22]=(float)hitmult[1];
        ntpart[23]=(float)hitmult[2];
        ntpart[24]=(float)hitmult[3];
        ntpart[25]=(float)hitmult[4];
        ntpart[26]=(float)hitmult[5];
        ntpart[27]=(float)hitmult[6];
        ntpart[28]=(float)hitmult[7];
        ntpart[29]=(float)hitmulthigh[0];
        ntpart[30]=(float)hitmulthigh[1];
        ntpart[31]=(float)hitmulthigh[2];
        ntpart[32]=(float)hitmulthigh[3];
        ntpart[33]=(float)hitmulthigh[4];
        ntpart[34]=(float)hitmulthigh[5];
        ntpart[35]=(float)hitmulthigh[6];
        ntpart[36]=(float)hitmulthigh[7];
        ntpart[37]=dlim;
          ntp0->Fill(ntpart);
          cout << "RESULTS: " << dlim << " " << dxin << " " << dyin << endl;
}

cout << "******* finished with event # " << eventNumber << endl;

//    } // central events only 
    } // good event

// Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DCM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TEC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("BBC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
  }

  top->Write();
  top->Close();

}


