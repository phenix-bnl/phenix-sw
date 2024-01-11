
void tecrecrun(Int_t firstEvent=1,Int_t lastEvent=5, const char *prdfIFile="/phenix/data11/evt_data/EVENTDATAxxx_P01-0000009978-0000.PRDFF", const char *parIFile="rawpar.root", const char *relIFile="rawrel.root") {

//void tecrecrun(Int_t firstEvent=1,Int_t lastEvent=500, const char *prdfIFile="/phenix/data09/lebedev/example1/datahij10000_500evts.prdf", const char *parIFile="rawpar.root", const char *relIFile="/phenix/data09/lebedev/example1/rawrelhij10000_500evts.root") {

  Int_t eventNumber = 0;

//  TFile *top = new TFile("tmp10000.root","recreate");
//  float ntpart[4];
//  TNtuple *ntp0 = new TNtuple("ntp0","tec","evt:tecmult:nh:phi");

// Executing initialization and parameter macros
  gROOT->Macro("tecrecini.C");

// Open the output DST file
  PHNodeIOManager *ioDST = new PHNodeIOManager("DST.root", PHWrite);

  int runNumber;
  char runno[6]={'0','0','0','0','0','\0'};
  char longrunno[11]={'0','0','0','0','0','-','0','0','0','0','\0'};
  int a1,a2,a3,a4,a5;
  if(isSimulation==0) {
    for(int i=0; i<10; i++) longrunno[i] = prdfIFile[46+i];
    runno[0]=prdfIFile[46]; a1=(int)runno[0] - 48;
    runno[1]=prdfIFile[47]; a2=(int)runno[1] - 48;
    runno[2]=prdfIFile[48]; a3=(int)runno[2] - 48;
    runno[3]=prdfIFile[49]; a4=(int)runno[3] - 48;
    runno[4]=prdfIFile[50]; a5=(int)runno[4] - 48;
    runNumber = a1*10000+a2*1000+a3*100+a4*10+a5;
  }
  else {
    runNumber = 0;
  }

  mainIter.cd();

  if (verbose>5) printf("Entering event loop.\n");
  while ((thisEvent = eventIter->getNextEvent()) && eventNumber < lastEvent) {

  eventNumber++;

// Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

// First event initializations
    if(eventNumber==1) {

//TecCalibration->setDebugLevel(2);

// Set search time
      PHTimeStamp Tsearch(2000,8,10,0,0,0);
      PHRunHeaderRoot* PHRunHeaderRoot = new PHRunHeaderRoot();
      PHRunHeaderRoot->set_run(runNumber);
      PHRunHeaderRoot->lookup_timeStamp("runtotime.txt");
      PHTimeStamp TimeStamp = PHRunHeaderRoot->get_timeStamp();

      if(isSimulation==1) {
        TecAddress->setTimeStamp(Tsearch);
        TecGeometry->setTimeStamp(Tsearch);
        TecCalibration->setTimeStamp(Tsearch);
        cout << " Analyzing simulated run." << endl;
      }
      else {
        TecAddress->setTimeStamp(TimeStamp);
        TecGeometry->setTimeStamp(TimeStamp);
        TecCalibration->setTimeStamp(TimeStamp);
        cout << " Analyzing run # " << runNumber;
        cout << " with TimeStamp:  " << TimeStamp << "\n";
      }

// Set calibration bank names
      if(isSimulation==1) {
        TecAddress->UseSimulationDatabase();
        TecGeometry->UseSimulationDatabase();
        TecCalibration->UseSimulationDatabase();
      }

// Rotate if real data
        float rotangle1 = 0.0023;
        float rotangle2 = -0.0050;
        float rotcos1 = cos(rotangle1);
        float rotsin1 = sin(rotangle1);
        float rotcos2 = cos(rotangle2);
        float rotsin2 = sin(rotangle2);
        PHVector xaxisrot1 = PHVector(rotcos1,-rotsin1,0.);
        PHVector yaxisrot1 = PHVector(rotsin1,rotcos1,0.);
        PHVector zaxisrot1 = PHVector(0.,0.,1.);
        PHVector xaxisrot2 = PHVector(rotcos2,-rotsin2,0.);
        PHVector yaxisrot2 = PHVector(rotsin2,rotcos2,0.);
        PHVector zaxisrot2 = PHVector(0.,0.,1.);
        PHPoint center = PHPoint(0.,0.,0.);
        PHFrame frame1 = PHFrame(center,xaxisrot1,yaxisrot1,zaxisrot1);
        PHFrame frame2 = PHFrame(center,xaxisrot2,yaxisrot2,zaxisrot2);
        if(isSimulation==0) {
          TecGeometry->setTecSectorFrame(1, frame1);
          TecGeometry->setTecSectorFrame(2, frame2);
        }

      if(UseObjy==1) {
// Fetch info from the database
        TecAddress->Fetch();
        TecGeometry->Fetch();
// Shift geometry if necessary
//        TecGeometry->Shift(-44.0,0.0,0.0);
        TecCalibration->Fetch();
//        TecCalibration->FetchRelGain();
//        TecCalibration->FetchAbsGain();
//        TecCalibration->FetchTimingGain();
      }
      else {
        if(isSimulation==1) {
          TecAddress->FetchFromFile("tecmap_database_east0.txt");
          TecGeometry->FetchFromFile(); 	// standard Geant geometry
          TecCalibration->FetchFromFile(); 	// set all gains to 1
        }
        else {
          TecAddress->FetchFromFile("tecmap_database_run00.txt");
// Fetch retracted arms geometry
          TecGeometry->FetchFromFile("tecgeom_database_run00_00.dat");
// Make non-retracted geometry
          TecGeometry->Shift(44.0,0.0,0.0);
          TecCalibration->FetchAbsGainFromFile("tecabsgain_database.txt");
          TecCalibration->FetchRelGainFromFile("tecrelgain_database_0.txt");
          TecCalibration->FetchTimingGainFromFile("tectimecalib_database.txt");
        }
      }

        if (verbose>10) printf("Calling mTecAlign\n");
        mTecAlign->event(topNode, TecAddress, TecGeometry);
        if (verbose>10) dTecWire->Show();

    } // first event initialization


    if (verbose>10) printf("Calling mTecDecode\n");
     mTecDecode->event(topNode,TecAddress);
      if (verbose>10) dTecFemData->Show();

    if (verbose>10) printf("Calling mTecRemap\n");
     mTecRemap->event(topNode, TecAddress);
      if (verbose>10) dTecRaw->Show();

    if (verbose>10) printf("Calling mTecCalib\n");
     mTecCalib->event(topNode, TecAddress, TecCalibration);
      if (verbose>10) dTecCalib->Show();

    if (verbose>10) printf("Calling mTecHoughTrack\n");
     mTecHoughTrack->event(topNode);
      if (verbose>10) dTecTrack->Show();

    if (verbose>10) printf("Calling mTecPID\n");
     mTecPID->event(topNode);
      if (verbose>10) dTecPID->Show();

// copy old track table to a new one.
     dTecTrack->copy2dTecTrackTable(dTecTrackT);
// create an array of new tracks
     TecTrack* track = dTecTrackT->TableData();
     for(int i=0; i<dTecTrackT->RowCount(); i++) {
       float tecPhi = track[i].getPhi();
       float nhits = (float)track[i].getNhits();
//        ntpart[0]=(float)eventNumber;
//        ntpart[1]=(float)dTecTrack->RowCount();
//        ntpart[2]=nhits;
//        ntpart[3]=tecPhi;
//        ntp0->Fill(ntpart);

     }

// Display events
//  if(eventNumber>=firstEvent) {
//    mTecHoughTrack->Draw(topNode,1,0,eventNumber,prdfIFile);
//    mTecHoughTrack->Draw(topNode,1,1,eventNumber,prdfIFile);
//    mTecHoughTrack->Draw(topNode,2,0,eventNumber,prdfIFile);
//    mTecHoughTrack->Draw(topNode,2,1,eventNumber,prdfIFile);
//  } 

// create ntuple for gain determination
//      mTecGains->set_RunNumber(0,runNumber);
//	mTecGains->event(topNode);

// write out the DST
    if (verbose>10) cout << "Writing out DSTs..." << endl;
    ioDST->write(dstNode);

// Reset all data for this event
    if (verbose>10) cout << "Resetting tables..." << endl;
    mainIter.cd();
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("DCM")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("GEA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("EVA")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }
    if (mainIter.cd("TEC")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }

  } // end loop over events

//  top->Write();
//  top->Close();

  // Take out the garbage
  delete ioDST;
  delete TecAddress;
  delete TecGeometry;
  delete TecCalibration;

}

