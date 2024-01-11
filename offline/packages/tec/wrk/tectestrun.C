
void tectestrun(Int_t maxEvents=1000, const char *pisaIFile="/phenix/data23/lebedev/pisa2000/PISA_pionminus_10gev_2640evts_3D.root", const char *parOFile="rawpar.root", const char *relOFile="rawrel.root") {

  Int_t eventNumber = 0;
  Int_t runNumber=0;

// Create output ntuple
  TFile *top = new TFile("tmp.root","recreate");
  float ntpart[23];
  TNtuple *ntp0 = new TNtuple("ntp0","tec","xin:yin:xout:yout:gx1:gx2:gx3:gx4:gy1:gy2:gy3:gy4:nh1:nh2:nh3:nh4:phi:alpha:distin:distout:gphi:nh5:nh6");

// Executing initialization and parameter macros
  gROOT->Macro("tectestini.C");

// Read Database
      PHTimeStamp Tsearch(2001,8,20,0,0,0);
      TecAddress->setTimeStamp(Tsearch);
      TecGeometry->setTimeStamp(Tsearch);
      TecCalibration->setTimeStamp(Tsearch);
      TecAddress->UseSimulationDatabase();
      TecGeometry->UseSimulationDatabase();
      TecCalibration->UseSimulationDatabase();

      TecGeometry->setDebugLevel(5);

      if(UseObjy==1) {
// Fetch info from the database
        TecAddress->Fetch();
        TecGeometry->Fetch();
        TecCalibration->Fetch();
      }
      else {
        TecAddress->FetchFromFile("tecmap_database_east0.txt");
        TecGeometry->FetchFromFile();
        TecCalibration->FetchFromFile(); // set all gains to 1
      }

  mainIter.cd();

float xx[4],yy[4],exx[4],eyy[4];
for(int i=0; i<4; i++) {exx[i]=0.1; eyy[i]=0.1;}
float Slope,Intercept,a,b,x0,y0,dist1,dist2;

//-------------------------------------------------------------------------------------

  if (verbose>5) printf("Entering event loop.\n");
  while (kevent < maxEvents) {

// Fetch a PISA99 event
    eventNumber = kevent + 1;
    pisarun->GetOneEvent(pisaevent,&kevent,T);

    mainIter.cd();

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

    KinGetGEA(topNode);
    TecGetGEA(topNode);
//	tecghit->Show();
//        fkin->Show();

//    if (verbose>10) printf("Calling mTecSlowSim\n");
// SET GAINS 
     mTecSlowSim->set_GasGain(10000.);

     mTecSlowSim->event(topNode, TecAddress);
//      if (verbose>10) dTecFemData->Show();

//    if (verbose>10) printf("Calling mTecCalib\n");
     mTecCalib->event(topNode, TecAddress, TecCalibration);
//      if (verbose>10) dTecCalib->Show();

//    if (verbose>10) printf("Calling mTecHoughTrack\n");
     mTecHoughTrack->event(topNode);
      if (verbose>10) {
        cout << "TecOutV1: " << tecout->getNTracks() << " " << tecout->getNHits() << endl;
      } 

      if(tecout->getNTracks()==1 && tecghit->RowCount()==6 && fkin->RowCount()==1) {

	float gphi=fkin->get_pphi(0);
        ntpart[0]=tecout->getTrackXin(0);
        ntpart[1]=tecout->getTrackYin(0);
        ntpart[2]=tecout->getTrackXout(0);
        ntpart[3]=tecout->getTrackYout(0);
        ntpart[4]=tecghit->get_xyzinglo(0,0);
        ntpart[5]=tecghit->get_xyzinglo(0,1);
        ntpart[6]=tecghit->get_xyzinglo(0,2);
        ntpart[7]=tecghit->get_xyzinglo(0,3);
        ntpart[8]=tecghit->get_xyzinglo(1,0);
        ntpart[9]=tecghit->get_xyzinglo(1,1);
        ntpart[10]=tecghit->get_xyzinglo(1,2);
        ntpart[11]=tecghit->get_xyzinglo(1,3);
	ntpart[12]=tecout->getTrackNhits(0,0);
	ntpart[13]=tecout->getTrackNhits(0,1);
	ntpart[14]=tecout->getTrackNhits(0,2);
	ntpart[15]=tecout->getTrackNhits(0,3);
	ntpart[16]=tecout->getTrackPhi(0);
	ntpart[17]=tecout->getTrackAlpha(0);
	ntpart[18]=dist1;
	ntpart[19]=dist2;
	ntpart[20]=gphi;
	ntpart[21]=tecout->getTrackNhits(0,4);
	ntpart[22]=tecout->getTrackNhits(0,5);
        ntp0->Fill(ntpart);

        if(tecout->getTrackSector(0)==0 || tecout->getTrackSector(0)==2)
		cout << "####################################okok: " << endl;
	cout 
	<< ntpart[12] << " "
	<< ntpart[13] << " "
	<< ntpart[14] << " "
	<< ntpart[15] << " "
	<< ntpart[21] << " "
	<< ntpart[22] << " "
	<< tecout->getTrackSector(0) << " "
	<< tecout->getTrackSide(0) << " "
	<< endl;


      }
      else {
        float gphi=fkin->get_pphi(0);
        ntpart[0]=-999.;
        ntpart[1]=-999.;
        ntpart[2]=-999.;
        ntpart[3]=-999.;
        ntpart[4]=-999.;
        ntpart[5]=-999.;
        ntpart[6]=-999.;
        ntpart[7]=-999.;
        ntpart[8]=-999.;
        ntpart[9]=-999.;
        ntpart[10]=-999.;
        ntpart[11]=-999.;
        ntpart[12]=-999.;
        ntpart[13]=-999.;
        ntpart[14]=-999.;
        ntpart[15]=-999.;
        ntpart[16]=-999.;
        ntpart[17]=-999.;
        ntpart[18]=-999.;
        ntpart[19]=-999.;
        ntpart[20]=gphi;
        ntp0->Fill(ntpart);
      }

// Reset all data for this event
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

    pisarun->HitsClear();

  }

  top->Write();
  top->Close();

  pisaFile->Close();

}

