//************************************************************
// Analysis macro for doing TEC tracking efficiency evaluation 
//                 starting with pisa file.         
//************************************************************

void tecevalrun(Int_t maxEvents=1, Int_t PRDFverbose=10, Int_t kFiles=2,
     const char *fname="/phenix/data02/rhphemds/pisatest/vrdc_cent/hji_100auaucentsq08_121199z00.root",
//     const char* fname="/phenix/data02/rhphemds/pisatest/retract/hji70_200auaucentsq01_050300z00Retract.root",
//     const char *fname="/phenix/data02/rhphemds/pisatest/vrdc_cent/rvjpsiee_200sq01_011300z00.root",
//     const char *gname="/phenix/data02/rhphemds/pisatest/vrdc_cent/rvjpsiee_200sq01_011300z00.root",
//     const char *gname="/phenix/data04/lebedev/pisa99/bin/PISAEvent10e.root",
     const char *gname="/phenix/data04/lebedev/afspisa/pisa_100e_2gev.root",
//     const char *gname="/phenix/data04/lebedev/afspisa/retract/pisa_100e_2gev_retract.root",
     const char *hname="/phenix/data02/rhphemds/pisatest/vrdc_cent/rvphikk_200sq01_011200z00.root")
{

// Executing initialization and parameter macros
  gROOT->Macro("tecevalini.C");
//  gROOT->Macro("tecevalpar.C");

  //
  // TEC setup module calls
  //
  cout << "calling mTecAlign..." << endl;
//  TecAddressObject* TAO = new TecAddressObject();
//  TAO->Fetch();
//  TecGeometryObject* TGO = new TecGeometryObject();
//  TGO->setCalibName("geom.tec.geant02");
//  TGO->Fetch();
//  mTecAlign->set_UseObjy(0,1);
//  mTecAlign->event(topNode, TAO, TGO);
  mTecAlign->set_Retracted(0,0);
  mTecAlign->event(topNode);

  Int_t kevent = 0;  // counts number of full events processed
  //
  // Read and process PISA events
  //
  while (kevent < maxEvents) {

    //
    // NOTE: kevent is incremented by the GetOneEvent method
    //
    pisarun->GetOneEvent(pisaEventArray, &kevent, TTArray, kFiles);  // get GEANT hits
    mainIter.cd();

    KinGetGEA(topNode);  // fill the fkin table
    TecGetGEA(topNode);  // fill the tecghit table

    if(kevent <= PRDFverbose){
      fkin->Show();
      tecghit->Show();
      cout << "\n";
    }

cout << " calling mTecSlowSim..." << endl;
    mTecSlowSim->event(topNode);  // do the Tec response and produce FEM
    if(kevent <= PRDFverbose){
      dTecGhitRaw->Show();
      dTecFemData->Show();
      cout << "\n";
    }

cout << " calling mTecRemap..." << endl;
    mTecRemap->event(topNode);
    if(kevent <= PRDFverbose){
      dTecRaw->Show();
      cout << "\n";
    }

cout << " calling mTecCalib..." << endl;
    mTecCalib->event(topNode);
    if(kevent <= PRDFverbose){
      dTecCalib->Show();
      cout << "\n";
    }

cout << " calling mTecTrack..." << endl;
    mTecHoughTrack->set_clPar(0,0,0.18);
    mTecHoughTrack->set_clPar(1,0,0.25);
    mTecHoughTrack->set_clPar(2,0,0.25);
    mTecHoughTrack->set_clPar(3,0,0.50);
    mTecHoughTrack->set_clPar(4,0,5.00);
//    mTecHoughTrack->set_clPar(0,0,0.15);
//    mTecHoughTrack->set_clPar(1,0,0.20);
//    mTecHoughTrack->set_clPar(2,0,0.20);
//    mTecHoughTrack->set_clPar(3,0,0.40);
//    mTecHoughTrack->set_clPar(4,0,3.00);
    mTecHoughTrack->set_Verbose(0,1);
//    mTecHoughTrack->set_Write2File(0,1);
    mTecHoughTrack->set_INDEX(0,11);
    mTecHoughTrack->event(topNode);
    if(kevent <= PRDFverbose){
      dTecTrack->Show();
      dTecRawTrack->Show();
      cout << "\n";
    }

//cout << " calling mTecCalibrateModule..." << endl;
//    mTecCalibrate->event(topNode);

cout << " calling mTecTrackEval..." << endl;
    mTecTrackEval->set_Verbose(0,2);
//    mTecTrackEval->set_Write2File(0,1);
    mTecTrackEval->event(topNode);

   // Reset all data for this event
   mainIter.cd();
   if(mainIter.cd("DST")) {
     // cout << "\n In DST" << endl;
     mainIter.forEach(reset);
     mainIter.cd();
    }
   if(mainIter.cd("TEC")) {
     // cout << "\n In TEC" << endl;
     mainIter.forEach(reset);
     mainIter.cd();
    }
   if(mainIter.cd("GEA")) {
     // cout << "\n In GEA " << endl;
     mainIter.forEach(reset);
     mainIter.cd();
   }
   if(mainIter.cd("EVA")) {
     // cout << "\n In EVA " << endl;
     mainIter.forEach(reset);
     mainIter.cd();
   }
   if(mainIter.cd("DCM")) {
     // cout << "\n In DCM " << endl;
     mainIter.forEach(reset);
     mainIter.cd();
   }

   //
   // Clean up memory use at end of full event
   //
   pisarun->HitsClear();  // releases memory assigned to XxxPISAHit globals

  }  // loop over full events

  delete pisarun;    // remove instance created with new
//  delete TAO;
//  delete TGO;

  for(Int_t iFile=0; iFile<kFiles; iFile++) {
    delete pisaEventArray[iFile];
    pisaFileArray[iFile]->Close();  // close the PISA hits file
  } // loop over input files

}

