
void tecmixrun(Int_t maxEvents=1, const char *prdfIFile1="test1.prdf", const char *prdfIFile2="test2.prdf") {

  Int_t eventNumber = 0;

// Executing initialization macro
  gROOT->Macro("tecmixini.C");

  mainIter.cd();

  if (verbose>5) printf("Entering event loop.\n");

  for(int ii=0; ii<maxEvents; ii++) {

  thisEvent1 = eventIter1->getNextEvent();
//  cout << "thisEvent1 = " << thisEvent1 << endl;
  if(thisEvent1) {
  eventNumber++;

// Point the data node to the new event
//  mainIter.cd("thisEvent1");
  mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent1);

    if (verbose>5) printf("Fetched event %d\n",eventNumber);

    if (verbose>10) printf("Calling first event only modules.\n");
    if (eventNumber == 1) {
      if (verbose>10) printf("Calling mTecAlign\n");
      mTecAlign->event(topNode);
    }
 
    if (verbose>10) printf("Calling event modules\n");

    if (verbose>10) printf("Calling TecGetDCM\n");
    TecGetDCM(topNode);
//    if (verbose>10) dTecDcmData->Show();

    if (verbose>10) printf("Calling mTecUnpack\n");
     mTecUnpack->event(topNode);
//    if (verbose>10) dTecFemData->Show();

    if (verbose>10) printf("Calling mTecRemap\n");
     mTecRemap->event(topNode);
    if (verbose>10) dTecRaw->Show();

    if (verbose>10) cout << "Resetting tables..." << endl;
    dTecDcmData->SetRowCount(0);
    dTecFemData->SetRowCount(0);
    if (verbose>10) cout << "Copying dTecRaw to dTecRaw2..." << endl;
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
    if (verbose>10) cout << "dTecRaw tables after reset and copy: " << endl;
    if (verbose>10) dTecRaw->Show();
    if (verbose>10) dTecRaw2->Show();

    if (verbose>10) cout << "Readind second prdf file..." << endl;

    thisEvent2 = eventIter2->getNextEvent();
    cout << "thisEvent2 = " << thisEvent2 << endl;
//    mainIter.cd("thisEvent2");
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent2);

    if (verbose>10) cout << "Event from second file fetched." << endl;

    if (verbose>10) printf("Calling TecGetDCM on second file\n");
    TecGetDCM(topNode);
//    if (verbose>10) dTecDcmData->Show();

    if (verbose>10) printf("Calling mTecUnpack on second file\n");
     mTecUnpack->event(topNode);
//    if (verbose>10) dTecFemData->Show();

    if (verbose>10) printf("Calling mTecRemap on second file\n");
     mTecRemap->event(topNode);
    if (verbose>10) cout << "dTecRaw tables after reading from both files: " << endl;
    if (verbose>10) dTecRaw->Show();
    if (verbose>10) dTecRaw2->Show();

    if (verbose>10) cout << "Get first calibration constants set..." << endl;
    TecCalibrationObject* TCO1 = new TecCalibrationObject();
    PHTimeStamp TS1 = PHTimeStamp(1999,3,10,0,0,0);
    TCO1->setTimeStamp(TS1);
    TCO1->FetchFromFile();

    if (verbose>10) cout << "Get second calibration constants set..." << endl;
    TecCalibrationObject* TCO2 = new TecCalibrationObject();
    PHTimeStamp TS2 = PHTimeStamp(2000,2,20,0,0,0);
    TCO2->setTimeStamp(TS2);
    TCO2->FetchFromFile();

// Do mixing
    if (verbose>10) printf("Calling mTecCalib\n");
     mTecCalib->set_Verbose(0,1);
     mTecCalib->event(topNode, TCO1, TCO2);
//     mTecCalib->event(topNode);
    if (verbose>10) dTecCalib->Show();

    if (verbose>10) printf("Calling mTecHoughTrack\n");
     mTecHoughTrack->set_INDEX(0,11);
     mTecHoughTrack->set_Verbose(0,1);
     mTecHoughTrack->event(topNode);
    if (verbose>10) dTecTrack->Show();

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

    cout << "********* eventNumber = " << eventNumber << endl;
//if(thisEvent1) delete thisEvent1;
//if(thisEvent2) delete thisEvent2;
//    mainIter.cd();

  } 
  } // loop over events

  // Take out the garbage
  delete eventIter1;
  delete eventIter2;
  delete TCO1;
  delete TCO2;

}

