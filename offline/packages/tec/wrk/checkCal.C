{

// Load shared libraries
//  gROOT->Macro("loadlibraries.C");
//  gSystem->Load("libpreco.so");

// Create calibration object
  TecCalibrationObject* TCO = new TecCalibrationObject();
  cout << TCO->getName() << " created." << endl;
//  TCO->setDebugLevel(5);

   for(int i=69830; i<69840; i++) {
     PHBoolean status = TCO->FetchAbsGain(i);
     PHBoolean calibstatus = 0;
     if(status) {
//        for(int j=0; j<48; j++) {cout << TCO->getAbsoluteGain(j) << " ";}
        for(int j=0; j<48; j++) {
          if(TCO->getAbsoluteGain(j)!=1.0) {
            calibstatus = 1;
            break;
          }
        }
	cout << endl;
     }
     cout << "run # " << i << "    status = " << status << " ";
     if(calibstatus) cout << " CALIBRATED" << endl;
       else cout << " Not calibrated." << endl;
   }

}

