//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void monitorOneByOne(int loop){

  
  gROOT->Macro("monitorParOneByOne.C");

  mainIter.cd();
  printf("First event initializer \n");
  
  int end = 0;
  float propWidth = 0.3;
  mDchDVCalibrator->setWidthOfProportionalRegion(propWidth);  // prop region

  for (int ii=0; ii < loop ; ii++) {
    thisEvent = eventIter->getNextEvent();
    eventNumber++;
    cout << "skip event "<< eventNumber << endl;
  }
 
  if (thisEvent = eventIter->getNextEvent() && eventNumber++ ) {
    
    cout << "-------------------------------" << endl; 
    cout << "Analyzing Event "<< eventNumber << endl; 
    cout << "-------------------------------" << endl; 
    int result = analyze();
  }


}

int analyze(){
  
  mainIter.cd();
  cleanup(); 
  float binSize = (1.0/0.038)/32.0;
  mDchInitializer->event(topNode);
  //------------------------------------------------------------- 
  // set here the calibration values (to in bins, vd in cm/ns )
  //-------------------------------------------------------------
    if (first == 1) {
      PdbIndex *armEast = new PdbIndex(0,1,0,"EAST");
      PdbIndex *armWest = new PdbIndex(0,1,1,"WEST");
      mDchInitializer->getDCO()->setCalibration(armEast,150,0.005267*binSize,binSize);
      mDchInitializer->getDCO()->setCalibration(armWest,98.49,0.005267*binSize,binSize);
      mDchInitializer->getDCO()->printCalibration(armEast);
      mDchInitializer->getDCO()->printCalibration(armWest);
      delete armEast;
      delete armWest;
      //mDchInitializer->getDGO()->rotateAndTranslate();
      first = 0;
    }

  
  if (verbose>5) printf("Setting Data \n");
  ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);
  
  DchGetDCM(topNode);
  mDchUnpacker->event(topNode);
  cout << "noise analysis "<<endl;
  mDchNoiseAnalyzer->event(topNode);
  cout << "calibrator "<< endl;
  mDchCalibrator->event(topNode);
  cout << "numberOfHits "<< mDchCalibrator->numberOfHits() << endl;
  int ntracks = 0;
  if (mDchCalibrator->numberOfHits() > 0) {
    mDchTracker->event(topNode);
    mDchBuilder->event(topNode);
    gPhenix->event(topNode);  
    gPhenix->Draw();
  }
  return ntracks;

}

void cleanup(){
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
     if (mainIter.cd("DCH")) {
       mainIter.forEach(reset);
       mainIter.cd();
     }
     if (mainIter.cd("BBC")) {
       mainIter.forEach(reset);
       mainIter.cd();        
     }
     if (mainIter.cd("ZDC")) {
       mainIter.forEach(reset);
       mainIter.cd();
       
     }
}





