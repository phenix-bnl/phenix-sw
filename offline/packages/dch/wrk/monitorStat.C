//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void monitorStat(int loop){

  gROOT->Macro("monitorPar.C");

  mainIter.cd();
  printf("First event initializer ");

  topNode->print();

  int end = 1;
  float propWidth = 0.3;
  mDchDVCalibrator->setWidthOfProportionalRegion(propWidth);  // prop region
  for (int ll =0; ll < loop; ll++) {
    if((thisEvent = eventIter->getNextEvent())) {
      prdfEventNode->setData(thisEvent);
      eventNumber++;
      cleanup();
      cout << "**********************************"<< endl;
      cout << "Analyzing event "<< eventNumber<< endl;
      cout << "**********************************"<< endl;
      int result = analyze();
    }
  }
  if (end == 1) {
     cout << "Writing  noise file "<< endl;
     mDchNoiseAnalyzer->writeFile(eventNumber, 1.); // noise threshold
     cout << "Fill Statistics "<< endl;
     mDchNoiseAnalyzer->fillNoiseNtuple();
     cout << "Calibrator integration "<< endl;
     mDchDVCalibrator->integrate();
     cout << "number Of Good events "<< goodEvents << endl;
     cout << "Cal Channels "<< mDchInitializer->getDCO()->numberOfCalibrationChannels() << endl;


     // cout << "Calibrator fit "<< endl;
     // mDchDVCalibrator->fitTimeDistanceRelation(180,550,0,0.004);
     // cout << "Calibrator calibrate from edges"<< endl;
     // mDchDVCalibrator->calculateCalibrationFromEdges(0,100,500,700);
   }

}

int analyze(){

    float binSize = (1.0/0.038)/32.0;
    mainIter.cd();
 
    //------------------------------------------------------------- 
    // set here the calibration values (to in bins, vd in cm/ns )
    //-------------------------------------------------------------
    if(eventNumber == 1) {
       mDchInitializer->event(topNode);
       PdbIndex* armEAST = new PdbIndex(0,1,0,"EAST");
       PdbIndex* armWEST = new PdbIndex(0,1,1,"WEST");

       mDchInitializer->getDCO()->setCalibration(armEAST,150,0.005267*binSize,binSize);
       mDchInitializer->getDCO()->setCalibration(armWEST,98.49,0.005267*binSize,binSize);
       mDchInitializer->getDCO()->printCalibration(armEAST);
       mDchInitializer->getDCO()->printCalibration(armWEST);
       delete armEAST;
       delete armWEST;
    }
    histogrammer = mDchInitializer->getHistogrammer();
   

    if (verbose>5) printf("Setting Data \n");
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);
  
           
    DchGetDCM(topNode);
    mDchUnpacker->event(topNode);
    
    if (mDchNoiseAnalyzer->event(topNode)) {
      goodEvents++; //goodEvents++;
    }
    if (verbose>10) printf("Calling DRift Chamber\n");
    mDchCalibrator->event(topNode);
    int ntracks = 1;
    if (mDchCalibrator->numberOfHits() > 0) {
      
      mDchTracker->event(topNode);
      ntracks = mDchTracker->numberOfCandidates();
      cout << "numberOfCandidates "<< ntracks << endl;
      mDchBuilder->event(topNode);
      mDchDVCalibrator->event(topNode);
      if(ntracks>=10) { 
	//   dchVertexFit->event(topNode);
       }     
       histogrammer->fillRawData(topNode);
       histogrammer->fillHits(topNode);
       histogrammer->fillTracks(topNode);
       //gPhenix->event(topNode);  
       //gPhenix->Draw();
    }
    return 1;

   
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





