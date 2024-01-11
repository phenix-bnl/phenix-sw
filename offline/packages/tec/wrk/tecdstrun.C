
void tecdstrun(Int_t maxEvents=1, const char *dstIFile="DST.root") {

  Int_t eventNumber = 0;

  Int_t verbose = 12;

// Set up node tree

  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

// Build histogram factory (if you want to use PhHistograms)
  PhRootHistogramFactory::buildFactory();

// Open file and read the first event
  PHString dstInFile = dstIFile;
  PHNodeIOManager *dstIn = new PHNodeIOManager(dstInFile,PHReadOnly);
  dstIn->read(dstNode);

// Get a pointer to dTecTrack table
  PHIODataNode<PHTable>* dTecTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecTrack");
  if (!dTecTrackNode) {
    cerr < "DST Read ERROR: Can not find data node dTecTrack" << endl;
  } else {
    dTecTrackWrapper* dTecTrack = (dTecTrackWrapper*)dTecTrackNode->getData();
  }

  if (verbose>5) cout << "Entering event loop." << endl;

// Start event loop
do {

eventNumber++;
  if (verbose>5) cout << "Fetched event " << eventNumber << endl;

// Read next events and get pointer to dTecTrack table
  if(eventNumber>1) {
    dstIn->read(dstNode);
    dTecTrack = (dTecTrackWrapper*)dTecTrackNode->getData();
  }

// Print out dTecTrack table contents to make sure that it is OK
  cout << "dTecTrack row count: " << dTecTrack->RowCount() << endl;
    for(int i=0; i<dTecTrack->RowCount(); i++) {
      cout << 
        dTecTrack->get_id(i) << " " <<
        dTecTrack->get_ntime(i) << " " <<
        dTecTrack->get_xyzin(0,i) << " " <<
        dTecTrack->get_xyzout(0,i) << " " <<
        dTecTrack->get_xyzin(1,i) << " " <<
        dTecTrack->get_xyzout(1,i) << " " << endl;
    }

// Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }

}
while(eventNumber<maxEvents);
// End event loop;

}








