
void readprdf(Int_t maxEvents=1, const char *prdfIFile="data.prdf") {

  Int_t eventNumber = 0;

// Set up the node tree
  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dcmNode = new PHCompositeNode("DCM");
  topNode->addNode(dcmNode);
  PHCompositeNode* tecNode = new PHCompositeNode("TEC");
  topNode->addNode(tecNode);
  PHCompositeNode* parNode = new PHCompositeNode("PAR");
  topNode->addNode(parNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

// Create event iterator
  Event *thisEvent = 0;
  mainIter.addNode(new PHDataNode<Event>(thisEvent, "PRDF"));
  Eventiterator *eventIter = new fileEventiterator(prdfIFile);

// Set up the modules
  mTecDecodeModule* mTecDecode = new mTecDecodeModule;

// Initialize the tables
  size_t mr=8000;
  dTecFemDataWrapper* dTecFemData = new dTecFemDataWrapper("dTecFemData",mr);
  PHIODataNode<PHTable>* dTecFemDataNode = new PHIODataNode<PHTable>(dTecFemData,"dTecFemData");
  tecNode->addNode(dTecFemDataNode);

// Create Address Object
  TecAddressObject* TecAddress = new TecAddressObject();
  PHIODataNode<TObject>* TAONode = new PHIODataNode<TObject>(TecAddress,"TecAddress");
  parNode->addNode(TAONode);
  TecAddress->FetchFromFile("tecmap_database_run00.txt");
 
  mainIter.cd();

// Start loop over events
  printf("Entering event loop.\n");
  while ((thisEvent = eventIter->getNextEvent()) && eventNumber++ < maxEvents) {

// Point the data node to the new event
    mainIter.cd();
    ((PHDataNode<Event>*)(mainIter.findFirst("PHDataNode","PRDF")))->setData(thisEvent);

    printf("Fetched event %d\n",eventNumber);
// See what tables are in the topNode
    topNode->print();

// Unpack dTecDcmData table and put the data into the dTecFemData table
    printf("Calling mTecUnpack...\n");
    mTecDecode->event(topNode,TecAddress);
    dTecFemData->Show();

// Check table content
  for(int i=0; i<dTecFemData->RowCount(); i++) {
    int icrate = dTecFemData->get_crate(i);
    int islot = dTecFemData->get_slot(i);
    int ichan = dTecFemData->get_ichan(i);
    cout << icrate << " " << islot << " " << ichan << endl;
  }

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

}

