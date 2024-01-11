
void readtecout(Int_t maxEvents=15, const char *dstFile="dstout.root") {

// Load all libraries. The simplest way is to load libpreco.so
//  gSystem->Load("/phenix/data23/lebedev/offline/installpreco/lib/libpreco.so");
  gSystem->Load("libpreco.so");

  PHCompositeNode* topNode = new PHCompositeNode("TOP");
  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  topNode->addNode(dstNode);
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;

  PHNodeIOManager* dstIn = new PHNodeIOManager(dstFile,PHReadOnly) ;

  dstIn->read(dstNode) ;

  topNode->print() ;

  TecOutV2* tecout;
  PHIODataNode<TObject> *TecOutNode = (PHIODataNode<TObject>*)mainIter.findFirst("PHIODataNode","TecOut") ;

  for (int n=0; n<maxEvents; n++) {

    dstIn->read(dstNode) ;

    tecout = (TecOutV2*)TecOutNode->getData() ;

    cout << "tecout: " << tecout->getNTracks() << " " << tecout->getNHits() << endl;

// Reset all data for this event
    mainIter.cd();
    if (mainIter.cd("DST")) {
      mainIter.forEach(reset);
      mainIter.cd();
    }

  }

}


