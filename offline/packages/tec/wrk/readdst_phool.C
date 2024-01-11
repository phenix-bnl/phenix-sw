void readdst_phool(const char* dstFile="dstout26040.root") {

gSystem->Load("libpreco.so");

  PHCompositeNode* dstNode = new PHCompositeNode("DST");
  PHNodeIterator mainIter(dstNode);
  PHNodeReset reset;

  dTecTrackWrapper* dTecTrack;
  dDchTracksWrapper* dDchTracks;
  dPadClusterWrapper* dPc3Cluster;

  PHNodeIOManager* dstIn = new PHNodeIOManager(dstFile,PHReadOnly) ;

  TTree *T=(TTree*)gDirectory->Get("T");
  cout << "Number of Entries: " << T->GetEntries() << endl;

//  for (int n=0; n<T->GetEntries(); n++) {
  for (int n=0; n<10; n++) {

    dstIn->read(dstNode);

    if(n==0) dstNode->print();

    PHNodeIterator mainIter(dstNode);

    PHIODataNode<PHTable>* dTecTrackNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dTecTrack");
    if (!dTecTrackNode) {
      cerr << "ERROR: Can not find data node dTecTrack" << endl;
    } 
    else {
      dTecTrackWrapper* dTecTrack = (dTecTrackWrapper*)dTecTrackNode->getData();
    }

    PHIODataNode<PHTable>* dDchTracksNode = (PHIODataNode<PHTable>*)mainIter->findFirst("PHIODataNode","dDchTracks");
    if (!dDchTracksNode) {
      cerr << "ERROR: Can not find data node dDchTracks" << endl;
    }
    else {
      dDchTracksWrapper* dDchTracks = (dDchTracksWrapper*)dDchTracksNode->getData();
    }

    
cout << dTecTrack->RowCount() << " " << dDchTracks->RowCount() << endl;

  }

}

