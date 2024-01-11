void simple(char *fname="DST.root"){

  gSystem->Load("libndst");
  TFile *dst = new TFile(fname);

  TTree *T = (TTree*)dst->Get("T");

  int nentry;
  nentry = (int)T->GetEntries();

  for (int ievt = 0 ; ievt < nentry; ievt++){
    if(ievt % 1000 == 0)
      cout << ievt << " events processed" << endl;
   T->GetEntry(ievt);
  }
}

