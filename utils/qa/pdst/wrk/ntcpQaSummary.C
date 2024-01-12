{
  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- NTCP QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;

  Int_t ntcpstatus = 0;

  TH1F *ntcpt0  = (TH1F *) qafile->Get("ntcpt0");;
  TH1F *ntcpvtx = (TH1F *) qafile->Get("ntcpvtx");
  TH1F *ntcppmt = (TH1F *) qafile->Get("ntcppmt");
  TH1F *ntcpQN  = (TH1F *) qafile->Get("ntcpQN");
  TH1F *ntcpQS  = (TH1F *) qafile->Get("ntcpQS");
  TH1F *ntcpbbcvtx = (TH1F *) qafile->Get("ntcpbbcvtx");
  TH1F *ntcpbbct0  = (TH1F *) qafile->Get("ntcpbbct0");

  // add here analysis and printout results to textFile

  if ( ntcpt0  == 0 ||
       ntcpvtx == 0 || 
       ntcppmt == 0 ||
       ntcpQN  == 0 ||
       ntcpQS  == 0 || 
       ntcpbbcvtx == 0 ||
       ntcpbbct0  == 0 )
  {
    textFile << " NTCP ERROR: could not extract histograms" << endl;
    ntcpstatus = 1;
  }
  else
  {
    float vdmean   = ntcpbbcvtx->GetMean ();
    float vdrms    = ntcpbbcvtx->GetRMS ();
    float t0dmean  = ntcpbbct0->GetMean ();
    float t0drms   = ntcpbbct0->GetRMS ();

    // check if a channel is dead     
    float  nhits  = ntcppmt->GetEntries()/16.0;    
    int ndead     = 0;
    float minhits = nhits - 6.0*sqrt(nhits); // 6sigma away
    for ( int i = 0; i < 16; i++ ) 
      {
	if ( ntcppmt->GetBinContent(i+1) < minhits )
	  ndead ++;
      }

    textFile << " NTCP-BBC Vertex: " << vdmean << " +- " << vdrms << endl;
    textFile << " NTCP-BBC     T0: " << t0dmean << " +- " << t0drms << endl;
    textFile << " NTCP     Nhits : " << nhits  << ", " << ndead << endl;
    textFile << " NTCP Status = " << ntcpstatus << endl;
    statusFile << ntcpstatus << endl;


  }
}


