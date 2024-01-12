{
  textFile << " ----------------------------------------------------" << endl;
  textFile << " -- BBC QA Summary --" << endl;
  textFile << " ----------------------------------------------------" << endl;

  Int_t bbcstatus = 0;

  TH1F *bbct0 = (TH1F *) qafile->Get ("bbct0");
  TH1F *bbcvtx = (TH1F *) qafile->Get ("bbcvtx");
  TH1F *bbcnpmt = (TH1F *) qafile->Get ("bbcnpmt");
  TH1F *bbcQ = (TH1F *) qafile->Get ("bbcQ");
  TH2F *bbcQnQs = (TH2F *) qafile->Get ("bbcQnQs");
  TH2F *bbcvtxt0 = (TH2F *) qafile->Get ("bbcvtxt0");

// add here analysis and printout results to textFile

  if (bbct0 == NULL || bbcvtx == NULL || bbcnpmt == NULL ||
      bbcQ == NULL || bbcQnQs == NULL || bbcvtxt0 == NULL)
  {
    textFile << " BBC ERROR: could not extract histograms" << endl;
    bbcstatus = 1;
  }
  else
  {

    float BbcT0Mean = bbct0->GetMean ();
    float BbcT0RMS = bbct0->GetRMS ();
    float BbcZvtxMean = bbcvtx->GetMean ();
    float BbcZvtxRMS = bbcvtx->GetRMS ();

    TH1D *qs = bbcQnQs->ProjectionX();
    TH1D *qn = bbcQnQs->ProjectionY();

//    TH1F *qs = (TH1F *) gROOT->FindObject ("bbcQnQs_px");
//    TH1F *qn = (TH1F *) gROOT->FindObject ("bbcQnQs_py");

    float chargeN = qs->GetMean ();
    float chargeS = qn->GetMean ();
    int nHit = bbcnpmt->GetMean ();
    float EndPoint, Sum = 0;
    int MaxBin = 0, Norm = 0; 
    for (int i=200; i>0; i--) {
      if (bbcQ->GetBinContent(i) > 0) {
        MaxBin = i;
        break;
      }
    }
    for (i=MaxBin; i>MaxBin-5; i--) {
      Norm += bbcQ->GetBinContent(i);
      Sum  += i*10.0*bbcQ->GetBinContent(i);
    }

    if(Norm>0)
			{
    EndPoint = Sum/(float)Norm;
			}
    else
		{
    EndPoint = 0.;
    cout <<"BBC error Endpoint set to zero"<<endl;
		}

    textFile << " BBC TimeZero distribution [ns] = " << BbcT0Mean << " +- " <<
      BbcT0RMS << endl;
    textFile << " BBC Z Vertex distribution [cm] = " << BbcZvtxMean << " +- "
      << BbcZvtxRMS << endl;

    if (fabs (BbcT0Mean) > 10)
      bbcstatus = 1;
    if (chargeN == 0 || chargeS == 0)
      bbcstatus = 1;
    if (nHit == 0 || nHit > 128)
      bbcstatus = 1;
    if (EndPoint < 1550 || EndPoint > 1650)
      bbcstatus = bbcstatus + 2;

    textFile << " EndPoint(mip) = " << EndPoint << endl
             << " BBC Status = " << bbcstatus << endl;
    statusFile << bbcstatus << " ";

  }
}
