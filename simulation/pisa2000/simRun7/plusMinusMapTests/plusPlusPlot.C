void plusPlusPlot(int iOption=1, const Long64_t maxEntries=0,
		  const float minMomentum=0.3, const float maxMomentum=5.0,
		  const float sigmaPC3Cut=3.0) {

  //
  // Macro to study the reconstruction using the ++ Run 4 field map
  //
  // Input arguments
  // iOption  plotting choice
  // maxEntries  number of entries of input evalPlusPlus.root NTUPLE which will be processed
  // minMomentum minimum value for track momentum in GeV/c
  // maxMomentum maximum value for track momentum in GeV/c
  // sigmaPC3Cut maximum value of tracking residual at PC3
  //
  gROOT->Reset();

  //
  // Open ROOT NTUPLE file
  //
  TFile *f = new TFile("evalPlusPlus.root");
  if (!f) {
    cerr << "\n Unable to evalPlusPlus.root file" << endl;
    exit (1);
  }

  //
  // Obtain NTUPLE pointer to single tracks
  //
  TNtuple *cglTrk = (TNtuple*)f->Get("evalCglNtuple");
  if(!cglTrk) {
    cerr << "\n Unable to find evalCglNtuple NTUPLE" << endl;
    exit (2);
  }
  //
  // Obtain NTUPLE pointer to single tracks
  //

  TNtuple *CglPair = (TNtuple*)f->Get("CglPair");
  if(!CglPair) {
    cerr << "\n Unable to find CglPair NTUPLE" << endl;
    exit (2);
  }

  //
  // Set up graphics display
  //
  TCanvas *c1 = new TCanvas("c1","c1",1);
  gStyle->SetOptStat(1110);
  gStyle->SetOptFit(1111);
  gStyle->SetStatTextColor(kRed);
  gStyle->SetFuncColor(kRed);
  gStyle->SetFuncWidth(2.0);
  gStyle->SetFitFormat("6.5g");

  //
  // Momentum resolution histograms for single particles
  //
  TH1F *eastPlusResolution;
  TH1F *eastMinusResolution;
  TH1F *westPlusResolution;
  TH1F *westMinusResolution;

  bool hbdPresent = true;
  if(iOption < 0) {
    hbdPresent = false;
    iOption = -iOption;
  }

  if(iOption==1 || iOption==2) {
    eastPlusResolution = new TH1F("eastPlusResolution", "East arm momentum resolution for K+", 50, -0.1, +0.1);
    eastMinusResolution = new TH1F("eastMinusResolution", "East arm momentum resolution for K-", 50, -0.1, +0.1);
    westPlusResolution = new TH1F("westPlusResolution", "West arm momentum resolution for K+", 50, -0.1, +0.1);
    westMinusResolution = new TH1F("westMinusResolution", "West arm momentum resolution for K-", 50, -0.1, +0.1);
  }

  if(iOption==3 || iOption==4) {
    eastPlusResolution = new TH1F("eastPlusResolution", "East arm momentum resolution for e+", 50, -0.1, +0.1);
    eastMinusResolution = new TH1F("eastMinusResolution", "East arm momentum resolution for e-", 50, -0.1, +0.1);
    westPlusResolution = new TH1F("westPlusResolution", "West arm momentum resolution for e+", 50, -0.1, +0.1);
    westMinusResolution = new TH1F("westMinusResolution", "West arm momentum resolution for e-", 50, -0.1, +0.1);
  }

  westPlusResolution->SetLineColor(kBlue);
  westPlusResolution->SetXTitle("Fractional Momentum Error: (RECO-TRUE)/TRUE");
  westPlusResolution->SetYTitle("Counts per 0.002 bin");

  westMinusResolution->SetLineColor(kBlue);
  westMinusResolution->SetXTitle("Fractional Momentum Error: (RECO-TRUE)/TRUE");
  westMinusResolution->SetYTitle("Counts per 0.002 bin");

  eastPlusResolution->SetLineColor(kBlue);
  eastPlusResolution->SetXTitle("Fractional Momentum Error: (RECO-TRUE)/TRUE");
  eastPlusResolution->SetYTitle("Counts per 0.002 bin");

  eastMinusResolution->SetLineColor(kBlue);
  eastMinusResolution->SetXTitle("Fractional Momentum Error: (RECO-TRUE)/TRUE");
  eastMinusResolution->SetYTitle("Counts per 0.002 bin");

  //
  // Momentum resolution histograms for pair mass and pair transverse momentum
  //
  double massLimit = 10.0;
  if(iOption>2)
    massLimit = 40.0;

  TH1F *pairMassResolution = new TH1F("pairMassResolution", "Resolution for Phi pair mass", 100, -massLimit, +massLimit);
  TH1F *pairPTranResolution = new TH1F("pairPTranResolution", "Resolution for Phi pair transverse momentum", 50, -0.1, +0.1);
 
  pairMassResolution->SetLineColor(kBlue);
  pairMassResolution->SetXTitle("Pair Mass Error: RECO-TRUE (MeV)");
  if(iOption<3)
    pairMassResolution->SetYTitle("Counts per 0.2 MeV bin");
  else
    pairMassResolution->SetYTitle("Counts per 0.8 MeV bin");

  pairPTranResolution->SetLineColor(kBlue);
  pairPTranResolution->SetXTitle("Pair Transverse Fractional Momentum Error: (RECO-TRUE)/TRUE");
  pairPTranResolution->SetYTitle("Counts per 0.002 bin");

  int plusID = 51;
  int minusID = 52;
  if(iOption==3 || iOption==4) {
    plusID = 2;
    minusID = 3;
  }

  char westPlusCut[200];
  sprintf(westPlusCut, 
	  "DCARM==1&&DCMOMEN>%f&&DCMOMEN<%f&&PC3IDPAR==%d&&abs(DSIGPPC3)<%f&&DSIGZPC3<%f&&(DCQUAL==31||DCQUAL==63)&&MCMOMEN>0.0",
	  minMomentum, maxMomentum, plusID, sigmaPC3Cut, sigmaPC3Cut);

  cglTrk->Project("westPlusResolution", "(DCMOMEN-MCMOMEN)/MCMOMEN", westPlusCut);
  
  char westMinusCut[200];
  sprintf(westMinusCut, 
	  "DCARM==1&&DCMOMEN>%f&&DCMOMEN<%f&&PC3IDPAR==%d&&abs(DSIGPPC3)<%f&&DSIGZPC3<%f&&(DCQUAL==31||DCQUAL==63)&&MCMOMEN>0.0",
	  minMomentum, maxMomentum, minusID, sigmaPC3Cut, sigmaPC3Cut);

  cglTrk->Project("westMinusResolution", "(DCMOMEN-MCMOMEN)/MCMOMEN", westMinusCut);

  char eastPlusCut[200];
  sprintf(eastPlusCut, 
	  "DCARM==0&&DCMOMEN>%f&&DCMOMEN<%f&&PC3IDPAR==%d&&abs(DSIGPPC3)<%f&&DSIGZPC3<%f&&(DCQUAL==31||DCQUAL==63)&&MCMOMEN>0.0",
	  minMomentum, maxMomentum, plusID, sigmaPC3Cut, sigmaPC3Cut);

  cglTrk->Project("eastPlusResolution", "(DCMOMEN-MCMOMEN)/MCMOMEN", eastPlusCut);
  
  char eastMinusCut[200];
  sprintf(eastMinusCut, 
	  "DCARM==0&&DCMOMEN>%f&&DCMOMEN<%f&&PC3IDPAR==%d&&abs(DSIGPPC3)<%f&&DSIGZPC3<%f&&(DCQUAL==31||DCQUAL==63)&&MCMOMEN>0.0",
	  minMomentum, maxMomentum, minusID, sigmaPC3Cut, sigmaPC3Cut);

  cglTrk->Project("eastMinusResolution", "(DCMOMEN-MCMOMEN)/MCMOMEN", eastMinusCut);

  Int_t nEntries = CglPair->GetEntries();
  Int_t readEntries = maxEntries;
  if(maxEntries>nEntries || maxEntries==0)
    readEntries = nEntries;

  if(maxEntries > 0)
    cout << "\n nentries = " << nentries << ",  readEntries = " << readEntries << endl;

  if(maxEntries == 0)
    cout << "\n pair NTUPLE nEntries = " << nEntries << endl;

  gROOT->Macro("CglPair.C");

  for(int kEntry=0; kEntry<readEntries; kEntry++) {

    CglPair->GetEntry(kEntry);

    if((iOption==1 || iOption==2) &&
       MCIDPAR1 + MCIDPAR2 != 103)
      continue;

   if((iOption==3 || iOption==4) &&
       MCIDPAR1 + MCIDPAR2 != 5)
      continue;

    if((DCQUAL1 != 31 && DCQUAL1 != 63) ||
       (DCQUAL2 != 31 && DCQUAL2 != 63))
      continue;

    if(PTOT1<minMomentum || PTOT1>maxMomentum || PTOT2<minMomentum || PTOT2> maxMomentum)
      continue;

    if(VERTMASS>0.0 && PAIRMASS>0.0) {
      pairMassResolution->Fill(1000.0*(PAIRMASS-VERTMASS));
    }

    if(VERTPTRAN>0.0 && PAIRPTRAN>0.0) {
      pairPTranResolution->Fill((PAIRPTRAN-VERTPTRAN)/VERTPTRAN);
    }

    if(fabs(DSG1ZPC3)>sigmaPC3Cut ||
       fabs(DSG2ZPC3)>sigmaPC3Cut ||
       fabs(DSG1PPC3)>sigmaPC3Cut ||
       fabs(DSG2PPC3)>sigmaPC3Cut)
      continue;
  }

  c1->Divide(2,3);

  c1->cd(1);
  double rFact = 1.0;
  if(iOption < 3) {
    westPlusResolution->SetMaximum(145.);
  }
  else {
    westPlusResolution->SetMaximum(300.);
    rFact = 1.5;
  }
  westPlusResolution->Fit("gaus","","", -0.04, +0.04);
  westPlusResolution->Draw();

  if(iOption==1 || iOption==2) {
    TLatex *t0 = new TLatex(-0.080, rFact*120, "#phi #rightarrow K^{+}K^{-}");
    t0->SetTextAlign(12);
    t0->SetTextColor(kBlack);
    t0->SetTextSize(0.07);
    t0->Draw();
  }

  if(iOption==3 || iOption==4) {
    TLatex *t0 = new TLatex(-0.080, rFact*120, "#phi #rightarrow e^{+}e^{-}");
    t0->SetTextAlign(12);
    t0->SetTextColor(kBlack);
    t0->SetTextSize(0.07);
    t0->Draw();
  }

  TLatex *t1 = new TLatex(-0.085, rFact*100, "Variable p_{T}");
  t1->SetTextAlign(12);
  t1->SetTextColor(kBlack);
  t1->SetTextSize(0.07);
  t1->Draw();

  if(iOption==1 || iOption==3) {
    TLatex *t2 = new TLatex(-0.09, rFact*80, "Fixed Vertex Z=0");
    t2->SetTextAlign(12);
    t2->SetTextColor(kBlack);
    t2->SetTextSize(0.07);
    t2->Draw();
  }

  if(iOption==2 || iOption==4) {
    TLatex *t2 = new TLatex(-0.095, rFact*80, "-30<Z Vertex<+30 cm");
    t2->SetTextAlign(12);
    t2->SetTextColor(kBlack);
    t2->SetTextSize(0.07);
    t2->Draw();
  }

  TLatex *t3 = new TLatex(-0.09, rFact*60, "++ Magnetic Field");
  t3->SetTextAlign(12);
  t3->SetTextColor(kBlack);
  t3->SetTextSize(0.07);
  t3->Draw();

  TLatex *t4 = new TLatex(-0.09, rFact*40, "December 15, 2004");
  t4->SetTextAlign(12);
  t4->SetTextColor(kBlack);
  t4->SetTextSize(0.07);
  t4->Draw();

  TLatex *t5;
  if(hbdPresent) {
    t5 = new TLatex(-0.085, rFact*140, "HBD Present");
  }
  else {
    t5 = new TLatex(-0.085, rFact*140, "HBD Absent");
  }
  t5->SetTextAlign(12);
  t5->SetTextColor(kBlack);
  t5->SetTextSize(0.07);
  t5->Draw();

  c1->cd(2);
  eastPlusResolution->Draw();
  eastPlusResolution->Fit("gaus","","", -0.04, +0.04);

  c1->cd(3);
  westMinusResolution->Draw();
  westMinusResolution->Fit("gaus","","", -0.04, +0.04);

  c1->cd(4);
  eastMinusResolution->Draw();
  eastMinusResolution->Fit("gaus","","", -0.04, +0.04);

  c1->cd(5);
  pairMassResolution->Draw();
  pairMassResolution->Fit("gaus","","", -0.5*massLimit, +0.5*massLimit);

  c1->cd(6);
  pairPTranResolution->Draw();
  pairPTranResolution->Fit("gaus","","", -0.04, +0.04);
}
