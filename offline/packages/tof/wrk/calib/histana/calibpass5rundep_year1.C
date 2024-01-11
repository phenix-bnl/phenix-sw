// calcoffset.C
//  KEY: TH1F     tofSlatD;1       TOF hits/slatid
//  KEY: TH2F     tofTime2d;1     Time - L/C:slatid
//  KEY: TH3F     tofDiff3d;1     (T_l - T_u)/2:slatid
//  KEY: TH3F     tofQvcL3d;1     qvc[0]:TOF:slatid
//  KEY: TH3F     tofQvcU3d;1     qvc[1]:TOF:slatid

void calibpass5rundep_year1(const char *histFile="tofcalib_pass5.root", 
			    const char *psFile ="pass5.ps", 
			    const char *psFile2="pass5chk.ps", 
			    const char *txtFile="tofGlobalT_rundep.txt"){

  // setup
  gROOT->SetStyle("Plain");
  gStyle->SetTitleW(0.4);
  gStyle->SetTitleH(0.1);
  gStyle->SetStatW(0.2);
  gStyle->SetStatH(0.2);
  gStyle->SetOptStat(10);        // only for numEvents
  gStyle->SetOptFit(1);
  Int_t verbose = 7;
  Int_t type = 111;    // portrait
  TFile *tofhfile = new TFile(histFile);
  TCanvas *c = new TCanvas("c", "TOF PostScript", 600, 800);
  TPostScript *ps = new TPostScript(psFile, type);
  ofstream outputfile; 
  outputfile.open(txtFile);

  Float_t width  = 20;
  Float_t height = 29;
  ps->Range(width,height);
  Int_t yoko = 3;
  Int_t tate = 6;
  Int_t npad = yoko*tate;
  c->Divide(yoko,tate);
  // RUN dependence calc.
  int run, nextrun;
  const int runset = 54;
  int runperiod[runset]  = {
    8885,  9251,  9438,  9591,  9844,  9972,  10108, 10205, 10728, 10758,
    10902, 11011, 11016, 11026, 11035, 11058, 11159, 11201, 11460, 11477,
    11512, 11572, 11597, 11610, 11615, 11654, 11679, 11682, 11684, 11720,
    11740, 11757, 11775, 11817, 11881, 11902, 11928, 11951, 11997, 12005,
    12010, 12026, 12052, 12087, 12089, 12156, 12167, 12275, 12299, 12323, 
    12335, 12350, 12365, 12374};

  // memo: 10505,11807-Fleld OFF, 12323-HALF,
  //       12404,12424,12431-Special for BBC

  float t0, t0offset = 0;
  float gmin, gmax, lmin, lmax, emin, emax, mip;
  double timepar[3], chargepar[5];
  char name[20],title[80];
  int run4seq = 0, run6seq = 0, run8seq = 0;

  sprintf(title,"run dep.");
  // # of slat
  const Int_t  NTOF = 200;
  TH1D *tofTrun4ProjX;
  TH1D *tofTrun6ProjX;
  TH1D *tofTrun8ProjX;
  TH1D *tofTrunProj[runset];
  TH2F *Trun4;
  TH2F *Trun6;
  TH2F *Trun8;

  Trun4 = (TH2F*)tofTrun4->Clone();
  Trun6 = (TH2F*)tofTrun6->Clone();
  Trun8 = (TH2F*)tofTrun8->Clone();
  sprintf(title,"%s  (0.2<pt<2.0)",title);
  
  Trun4->SetName("Trun4");
  Trun6->SetName("Trun6");
  Trun8->SetName("Trun8");

  // Checking Histgram
  Int_t tbin = 120; Float_t tmin = -0.15, tmax = 0.15;

  TH2F *tofTrun4chk = new TH2F("tofTrun4chk","TOF - T0 - flightTime:run",
			       1600, 8850.5, 10450.5, tbin, tmin, tmax);
  TH2F *tofTrun6chk = new TH2F("tofTrun6chk","TOF - T0 - flightTime:run",
			       1100,10700.5, 11800.5, tbin, tmin, tmax);
  TH2F *tofTrun8chk = new TH2F("tofTrun8chk","TOF - T0 - flightTime:run",
			       700, 11800.5, 12500.5, tbin, tmin, tmax);
  TH1F *tofGlobalT = new TH1F("tofGlobalT",title, tbin, tmin, tmax);
  tofGlobalT->SetXTitle("[ns]");
  tofGlobalT->SetFillColor(5);

  Int_t pad = 0;
  tofhfile->cd();

  tofTrun4ProjX = Trun4->ProjectionX();
  tofTrun6ProjX = Trun6->ProjectionX();
  tofTrun8ProjX = Trun8->ProjectionX();

  for(Int_t i = 0; i<runset;i++){
    if(i%npad == 0){
      cout<<"  ## ps->NewPage(); i =  "<<i<<"  npad = "<<npad<<endl;
      pad = 1;
      c->Update();
      ps->NewPage();
    }
    run = runperiod[i];
    sprintf(name,"time%d",run);
    if(i == runset-1) nextrun = 12500;
    else nextrun = runperiod[i+1];

    cout<<"  "<<i<<" : run "<<run<<" - "<<nextrun<<endl;

    Int_t runmin, runmax;
    if(run >= 8850 && run < 10450){
      if(nextrun > 10450) nextrun = 10450;
      runmin = tofTrun4ProjX->FindBin(run);
      runmax = tofTrun4ProjX->FindBin(nextrun) - 1;
      tofTrunProj[i] = Trun4->ProjectionY(name,runmin,runmax);
      run4seq++;
    } else if(run >= 10700 && run < 11800){
      if(nextrun > 11800) nextrun = 11800;
      runmin = tofTrun6ProjX->FindBin(run);
      runmax = tofTrun6ProjX->FindBin(nextrun) - 1;
      tofTrunProj[i] = Trun6->ProjectionY(name,runmin,runmax);
      run6seq++;
    } else if(run >= 11817 && run < 12500){
      if(nextrun > 12500) nextrun = 12500;
      runmin = tofTrun8ProjX->FindBin(run);
      runmax = tofTrun8ProjX->FindBin(nextrun) - 1;
      tofTrunProj[i] = Trun8->ProjectionY(name,runmin,runmax);
      run8seq++;
    }

    c->cd(pad++);
    gmin = -1.;
    gmax = +1.;
    TF1* fitg = new TF1("fitg","gaus",gmin,gmax);
    tofTrunProj[i]->Fit("fitg","RQ0");
    t0 = fitg->GetParameter(1);
    t0 -= t0offset;
    sprintf(title,"%d - %f",run,t0);
    Int_t entries = tofTrunProj[i]->Integral(1,tbin);
    tofTrunProj[i]->SetEntries(entries);
    tofTrunProj[i]->SetTitle(title);
    tofTrunProj[i]->SetXTitle("[ns]");
    tofTrunProj[i]->Draw();
    cout<<"   "<< run <<"  \t"<< t0 <<endl;
    outputfile<<"   "<< run <<"  \t"<< t0 <<endl;

    tofGlobalT->Fill(t0);

    if(run >= 8850 && run < 10450){
      for(Int_t j = run; j < nextrun; j++) tofTrun4chk->Fill(j,t0);
    } else if(run >= 10700 && run < 11800){
      for(Int_t j = run; j < nextrun; j++) tofTrun6chk->Fill(j,t0);
    } else if(run >= 11817 && run < 12500){
      for(Int_t j = run; j < nextrun; j++) tofTrun8chk->Fill(j,t0);
    }
  }
  ps->Close();
  cout<<"Write ps file = "<<psFile<<endl;

  // Let's look at the checking histograms
  gROOT->SetStyle("Plain");
  float min = -0.3;
  float max =  0.3;
  TCanvas *c0 = new TCanvas("c0","",600,800);
  c0->Divide(1,4);
  c0->cd(1);
  tofTrun4chk->SetXTitle("RUN #");
  tofTrun4chk->SetYTitle("[ns]");
  tofTrun4chk->Draw("box");
  c0->cd(2);
  tofTrun6chk->SetXTitle("RUN #");
  tofTrun6chk->SetYTitle("[ns]");
  tofTrun6chk->Draw("box");
  c0->cd(3);
  tofTrun8chk->SetXTitle("RUN #");
  tofTrun8chk->SetYTitle("[ns]");
  tofTrun8chk->Draw("box");
  c0->cd(4);
  //tofGlobalT->Draw();
  tofGlobalT->Fit("gaus");

  c0->Print(psFile2);
  //tofhfile->Close();
}
