// calcoffset.C
//  KEY: TH1F     tofSlatT;1       TOF hits/slatid
//  KEY: TH2F     tofT2d;1         Time - L/C:slatid

void calibpass4offsetT(const char *histFile="tofcalib.root", 
		       const char *psFile="pass4.ps", 
		       const char *psFile2="pass4chk.ps"){
  // Loading PHOOL libraries
  gSystem->Load("libEvent.so");
  gSystem->Load("libphool.so");
  gSystem->Load("libWrappers.so");
  gSystem->Load("libPdbCal.so");
  gSystem->Load("libPhHistogramFactory.so");
  gSystem->Load("libuti.so");
  gSystem->Load("libdcm.so");
  gSystem->Load("libphgeo.so");

  // Loading subsystem libraries
  gSystem->Load("libtof.so");

  // setup
  gROOT->SetStyle("Plain");
  gStyle->SetTitleW(0.6);
  gStyle->SetTitleH(0.1);
  gStyle->SetStatW(0.2);
  gStyle->SetStatH(0.2);
  gStyle->SetOptStat(10);        // only for numEvents
  gStyle->SetOptFit(1);
  Int_t verbose = 7;
  Int_t type = 111;    // portrait
  //Int_t type = 112;     // landscape
  TFile *tofhfile = new TFile(histFile);
  TCanvas *c = new TCanvas("c", "TOF PostScript", 600, 800);
  TPostScript *ps = new TPostScript(psFile, type);
  Float_t width  = 20;
  Float_t height = 29;
  ps->Range(width,height);
  //c->UseCurrentStyle();
  Int_t yoko = 4;
  Int_t tate = 8;
  Int_t npad = tate*yoko;
  c->Divide(yoko,tate);
  // # of slat
  const Int_t  NTOF = 960;

  // Define the time stamp for database access
  PHTimeStamp TimeStamp = PHTimeStamp(2000,4,5,0,0,0);

  // Set up the modules
  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofCalibObject* TofCalib = new TofCalibObject();
  TofCalibrator* TofCalibrator = new TofCalibrator();

  if (verbose>10) printf("Calling TofAddress\n");
  TofAddress->setTimeStamp(TimeStamp);
  TofAddress->fetchFromFile("toffemmap.txt","tofcablemap.txt");

  if (verbose>10) printf("Calling TofGeometry\n");
  TofGeometry->setTimeStamp(TimeStamp);
  //TofGeometry->setEastCarriage(-44.0, 0.0, 0.0);
  TofGeometry->fetchFromFile("tofpanelgeo.txt","tofslatoffset.txt");

  if (verbose>10) printf("Calling TofCalib\n");
  TofCalib->setTimeStamp(TimeStamp);
  //TofCalib->fetchPedestalFromFile("tofPedestal.txt", TofAddress);
  //TofCalib->fetchTvcConvFromFile("tofTvcConv.txt", TofAddress);
  //TofCalib->fetchQvcConvFromFile("tofQvcConv.txt", TofAddress);
  //TofCalib->fetchSlewParFromFile("tofSlewPar.txt");
  //TofCalib->fetchToffsetFromFile("tofToffset.txt.pass0");
  TofCalib->fetchToffsetFromFile("tofToffset.txt");
  //TofCalib->fetchToffsetFromFile("tmpToffset.txt.pass2");
  //TofCalib->fetchYoffsetFromFile("tofYoffset.txt.pass0");
  //TofCalib->fetchVelocityFromFile("tofVelocity.txt.pass0");
  //TofCalib->fetchElossConvFromFile("tofElossConv.txt");
  //TofCalib->fetchGlobalTFromFile("tofGlobalT.txt");

  // Offset calc
  int slatid, slatbin;
  float scinti_vlight; //[cm/ns]
  float diff, y0, t0, par1;
  float tdiff, tsigma;
  float gmin, gmax, lmin, lmax, emin, emax, mip;
  double timepar[3], chargepar[5];
  char name[20],title[80];

  TH1D *tofTime2dProj[NTOF];
  TH1D *tofT2dProj[NTOF];

  // Checking Histgram
  int slatbin = 1000; float slatmin = -19.5; float slatmax = 980.5;

  TH2F *tofTdiff2d = new TH2F("tofTdiff2d", "newT0 - oldT0:slatid", 
			      slatbin,slatmin,slatmax,200,-0.5,0.5);
  tofTdiff2d->SetXTitle("Slat ID");
  tofTdiff2d->SetYTitle("[ns]");
  tofTdiff2d->SetMarkerColor(4);

  TH1F *tofTdiff = new TH1F("tofTdiff", "newT0 - oldT0", 100,-0.2,0.2);
  tofTdiff->SetXTitle("[ns]");
  tofTdiff->SetFillColor(5);

  TH1F *tofToffset = new TH1F("tofToffset", "T offset", 80,0.0,40.0);
  tofToffset->SetXTitle("[ns]");
  tofToffset->SetFillColor(5);

  Int_t pad = 1;
  tofhfile->cd();
  for(int id = 0; id < NTOF; id++){
  //for(int id = 0; id < 32; id++){
    slatid = id;
    if((pad-1)%npad == 0){
      cout<<"  ## ps->NewPage(); pad =  "<<pad<<"  npad = "<<npad<<endl;
      pad = 1;
      c->Update();
      ps->NewPage();
    }
    Int_t slatbin = tofSlatT->FindBin(slatid);
    Int_t entry = tofSlatT->GetBinContent(slatbin);
    cout<<" slatid =  "<<slatid;

    // T0 calc
    c->cd(pad++);
    sprintf(name,"time%d",slatid);
    sprintf(title,"Time %d ",slatid);
    tofT2dProj[id] = tofT2d->ProjectionY(name,slatbin,slatbin);
    tofT2dProj[id]->SetTitle(title);
    tofT2dProj[id]->SetXTitle("[ns]");
    tofT2dProj[id]->SetEntries(entry);
    //if(entry<100)tofT2dProj[id]->Rebin(2);
    if(entry<50)tofT2dProj[id]->Rebin(2);
    Int_t peakbin = tofT2dProj[id]->GetMaximumBin();
    Float_t peak  = tofT2dProj[id]->GetBinCenter(peakbin);
    Float_t maximum = tofT2dProj[id]->GetMaximum();
    gmin = peak - 0.5;
    gmax = peak + 0.5;
    TF1* fitg = new TF1("fitg","gaus",gmin,gmax);
    tofT2dProj[id]->Fit("fitg","RQ0");

    tdiff  = fitg->GetParameter(1);
    tsigma = fitg->GetParameter(2);

    sprintf(title,"%s: %f",title,tdiff);
    tofT2dProj[id]->SetTitle(title);
    tofT2dProj[id]->Draw();

    //tdiff -= -9.76492e-03; // for 0.2-0.6GeV/c
    //tdiff -= 1.05054e-02; // for 0.6-1.2GeV/c
    //tdiff -= 1.55602e-02; // for 1.2-2.0GeV/c

    // set new calibration parameter
    Float_t oldT0 = TofCalib->getToffset(slatid);
    t0 = oldT0 + tdiff;

    //if(entry>0&&fabs(tdiff)<0.20&&tsigma<0.5){
    if(entry>0&&tsigma<0.5){
      // checking histgram
      tofToffset->Fill(t0);
      tofTdiff2d->Fill(slatid,tdiff);
      tofTdiff->Fill(tdiff);
      cout<<"\t # of entry = "<<entry;
      cout<<"\t t0 = "<<t0<<" ns";
      cout<<"  = "<<oldT0<<" + "<<tdiff;
      TofCalib->setToffset(slatid,t0);
      //TofCalib->setElossConv(slatid,elossconv);
    } else {
      cout<<"\t # of entry = "<<entry;
      cout<<"\t mean = "<<tdiff<<" ns";
      TofCalib->setToffset(slatid,oldT0);
    }   
    cout<<"  sigma "<<tsigma;
    cout<<endl;

  }
  TofCalib->writeToffsetToFile("tmpToffset.txt");
  //TofCalib->writeElossConvToFile("tmpElossConv.txt");

  ps->Close();
  cout<<"Write ps file = "<<psFile<<endl;

  // Let's look at the checking histograms
  gStyle->SetOptStat(1111);
  gStyle->SetOptFit(1);
  gStyle->SetTitleW(0.3);
  gStyle->SetTitleH(0.05);
  gStyle->SetStatW(0.2);
  gStyle->SetStatH(0.2);
  TCanvas *c0 = new TCanvas("c0", "TOF Checking histgram", 600, 800);
  c0->Divide(1,3);
  c0->cd(1);
  tofSlatT->Draw();
  c0->cd(2);
  tofTdiff2d->Draw();
  c0->cd(3);
  c0_3->Divide(2,1);
  c0_3->cd(1);
  tofToffset->Draw();
  c0_3->cd(2);
  tofTdiff->Fit("gaus");
  c0->Print(psFile2);
}


