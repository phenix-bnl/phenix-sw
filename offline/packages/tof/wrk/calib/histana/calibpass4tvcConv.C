// calcoffset.C
//  KEY: TH1F     tofSlatT;1       TOF hits/slatid
//  KEY: TH2F     tofT2d;1         Time - L/C:slatid

void calibpass4tvcConv(const char *histFile="tofcalib.root", 
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
  TFile *tofhfile = new TFile(histFile);
  TCanvas *c = new TCanvas("c", "TOF PostScript", 600, 800);
  TPostScript *ps = new TPostScript(psFile, type);
  Float_t width  = 20;
  Float_t height = 26;
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
  TofCalib->fetchTvcConvFromFile("tofTvcConv.txt", TofAddress);
  TofCalib->fetchToffsetFromFile("tofToffset.txt");
  //TofCalib->fetchToffsetFromFile("tmpToffset.txt.pass2");

  // Offset calc
  int slatid, slatbin;
  float scinti_vlight; //[cm/ns]
  float diff, y0, t0, par1;
  float tdiff, tsigma;
  float tconv, talpha, tbeta, talpha_err;
  float gmin, gmax, lmin, lmax, emin, emax, mip;
  double timepar[3], chargepar[5];
  char name[20],title[80], convtext[60];

  TH1D *tofTime2dProj[NTOF];
  TH1D *tofT2dProj[NTOF];

  TH2D      *tofTbbct0[NTOF];
  TProfile  *tofTbbct0pfx;
  TH1D      *tofTbbct0sly;
  TLatex    *text;

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

  TH2F *tofTconv2d = new TH2F("tofTconv2d", "Tconv:slatid", 
			      slatbin,slatmin,slatmax,200,0.9,1.1);
  tofTconv2d->SetXTitle("Slat ID");
  tofTconv2d->SetMarkerColor(4);
  TH1F *tofTconv = new TH1F("tofTconv", "T conv", 100,0.95,1.05);
  tofTconv->SetFillColor(5);

  TH1F *tofToffset = new TH1F("tofToffset", "T offset", 80,0.0,40.0);
  tofToffset->SetXTitle("[ns]");
  tofToffset->SetFillColor(5);

  Int_t pad = 1;
  tofhfile->cd();
  for(int id = 0; id < NTOF; id++){
  //for(int id = 400; id < 416; id++){
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
    gmin = peak - 1.;
    gmax = peak + 1.;
    TF1* fitg = new TF1("fitg","gaus",gmin,gmax);
    tofT2dProj[id]->Fit("fitg","RQ0");

    tdiff  = fitg->GetParameter(1);
    tsigma = fitg->GetParameter(2);

    sprintf(title,"%s: %f",title,tdiff);
    tofT2dProj[id]->SetTitle(title);
    tofT2dProj[id]->Draw();

    c->cd(pad++);
    sprintf(name,"T_t0bbc%d",slatid);
    sprintf(title,"T_t0bbc %d ",slatid);
    tofTbb3d->GetXaxis()->SetRange(slatbin,slatbin);
    tofTbbct0[id] = (TH2D*)tofTbb3d->Project3D("zy");
    tofTbbct0[id]->SetName(name);
    tofTbbct0[id]->SetXTitle("t0 [ns]");
    tofTbbct0[id]->SetYTitle("T [ns]");
    tofTbbct0[id]->Draw("colz");
    Int_t cutmin = 25 + (tdiff-4*tsigma)*10;
    Int_t cutmax = 25 + (tdiff+4*tsigma)*10;
    tofTbbct0pfx = tofTbbct0[id]->ProfileX("",cutmin,cutmax);
    tofTbbct0pfx->Fit("pol1","RQ","same",-7,7);

    tbeta  = pol1->GetParameter(0);
    talpha = pol1->GetParameter(1);
    talpha_err = pol1->GetParError(1);
    tconv = 1/(1+talpha);
    sprintf(convtext,"A = %7.6f +- %6.5f", talpha, talpha_err);
    text = new TLatex(-9.5, -2.3, convtext);
    text->SetTextColor(2);
    text->SetTextSize(0.1);
    text->Draw();
    // set new calibration parameter
    Float_t oldT0 = TofCalib->getToffset(slatid);
    t0 = oldT0 + tdiff;

    if(entry > 20&&fabs(tdiff)<0.30&&tsigma<0.5){
      tofTconv2d->Fill(slatid,tconv);
      if(tconv>0.975&&tconv<1.025){
	tofTconv->Fill(tconv);
	// set new calibration parameter
	Float_t oldConv0 = TofCalib->getTvcConv(0,slatid);
	Float_t oldConv1 = TofCalib->getTvcConv(1,slatid);
	TofCalib->setTvcConv(0,slatid,oldConv0*tconv);
	TofCalib->setTvcConv(1,slatid,oldConv1*tconv);

	t0 = t0*tconv;
      }
      // checking histgram
      tofToffset->Fill(t0);
      tofTdiff2d->Fill(slatid,tdiff);
      tofTdiff->Fill(tdiff);

      cout<<"\t # of entry = "<<entry;
      cout<<"\t t0 = "<<t0<<" ns";
      cout<<"  = "<<oldT0<<" + "<<tdiff;
      cout<<"\t tconv = "<<tconv<<" "<<talpha;;
      TofCalib->setToffset(slatid,t0);
    } else {
      cout<<"\t # of entry = "<<entry;
      TofCalib->setToffset(slatid,oldT0);
    }   
    cout<<endl;

  }
  TofCalib->writeToffsetToFile("tmpToffset.txt");
  TofCalib->writeTvcConvToFile("tmpTvcConv.txt", TofAddress);

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
  c0->Divide(1,4);
  c0->cd(1);
  tofSlatT->Draw();
  c0->cd(2);
  tofTdiff2d->Draw();
  c0->cd(3);
  tofTconv2d->Draw();
  c0->cd(4);
  c0_4->Divide(2,1);
  c0_4->cd(1);
  //tofToffset->Draw();
  tofTconv->Fit("gaus");
  c0_4->cd(2);
  tofTdiff->Fit("gaus");
  c0->Print(psFile2);
}


