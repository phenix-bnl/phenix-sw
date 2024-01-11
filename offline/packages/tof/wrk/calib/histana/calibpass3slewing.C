// calcoffset.C
//  KEY: TH3F     tofQvcL3d;1     qvc[0]:TOF:slatid
//  KEY: TH3F     tofQvcU3d;1     qvc[1]:TOF:slatid

void calibpass3slewing(const char *histFile="tofcalib.root", 
		       const char *psFile="pass3.ps"){
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
  gStyle->SetTitleW(0.4);
  gStyle->SetTitleH(0.05);
  gStyle->SetStatW(0.3);
  gStyle->SetStatH(0.2);
  gStyle->SetOptStat(11);
  gStyle->SetOptFit(1);
  Int_t verbose = 7;
  Int_t type = 111;    // portrait
  TFile *tofhfile = new TFile(histFile);
  TCanvas *c = new TCanvas("c", "TOF PostScript", 600, 800);
  TPostScript *ps = new TPostScript(psFile, type);
  Float_t width  = 20;
  Float_t height = 28;
  ps->Range(width,height);
  Int_t yoko = 4;
  Int_t tate = 4;
  Int_t npad = tate*yoko;
  c->Divide(yoko,tate);
  c_1->SetLogz();
  c_2->SetLogz();
  c_3->SetLogz();
  c_4->SetLogz();
  c_5->SetLogz();
  c_6->SetLogz();
  c_7->SetLogz();
  c_8->SetLogz();
  c_9->SetLogz();
  c_10->SetLogz();
  c_11->SetLogz();
  c_12->SetLogz();
  c_13->SetLogz();
  c_14->SetLogz();
  c_15->SetLogz();
  c_16->SetLogz();
  // # of slat
  const Int_t  NTOF = 960;

  // Set up the modules
  TofAddressObject* TofAddress = new TofAddressObject();
  TofGeometryObject* TofGeometry = new TofGeometryObject();
  TofCalibObject* TofCalib = new TofCalibObject();

  // Slewing calc
  Int_t   slatid, slatbin;
  Float_t a0, a1, b0, b1, par_a[10][8], par_b[10][8];
  Float_t gmin, gmax, lmin, lmax, smin, smax, mippeak, mipsigma;
  Char_t  name[20], name2[80], title[80], slewtext[80];
  Float_t mipL, mipU, mipLsigma, mipUsigma;
  Float_t mip[960][2], pmtfactor[960][2];
  Float_t mip_mean[10][8], tmp_mean[2], tmp;
  Int_t   mip_count[10][8];

  // Checking Histgram
  int slatbin = 1000; float slatmin = -19.5; float slatmax = 980.5;

  TH2F *tofSlewA2d = new TH2F("tofSlewA2d", "slewpar_A:slatid", 
			      slatbin,slatmin,slatmax,200,-3.0,1.0);
  tofSlewA2d->SetXTitle("Slat ID");
  tofSlewA2d->SetYTitle("[ns]");
  tofSlewA2d->SetMarkerColor(4);
  TH1F *tofSlewA = new TH1F("tofSlewA", "slewpar_A", 200,-2.5,1.5);
  tofSlewA->SetFillColor(5);

  TH2F *tofSlewB2d = new TH2F("tofSlewB2d", "slewpar_B:slatid", 
			      slatbin,slatmin,slatmax,250,0.0,50.0);
  tofSlewB2d->SetXTitle("Slat ID");
  tofSlewB2d->SetYTitle("[ns]");
  tofSlewB2d->SetMarkerColor(4);
  TH1F *tofSlewB = new TH1F("tofSlewB", "slewpar_B", 50,0.0,50.0);
  tofSlewB->SetFillColor(5);

  TH2F     *tofSlew2d[10][8];
  TH2F     *tofSlew2dTmp;
  TH1D     *tofQvc;
  TProfile *tofSlewPfx;
  TH1D     *tofSlewSlicesY;
  TLatex   *text;

  Int_t pad = 1;
  tofhfile->cd();
  for(Int_t ipanel = 0; ipanel<10; ipanel++){
  //for(Int_t ipanel = 2; ipanel<4; ipanel++){
    for(Int_t islat = 0; islat<8; islat++){
      if((pad-1)%npad == 0){
	cout<<"  ## ps->NewPage(); pad =  "<<pad<<"  npad = "<<npad<<endl;
	pad = 1;
	c->Update();
	ps->NewPage();
      }
      sprintf(name, "tofSlew%d_%d", ipanel, islat);
      tofSlew2d[ipanel][islat] = (TH2F*)gDirectory->Get(name)->Clone();
      tofSlew2d[ipanel][islat]->SetMinimum(0.1);

      c->cd(pad++);
      tofSlew2d[ipanel][islat]->Draw("colz"); // select pion peak
      tofQvc = (TH1D*)tofSlew2d[ipanel][islat]->ProjectionX("tofQvc",20,70); 
      lmin = tofQvc->GetMean()*0.6;
      lmax = tofQvc->GetMean()*2.0;
      TF1* fitqvc = new TF1("fitqvc","landau",lmin,lmax);
      //TF1* fitqvc = new TF1("fitqvc","gaus",lmin,lmax);
      Int_t lwidth = 6*tofQvc->GetLineWidth();
      fitqvc->SetLineWidth(lwidth);
      fitqvc->SetLineColor(4);
      tofQvc->Fit("fitqvc","RQ0");
      mippeak  = (Float_t)fitqvc->GetParameter(1);  //mip [ch]
      mipsigma = (Float_t)fitqvc->GetParameter(2);  //sigma [ch]
      smin = mippeak*0.2;
      smax = mippeak*1.8;
      //smax = mippeak+mipsigma*8.0;
      if(smin<50)   smin = 50;
      //if(smax>1700) smax = 1700;
      TF1* slew = new TF1("slew","[0]+[1]/sqrt(x)",smin,smax);
      slew->SetLineColor(4);
      slew->SetLineWidth(4);
      slew->SetParNames("A","B");
      //Int_t slicesmax =  (mippeak+mipsigma*4.0)/40;
      Int_t slicesmin =  smin/40;
      Int_t slicesmax =  (smax+200)/40;
      TF1* fitg = new TF1("fitg","gaus",-1.0,1.5);
      tofSlew2d[ipanel][islat]->FitSlicesY(fitg, 2, slicesmax);
      sprintf(name, "%s_1", name);
      tofSlewSlicesY = (TH1D*)gDirectory->Get(name);
      tofSlewSlicesY->Fit("slew","RQ","same");
      par_a[ipanel][islat] = slew->GetParameter(0);
      par_b[ipanel][islat] = slew->GetParameter(1);
      sprintf(slewtext,"A = %6.3f +- %5.4f", 
	      slew->GetParameter(0), slew->GetParError(0));
      text = new TLatex(50, -1.5, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();
      sprintf(slewtext,"B = %6.3f +- %6.5f", 
	      slew->GetParameter(1), slew->GetParError(1));
      text = new TLatex(60, -1.9, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();

      cout<<"  "<<ipanel<<" - "<<islat;
      cout<<" MIP peak: "<<mippeak<<" sigma: "<<mipsigma;
      cout<<"| B = "<<slew->GetParameter(1);
      cout<<" + "<<slew->GetParError(1)<<endl;
      //c->cd(pad++);
      //tofQvc->SetFillColor(5);
      //tofQvc->Draw();
    }
  }

  ps->Close();
  cout<<"Write ps file = "<<psFile<<endl;

  // Open 
  ifstream PmtGainFile;
  PmtGainFile.open("tofPmtGain.txt.pass3");
  if(!PmtGainFile){ 
    cerr << "  Can not open tofPmtGain.txt.pass3 file." << endl;
    cerr << endl;
  }else{ 
    while(!PmtGainFile.eof()){
      PmtGainFile >> slatid >> mipL >> mipLsigma >> mipU >> mipUsigma;
      mip[slatid][0] = mipL;
      mip[slatid][1] = mipU;
    }
  }
  // Initialize
  for(Int_t ipanel = 0; ipanel<10; ipanel++){
    for(Int_t islat = 0; islat<8; islat++){
      mip_mean[ipanel][islat] = 0.0;
      mip_count[ipanel][islat] = 0;
    }
  }
  for(int id = 0; id < NTOF; id++){
    slatid = id;
    Int_t panel = slatid/96;
    Int_t slat  = slatid%96;
    bool  slatL  = slat>=16&&slat<80;
    bool  slatLa = slatL&&((slat%16)/4==0);
    bool  slatLb = slatL&&((slat%16)/4==1);
    bool  slatLc = slatL&&((slat%16)/4==2);
    bool  slatLd = slatL&&((slat%16)/4==3);
    bool  slatSd = slat>=0&&slat<16;
    bool  slatSu = slat>=80&&slat<96;
    bool  slatS  = slatSd||slatSu;

    if(mip[slatid][0]>0){
      if(slatL) {mip_mean[panel][0] += mip[slatid][0]; mip_count[panel][0]++;}
      if(slatS) {mip_mean[panel][1] += mip[slatid][0]; mip_count[panel][1]++;}
      if(slatSd){mip_mean[panel][3] += mip[slatid][0]; mip_count[panel][3]++;}
      if(slatSu){mip_mean[panel][2] += mip[slatid][0]; mip_count[panel][2]++;}
      if(slatLa){mip_mean[panel][4] += mip[slatid][0]; mip_count[panel][4]++;}
      if(slatLb){mip_mean[panel][5] += mip[slatid][0]; mip_count[panel][5]++;}
      if(slatLc){mip_mean[panel][6] += mip[slatid][0]; mip_count[panel][6]++;}
      if(slatLd){mip_mean[panel][7] += mip[slatid][0]; mip_count[panel][7]++;}
    }
    if(mip[slatid][1]>0){
      if(slatL) {mip_mean[panel][0] += mip[slatid][1]; mip_count[panel][0]++;}
      if(slatS) {mip_mean[panel][1] += mip[slatid][1]; mip_count[panel][1]++;}
      if(slatSd){mip_mean[panel][2] += mip[slatid][1]; mip_count[panel][2]++;}
      if(slatSu){mip_mean[panel][3] += mip[slatid][1]; mip_count[panel][3]++;}
      if(slatLa){mip_mean[panel][4] += mip[slatid][1]; mip_count[panel][4]++;}
      if(slatLb){mip_mean[panel][5] += mip[slatid][1]; mip_count[panel][5]++;}
      if(slatLc){mip_mean[panel][6] += mip[slatid][1]; mip_count[panel][6]++;}
      if(slatLd){mip_mean[panel][7] += mip[slatid][1]; mip_count[panel][7]++;}
    }
  }
  for(Int_t ipanel = 0; ipanel<10; ipanel++){
    for(Int_t islat = 0; islat<8; islat++){
      if(mip_count[ipanel][islat]!=0){
	tmp = mip_mean[ipanel][islat]/(Float_t)mip_count[ipanel][islat];
	mip_mean[ipanel][islat] = tmp;
      }
    }
  }


  for(int id = 0; id < NTOF; id++){
    slatid = id;

    Int_t panel = slatid/96;
    Int_t slat  = slatid%96;
    bool  slatL  = slat>=16&&slat<80;
    bool  slatLa = slatL&&((slat%16)/4==0);
    bool  slatLb = slatL&&((slat%16)/4==1);
    bool  slatLc = slatL&&((slat%16)/4==2);
    bool  slatLd = slatL&&((slat%16)/4==3);
    bool  slatSd = slat>=0&&slat<16;
    bool  slatSu = slat>=80&&slat<96;
    bool  slatS  = slatSd||slatSu;

    if(slatSd){a0 = par_a[panel][3]; b0 = par_b[panel][3];}//90
    if(slatSd){tmp_mean[0] = mip_mean[panel][3];}//90
    if(slatSd){a1 = par_a[panel][2]; b1 = par_b[panel][2];}//180
    if(slatSd){tmp_mean[1] = mip_mean[panel][2];}//180
    if(slatSu){a0 = par_a[panel][2]; b0 = par_b[panel][2];}//180
    if(slatSu){tmp_mean[0] = mip_mean[panel][2];}//180
    if(slatSu){a1 = par_a[panel][3]; b1 = par_b[panel][3];}//90
    if(slatSu){tmp_mean[1] = mip_mean[panel][3];}//90
    if(slatLa){a0 = par_a[panel][4]; b0 = par_b[panel][4];}
    if(slatLa){a1 = par_a[panel][4]; b1 = par_b[panel][4];}
    if(slatLa){tmp_mean[0] = tmp_mean[1] = mip_mean[panel][4];}
    if(slatLb){a0 = par_a[panel][5]; b0 = par_b[panel][5];}
    if(slatLb){a1 = par_a[panel][5]; b1 = par_b[panel][5];}
    if(slatLb){tmp_mean[0] = tmp_mean[1] = mip_mean[panel][5];}
    if(slatLc){a0 = par_a[panel][6]; b0 = par_b[panel][6];}
    if(slatLc){a1 = par_a[panel][6]; b1 = par_b[panel][6];}
    if(slatLc){tmp_mean[0] = tmp_mean[1] = mip_mean[panel][6];}
    if(slatLd){a0 = par_a[panel][7]; b0 = par_b[panel][7];}
    if(slatLd){a1 = par_a[panel][7]; b1 = par_b[panel][7];}
    if(slatLd){tmp_mean[0] = tmp_mean[1] = mip_mean[panel][7];}

    pmtfactor[slatid][0] = pmtfactor[slatid][1] = 1.0; // Initialize;
    if(mip[slatid][0]>0) pmtfactor[slatid][0] = tmp_mean[0]/mip[slatid][0];
    if(mip[slatid][1]>0) pmtfactor[slatid][1] = tmp_mean[0]/mip[slatid][1];
    //cout<<"  slatid = "<<slatid;
    //cout<<"  L: "<<mip[slatid][0]<<" "<<tmp_mean[0];
    //cout<<" "<<pmtfactor[slatid][0];
    //cout<<"\tU: "<<mip[slatid][1]<<" "<<tmp_mean[1];
    //cout<<" "<<pmtfactor[slatid][1];
    //cout<<endl;

    if(pmtfactor[slatid][0]) b0 = b0/sqrt(pmtfactor[slatid][0]);
    if(pmtfactor[slatid][1]) b1 = b1/sqrt(pmtfactor[slatid][1]);

    // set new calibration parameter
    if(panel>=0){
      // checking histgram
      tofSlewA2d->Fill(slatid,a0);
      tofSlewA2d->Fill(slatid,a1);
      tofSlewA->Fill(a0);
      tofSlewA->Fill(a1);
      tofSlewB2d->Fill(slatid,b0);
      tofSlewB2d->Fill(slatid,b1);
      tofSlewB->Fill(b0);
      tofSlewB->Fill(b1);

      //cout<<"\t A = "<<a0<<"  "<<a1<<" ; B = "<<b0<<" "<<b1<<endl;
      TofCalib->setSlewPar_a(0,slatid,a0);
      TofCalib->setSlewPar_a(1,slatid,a1);
      TofCalib->setSlewPar_b(0,slatid,b0);
      TofCalib->setSlewPar_b(1,slatid,b1);
    } else {
      TofCalib->setSlewPar_a(0,slatid, -0.6);
      TofCalib->setSlewPar_a(1,slatid, -0.6);
      TofCalib->setSlewPar_b(0,slatid, 12.0);
      TofCalib->setSlewPar_b(1,slatid, 12.0);
    }
  }
  TofCalib->writeSlewParToFile("tmpSlewPar.txt");

  gStyle->SetOptStat(1111);
  gStyle->SetOptFit(1);
  // Let's look at the checking histograms
  char *w = new char[1];
  TCanvas *c0 = new TCanvas("c0", "TOF Checking histgram", 600, 800);
  c0->Divide(1,4);
  c0->cd(1);
  tofSlatT->Draw();
  c0->cd(2);
  tofSlewA2d->Draw();
  c0->cd(3);
  tofSlewB2d->Draw();
  c0->cd(4);
  c0_4->Divide(2,1);
  c0_4->cd(1);
  tofSlewA->Draw();
  c0_4->cd(2);
  tofSlewB->Draw();

}


