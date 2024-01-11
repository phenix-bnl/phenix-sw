// calcoffset.C
//  KEY: TH1F     tofSlatD;1       TOF hits/slatid
//  KEY: TH2F     tofTime2d;1     Time - L/C:slatid
//  KEY: TH3F     tofDiff3d;1     (T_l - T_u)/2:slatid
//  KEY: TH3F     tofQvcL3d;1     qvc[0]:TOF:slatid
//  KEY: TH3F     tofQvcU3d;1     qvc[1]:TOF:slatid

void calibpass12offset(const char *histFile="tofcalib.root", const char *psFile="pass12.ps"){
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
  gStyle->SetStatW(0.4);
  gStyle->SetStatH(0.5);
  gStyle->SetOptStat(0);        // no 
  //gStyle->SetOptStat(10);        // only for numEvents
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
  Int_t yoko = 6;
  Int_t tate = 8;
  Int_t npad = tate*yoko;
  c->Divide(yoko,tate);
  // # of slat
  const Int_t  NTOF = 960;
  //const Int_t  NTOF = 48;

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
  TofCalib->fetchPedestalFromFile("tofPedestal.txt", TofAddress);
  TofCalib->fetchTvcConvFromFile("tofTvcConv.txt", TofAddress);
  //TofCalib->fetchQvcConvFromFile("tofQvcConv.txt", TofAddress);
  //TofCalib->fetchSlewParFromFile("tofSlewPar.txt");
  TofCalib->fetchToffsetFromFile("tofToffset.txt");
  TofCalib->fetchYoffsetFromFile("tofYoffset.txt");
  TofCalib->fetchVelocityFromFile("tofVelocity.txt");
  //TofCalib->fetchElossConvFromFile("tofElossConv.txt");
  //TofCalib->fetchGlobalTFromFile("tofGlobalT.txt");

  // Offset calc
  int slatid, slatbin;
  float scinti_vlight; //[cm/ns]
  float diffmean, diff, y0, t0, par1;
  float gmin, gmax, lmin, lmax, emin, emax, mip;
  double timepar[3], chargepar[5];
  char name[20],title[80];

  TH2D      *tofDiffYpos[NTOF];
  TH1D      *tofDiffYpx;
  TProfile  *tofDiffYpfx;
  TH1D      *tofDiffYsly;
  TH1D      *tofDiff2dProj;
  TH1D      *tofYpos2dProj;
  TH1D      *tofZpos2dProj;
  TH1D      *tofTime2dProj;
  TLatex    *text;

  // Checking Histgram
  int slatbin = 1000; float slatmin = -19.5; float slatmax = 980.5;

  TH2F *tofVelocity2d = new TH2F("tofVelocity2d", "Light Velocity:slatid", 
				 slatbin,slatmin,slatmax,100,10.0,20.0);
  tofVelocity2d->SetXTitle("Slat ID");
  tofVelocity2d->SetYTitle("[cm/ns]");
  tofVelocity2d->SetMarkerColor(4);

  TH1F *tofVelocity = new TH1F("tofVelocity", "Light Velocity", 100,10.0,20.0);
  tofVelocity->SetXTitle("[cm/ns]");
  tofVelocity->SetFillColor(5);

  TH1F *tofVelocityL = new TH1F("tofVelocityL","Light Velocity (long slat)",
				100,10.,20);
  tofVelocityL->SetXTitle("[cm/ns]");
  tofVelocityL->SetFillColor(5);
  TH1F *tofVelocityS = new TH1F("tofVelocityS","Light Velocity (short slat)",
				100,10.,20.);
  tofVelocityS->SetXTitle("[cm/ns]");
  tofVelocityS->SetFillColor(5);

  TH2F *tofYdiff2d = new TH2F("tofYdiff2d", "newY0 - oldY0:slatid",
			      slatbin,slatmin,slatmax,100,-10.0,10.0);
  tofYdiff2d->SetXTitle("Slat ID");
  tofYdiff2d->SetYTitle("[cm]");
  tofYdiff2d->SetMarkerColor(4);

  TH1F *tofYdiff = new TH1F("tofYdiff", "newY0 - oldY0", 100,-10.0,10.0);
  tofYdiff->SetXTitle("[cm]");
  tofYdiff->SetFillColor(5);

  TH1F *tofYdiffL = new TH1F("tofYdiff", "newY0 - oldY0 (long slat)",
			     100,-10.0,10.0);
  tofYdiffL->SetXTitle("[cm]");
  tofYdiffL->SetFillColor(5);
  TH1F *tofYdiffS = new TH1F("tofYdiff", "newY0 - oldY0 (short slat)",
			     100,-10.0,10.0);
  tofYdiffS->SetXTitle("[cm]");
  tofYdiffS->SetFillColor(5);

  TH2F *tofTdiff2d = new TH2F("tofTdiff2d", "newT0 - oldT0:slatid", 
			      slatbin,slatmin,slatmax,200,-1.0,4.0);
  tofTdiff2d->SetXTitle("Slat ID");
  tofTdiff2d->SetYTitle("[ns]");
  tofTdiff2d->SetMarkerColor(4);

  TH1F *tofTdiff = new TH1F("tofTdiff", "newT0 - oldT0", 200,-1.0,4.0);
  tofTdiff->SetXTitle("[ns]");
  tofTdiff->SetFillColor(5);

  TH1F *tofToffset = new TH1F("tofToffset", "T offset", 80,0.0,40.0);
  tofToffset->SetXTitle("[ns]");
  tofToffset->SetFillColor(5);

  Int_t pad = 1;
  tofhfile->cd();
  for(int id = 0; id < NTOF; id++){
  //for(int id = 128; id < 144; id++){
    slatid = id;
    Int_t panel = slatid/96;
    Int_t slat  = slatid%96;
    bool  slatL  = slat>=16&&slat<80;
    bool  slatSd = slat>=0&&slat<16;
    bool  slatSu = slat>=80&&slat<96;
    bool  slatS  = slatSd||slatSu;

    if((pad-1)%npad == 0){
      cout<<"  ## ps->NewPage(); slatid =  "<<slatid<<"  npad = "<<npad<<endl;
      pad = 1;
      c->Update();
      ps->NewPage();
    }
    Int_t slatbin = tofSlatD->FindBin(slatid);
    cout<<" slatid =  "<<slatid;
    // Y0 calc
    c->cd(pad++);
    sprintf(name,"diff%d",slatid);
    sprintf(title,"Diff %d ",slatid);
    tofDiff2dProj = tofDiff2d->ProjectionY(name,slatbin,slatbin);
    tofDiff2dProj->SetTitle(title);
    tofDiff2dProj->Draw();
    diffmean = tofDiff2dProj->GetMean();

    c->cd(pad++);
    sprintf(title,"Ypos:Diff %d ",slatid);
    tofDiff3d->GetXaxis()->SetRange(slatbin,slatbin);
    tofDiffYpos[id] = (TH2D*)tofDiff3d->Project3D("yz");
    tofDiffYpos[id]->SetTitle(title);
    tofDiffYpos[id]->SetXTitle("[cm]"); // tofDiff3d Z
    tofDiffYpos[id]->SetYTitle("[ns]"); // tofDiff3d Y

    tofDiffYpx = tofDiffYpos[id]->ProjectionX();
    if(slatL){
      lmin = tofDiffYpx->GetMean() - 30;
      lmax = tofDiffYpx->GetMean() + 30;
      if(lmin < -32.0)lmin = -32.0; 
      if(lmax >  32.0)lmax =  32.0;
    }
    if(slatS){
      lmin = tofDiffYpx->GetMean() - 20;
      lmax = tofDiffYpx->GetMean() + 20;
      if(lmin < -22.0)lmin = -22.0; 
      if(lmax >  22.0)lmax =  22.0;
    }
    cout<<" line: "<<lmin<<" "<<lmax;
    TF1* line = new TF1("line","pol1",lmin,lmax);
    Int_t cutmin = 50 + (diffmean-4)*100/20;
    Int_t cutmax = 50 + (diffmean+4)*100/20;
    if(cutmin<0)  cutmin = 0;
    if(cutmax>100)cutmax = 100;
    tofDiffYpos[id]->Draw("colz");

    //tofDiffYpfx = tofDiffYpos[id]->ProfileX("",cutmin,cutmax);
    //tofDiffYpfx->Fit("line","ERQ","same");
    tofDiffYpos[id]->FitSlicesY();
    tofDiffYsly = (TH1D*)gDirectory->Get("tofDiff3d_yz_1");
    tofDiffYsly->Fit("line","ERQ","same");

    diff = line->GetParameter(0);
    par1 = line->GetParameter(1);
    scinti_vlight = 14.76;
    if(par1>0) scinti_vlight = 1./par1;
    y0 = diff*scinti_vlight;
    sprintf(title,"Y0:%6.4f", y0);
    text = new TLatex(-40, -7, title);
    text->SetTextColor(2);
    text->SetTextSize(0.08);
    text->Draw();
    sprintf(title,"V :%6.4f", scinti_vlight);
    text = new TLatex(-40, -9, title);
    text->SetTextColor(2);
    text->SetTextSize(0.08);
    text->Draw();

    // dY dZ
    //c->cd(pad++);
    //sprintf(name,"dy%d",slatid);
    //tofYpos2dProj = tofYpos2d->ProjectionY(name,slatbin,slatbin);
    //tofYpos2dProj->SetXTitle("[cm]");
    //tofYpos2dProj->Fit("gaus","Q0");
    //par1 = gaus->GetParameter(1);
    //sprintf(title,"%f",par1);
    //tofYpos2dProj->SetTitle(title);
    //tofYpos2dProj->Draw();
    //c->cd(pad++);
    //sprintf(name,"dz%d",slatid);
    //tofZpos2dProj = tofZpos2d->ProjectionY(name,slatbin,slatbin);
    //tofZpos2dProj->SetXTitle("[cm]");
    //tofZpos2dProj->Fit("gaus","Q0");
    //par1 = gaus->GetParameter(1);
    //sprintf(title,"%f",par1);
    //tofZpos2dProj->SetTitle(title);
    //tofZpos2dProj->Draw();

    // T0 calc
    c->cd(pad++);
    sprintf(name,"time%d",slatid);
    sprintf(title,"Time %d ",slatid);
    tofTime2dProj = tofTime2d->ProjectionY(name,slatbin,slatbin);
    tofTime2dProj->SetTitle(title);
    tofTime2dProj->SetXTitle("[ns]");
    Int_t peakbin = tofTime2dProj->GetMaximumBin();
    Float_t peak  = tofTime2dProj->GetBinCenter(peakbin);
    Float_t maximum = tofTime2dProj->GetMaximum();
    gmin = peak - 2.;
    gmax = peak + 2.;
    TF1* fitg = new TF1("fitg","gaus",gmin,gmax);
    tofTime2dProj->Fit("fitg","RQ0");
    t0 = fitg->GetParameter(1);

    sprintf(title,"%f",t0);
    tofTime2dProj->SetTitle(title);
    tofTime2dProj->Draw();

    // set new calibration parameter
    Float_t slatlength = TofGeometry->getSlatLength(slatid);
    Float_t oldY0 = TofCalib->getYoffset(slatid);
    Float_t oldT0 = TofCalib->getToffset(slatid);
    Float_t ydiff = y0 - oldY0;
    Float_t tdiff = t0 - oldT0;

    Int_t entry = tofSlatD->GetBinContent(slatbin);
    if(entry > 100&&par1>0&&scinti_vlight>13.0&&scinti_vlight<16.5){
      // checking histgram
      tofVelocity2d->Fill(slatid,scinti_vlight);
      tofVelocity->Fill(scinti_vlight);
      tofToffset->Fill(t0);
      tofYdiff2d->Fill(slatid,ydiff);
      tofYdiff->Fill(ydiff);
      tofTdiff2d->Fill(slatid,tdiff);
      tofTdiff->Fill(tdiff);
      if(slatlength > 50){
	tofVelocityL->Fill(scinti_vlight);
	tofYdiffL->Fill(ydiff);
      } else {
	tofVelocityS->Fill(scinti_vlight);
	tofYdiffS->Fill(ydiff);
      }
      cout<<"\t t0 = "<<t0<<" ns ; diff = "<<diff<<" ns ; y0 = "<<y0<<" cm";
      cout<<" ; v = "<<scinti_vlight<<" cm/ns";
      TofCalib->setVelocity(slatid,scinti_vlight);
      TofCalib->setYoffset(slatid,y0);
      TofCalib->setToffset(slatid,t0);
      //TofCalib->setElossConv(slatid,elossconv);
    } else {
      cout<<"\t # of entry = "<<entry;
      TofCalib->setVelocity(slatid,14.76);
      TofCalib->setYoffset(slatid,oldY0);
      TofCalib->setToffset(slatid,oldT0);
      //TofCalib->setToffset(slatid,oldT0 + 1.092);
    }
    cout<<endl;

  }
  TofCalib->writeVelocityToFile("tmpVelocity.txt");
  TofCalib->writeYoffsetToFile("tmpYoffset.txt");
  TofCalib->writeToffsetToFile("tmpToffset.txt");

  ps->Close();
  cout<<"Write ps file = "<<psFile<<endl;

  // Let's look at the checking histograms
  TCanvas *c0 = new TCanvas("c0", "TOF Checking histgram", 600, 800);
  c0->Divide(1,4);
  c0->cd(1);
  tofSlatD->Draw();
  c0->cd(2);
  tofVelocity2d->Draw();
  c0->cd(3);
  tofYdiff2d->Draw();
  c0->cd(4);
  tofTdiff2d->Draw();

  // 
  gStyle->SetOptStat(1111);
  gStyle->SetOptFit(1);
  gStyle->SetTitleW(0.3);
  gStyle->SetTitleH(0.05);
  gStyle->SetStatW(0.2);
  gStyle->SetStatH(0.2);
  TCanvas *c1 = new TCanvas("c1", "TOF Checking histgram", 600, 800);
  c1->Divide(2,4);
  c1->cd(1);
  tofVelocity->Fit("gaus");
  c1->cd(2);
  tofYdiff->Draw();
  c1->cd(3);
  tofVelocityL->Fit("gaus");
  c1->cd(4);
  tofYdiffL->Draw();
  c1->cd(5);
  tofVelocityS->Fit("gaus");
  c1->cd(6);
  tofYdiffS->Draw();

  c1->cd(7);
  tofToffset->Draw();
  c1->cd(8);
  tofTdiff->Fit("gaus");
}


