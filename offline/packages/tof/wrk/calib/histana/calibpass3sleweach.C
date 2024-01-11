// calcoffset.C
//  KEY: TH3F     tofQvcL3d;1     qvc[0]:TOF:slatid
//  KEY: TH3F     tofQvcU3d;1     qvc[1]:TOF:slatid

void calibpass3sleweach(const char *histFile="tofcalib.root", 
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
  gStyle->SetTitleW(0.6);
  gStyle->SetTitleH(0.1);
  gStyle->SetStatW(0.4);
  gStyle->SetStatH(0.3);
  gStyle->SetOptStat(10);        // only for numEvents
  gStyle->SetOptFit(1);
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
  const Int_t  NTOF8 = NTOF/8;

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
  //TofCalib->fetchYoffsetFromFile("tofYoffset.txt.pass0");
  //TofCalib->fetchVelocityFromFile("tofVelocity.txt.pass0");
  //TofCalib->fetchElossConvFromFile("tofElossConv.txt");
  //TofCalib->fetchGlobalTFromFile("tofGlobalT.txt");
  TofCalib->fetchMipPeakFromFile("tofMipPeak.txt.pass0");

  // Offset calc
  int slatid, slatbin;
  float scinti_vlight; //[cm/ns]
  float diff, y0, t0, par1;
  float a0, a1, b0, b1;
  float a8, b8;
  float gmin, gmax, lmin, lmax, emin, emax, mip;
  float smin, smax, mippeak, mipsigma;
  double timepar[3], chargepar[5];
  char name[20], title[80], slewtext[80];

  TH2D      *tofSlewL[NTOF];
  TH2D      *tofSlewU[NTOF];
  TH1D      *tofQvc;
  TProfile  *tofSlewLpfx[NTOF];
  TProfile  *tofSlewUpfx[NTOF];
  TLatex   *text;

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
			      slatbin,slatmin,slatmax,250,-10.0,40.0);
  tofSlewB2d->SetXTitle("Slat ID");
  tofSlewB2d->SetYTitle("[ns]");
  tofSlewB2d->SetMarkerColor(4);
  TH1F *tofSlewB = new TH1F("tofSlewB", "slewpar_B", 500,-10.0,40.0);
  tofSlewB->SetFillColor(5);


  Int_t pad = 1;
  tofhfile->cd();
  for(int id = 0; id < NTOF; id++){
    slatid = id;
    if((pad-1)%npad == 0){
      cout<<"  ## ps->NewPage(); slatid =  "<<slatid<<"  npad = "<<npad<<endl;
      pad = 1;
      c->Update();
      ps->NewPage();
    }
    Int_t slatbin = tofSlatD->FindBin(slatid);
    cout<<" slatid =  "<<slatid;

    c->cd(pad++); //[1]
    tofQvcL3d->GetXaxis()->SetRange(slatbin,slatbin);
    tofSlewL[id] = (TH2D*)tofQvcL3d->Project3D("yz");
    tofSlewL[id]->Draw("colz");
    tofQvc = (TH1D*)tofSlewL[id]->ProjectionX("tofQvc",20,70); 
    lmin = tofQvc->GetMean()*0.4;
    lmax = tofQvc->GetMean()*3.0;
    //TF1* fitqvc = new TF1("fitqvc","landau",lmin,lmax);
    TF1* fitqvc = new TF1("fitqvc","gaus",lmin,lmax);
    Int_t lwidth = 6*tofQvc->GetLineWidth();
    fitqvc->SetLineWidth(lwidth);
    fitqvc->SetLineColor(4);
    tofQvc->Fit("fitqvc","RQ0");
    mippeak  = (Float_t)fitqvc->GetParameter(1);  //mip [ch]
    mipsigma = (Float_t)fitqvc->GetParameter(2);  //sigma [ch]
    smin = mippeak*0.20;
    smax = mippeak+mipsigma*3.0;
    if(smin<50)   smin = 50;
    if(smax>1700) smax = 1700;
    TF1* slew = new TF1("slew","[0]+[1]/sqrt(x)",smin,smax);
    slew->SetLineColor(4);
    slew->SetLineWidth(4);
    slew->SetParNames("A","B");
    //tofSlewLpfx[id] = tofSlewL[id]->ProfileX("",35,45); // -1.0<tof<2.0ns
    tofSlewLpfx[id] = tofSlewL[id]->ProfileX("",20,70); // -1.0<tof<2.0ns
    tofSlewLpfx[id]->Fit("slew","ERQ","same");
    a0 = slew->GetParameter(0);
    b0 = slew->GetParameter(1);
    sprintf(slewtext,"A = %6.3f +- %5.4f", 
	    slew->GetParameter(0), slew->GetParError(0));
    text = new TLatex(100, -1.2, slewtext);
    text->SetTextColor(2);
    text->SetTextSize(0.10);
    text->Draw();
    sprintf(slewtext,"B = %6.3f +- %6.5f", 
	    slew->GetParameter(1), slew->GetParError(1));
    text = new TLatex(100, -1.8, slewtext);
    text->SetTextColor(2);
    text->SetTextSize(0.10);
    text->Draw();

    c->cd(pad++); //[1]
    tofQvcU3d->GetXaxis()->SetRange(slatbin,slatbin);
    tofSlewU[id] = (TH2D*)tofQvcL3d->Project3D("yz");
    tofSlewU[id]->Draw("colz");
    tofQvc = (TH1D*)tofSlewU[id]->ProjectionX("tofQvc",20,70); 
    lmin = tofQvc->GetMean()*0.4;
    lmax = tofQvc->GetMean()*3.0;
    //TF1* fitqvc = new TF1("fitqvc","landau",lmin,lmax);
    TF1* fitqvc = new TF1("fitqvc","gaus",lmin,lmax);
    Int_t lwidth = 6*tofQvc->GetLineWidth();
    fitqvc->SetLineWidth(lwidth);
    fitqvc->SetLineColor(4);
    tofQvc->Fit("fitqvc","RQ0");
    mippeak  = (Float_t)fitqvc->GetParameter(1);  //mip [ch]
    mipsigma = (Float_t)fitqvc->GetParameter(2);  //sigma [ch]
    smin = mippeak*0.20;
    smax = mippeak+mipsigma*3.0;
    if(smin<50)   smin = 50;
    if(smax>1700) smax = 1700;
    TF1* slew = new TF1("slew","[0]+[1]/sqrt(x)",smin,smax);
    slew->SetLineColor(4);
    slew->SetLineWidth(4);
    slew->SetParNames("A","B");
    //tofSlewUpfx[id] = tofSlewU[id]->ProfileX("",35,45); // -1.0<tof<2.0ns
    tofSlewUpfx[id] = tofSlewU[id]->ProfileX("",20,70); // -1.0<tof<2.0ns
    tofSlewUpfx[id]->Fit("slew","ERQ","same");
    a1 = slew->GetParameter(0);
    b1 = slew->GetParameter(1);
    sprintf(slewtext,"A = %6.3f +- %5.4f", 
	    slew->GetParameter(0), slew->GetParError(0));
    text = new TLatex(100, -1.2, slewtext);
    text->SetTextColor(2);
    text->SetTextSize(0.10);
    text->Draw();
    sprintf(slewtext,"B = %6.3f +- %6.5f", 
	    slew->GetParameter(1), slew->GetParError(1));
    text = new TLatex(100, -1.8, slewtext);
    text->SetTextColor(2);
    text->SetTextSize(0.10);
    text->Draw();

    // set new calibration parameter
    Int_t entry = tofSlatT->GetBinContent(slatbin);
    if(entry > 50){
      // checking histgram
      tofSlewA2d->Fill(slatid,a0);
      tofSlewA2d->Fill(slatid,a1);
      tofSlewA->Fill(a0);
      tofSlewA->Fill(a1);
      tofSlewB2d->Fill(slatid,b0);
      tofSlewB2d->Fill(slatid,b1);
      tofSlewB->Fill(b0);
      tofSlewB->Fill(b1);

      cout<<"\t A = "<<a0<<"  "<<a1<<" ; B = "<<b0<<" "<<b1;
      TofCalib->setSlewPar_a(0,slatid,a0);
      TofCalib->setSlewPar_a(1,slatid,a1);
      TofCalib->setSlewPar_b(0,slatid,b0);
      TofCalib->setSlewPar_b(1,slatid,b1);
    } else {
      cout<<"\t # of entry = "<<entry;
      TofCalib->setSlewPar_a(0,slatid, -0.6);
      TofCalib->setSlewPar_a(1,slatid, -0.6);
      TofCalib->setSlewPar_b(0,slatid, 12.0);
      TofCalib->setSlewPar_b(1,slatid, 12.0);
    }
    cout<<endl;

  }
  TofCalib->writeSlewParToFile("tmpSlewPar.txt");

  ps->Close();
  cout<<"Write ps file = "<<psFile<<endl;


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


