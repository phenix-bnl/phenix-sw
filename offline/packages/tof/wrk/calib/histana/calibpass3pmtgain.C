// calcoffset.C
//  KEY: TH1F     tofSlat;1       TOF hits/slatid
//  KEY: TH2F     tofQvcL2d;1     qvc[0]:slatid
//  KEY: TH2F     tofQvcU2d;1     qvc[1]:slatid

void calibpass3pmtgain(const char *histFile="tofcalib.root", 
		       const char *psFile="pass3gain.ps",
		       const Int_t page = 0){
  // setup
  gROOT->SetStyle("Plain");
  gStyle->SetTitleW(0.4);
  gStyle->SetTitleH(0.1);
  gStyle->SetStatW(0.2);
  gStyle->SetStatH(0.2);
  //gStyle->SetOptStat(10);        // only for numEvents
  gStyle->SetOptStat(101);
  gStyle->SetOptFit(1);
  Int_t verbose = 7;
  Int_t type = 111;    // portrait
  //Int_t type = 112;     // landscape
  TFile *tofhfile = new TFile(histFile);
  TCanvas *c = new TCanvas("c", "TOF PostScript", 600, 800);
  TPostScript *ps = new TPostScript(psFile, type);
  Float_t width  = 20;
  Float_t height = 28;
  ps->Range(width,height);
  //Int_t yoko = 8;
  //Int_t tate = 12;
  Int_t yoko = 4;
  Int_t tate = 8;
  Int_t npad = yoko*tate;
  c->Divide(yoko,tate);
  const Int_t  NTOF = 960;

  // Offset calc
  Int_t slatid, slatbin, entry, entryL, entryU;
  Int_t lwidth;
  Float_t lmin, lmax, emin, emax;
  Float_t mip, mipsigma, elossconv;
  Float_t mipL, mipU, mipLsigma, mipUsigma;
  Double_t par[3];
  Double_t chargepar[5];
  char name[20],title[80];

  // Checking Histgram
  int slatbin = 1000; float slatmin = -19.5; float slatmax = 980.5;

  TH2F *tofPmtGain2d = new TH2F("tofPmtGain2d", "PMT Gain:slatid", 
				slatbin,slatmin,slatmax,200,0,2000);
  tofPmtGain2d->SetXTitle("Slat ID");
  tofPmtGain2d->SetYTitle("[ch]");
  tofPmtGain2d->SetMarkerColor(4);
  TH1F *tofPmtGain = new TH1F("tofPmtGain", "PMT Gain", 100,0,2000);
  tofPmtGain->SetFillColor(5);

  TH1F *tofPmtGainL = new TH1F("tofPmtGainL", "PMT Gain (long)", 100,0,2000);
  tofPmtGainL->SetFillColor(5);

  TH1F *tofPmtGainSa = new TH1F("tofPmtGainSa", "PMT Gain (short-180)", 
				100,0,2000);
  tofPmtGainSa->SetFillColor(5);

  TH1F *tofPmtGainSb = new TH1F("tofPmtGainSb", "PMT Gain (short-90)",
				100,0,2000);
  tofPmtGainSb->SetFillColor(5);


  TH1D *tofQvcL2dProj;
  TH1D *tofQvcU2dProj;
  TH1D *tofCharge2dProj;

  // output file
  ofstream PmtGainFile("tmpPmtGain.txt",ios::out);

  Int_t pad = 1;
  for(slatid = 0; slatid < 960; slatid++){
  //for(slatid = 16*page; slatid < 16*(page+1); slatid++){
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
    slatbin = tofSlatD->FindBin(slatid);
    entry = tofSlatD->GetBinContent(slatbin);
    // Pmt Gain calc
    //## Lower
    c->cd(pad++);
    sprintf(name,"qvcL%d",slatid);
    sprintf(title,"QvcL %d ",slatid);
    tofQvcL2dProj = tofQvcL2d->ProjectionY(name,slatbin,slatbin);
    entryL = tofQvcL2dProj->Integral(0,50);
    tofQvcL2dProj->SetEntries(entryL);
    tofQvcL2dProj->SetTitle(title);
    tofQvcL2dProj->SetXTitle("[ch]");
    lmin = tofQvcL2dProj->GetMean()*0.6;
    lmax = tofQvcL2dProj->GetMean()*2.0;
    if(slatid == 40)lmin = tofQvcL2dProj->GetMean()*0.8;
    if(slatid == 66)lmin = tofQvcL2dProj->GetMean()*0.8;
    if(slatid == 80)lmin = tofQvcL2dProj->GetMean()*0.8;
    if(slatid == 81)lmin = tofQvcL2dProj->GetMean()*0.8;
    if(slatid == 82)lmin = tofQvcL2dProj->GetMean()*0.8;
    if(slatid ==234){lmin = 300; lmax = 500;}
    if(slatid ==602)lmin = tofQvcL2dProj->GetMean()*0.3;
    if(slatid ==702)lmin = tofQvcL2dProj->GetMean()*0.8;
    if(slatid ==731)lmin = tofQvcL2dProj->GetMean()*0.8;
    if(slatid ==749)lmin = tofQvcL2dProj->GetMean()*0.8;

    TF1* fitg = new TF1("fitg","landau",lmin,lmax);
    //TF1* fitg = new TF1("fitg","gaus",lmin,lmax);
    fitg->SetLineWidth(0.4);
    fitg->SetLineColor(4);
    tofQvcL2dProj->Fit("fitg","RQ");
    fitg->GetParameters(par);
    mipL = mipLsigma = 0; // Initialize
    if(entryL>20&&par[1]>0&&par[1]<2000){
      mipL = (Float_t)par[1];  //mip [ch]
      mipLsigma = (Float_t)par[2];  //sigma [ch]
    }
    sprintf(title,"%s - %d ",title,(int)mipL);
    tofQvcL2dProj->SetTitle(title);
    tofQvcL2dProj->Draw();

    //## Upper
    c->cd(pad++);
    sprintf(name,"qvcU%d",slatid);
    sprintf(title,"QvcU %d ",slatid);
    tofQvcU2dProj = tofQvcU2d->ProjectionY(name,slatbin,slatbin);
    entryU = tofQvcU2dProj->Integral(0,50);
    tofQvcU2dProj->SetEntries(entryU);
    tofQvcU2dProj->SetTitle(title);
    tofQvcU2dProj->SetXTitle("[ch]");
    lmin = tofQvcU2dProj->GetMean()*0.6;
    lmax = tofQvcU2dProj->GetMean()*2.0;
    if(slatid == 92)lmin = tofQvcU2dProj->GetMean()*0.8;
    if(slatid ==234){lmin = 200; lmax = 400;}
    if(slatid ==670)lmin = tofQvcU2dProj->GetMean()*0.8;
    if(slatid ==684)lmin = tofQvcU2dProj->GetMean()*0.4;
    if(slatid ==702)lmin = tofQvcU2dProj->GetMean()*0.7;
    if(slatid ==731)lmin = tofQvcU2dProj->GetMean()*0.8;
    if(slatid ==752)lmin = tofQvcU2dProj->GetMean()*0.8;

    TF1* fitg = new TF1("fitg","landau",lmin,lmax);
    //TF1* fitg = new TF1("fitg","gaus",lmin,lmax);
    fitg->SetLineWidth(0.4);
    fitg->SetLineColor(4);
    tofQvcU2dProj->Fit("fitg","RQ0");
    fitg->GetParameters(par);
    mipU = mipUsigma = 0; // Initialize
    if(entryU>20&&par[1]>0&&par[1]<2000){
      mipU = (Float_t)par[1];  //mip [ch]
      mipUsigma = (Float_t)par[2];  //sigma [ch]
    }
    sprintf(title,"%s - %d ",title,(int)mipU);
    tofQvcU2dProj->SetTitle(title);
    tofQvcU2dProj->Draw();

    cout<<" slatid = "<<slatid;
    cout<<"  L: "<<mipL<<" "<<mipLsigma;
    cout<<"  U: "<<mipU<<" "<<mipUsigma;
    cout<<"  entry = "<<entry<<" "<<entryL<<" "<<entryU<<endl;

    tofPmtGain2d->Fill(slatid,mipL);
    tofPmtGain2d->Fill(slatid,mipU);
    tofPmtGain->Fill(mipL);
    tofPmtGain->Fill(mipU);
    if(slatL)tofPmtGainL->Fill(mipL);
    if(slatL)tofPmtGainL->Fill(mipU);
    if(slatSd)tofPmtGainSb->Fill(mipL);
    if(slatSd)tofPmtGainSa->Fill(mipU);
    if(slatSu)tofPmtGainSa->Fill(mipL);
    if(slatSu)tofPmtGainSb->Fill(mipU);

    // debug
    PmtGainFile<<"   "<<slatid<<"\t"
	       <<mipL<<"\t\t"<<mipLsigma<<"\t\t"
	       <<mipU<<"\t\t"<<mipUsigma<<endl;
  }
  PmtGainFile.close();
  ps->Close();
  cout<<"Write ps file = "<<psFile<<endl;

  // Let's look at the checking histograms
  TCanvas *c0 = new TCanvas("c0", "TOF Checking histgram", 600, 800);
  c0->Divide(1,4);
  c0->cd(1);
  tofSlatD->Draw();
  c0->cd(2);
  tofPmtGain2d->Draw();
  c0->cd(3);
  c0_3->Divide(2,1);
  c0_3->cd(1);
  tofPmtGain->Draw();
  c0_3->cd(2);
  tofPmtGainL->Draw();
  c0->cd(4);
  c0_4->Divide(2,1);
  c0_4->cd(1);
  tofPmtGainSa->Draw();
  c0_4->cd(2);
  tofPmtGainSb->Draw();
}


