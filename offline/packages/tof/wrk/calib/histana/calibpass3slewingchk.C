// calcoffset.C
//  KEY: TH3F     tofQvcL3d;1     qvc[0]:TOF:slatid
//  KEY: TH3F     tofQvcU3d;1     qvc[1]:TOF:slatid

void calibpass3slewingchk(const char *histFile="tofcalib.root"){

  // setup
  gROOT->SetStyle("Plain");
  gStyle->SetTitleW(0.4);
  gStyle->SetTitleH(0.05);
  gStyle->SetStatW(0.3);
  gStyle->SetStatH(0.2);
  //gStyle->SetOptStat(10);        // only for numEvents
  gStyle->SetOptStat(11);
  gStyle->SetOptFit(1);
  Int_t verbose = 7;
  Int_t type = 111;    // portrait
  //Int_t type = 112;     // landscape
  TFile *tofhfile = new TFile(histFile);
  TCanvas *c = new TCanvas("c", "TOF PostScript", 600, 800);
  Float_t width  = 20;
  Float_t height = 29;
  Int_t yoko = 2;
  Int_t tate = 3;
  Int_t npad = tate*yoko;
  c->Divide(yoko,tate);
  c_1->SetLogz();
  c_2->SetLogz();
  c_3->SetLogz();
  c_4->SetLogz();
  c_5->SetLogz();
  c_6->SetLogz();
  //c_7->SetLogz();
  //c_8->SetLogz();
  // # of slat
  const Int_t  NTOF = 960;

  // Slewing calc
  Int_t   slatid, slatbin;
  Float_t a0, a1, b0, b1, par_a[10][8], par_b[10][8];
  Float_t gmin, gmax, lmin, lmax, smin, smax, mippeak, mipsigma;
  Char_t  name[20], name2[80], title[80], slewtext[80];
  Int_t   slicesmin, slicesmax;

  TH2F     *tofSlewPos2d[10][8][6];
  TH2F     *tofSlew2d[10][8];
  TH2F     *tofSlewS2d[10][8];
  TH2F     *tofSlewT2d[10][8];
  TH2F     *tofSqvc2d[10][8];
  TH2F     *tofEloss2d[10][8];
  TH1D     *tofQvc;
  TProfile *tofSlewPosPfx;
  TProfile *tofSlewPfx;
  TProfile *tofSqvcPfx;
  TProfile *tofElossPfx;
  TProfile *tofSlewSPfx;
  TProfile *tofSlewTPfx;

  TH1D     *tofSlewPosSlicesY;
  TH1D     *tofSlewSlicesY;
  TH1D     *tofSqvcSlicesY;
  TH1D     *tofElossSlicesY;
  TH1D     *tofSlewSSlicesY;
  TH1D     *tofSlewTSlicesY;
  TLatex   *text;

  Int_t pad = 1;
  tofhfile->cd();
  //for(Int_t ipanel = 0; ipanel<10; ipanel++){
  for(Int_t ipanel = 2; ipanel<3; ipanel++){
    //for(Int_t islat = 0; islat<8; islat++){
    for(Int_t islat = 5; islat<6; islat++){
      if((pad-1)%npad == 0){
	cout<<"  ## ps->NewPage(); pad =  "<<pad<<"  npad = "<<npad<<endl;
	pad = 1;
	c->Update();
      }


      c->cd(pad++); //tofSlewT
      sprintf(name, "tofSlewT%d_%d", ipanel, islat);
      tofSlewT2d[ipanel][islat] = (TH2F*)gDirectory->Get(name)->Clone();
      tofSlewT2d[ipanel][islat]->SetMinimum(0.1);
      tofSlewT2d[ipanel][islat]->Draw("colz");
      tofQvc = (TH1D*)tofSlewT2d[ipanel][islat]->ProjectionX("tofQvc",20,70); 
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
      smin = mippeak*0.2;
      smax = mippeak*1.8;
      if(smin<50)   smin = 50;
      if(smax>1800) smax = 1500;
      if(ipanel==0&&islat==4)smax = mippeak+mipsigma*2.0; // South edge
      if(ipanel==7&&islat==7)smax = mippeak+mipsigma*2.0; // North edge
      TF1* slew = new TF1("slew","[0]+[1]/sqrt(x)",smin,smax);
      slew->SetLineColor(4);
      slew->SetLineWidth(8);
      slew->SetParNames("A","B");
      //tofSlewTPfx = tofSlewT2d[ipanel][islat]->ProfileX("tofSlewTPfx",20,70); 
      //tofSlewTPfx->Fit("slew","ERQ","same");
      slicesmin =  smin/40;
      slicesmax =  (smax+200)/40;
      TF1* fitg = new TF1("fitg","gaus",-1.0,1.5);
      tofSlewT2d[ipanel][islat]->FitSlicesY(fitg, 2, slicesmax);
      sprintf(name, "%s_1", name);
      tofSlewTSlicesY = (TH1D*)gDirectory->Get(name);
      tofSlewTSlicesY->Fit("slew","RQ","same");
      //par_a[ipanel][islat] = slew->GetParameter(0);
      //par_b[ipanel][islat] = slew->GetParameter(1);
      sprintf(slewtext,"A = %6.3f +- %5.4f", 
	      slew->GetParameter(0), slew->GetParError(0));
      text = new TLatex(200, -1.3, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();
      sprintf(slewtext,"B = %6.3f +- %6.5f", 
	      slew->GetParameter(1), slew->GetParError(1));
      text = new TLatex(200, -1.8, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();

      c->cd(pad++); //tofSqvc
      sprintf(name, "tofSqvc%d_%d", ipanel, islat);
      tofSqvc2d[ipanel][islat] = (TH2F*)gDirectory->Get(name)->Clone();
      tofSqvc2d[ipanel][islat]->SetMinimum(0.1);
      tofSqvc2d[ipanel][islat]->Draw("colz");
      tofQvc = (TH1D*)tofSqvc2d[ipanel][islat]->ProjectionX("tofQvc",20,70); 
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
      smax = mippeak*1.8;
      if(smin<50)   smin = 50;
      if(smax>1800) smax = 1500;
      if(ipanel==0&&islat==4)smax = mippeak+mipsigma*2.0; // South edge
      if(ipanel==7&&islat==7)smax = mippeak+mipsigma*2.0; // North edge
      TF1* slew = new TF1("slew","[0]+[1]/sqrt(x)",smin,smax);
      slew->SetLineColor(4);
      slew->SetLineWidth(8);
      slew->SetParNames("A","B");
      //tofSqvcPfx = tofSqvc2d[ipanel][islat]->ProfileX("tofSqvcPfx",20,70); 
      //tofSqvcPfx->Fit("slew","ERQ","same");
      slicesmin =  smin/40;
      slicesmax =  (smax+200)/40;
      TF1* fitg = new TF1("fitg","gaus",-1.0,1.5);
      tofSqvc2d[ipanel][islat]->FitSlicesY(fitg, 2, slicesmax);
      sprintf(name, "%s_1", name);
      tofSqvcSlicesY = (TH1D*)gDirectory->Get(name);
      tofSqvcSlicesY->Fit("slew","RQ","same");
      //par_a[ipanel][islat] = slew->GetParameter(0);
      //par_b[ipanel][islat] = slew->GetParameter(1);
      sprintf(slewtext,"A = %6.3f +- %5.4f", 
	      slew->GetParameter(0), slew->GetParError(0));
      text = new TLatex(200, -1.3, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();
      sprintf(slewtext,"B = %6.3f +- %6.5f", 
	      slew->GetParameter(1), slew->GetParError(1));
      text = new TLatex(200, -1.8, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();

      c->cd(pad++); //tofSlew
      sprintf(name, "tofSlew%d_%d", ipanel, islat);
      tofSlew2d[ipanel][islat] = (TH2F*)gDirectory->Get(name)->Clone();
      tofSlew2d[ipanel][islat]->SetMinimum(0.1);
      tofSlew2d[ipanel][islat]->Draw("colz");
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
      //smax = mippeak+mipsigma*3.0;
      if(smin<50)   smin = 50;
      if(smax>1800) smax = 1500;
      if(ipanel==0&&islat==4)smax = mippeak+mipsigma*2.0; // South edge
      if(ipanel==7&&islat==7)smax = mippeak+mipsigma*2.0; // North edge
      TF1* slew = new TF1("slew","[0]+[1]/sqrt(x)",smin,smax);
      slew->SetLineColor(4);
      slew->SetLineWidth(8);
      slew->SetParNames("A","B");
      //tofSlewPfx = tofSlew2d[ipanel][islat]->ProfileX("tofSlewPfx",20,70); 
      //tofSlewPfx->Fit("slew","ERQ","same");
      slicesmin =  smin/40;
      slicesmax =  (smax+200)/40;
      TF1* fitg = new TF1("fitg","gaus",-1.0,1.5);
      tofSlew2d[ipanel][islat]->FitSlicesY(fitg, 2, slicesmax);
      sprintf(name, "%s_1", name);
      tofSlewSlicesY = (TH1D*)gDirectory->Get(name);
      tofSlewSlicesY->Fit("slew","RQ","same");
      //par_a[ipanel][islat] = slew->GetParameter(0);
      //par_b[ipanel][islat] = slew->GetParameter(1);
      sprintf(slewtext,"A = %6.3f +- %5.4f", 
	      slew->GetParameter(0), slew->GetParError(0));
      text = new TLatex(200, -1.3, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();
      sprintf(slewtext,"B = %6.3f +- %6.5f", 
	      slew->GetParameter(1), slew->GetParError(1));
      text = new TLatex(200, -1.8, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();


//        c->cd(pad++); // tofSlewS
//        sprintf(name, "tofSlewS%d_%d", ipanel, islat);
//        tofSlewS2d[ipanel][islat] = (TH2F*)gDirectory->Get(name)->Clone();
//        tofSlewS2d[ipanel][islat]->SetMinimum(0.1);
//        tofSlewS2d[ipanel][islat]->Draw("colz");
//        tofQvc = (TH1D*)tofSlewS2d[ipanel][islat]->ProjectionX("tofQvc",20,70); 
//        lmin = tofQvc->GetMean()*0.4;
//        lmax = tofQvc->GetMean()*3.0;
//        //TF1* fitqvc = new TF1("fitqvc","landau",lmin,lmax);
//        TF1* fitqvc = new TF1("fitqvc","gaus",lmin,lmax);
//        Int_t lwidth = 6*tofQvc->GetLineWidth();
//        fitqvc->SetLineWidth(lwidth);
//        fitqvc->SetLineColor(4);
//        tofQvc->Fit("fitqvc","RQ0");
//        mippeak  = (Float_t)fitqvc->GetParameter(1);  //mip [ch]
//        mipsigma = (Float_t)fitqvc->GetParameter(2);  //sigma [ch]
//        smin = mippeak*0.2;
//        smax = mippeak*1.8;
//        if(smin<50)   smin = 50;
//        if(smax>1800) smax = 1500;
//        if(ipanel==0&&islat==4)smax = mippeak+mipsigma*2.0; // South edge
//        if(ipanel==7&&islat==7)smax = mippeak+mipsigma*2.0; // North edge
//        TF1* slew = new TF1("slew","[0]+[1]/sqrt(x)",smin,smax);
//        slew->SetLineColor(4);
//        slew->SetLineWidth(8);
//        slew->SetParNames("A","B");
//        //tofSlewSPfx = tofSlewS2d[ipanel][islat]->ProfileX("tofSlewSPfx",20,70); 
//        //tofSlewSPfx->Fit("slew","ERQ","same");
//        slicesmin =  smin/40;
//        slicesmax =  (smax+200)/40;
//        TF1* fitg = new TF1("fitg","gaus",-1.0,1.5);
//        tofSlewS2d[ipanel][islat]->FitSlicesY(fitg, 2, slicesmax);
//        sprintf(name, "%s_1", name);
//        tofSlewSSlicesY = (TH1D*)gDirectory->Get(name);
//        tofSlewSSlicesY->Fit("slew","RQ","same");
//        //par_a[ipanel][islat] = slew->GetParameter(0);
//        //par_b[ipanel][islat] = slew->GetParameter(1);
//        sprintf(slewtext,"A = %6.3f +- %5.4f", 
//  	      slew->GetParameter(0), slew->GetParError(0));
//        text = new TLatex(200, -1.3, slewtext);
//        text->SetTextColor(2);
//        text->SetTextSize(0.075);
//        text->Draw();
//        sprintf(slewtext,"B = %6.3f +- %6.5f", 
//  	      slew->GetParameter(1), slew->GetParError(1));
//        text = new TLatex(200, -1.8, slewtext);
//        text->SetTextColor(2);
//        text->SetTextSize(0.075);
//        text->Draw();

      c->cd(pad++);
      sprintf(name, "tofEloss%d_%d", ipanel, islat);
      tofEloss2d[ipanel][islat] = (TH2F*)gDirectory->Get(name)->Clone();
      tofEloss2d[ipanel][islat]->SetMinimum(0.1);
      tofEloss2d[ipanel][islat]->Draw("colz");
      TF1* slew = new TF1("slew","[0]+[1]/sqrt(x)",0.0005,0.005);
      slew->SetLineColor(4);
      slew->SetLineWidth(8);
      slew->SetParNames("A","B");
      //tofElossPfx = tofEloss2d[ipanel][islat]->ProfileX("tofElossPfx",20,70); 
      //tofElossPfx->Fit("slew","ERQ","same");
      TF1* fitg = new TF1("fitg","gaus",-1.0,1.5);
      tofEloss2d[ipanel][islat]->FitSlicesY(fitg, 1, 35);
      sprintf(name, "%s_1", name);
      tofElossSlicesY = (TH1D*)gDirectory->Get(name);
      tofElossSlicesY->Fit("slew","RQ","same");
      //par_a[ipanel][islat] = slew->GetParameter(0);
      //par_b[ipanel][islat] = slew->GetParameter(1);
      sprintf(slewtext,"A = %6.3f +- %5.4f", 
	      slew->GetParameter(0), slew->GetParError(0));
      text = new TLatex(0.0002, -1.5, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();
      sprintf(slewtext,"B = %7.5f +- %6.5f", 
	      slew->GetParameter(1), slew->GetParError(1));
      text = new TLatex(0.0002, -1.9, slewtext);
      text->SetTextColor(2);
      text->SetTextSize(0.075);
      text->Draw();

      cout<<"  "<<ipanel<<" - "<<islat<<endl;
    }
  }
}
