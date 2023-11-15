TGraphErrors* gra_gam_conv[4];
TGraphErrors* gra_pi0gam_conv[4];
TGraphErrors* gra_pi0_conv[4];

void ana_ntupledraw(char* infname="ana_ntuple.root",char* outfname="ana_ntupledraw_out.root"){

  //================================================================================ Initialization
  cout<<" Open file : "<<infname<<endl;
  TFile* f_in = new TFile(infname);
  // From primary/fkin information
  TH1F* h_pri_accgam_cut[4];
  TH1F* h_pri_accpi0gam_cut[4];
  TH1F* h_pri_accpi0_cut[4];
  // From dEmcGeaClusterTrack information
  TH1F* h_emcv0_accgam_cut[4];
  TH1F* h_emcv0_accpi0gam_cut[4];
  TH1F* h_emcv0_accpi0_cut[4];
  //
  char hname[256],htitle[256];
  char* cut[4] = { "None","West(PbSc)", "East top(PbSc)","East bottom(PbGl)"};
  int icut,icut_pi0,ibin;
  //
  icut = 4;
  while( icut-- ){
    //
    sprintf(hname,"h_pri_accgam_%d",icut);
    h_pri_accgam_cut[icut] = (TH1F*) f_in->Get(hname);
    sprintf(hname,"h_pri_accpi0gam_%d",icut);
    h_pri_accpi0gam_cut[icut] = (TH1F*) f_in->Get(hname);
    //
    sprintf(hname,"h_pri_accpi0_%d",icut);
    h_pri_accpi0_cut[icut] = (TH1F*) f_in->Get(hname);
    //
    sprintf(hname,"h_emcv0_accgam_%d",icut);
    h_emcv0_accgam_cut[icut] = (TH1F*) f_in->Get(hname);
    //
    sprintf(hname,"h_emcv0_accpi0gam_%d",icut);
    h_emcv0_accpi0gam_cut[icut] = (TH1F*) f_in->Get(hname);
    //
    sprintf(hname,"h_emcv0_accpi0_%d",icut);
    h_emcv0_accpi0_cut[icut] = (TH1F*) f_in->Get(hname);
  }
  //
  //================================================================================ Output
  icut = 4;
  while( icut-- ){
    sprintf(hname,"gra_gam_conv_%d",icut);
    gra_gam_conv[icut] = new TGraphErrors();
    gra_gam_conv[icut]->SetName(hname);
    //
    sprintf(hname,"gra_pi0gam_conv_%d",icut);
    gra_pi0gam_conv[icut] = new TGraphErrors();
    gra_pi0gam_conv[icut]->SetName(hname);
    //
    sprintf(hname,"gra_pi0_conv_%d",icut);
    gra_pi0_conv[icut] = new TGraphErrors();
    gra_pi0_conv[icut]->SetName(hname);
  }

  //================================================================================
  //  float mom_cut_min[] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 1.0, 1.5, 2.0 };
  //  float mom_cut_max[] = { 0.2, 0.3, 0.4, 0.5, 0.7, 1.0, 1.5, 2.0, 3.0 };
  //  int nn_mom_cut = 9;
  float mom_cut_min[] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 1.0, 2.0, 3.0, 4.0, 5.0, 7.0, 10.0 };
  float mom_cut_max[] = { 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1.1, 2.1, 3.1, 4.1, 5.1, 7.1, 10.1 };
  int nn_mom_cut = 13;
  float x_org[13],x_cut[13];
  int nn;
  float mom,rat,rat_err,mom_mean,mom_width;
  nn = nn_mom_cut;
  while( nn-- ){ x_org[nn] = 0; x_cut[nn] = 0; }

  icut = 4;
  while( icut-- ){
    TAxis* xa;
    // --- gam
    xa = h_emcv0_accgam_cut[icut]->GetXaxis();
    for( ibin = 1 ; ibin <= xa->GetLast(); ibin++ ){
      mom = xa->GetBinCenter(ibin);
      for( nn = 0 ; nn < nn_mom_cut && !( mom >= mom_cut_min[nn] && mom < mom_cut_max[nn] ) ; nn++ );
      //cout<<" icut : "<<icut<<"   nn = "<<nn<<"  mom = "<<mom<<"  ibin = "<<ibin<<endl;
      if( nn != nn_mom_cut ){
	x_org[nn] += h_pri_accgam_cut[icut]->GetBinContent(ibin);
	x_cut[nn] += h_emcv0_accgam_cut[icut]->GetBinContent(ibin);
      }
    }
    nn = nn_mom_cut;
    while( nn-- ){
      if( x_org[nn] > 0 ){
	rat = x_cut[nn] / x_org[nn];
	rat_err = sqrt( rat * ( 1 - rat ) / x_org[nn]  );
      } else {
	rat = 0.;
	rat_err = 0;
      }
      mom_mean = ( mom_cut_min[nn] + mom_cut_max[nn] ) / 2.0;
      mom_width = ( mom_cut_max[nn] - mom_cut_min[nn] ) / 2.0;
      gra_gam_conv[icut]->SetPoint(nn,mom_mean,rat);
      gra_gam_conv[icut]->SetPointError(nn,mom_width,rat_err);
    }
    // --- pi0gam
    xa = h_emcv0_accpi0gam_cut[icut]->GetXaxis();
    for( ibin = 1 ; ibin <= xa->GetLast(); ibin++ ){
      mom = xa->GetBinCenter(ibin);
      for( nn = 0 ; nn < nn_mom_cut && !( mom >= mom_cut_min[nn] && mom < mom_cut_max[nn] ) ; nn++ );
      //cout<<" icut : "<<icut<<"   nn = "<<nn<<"  mom = "<<mom<<"  ibin = "<<ibin<<endl;
      if( nn != nn_mom_cut ){
	x_org[nn] += h_pri_accpi0gam_cut[icut]->GetBinContent(ibin);
	x_cut[nn] += h_emcv0_accpi0gam_cut[icut]->GetBinContent(ibin);
      }
    }
    nn = nn_mom_cut;
    while( nn-- ){
      if( x_org[nn] > 0 ){
	rat = x_cut[nn] / x_org[nn];
	rat_err = sqrt( rat * ( 1 - rat ) / x_org[nn]  );
      } else {
	rat = 0.;
	rat_err = 0;
      }
      mom_mean = ( mom_cut_min[nn] + mom_cut_max[nn] ) / 2.0;
      mom_width = ( mom_cut_max[nn] - mom_cut_min[nn] ) / 2.0;
      gra_pi0gam_conv[icut]->SetPoint(nn,mom_mean,rat);
      gra_pi0gam_conv[icut]->SetPointError(nn,mom_width,rat_err);
    }
    // --- pi0
    xa = h_emcv0_accpi0_cut[icut]->GetXaxis();
    for( ibin = 1 ; ibin <= xa->GetLast(); ibin++ ){
      mom = xa->GetBinCenter(ibin);
      nn = nn_mom_cut;
      for( nn = 0 ; nn < nn_mom_cut && !( mom >= mom_cut_min[nn] && mom < mom_cut_max[nn] ) ; nn++ );
      //cout<<" icut : "<<icut<<"   nn = "<<nn<<"  mom = "<<mom<<"  ibin = "<<ibin<<endl;
      if( nn != nn_mom_cut ){
	x_org[nn] += h_pri_accpi0_cut[icut]->GetBinContent(ibin);
	x_cut[nn] += h_emcv0_accpi0_cut[icut]->GetBinContent(ibin);
      }
    }
    nn = nn_mom_cut;
    while( nn-- ){
      if( x_org[nn] > 0 ){
	rat = x_cut[nn] / x_org[nn];
	rat_err = sqrt( rat * ( 1 - rat ) / x_org[nn]  );
      } else {
	rat = 0.;
	rat_err = 0;
      }
      mom_mean = ( mom_cut_min[nn] + mom_cut_max[nn] ) / 2.0;
      mom_width = ( mom_cut_max[nn] - mom_cut_min[nn] ) / 2.0;
      gra_pi0_conv[icut]->SetPoint(nn,mom_mean,rat);
      gra_pi0_conv[icut]->SetPointError(nn,mom_width,rat_err);
    }

  }

  //================================================================================ Draw
  gStyle->SetOptStat(0);
  TCanvas* c1 = new TCanvas("c1","c1");
  c1->Divide(2,2);
  TH2F* h2_frame;

  c1->cd(1);
  h2_frame = new TH2F("h2_frame","Photon Conversion study",100,0,3,100,0.75,1.05);
  h2_frame->GetXaxis()->SetNdivisions(10);
  h2_frame->GetXaxis()->SetTitle("p_{T}(GeV/c)");
  h2_frame->GetYaxis()->SetNdivisions(10);
  h2_frame->GetYaxis()->SetTitle("photon survival probability");
  h2_frame->DrawCopy();
  delete h2_frame;
  icut = 4;
  while( icut-- > 1 ){
    gra_gam_conv[icut]->SetMarkerStyle(20 + icut);
    gra_gam_conv[icut]->SetMarkerSize(1.0);
    gra_gam_conv[icut]->SetMarkerColor(icut);
    gra_gam_conv[icut]->Draw("P");
  }
  //
  c1->cd(2);
  h2_frame = new TH2F("h2_frame","Photon Conversion study",100,0,3,100,0.75,1.05);
  h2_frame->GetXaxis()->SetNdivisions(10);
  h2_frame->GetXaxis()->SetTitle("p_{T}(GeV/c)");
  h2_frame->GetYaxis()->SetNdivisions(10);
  h2_frame->GetYaxis()->SetTitle("photon survival probability");
  h2_frame->DrawCopy();
  delete h2_frame;
  icut = 4;
  while( icut-- > 1 ){
    gra_pi0gam_conv[icut]->SetMarkerStyle(20 + icut);
    gra_pi0gam_conv[icut]->SetMarkerSize(1.0);
    gra_pi0gam_conv[icut]->SetMarkerColor(icut);
    gra_pi0gam_conv[icut]->Draw("P");
  }
  //
  c1->cd(3);
  h2_frame = new TH2F("h2_frame","#pi^{0} Conversion study",100,0,3,100,0.75,1.05);
  h2_frame->GetXaxis()->SetNdivisions(10);
  h2_frame->GetXaxis()->SetTitle("p_{T}(GeV/c)");
  h2_frame->GetYaxis()->SetNdivisions(10);
  h2_frame->GetYaxis()->SetTitle("#pi^{0} Survival probability");
  h2_frame->DrawCopy();
  delete h2_frame;
  icut = 4;
  while( icut-- > 1 ){
    gra_pi0_conv[icut]->SetMarkerStyle(20 + icut);
    gra_pi0_conv[icut]->SetMarkerSize(1.0);
    gra_pi0_conv[icut]->SetMarkerColor(icut);
    gra_pi0_conv[icut]->Draw("P");
  }

  /*
    // ---- Fittting


    c1->Clear();
    h2_frame = new TH2F("h2_frame","photon Conversion study",100,0,3,100,0.85,0.95);
    h2_frame->GetXaxis()->SetNdivisions(10);
    h2_frame->GetXaxis()->SetTitle("p_{T}(GeV/c)");
    h2_frame->GetYaxis()->SetNdivisions(10);
    h2_frame->GetYaxis()->SetTitle("photon Survival probability");
    h2_frame->DrawCopy();
    TF1* fit = new TF1("fit","1 - [0] * ( 0.042*x^(-0.18)*log(x)+1.00)",0.1,3)
    gra_pi0gam_conv[1]->Fit(fit,"R");
    gra_pi0gam_conv[1]->Draw();


    TF1* fit_pi0 = new TF1("fit_pi0","1 - [0] * ( 0.042*0.5*x^(-0.18)*log(0.5*x)+1.00)",0.1,3)
    gra_pi0_conv[1]->Fit(fit,"R");
    gra_pi0_conv[1]->Draw();


   */




  //================================================================================ Finalization
  cout<<" Open file : "<<outfname<<endl;
  TFile* f_out = new TFile(outfname,"RECREATE");
  icut = 4;
  while( icut-- ){
    //cout<<" --------------------- icut = "<<icut<<endl;
    //gra_pi0gam_conv[icut]->Print();
    gra_gam_conv[icut]->Write();
    gra_pi0gam_conv[icut]->Write();
    gra_pi0_conv[icut]->Write();
  }
  cout<<" Close file : "<<f_out->GetName()<<endl;
  f_out->Close();

};
//================================================================================

