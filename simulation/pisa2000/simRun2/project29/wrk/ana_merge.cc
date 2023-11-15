
void ana_merge(char* inflist="ana_merge_list.txt",char* outfname="ana_merge.root"){

  //================================================================================ Initialization
  //Reset ROOT and connect tree file
  gROOT->Reset();
  //================================================================================ Histgrams
  cout<<" Open output file : "<<outfname<<endl;
  TFile* f_out = new TFile(outfname,"RECREATE");
  // From primary/fkin information
  TH1F* h_pri_rawgam;
  TH1F* h_pri_rawpi0gam;
  TH1F* h_pri_rawpi0;
  TH2F* h2_pri_rawpi0gam_phieta;
  TH1F* h_pri_accgam_cut[4];
  TH1F* h_pri_accpi0gam_cut[4];
  TH1F* h_pri_accpi0_cut[4];
  TH2F* h2_pri_accpi0gam_phieta_cut[4];
  // From dEmcGeaClusterTrack information
  TH1F* h_emcv0_accgam_cut[4];
  TH1F* h_emcv0_accpi0gam_cut[4];
  TH1F* h_emcv0_accpi0_cut[4];
  TH2F* h2_emcv0_accpi0gam_phieta_cut[4];
  //
  char hname[256],htitle[256];
  char* cut[4] = { "None","West(PbSc)", "East top(PbSc)","East bottom(PbGl)"};
  int icut,icut_pi0;
  //
  sprintf(hname,"h_pri_rawgam");
  sprintf(htitle,"Primary raw gam");
  h_pri_rawgam = new TH1F(hname,htitle,200,0,20);
  //
  sprintf(hname,"h_pri_rawpi0gam");
  sprintf(htitle,"Primary raw pi0gam");
  h_pri_rawpi0gam = new TH1F(hname,htitle,200,0,20);
  //
  sprintf(hname,"h_pri_rawpi0");
  sprintf(htitle,"Primary raw pi0");
  h_pri_rawpi0 = new TH1F(hname,htitle,200,0,20);
  //
  sprintf(hname,"h2_pri_accpi0gam_phieta");
  sprintf(htitle,"Primary rawpi0gam phi-eta");
  h2_pri_rawpi0gam_phieta = new TH2F(hname,htitle,100,-180,180,100,-10.0,10.0);
  //
  icut = 4;
  while( icut-- ){
    //
    sprintf(hname,"h_pri_accgam_%d",icut);
    sprintf(htitle,"Primary acc gam %s",cut[icut]);
    h_pri_accgam_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_pri_accpi0gam_%d",icut);
    sprintf(htitle,"Primary acc pi0gam %s",cut[icut]);
    h_pri_accpi0gam_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_pri_accpi0_%d",icut);
    sprintf(htitle,"Primary acc pi0 %s",cut[icut]);
    h_pri_accpi0_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h2_pri_accpi0gam_phieta_%d",icut);
    sprintf(htitle,"Primary accpi0gam phi-eta %s",cut[icut]);
    h2_pri_accpi0gam_phieta_cut[icut] = new TH2F(hname,htitle,100,-180,180,100,-10.0,10.0);
    //
    sprintf(hname,"h_emcv0_accgam_%d",icut);
    sprintf(htitle,"EMCal acc gam %s",cut[icut]);
    h_emcv0_accgam_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_emcv0_accpi0gam_%d",icut);
    sprintf(htitle,"EMCal acc pi0gam %s",cut[icut]);
    h_emcv0_accpi0gam_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h_emcv0_accpi0_%d",icut);
    sprintf(htitle,"EMCal acc pi0 %s",cut[icut]);
    h_emcv0_accpi0_cut[icut] = new TH1F(hname,htitle,200,0,20);
    //
    sprintf(hname,"h2_emcv0_accpi0gam_phieta_%d",icut);
    sprintf(htitle,"EMCal accpi0gam phi-eta %s",cut[icut]);
    h2_emcv0_accpi0gam_phieta_cut[icut] = new TH2F(hname,htitle,100,-180,180,100,-10.0,10.0);
  }
  //
  //================================================================================ loopingn
  cout<<" Open input file : "<<inflist<<endl;
  ifstream fin(inflist);
  char fname[256];
  while( fin>>fname ){
    cout<<" Attempt add file : "<<fname<<endl;
    TFile* f = new TFile(fname);
    h_pri_rawgam->Add((TH1*)f->Get("h_pri_rawgam"));
    h_pri_rawpi0gam->Add((TH1*)f->Get("h_pri_rawpi0gam"));
    h_pri_rawpi0->Add((TH1*)f->Get("h_pri_rawpi0"));
    h2_pri_rawpi0gam_phieta->Add((TH1*)f->Get("h2_pri_rawpi0gam_phieta"));
    icut = 4;
    while( icut-- ){
      sprintf(hname,"h_pri_accgam_%d",icut);
      h_pri_accgam_cut[icut]->Add((TH1*)f->Get(hname));
      sprintf(hname,"h_pri_accgam_%d",icut);
      h_pri_accpi0gam_cut[icut]->Add((TH1*)f->Get(hname));
      sprintf(hname,"h_pri_accpi0_%d",icut);
      h_pri_accpi0_cut[icut]->Add((TH1*)f->Get(hname));
      sprintf(hname,"h2_pri_accpi0gam_phieta_%d",icut);
      h2_pri_accpi0gam_phieta_cut[icut]->Add((TH2F*) f_out->Get(hname));
      //
      sprintf(hname,"h_emcv0_accgam_%d",icut);
      h_emcv0_accgam_cut[icut]->Add((TH1*)f->Get(hname));
      sprintf(hname,"h_emcv0_accpi0gam_%d",icut);
      h_emcv0_accpi0gam_cut[icut]->Add((TH1*)f->Get(hname));
      sprintf(hname,"h_emcv0_accpi0_%d",icut);
      h_emcv0_accpi0_cut[icut]->Add((TH1*)f->Get(hname));
      sprintf(hname,"h2_emcv0_accpi0gam_phieta_%d",icut);
      h2_emcv0_accpi0gam_phieta_cut[icut]->Add((TH2F*) f_out->Get(hname));
    }
    f->Close();
  }
  //================================================================================
  cout<<" Close file : "<<f_out->GetName()<<endl;
  f_out->cd();
  f_out->Write();
  f_out->Close();

};
//================================================================================

