float had_energy = 0;

double fit_hshower(Double_t * x, Double_t * par) {
  return (par[0]*TMath::Exp(-(x[0]-par[1])*(x[0]-par[1])/(par[2]*par[2]))
          + par[3]*pow((x[0]/0.3),(par[4]-1))*
          TMath::Exp(-par[5]*x[0]/had_energy) );
};

void Hist_mip_ana(){     //Hist_mip* hist_mip,Hist_mip* hist_mip_s){
  
#ifndef COMPILE
  gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
  gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
  gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");
  //gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
#endif

  Hist_mip* hist_mip;
  Hist_mip* hist_mip_s;
  hist_mip = (Hist_mip*) gROOT->FindObject("hist_mip");
  hist_mip_s = (Hist_mip*) gROOT->FindObject("hist_mip_s");
  if( hist_mip == 0 || hist_mip_s == 0 ){
    cout<<" Error:: Can't open hist_mip/hist_mip_s "<<endl;
    return;
  }
  //
  char hname[128],htitle[128],name[128];
  int isect,ism,ipid;
  int icanvas_num;
  bool drawopt = true;
  //
  TFile* f_plot;
  char f_plot_name[128];
  char fname[] = "Hist_mip_ana";
  sprintf(f_plot_name,"%s_plot.root",fname);
  //
  TPostScript* ps;
  char ps_name[128];
  sprintf(ps_name,"%s_plot.ps",fname);
  cout<<" Creating "<<ps_name<<" .... "<<endl;
  ps = new TPostScript(ps_name);


  gROOT->cd();
  TCanvas* c1 = new TCanvas("c1","Cluster energy",700,900);

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //=================================================================== Sector Calibration
  //#define SKIP_SECTOR
#ifdef SKIP_SECTOR
  cout<<" Sector Calibration "<<endl;
  float mpeak_sect[8],mpeak_sect_err[8];
  TH1F* h1_mpeak_sect = new TH1F("h1_mpeak_sect","Energy vs Momentum",8,0,8);

  c1->Clear();
  c1->Divide(1,3);
  icanvas_num = 0;
  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      c1->cd(++icanvas_num);
      //gPad->SetLogy();
      TH1* h1 = (TH1*) hist_mip->h_e_sect[isect]->Clone();
      h1->DrawCopy();
      h1->SetLineColor(5);
      hist_mip_s->h_e_sect[isect]->DrawCopy("same");
      h1->Add(hist_mip_s->h_e_sect[isect],-1);
      TF1* ftmp = fitpeak(h1,0.2,0.4,1.0);
      mpeak_sect[isect] = ftmp->GetParameter(1);
      mpeak_sect_err[isect] = ftmp->GetParError(1);
      int bin = h1_mpeak_sect->FindBin(isect);
      h1_mpeak_sect->SetBinContent(bin,mpeak_sect[isect]);
      h1_mpeak_sect->SetBinError(bin,mpeak_sect_err[isect]);
      delete h1;
      //cout<<" sect , bin, mpeak_sect = "<<isect<<","<<bin<<","
    }
  }
  c1->cd();
  c1->Update();
  if( drawopt ) getchar();
#endif
  //=================================================================== MIP monitor
  //#define SKIP_MIP
#ifdef SKIP_MIP
  cout<<" MIP in 8 sectors "<<endl;
  icanvas_num = 0;
  c1->Divide(2,4);
  isect = 8;
  while( isect-- ){
    c1->cd(++icanvas_num);
    TH1* h_e = hist_mip->h_e_sect[isect]->Clone();
    TH1* h_e_s = hist_mip_s->h_e_sect[isect]->Clone();
    hist_mip->h_e_sect[isect]->Draw();
    hist_mip_s->h_e_sect[isect]->Draw("same");
    h_e->Sumw2();
    h_e_s->Sumw2();
    h_e->Add(h_e_s,-1);
    h_e->DrawCopy("same");
    delete h_e;
    delete h_e_s;
  }
  cout<<" Drawing ......... "<<endl;
  c1->cd();
  c1->Update();
  if( drawopt ) getchar();
#endif
  //=================================================================== MIP vs mom monitor
  //#define SKIP_MIPMOM
#ifdef SKIP_MIPMOM
  cout<<" MIP vs mom in 8 sectors "<<endl;
  icanvas_num = 0;
  c1->Clear();
  c1->Divide(2,4);
  isect = 8;
  while( isect-- ){
    c1->cd(++icanvas_num);
    TH2* h2_e = (TH2*)hist_mip->h2_eangmom_sect[isect]->Clone();
    TH2* h2_e_s = (TH2*)hist_mip_s->h2_eangmom_sect[isect]->Clone();
    //    hist_mip->h2_eangmom_sect[isect]->Draw();
    //    hist_mip_s->h2_eangmom_sect[isect]->Draw("same");
    h2_e->Add(h2_e_s,-1);
    h2_e->DrawCopy("colz");
    delete h2_e;
    delete h2_e_s;
  }
  cout<<" Drawing ......... "<<endl;
  c1->cd();
  c1->Update();
  if( drawopt ) getchar();
#endif
  //=================================================================== MIP vs mom / pid for PbSc
  //#define SKIP_MIPMOMPID
#ifdef SKIP_MIPMOMPID
  cout<<" MIP vs mom for each pid in PbSc"<<endl;
  icanvas_num = 0;
  c1->Clear();
  c1->Divide(2,4);
  ipid = 8;
  while( ipid-- ){
    c1->cd(++icanvas_num);
    isect = 5;
    TH2* h2_e = (TH2*)hist_mip->h2_eangmom_sectpid[isect][ipid]->Clone();
    TH2* h2_e_s = (TH2*)hist_mip_s->h2_eangmom_sectpid[isect][ipid]->Clone();
    while( isect-- ){
      h2_e->Add(hist_mip->h2_eangmom_sectpid[isect][ipid]);
      h2_e_s->Add(hist_mip_s->h2_eangmom_sectpid[isect][ipid]);
    }
    h2_e->Add(h2_e_s,-1);
    h2_e->DrawCopy("colz");
    delete h2_e;
    delete h2_e_s;
  }
  cout<<" Drawing ......... "<<endl;
  c1->cd();
  c1->Update();
  if( drawopt ) getchar();
#endif

  //=================================================================== MIP vs mom / pid for PbSc
#define SKIP_MIP1GEV
#ifdef SKIP_MIP1GEV
  cout<<" MIP for 0.9-1.1GeV/c pid"<<endl;
  icanvas_num = 0;
  c1->Clear();
  c1->Divide(2,4);
  ipid = 8;
  while( ipid-- ){
    c1->cd(++icanvas_num);
    isect = 5;
    TH1* h_e = hist_mip->h2_eangmom_sectpid[isect][ipid]->ProjectionY("h_e",10,11);
    TH1* h_e_s = hist_mip_s->h2_eangmom_sectpid[isect][ipid]->ProjectionY("h_e_s",10,11);
    while( isect-- ){
      TH1* ht_e = hist_mip->h2_eangmom_sectpid[isect][ipid]->ProjectionY("ht_e",10,11);
      TH1* ht_e_s = hist_mip_s->h2_eangmom_sectpid[isect][ipid]->ProjectionY("ht_e_s",10,11);
      h_e->Add(ht_e);
      h_e_s->Add(ht_e_s);
      delete ht_e;
      delete ht_e_s;
    }
    if( ipid!= 7 ) h_e->GetXaxis()->SetRange(0,h_e->GetXaxis()->FindBin(1.5));
    h_e->DrawCopy();
    h_e_s->DrawCopy("same");
    h_e->Sumw2();
    h_e_s->SetLineStyle(2);
    h_e_s->Sumw2();
    h_e->Add(h_e_s,-1);
    h_e->SetLineWidth(2);
    h_e->SetLineColor(2);
    h_e->DrawCopy("same");
    delete h_e;
    delete h_e_s;
  }
  cout<<" Drawing ......... "<<endl;
  c1->cd();
  c1->Update();
  if( drawopt ) getchar();
#endif

  //=================================================================== MIP in SM
  //#define SKIP_MIPSM
#ifdef SKIP_MIPSM
  icanvas_num = 0;
  c1->Clear();
  isect = 8;
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      cout<<" mip in sm for sector "<<isect<<endl;
      c1->cd();
      c1->Clear();
      if( isect < 6 )
	c1->Divide(3,6);
      else
	c1->Divide(4,8);
      TH2* h2 = hist_mip->h2_esm_sect[isect];
      TH2* h2_s = hist_mip_s->h2_esm_sect[isect];
      icanvas_num = 0;
      ism = ( isect < 6 ? 18 : 32 );
      while( ism-- ){
	c1->cd(++icanvas_num);
	TH1* h_p = h2->ProjectionX("h_p",ism+1,ism+1);
	TH1* h_p_s = h2_s->ProjectionX("h_p_s",ism+1,ism+1);
	h_p->DrawCopy();
	h_p_s->SetLineStyle(2);
	h_p_s->DrawCopy("same");
	h_p->Add(h_p_s,-1);
	h_p->SetLineWidth(2);
	h_p->SetLineColor(2);
	h_p->DrawCopy("same");
	delete h_p;
	delete h_p_s;
      }
      cout<<" Drawing ......... "<<endl;
      c1->cd();
      c1->Update();
      if( drawopt ) getchar();
    }
  }
#endif

  //===================================================================  Final operation
  c1->cd();
  c1->Update();
  ps->Close();
  TDirectory* current = gDirectory;
  cout<<" Creating root file "<<f_plot_name<<endl;
  f_plot = new TFile(f_plot_name,"RECREATE");
  f_plot->cd();
  gROOT->GetList()->Write();
  f_plot->Close();
  current->cd();

  return;
  //===============================================================

  //=================================================================== MIP+HADRON shower
  cout<<" Plotting mip  "<<endl;
  ofstream fout("Hist_mip_ana_hshowerfit.txt");
  int ibin = 11;
  icanvas_num = 0;
  c1->Divide(2,3);
  while( ibin-- > 5 ){
    c1->cd(++icanvas_num);
    TAxis* x = hist_mip->h2_eangmom_lowmul_sectpid[0][2]->GetXaxis();
    isect = 0;
    TH1* h_py = hist_mip->h2_eangmom_lowmul_sectpid[isect][2]->ProjectionY("h_py",ibin,ibin);
    TH1* h_py_s = hist_mip_s->h2_eangmom_lowmul_sectpid[isect][2]->ProjectionY("h_py_s",ibin,ibin);
    isect = 1;
    TH1* ht_py = hist_mip->h2_eangmom_lowmul_sectpid[isect][2]->ProjectionY("h_py",ibin,ibin);
    TH1* ht_py_s = hist_mip_s->h2_eangmom_lowmul_sectpid[isect][2]->ProjectionY("h_py_s",ibin,ibin);
    h_py->Add(ht_py);
    h_py_s->Add(ht_py_s);
    delete ht_py;
    delete ht_py_s;
    //
    sprintf(htitle,"Cluster Energy * cos(angle) for (%.2f,%.2f) GeV/c pi+ at low multi",
	    x->GetBinLowEdge(ibin),
	    x->GetBinLowEdge(ibin)+x->GetBinWidth(ibin));
    h_py->SetTitle(htitle);
    h_py->GetXaxis()->SetRange(1,h_py->FindBin(1.5));
    h_py->SetLineStyle(1);
    h_py->DrawCopy();
    h_py->Sumw2();
    h_py_s->SetLineStyle(2);
    h_py_s->DrawCopy("same");
    h_py_s->Sumw2();
    h_py->Add(h_py_s,-1);
    h_py->SetLineWidth(3);
    h_py->SetLineColor(2);
    h_py->DrawCopy("same");
    //
    float in_e = x->GetBinCenter(ibin);
    sprintf(hname,"fit_%d",ibin);
    sprintf(htitle,"[0]*exp(-(x-[1])*(x-[1])/([2]*[2])) + [3]*pow((x/%f),([4]-1))*exp(-[5]*x/%f)",in_e,in_e);
    cout<<"  htitle = "<<htitle<<endl;
    TF1* fit = new TF1(hname,htitle,0.1,1.5);
    sprintf(htitle,"Cluster Energy * cos(angle) for (%.2f,%.2f) GeV/c",
	    x->GetBinLowEdge(ibin),
	    x->GetBinLowEdge(ibin)+x->GetBinWidth(ibin));
    //    fit->SetTitle(htitle);
    gROOT->Append(fit);
    float ratio = 188.0 - 3.84 * in_e;
    float h_p = -0.21*log(in_e)+5.0;
    float h_b = 0.63*log(in_e)+7.5;
    float mip_e = 0.0293 * log(in_e) + 0.284;
    float mip_w = 0.0378 * log(in_e) + 0.071;
    fit->SetParameters(h_py->GetMaximum(),mip_e,mip_w,ratio*h_py->GetMaximum(),h_p,h_b);
    h_py->Fit(hname,"RI","same");
    fout<<x->GetBinLowEdge(ibin)<<" "
	<<x->GetBinLowEdge(ibin)+x->GetBinWidth(ibin)<<" ";
    fout<<fit->GetParameter(3)/fit->GetParameter(0)<<" "
	<<fit->GetParameter(1)<<" "<<fabs(fit->GetParameter(2))<<" "
	<<fit->GetParameter(4)<<" "<<fit->GetParameter(5)<<endl;
    //
    sprintf(hname,"h_eangmom_lowmul_%d",ibin);
    sprintf(htitle,"Cluster Energy * cos(angle) for (%.2f,%.2f) GeV/c",
	    x->GetBinLowEdge(ibin),
	    x->GetBinLowEdge(ibin)+x->GetBinWidth(ibin));
    h_py->SetName(hname);
    h_py->SetTitle(htitle);
    delete h_py_s;
    c1->Update();
    //    getchar();
  }
  fout.close();
  c1->cd();
  c1->Update();
  if( drawopt ) getchar();
  ps->NewPage();

  //=================================================================================
  


  //=================================================================================


};
//
