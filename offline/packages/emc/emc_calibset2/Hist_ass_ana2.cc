
void Hist_ass_ana2(){

  
  gSystem->Load("/phenix/u/htorii/lib/libmicrodst.so");
  gSystem->Load("/phenix/u/htorii/lib/libclusttr.so") ;
  gSystem->Load("/phenix/u/htorii/lib/libcalibset2.so");
  //gSystem->Load("/phenix/u/htorii/local/photon/Calibset2/.libs/libcalibset2.so");
  
  Hist_ass* hist_ass;
  Hist_ass* hist_ass_s;
  hist_ass = (Hist_ass*) gROOT->FindObject("hist_ass");
  hist_ass_s = (Hist_ass*) gROOT->FindObject("hist_ass_s");
  if( hist_ass == 0 || hist_ass_s == 0 ){
    cout<<" Error:: Can't open hist_ass/hist_ass_s "<<endl;
    return;
  }

  char hname[128],htitle[128],name[128];
  int isect,ism,ipid;
  int icanvas_num;
  bool drawopt = true;
  //
  TFile* f_plot;
  char f_plot_name[128];
  char fname[] = "Hist_ass_ana";
  sprintf(f_plot_name,"%s_plot.root",fname);
  //
  char ps_name[128];
  sprintf(ps_name,"%splot.ps",fname);
  cout<<" Creating "<<ps_name<<" .... "<<endl;
  TPostScript* ps = new TPostScript(ps_name);
  //
  gROOT->cd();
  TCanvas* c1 = new TCanvas("c1","Cluster energy",700,900);

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //=================================================================== Position Difference
  //#define SKIP_POS
#ifdef SKIP_POS
  cout<<" Plotting position difference  "<<endl;
  icanvas_num = 0;
  isect = 8;
  c1->Divide(3,3);
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      c1->cd( ++icanvas_num );
      hist_ass->h2_pdzy_sect[isect]->Draw("surf");
      c1->cd( ++icanvas_num );
      hist_ass->h_pdz_sect[isect]->Draw();
      hist_ass_s->h_pdz_sect[isect]->SetLineColor(5);
      hist_ass_s->h_pdz_sect[isect]->Draw("same");
      c1->cd( ++icanvas_num );
      hist_ass->h_pdy_sect[isect]->Draw();
      hist_ass_s->h_pdy_sect[isect]->SetLineColor(5);
      hist_ass_s->h_pdy_sect[isect]->Draw("same");
    }
  }
  c1->cd();
  c1->Update();
  if( drawopt ) getchar();
  ps->NewPage();
#endif
  //=================================================================== Particle ID
  //#define SKIP_PID
#ifdef SKIP_PID
  cout<<" Particle ID "<<endl;
  icanvas_num = 0;
  isect = 8;
  c1->Clear();
  c1->Divide(2,3);
  while( isect-- ){
    if( isect == 0 || isect == 1 || isect == 7 ){
      c1->cd( ++icanvas_num );
      TH2* h2;
      h2 = (TH2*)hist_ass->h2_m2mom_sect[isect]->Clone();
      h2->Add(hist_ass_s->h2_m2mom_sect[isect],-1);
      h2->SetMinimum(0);
      h2->DrawCopy("colz");
      delete h2;
      c1->cd( ++icanvas_num );
      h2 = (TH2*)hist_ass->h2_mmom_sect[isect]->Clone();
      h2->Add(hist_ass_s->h2_mmom_sect[isect],-1);
      h2->SetMinimum(0);
      h2->DrawCopy("colz");
      delete h2;
    }
  }
  c1->cd();
  c1->Update();
  ps->NewPage();
  if( drawopt ) getchar();
#endif
  //===============================================================
  //=================================================================== Particle ID for PbSc
  cout<<" Particle ID for PbSc "<<endl;
  isect = 6;
  TH2* h2 = (TH2*)hist_ass->h2_m2mom_sect[isect]->Clone();
  TH2* h2_s = (TH2*)hist_ass_s->h2_m2mom_sect[isect]->Clone();
  h2->SetName("h2");
  h2_s->SetName("h2_s");
  h2->SetTitle("Mass vs Mom in PbSc ");
  h2_s->SetTitle("Mass vs Mom in PbSc ");
  TAxis* ax = hist_ass->h2_m2mom_sect[isect]->GetXaxis();
  while( isect-- ){
    h2->Add(hist_ass->h2_m2mom_sect[isect]);
    h2_s->Add(hist_ass_s->h2_m2mom_sect[isect]);
  }
  c1->cd();
  c1->Clear();
  c1->Divide(1,2);
  c1->cd(1);
  h2->Draw("colz");
  c1->cd(2);
  h2_s->Draw("colz");
  c1->cd();
  c1->Update();
  ps->NewPage();
  if( drawopt ) getchar();
  c1->Clear();
  //
  TGraphErrors* g_pi_cent = new TGraphErrors;
  TGraphErrors* g_pi_width = new TGraphErrors;
  TGraphErrors* g_k_cent = new TGraphErrors;
  TGraphErrors* g_k_width = new TGraphErrors;
  TGraphErrors* g_p_cent = new TGraphErrors;
  TGraphErrors* g_p_width = new TGraphErrors;
  //
  //
  icanvas_num = 0;
  c1->Clear();
  c1->Divide(3,3);
  float momstep = 0.1;
  float mom;
  for( mom=-1.5; mom<1.5 ; mom+=momstep ){
    cout<<"    mom = "<<mom<<endl;
    if( mom > 0.2 || mom < -0.2 ){
      if( icanvas_num == 3*3 + 1 ){
	c1->cd();
	c1->Update();
	ps->NewPage();
	if( drawopt ) getchar();
	c1->Clear();
	c1->Divide(3,3);
	c1->cd(1);
	icanvas_num = 0;
      }
      //
      int ibin_start = ax->FindBin(mom);
      int ibin_stop = ax->FindBin(mom+momstep)-1;
      cout<<" ProjectionY ("<<ibin_start<<","<<ibin_stop<<") "<<endl;
      TH1D* hp = h2->ProjectionY("hp",ibin_start,ibin_stop);
      TH1D* hp_s = h2_s->ProjectionY("hp_s",ibin_start,ibin_stop);
      c1->cd( ++icanvas_num );
      //      if( hp->GetMaximum() > 0 ) gPad->SetLogy();
      sprintf(hname,"Mass distribution for (%f,%f)",mom,mom+momstep);
      hp->SetName(hname);
      hp->DrawCopy();
      hp_s->SetLineStyle(2);
      hp_s->DrawCopy("same");
      hp->Add(hp_s,-1);
      hp->SetLineWidth(2);
      hp->SetLineColor(2);
      hp->DrawCopy("same");
      
      
      float max = hp->GetMaximum();
      TF1 fit_3g("fit_3g","gaus(0)+gaus(3)+gaus(6)",-0.5,1.5);
      fit_3g.SetParameters(max,0.01,0.1,max*0.05,0.2,0.1,max*0.05,1.0,0.1);
      hp->Fit("fit_3g","R","SAME");
      sprintf(hname,"Mass distribution fitting for (%f,%f)",mom,mom+momstep);
      fit_3g.SetName(hname);
      //
      c1->Update();
      //      if( drawopt ) getchar();
      delete hp;
      delete hp_s;
    }
  }
  //
  delete h2;
  delete h2_s;
  //
  c1->cd();
  c1->Update();
  ps->NewPage();
  if( drawopt ) getchar();


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

};
//
