
#include "/phenix/u/htorii/include/Pid.hh"

TGraphErrors gra_int1_pid[8];
TGraphErrors gra_int2_pid[8];
TGraphErrors gra_rat_pid[8];
TGraphErrors gra_wid1_pid[8];
TGraphErrors gra_wid2_pid[8];

void Hist_ass_ana(){

  gStyle->SetOptFit();
  
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
  
  int isect;
  char hname[128],htitle[128];
  int imom;
  int ipid;
  int ipar;
  double par[3],par_err[3];
  double mom,mom_err;
  
  //
  TH2F* h2_pdymom[8];
  TH2F* h2_pdymom_s[8];
  isect = 0;
  ipid = 8;
  while( ipid-- ){
    h2_pdymom[ipid] = hist_ass->h2_pdymom_sectpid[isect][ipid];
    h2_pdymom_s[ipid] = hist_ass_s->h2_pdymom_sectpid[isect][ipid];
  }
  //================================================================
  TCanvas* c1 = new TCanvas("c1","Pid analysis",600,800);
  TPostScript* ps = new TPostScript("Hist_ass_ana.ps");
  int canvas_num = 0;
  int canvas_div = 9;
  TLine line(-50,0,50,0);

  ipid = 8;
  while( ipid-- ){
    gra_int1_pid[ipid] = new TGraphErrors(14);
    gra_int2_pid[ipid] = new TGraphErrors(14);
    gra_rat_pid[ipid] = new TGraphErrors(14);
    gra_wid1_pid[ipid] = new TGraphErrors(14);
    gra_wid2_pid[ipid] = new TGraphErrors(14);
  }

  imom = 14;
  while( imom -- > 1 ){
    mom = imom * 0.1 + 0.05;
    mom_err = 0.05;
    //
    c1->Clear();
    c1->Divide(2,3);
    ipid = 8;
    while( ipid-- > 2){
      c1->cd(ipid-1);
      if( ( ( ipid == PIONPLUS || ipid == PIONMINUS ) && imom > 1 ) ||
	  ( ( ipid == KAONPLUS || ipid == KAONMINUS ) && imom > 4 && imom < 9 ) ||
	  ( ( ipid == PROTON || ipid == ANTIPROTON ) && imom > 5 ) ){
	TH1D* h_pdy = h2_pdymom[ipid]->ProjectionY("h_pdy",imom,imom);
	TH1D* h_pdy_diff = (TH1D*) h_pdy->Clone();
	TH1D* h_pdy_s = h2_pdymom_s[ipid]->ProjectionY("h_pdy_s",imom,imom);
	h_pdy_diff->SetName("h_pdy_diff");
	if( ipid == PIONPLUS )
	  sprintf(htitle,"Distance y mom=(%.1f,%.1f) pi+",imom*0.1,(imom+1)*0.1);
	if( ipid == PIONMINUS )
	  sprintf(htitle,"Distance y mom=(%.1f,%.1f) pi-",imom*0.1,(imom+1)*0.1);
	if( ipid == KAONPLUS )
	  sprintf(htitle,"Distance y mom=(%.1f,%.1f) K+",imom*0.1,(imom+1)*0.1);
	if( ipid == KAONMINUS )
	  sprintf(htitle,"Distance y mom=(%.1f,%.1f) K-",imom*0.1,(imom+1)*0.1);
	if( ipid == PROTON )
	  sprintf(htitle,"Distance y mom=(%.1f,%.1f) proton",imom*0.1,(imom+1)*0.1);
	if( ipid == ANTIPROTON )
	  sprintf(htitle,"Distance y mom=(%.1f,%.1f) anti-proton",imom*0.1,(imom+1)*0.1);
	h_pdy_diff->SetTitle(htitle);
	h_pdy->Sumw2();
	h_pdy_s->Sumw2();
	h_pdy_s->SetLineColor(3);
	h_pdy_diff->Add(h_pdy_s,-1);
	h_pdy->SetLineColor(4);
	h_pdy_diff->SetLineColor(2);
	//
	//	TF1* fit = new TF1("fit","gaus+gaus(3)",-40,40);
	//	float max = h_pdy->GetMaximum();
	//	fit->SetParameters(max,0,1.,max*0.1,0,1.);
	//	fit->SetLineWidth(1);
	//	fit->SetLineStyle(2);
	//	h_pdy_diff->Fit("fit","R");
	//	h_pdy_diff->SetMaximum(max*1.1);

	TF1* fit = new TF1("fit","gaus",-5,5);
	float max = h_pdy->GetMaximum();
	fit->SetParameters(max,0,1.,max*0.1,0,1.);
	fit->SetLineWidth(3);
	fit->SetLineStyle(2);
	h_pdy_diff->Fit("fit","R");
	h_pdy_diff->SetMaximum(max*1.1);
	//
	fit->GetParameters(par);
	ipar = 3;
	while( ipar-- )
	  par_err[ipar] = fit->GetParError(ipar);
	double integral_1,integral_2;
	double ratio,ratio_err;
	double width_1,width_2;
	double width_1_err,width_2_err;
	integral_1 = sqrt(2.*3.1414)*par[0]*fabs(par[2]) * 2; // 2 is binning effect
	width_1 = fabs(par[2]);
	width_1_err = par_err[2];
	//
	TH1D* h_pdy_sub = (TH1D*) h_pdy_diff->Clone();
	h_pdy_sub->SetName("h_pdy_sub");
	h_pdy_sub->Add(fit,-1);
	TAxis* axis_x = h_pdy_sub->GetXaxis();
	int bin = axis_x->GetLast();
	while( bin-- > axis_x->GetFirst() ){
	  if( fabs( axis_x->GetBinCenter(bin) ) < 10 ){
	    //h_pdy_sub->SetBinError(bin,10000000);
	  }
	}
	//
	TF1* fitsub = new TF1("fitsub","gaus",-40,40);
	h_pdy_sub->Fit("fitsub","R");
	width_2 = fabs( fitsub->GetParameter(2) );
	width_2_err = fitsub->GetParError(2);
	//
	cout<<" ---- "<<endl;
	cout<<h_pdy_diff->Integral(axis_x->GetFirst(),axis_x->GetLast())<<endl;
	cout<<h_pdy_sub->Integral(axis_x->GetFirst(),axis_x->GetLast())<<endl;
	int binlow = h_pdy_diff->FindBin( par[1] - 3.*width_1 );
	int binhigh = h_pdy_diff->FindBin( par[1] + 3.*width_1 );
	cout<<" Integral2 is done (41,"<<binlow<<" ) "<<endl;
	cout<<"    and           ("<<binhigh<<",160) "<<endl;
	integral_2 = h_pdy_diff->Integral(41,binlow) + h_pdy_diff->Integral(binhigh,160);
	cout<<" ---- "<<endl;
	
	//	if( fabs(par[1]) < fabs(par[4]) ){
	//	  integral_1 = sqrt(2.*3.14)*par[0]*par[2];
	//	  integral_2 = sqrt(2.*3.14)*par[3]*par[5];
	//	  ratio = integral_1/integral_2;
	//	  width_1 = fabs(par[1]);
	//	  width_2 = fabs(par[4]);
	//	  width_1_err = par_err[1];
	//	  width_2_err = par_err[4];
	//	} else {
	//	  integral_1 = sqrt(2.*3.14)*par[3]*par[5];
	//	  integral_2 = sqrt(2.*3.14)*par[0]*par[2];
	//	  width_1 = fabs(par[4]);
	//	  width_2 = fabs(par[1]);
	//	  width_1_err = par_err[4];
	//	  width_2_err = par_err[1];
	//	}
	ratio = integral_2/integral_1;
	ratio_err = sqrt(1./sqrt(integral_2)+1./sqrt(integral_1));
	ratio_err = ratio * ratio_err;
	cout<<" integral_1 = "<<integral_1<<endl;
	cout<<" integral_2 = "<<integral_2<<endl;
	cout<<" width_1 = "<<width_1<<endl;
	cout<<" width_2 = "<<width_2<<endl;
	//
	h_pdy_diff->DrawCopy();
	h_pdy->DrawCopy("same");
	h_pdy_s->DrawCopy("same");
	line->Draw();
	//
	cout<<" Filling into graph.... "<<endl;
	gra_int1_pid[ipid]->SetPoint(imom,mom,integral_1);
	gra_int1_pid[ipid]->SetPointError(imom,mom_err,sqrt(integral_1));
	gra_int2_pid[ipid]->SetPoint(imom,mom,integral_1);
	gra_int2_pid[ipid]->SetPointError(imom,mom_err,sqrt(integral_2));
	gra_rat_pid[ipid]->SetPoint(imom,mom,ratio);
	gra_rat_pid[ipid]->SetPointError(imom,mom_err,ratio_err);
	gra_wid1_pid[ipid]->SetPoint(imom,mom,width_1);
	gra_wid1_pid[ipid]->SetPointError(imom,mom_err,width_1_err);
	gra_wid2_pid[ipid]->SetPoint(imom,mom,width_2);
	gra_wid2_pid[ipid]->SetPointError(imom,mom_err,width_2_err);
	//
	//	h_pdy->Delete();
	//	h_pdy_s->Delete();
	//	h_pdy_diff->Delete();
	//	h_pdy_sub->Delete();
      }
    }
    c1->Update();
    c1->cd();
    getchar();
    ps->NewPage();
  }
  // ------------------ Write down to file
  TFile* nf = new TFile("Hist_ass_ana.root","RECREATE");
  gROOT->Write();
  ipid = 8;
  while( ipid-- > 2){
    gra_int1_pid[ipid]->Write();
    gra_int2_pid[ipid]->Write();
    gra_rat_pid[ipid]->Write();
  }
  nf->Write();
  nf->Close();

  //------------------- Plot

  c1->cd();
  c1->Clear();
  c1->Divide(2,3);
  // ------------ Yield
  TH2F* h2_frame;
  c1->cd(1);
  gPad->SetLogy();
  sprintf(htitle,"pT distribution plus charge");
  TH2F* h2_frame;
  h2_frame = new TH2F("h2_frame",htitle,100,0,1.5,100,10,1000000);
  h2_frame->DrawCopy();
  h2_frame->Delete();
  c1->cd(2);
  gPad->SetLogy();
  sprintf(htitle,"pT distribution minus charge");
  TH2F* h2_frame = new TH2F("h2_frame",htitle,100,0,1.5,100,10,1000000);
  h2_frame->DrawCopy();
  h2_frame->Delete();
  ipid = 8;
  while( ipid-- > 2){
    if( ipid % 2 == 0)
      c1->cd(1);
    else
      c1->cd(2);
    int color = (int)ipid/2;
    gra_int1_pid[ipid]->SetLineColor(color);
    gra_int1_pid[ipid]->Draw("P");
    gra_int2_pid[ipid]->SetLineColor(color);
    gra_int2_pid[ipid]->SetLineStyle(2);
    gra_int2_pid[ipid]->Draw("P");
    gra_rat_pid[ipid]->SetLineColor(color);
    gra_wid1_pid[ipid]->SetLineColor(color);
    gra_wid2_pid[ipid]->SetLineColor(color);
    gra_wid2_pid[ipid]->SetLineStyle(2);
  }
  // ------------ Ratio
  c1->cd(3);
  sprintf(htitle,"Ratio plus charge");
  h2_frame = new TH2F("h2_frame",htitle,100,0,1.5,100,0,1.3);
  h2_frame->DrawCopy();
  h2_frame->Delete();
  c1->cd(4);
  sprintf(htitle,"Ratio plus charge");
  h2_frame = new TH2F("h2_frame",htitle,100,0,1.5,100,0,1.3);
  h2_frame->DrawCopy();
  h2_frame->Delete();
  ipid = 8;
  while( ipid-- > 2){
    if( ipid % 2 == 0)
      c1->cd(3);
    else
      c1->cd(4);
    gra_rat_pid[ipid]->Draw("P");
  }
  // -------------- Width
  c1->cd(5);
  sprintf(htitle,"Width plus charge");
  TH2F* h2_frame = new TH2F("h2_frame",htitle,100,0,1.5,100,0,20);
  h2_frame->DrawCopy();
  h2_frame->Delete();
  c1->cd(6);
  sprintf(htitle,"Width plus charge");
  TH2F* h2_frame = new TH2F("h2_frame",htitle,100,0,1.5,100,0,20);
  h2_frame->DrawCopy();
  h2_frame->Delete();
  ipid = 8;
  while( ipid-- > 2){
    if( ipid % 2 == 0)
      c1->cd(5);
    else
      c1->cd(6);
    gra_wid1_pid[ipid]->Draw("P");
    gra_wid2_pid[ipid]->Draw("P");
  }



};
//
