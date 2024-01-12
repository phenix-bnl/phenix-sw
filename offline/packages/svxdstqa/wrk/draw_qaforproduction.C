using namespace std;

TCanvas *canvas;

void Makecanvas(const Char_t *canvasname);
void SetStyle()
{
  TH1::SetDefaultSumw2();
  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(1);
  gStyle->SetPadGridX(1);
  gStyle->SetPadGridY(1);
  gStyle->SetPadTickX(1);
  gStyle->SetPadTickY(1);
  gStyle->SetLabelSize(0.03,"x");
  gStyle->SetLabelSize(0.03,"y");
}

void Makecanvas(const Char_t *canvasname)
{
  canvas = new TCanvas(canvasname,canvasname,0,0,1200,800);
  canvas->SetGrid();
}


void draw_qaforproduction(string filename)
{
  TFile *f = TFile::Open(filename.c_str());
  
  SetStyle();
 
  //beam center histogram
  Makecanvas("c_prim_minus_beam");
  canvas->Divide(2);
  canvas->cd(1);
  h_primary_x_minus_dbcenter->SetXTitle("prim.x - beam center.x (cm)") ;
//  h_primary_x_minus_dbcenter->Rebin(5);
  h_primary_x_minus_dbcenter->Fit("gaus","","",-0.03,0.03);
  h_primary_x_minus_dbcenter->GetXaxis()->SetRangeUser(-0.1,0.1);
  h_primary_x_minus_dbcenter->Draw("E");
  canvas->cd(2);
  h_primary_y_minus_dbcenter->SetXTitle("prim.y - beam center.y (cm)");
//  h_primary_y_minus_dbcenter->Rebin(5);
  h_primary_y_minus_dbcenter->Fit("gaus","","",-0.02,0.02);
  h_primary_y_minus_dbcenter->GetXaxis()->SetRangeUser(-0.1,0.1);
  h_primary_y_minus_dbcenter->Draw("E");

  Makecanvas("c_eastwest_vertex");
  canvas->Divide(2,2);
  //x
  canvas->cd(1);
  h_EWoffset_x->Rebin(5);
  h_EWoffset_x->Fit("gaus","","",-0.030,0.030);
  h_EWoffset_x->Draw();
  //y
  canvas->cd(2);
  h_EWoffset_y->Rebin(5);
  h_EWoffset_y->Fit("gaus","","",-0.021,0.021);
  h_EWoffset_y->Draw();
  //z
  canvas->cd(3);
  h_EWoffset_z->Rebin(5);
  h_EWoffset_z->Fit("gaus","","",-0.030,0.030);
  h_EWoffset_z->Draw();

  Makecanvas("c_dca2d");
  canvas->Divide(2,1);
  canvas->cd(1);
  
  TH1F *h_svxdca_w = new TH1F("h_svxcdca_w","dca west",2000,-0.5,0.5);
  //h_svxdca_w->Add((TH1F*)f->Get("h_svxc_dca2d_pt0_W"));
  h_svxdca_w->Add((TH1F*)f->Get("h_svxc_dca2d_pt1_W"));
  h_svxdca_w->Add((TH1F*)f->Get("h_svxc_dca2d_pt2_W"));
  h_svxdca_w->Add((TH1F*)f->Get("h_svxc_dca2d_pt3_W"));
  h_svxdca_w->Add((TH1F*)f->Get("h_svxc_dca2d_pt4_W"));
  h_svxdca_w->Add((TH1F*)f->Get("h_svxc_dca2d_pt5_W"));
  h_svxdca_w->Rebin(2);
  h_svxdca_w->SetXTitle("dca2d (cm)");
  h_svxdca_w->Draw("");
  h_svxdca_w->Fit("gaus","","",-0.016,0.016);
  
  canvas->cd(2);
  TH1F *h_svxdca_e = new TH1F("h_svxdca_e","dca east",2000,-0.5,0.5);
  //h_svxdca_e->Add((TH1F*)f->Get("h_svxc_dca2d_pt0_E"));
  h_svxdca_e->Add((TH1F*)f->Get("h_svxc_dca2d_pt1_E"));
  h_svxdca_e->Add((TH1F*)f->Get("h_svxc_dca2d_pt2_E"));
  h_svxdca_e->Add((TH1F*)f->Get("h_svxc_dca2d_pt3_E"));
  h_svxdca_e->Add((TH1F*)f->Get("h_svxc_dca2d_pt4_E"));
  h_svxdca_e->Add((TH1F*)f->Get("h_svxc_dca2d_pt5_E"));
  //  h_svxdca_e->SetLineColor(2);
  h_svxdca_e->Rebin(2);
  h_svxdca_e->SetXTitle("dca2d (cm)");
  h_svxdca_e->Draw("");
  h_svxdca_e->Fit("gaus","","",-0.02,0.02);
  
  Makecanvas("c_cnt_vs_svxcnt");
  gPad->SetLogz();
  h_nsvxc_vs_ncnt->SetXTitle("# of SvxCnt tracks");
  h_nsvxc_vs_ncnt->SetYTitle("# of Cnt tracks");
  h_nsvxc_vs_ncnt->Draw("colz");
  
  /*
  h_svxc_dca2d_pt0_W->Add(h_svxc_dca2d_pt1_W);
  h_svxc_dca2d_pt0_W->Add(h_svxc_dca2d_pt2_W);
  h_svxc_dca2d_pt0_W->Add(h_svxc_dca2d_pt3_W);
  h_svxc_dca2d_pt0_W->Add(h_svxc_dca2d_pt4_W);
  h_svxc_dca2d_pt0_W->Add(h_svxc_dca2d_pt5_W);
  h_svxc_dca2d_pt0_W->Draw();
  */


  /*
  canvas->cd(2);
  TH1F *h_dca_e = new TH1F("h_dca_e","dca east",2000,-0.5,0.5);
  h_svxc_dca2d_pt0_E->Add(h_svxc_dca2d_pt1_E);
  h_svxc_dca2d_pt0_E->Add(h_svxc_dca2d_pt2_E);
  h_svxc_dca2d_pt0_E->Add(h_svxc_dca2d_pt3_E);
  h_svxc_dca2d_pt0_E->Add(h_svxc_dca2d_pt4_E);
  h_svxc_dca2d_pt0_E->Add(h_svxc_dca2d_pt5_E);
  h_svxc_dca2d_pt0_E->Draw();
  */

  //bbc charge distribution
  Makecanvas("bbc charge distribution");
  TLegend *leg = new TLegend(0.45,0.65,0.9,0.9);
  h_bbcq_zcut->SetTitle("bbc charge distribution with bbcz cut (+-8cm) ");
  h_bbcq_zcut->SetXTitle("bbc charge ");
  h_bbcq_zcut->SetLineWidth(2);
  h_bbcq_zcut->Draw("E");
  h_nseed_bbcqbin_zcut->SetLineColor(2);
  h_nseed_bbcqbin_zcut->SetLineWidth(2);
  h_nseed_bbcqbin_zcut->Draw("same");
  h_nprimary_bbcqbin_zcut->SetLineWidth(2);
  h_nprimary_bbcqbin_zcut->SetLineColor(3);
  h_nprimary_bbcqbin_zcut->Draw("same");
  leg->AddEntry(h_bbcq_zcut,"bbc charge distribution","lp");
  leg->AddEntry(h_nseed_bbcqbin_zcut,"bbc charge when seed(!=bbcz) is reconstructed","lp");
  leg->AddEntry(h_nprimary_bbcqbin_zcut,"bbc charge when primary vtx is reconstructed","lp");
  canvas->SetLogy(1);
  leg->SetFillColor(0);
  leg->Draw();
  

  // seed and primary reconstruction efficiency
  Makecanvas("vetex reconstruction efficiency");
  TLegend *leg2 = new TLegend(0.45,0.15,0.9,0.4);
  TH1F *h_bbcq_dividedby_bbcq =h_bbcq_zcut->Clone();
  TH1F *h_seed_dividedby_bbcq =h_nseed_bbcqbin_zcut->Clone();
  TH1F *h_prim_dividedby_bbcq =h_nprimary_bbcqbin_zcut->Clone();
  h_bbcq_dividedby_bbcq->Sumw2();
  h_seed_dividedby_bbcq->Sumw2();
  h_prim_dividedby_bbcq->Sumw2();
  h_bbcq_dividedby_bbcq->Divide(h_bbcq_zcut,h_bbcq_zcut,1.,1.,"");
  h_seed_dividedby_bbcq->Divide(h_nseed_bbcqbin_zcut,h_bbcq_zcut,1.,1.,"B");
  h_prim_dividedby_bbcq->Divide(h_nprimary_bbcqbin_zcut,h_bbcq_zcut,1.,1.,"B");
  h_seed_dividedby_bbcq->SetTitle("vertex reconstruction efficiency with bbcz cut (+-8cm) ");
  h_bbcq_dividedby_bbcq->SetLineWidth(2);
  h_seed_dividedby_bbcq->SetXTitle("bbc charge");
//  h_bbcq_dividedby_bbcq->Draw();
  h_seed_dividedby_bbcq->SetLineWidth(2);
  h_seed_dividedby_bbcq->SetLineColor(2);
  h_seed_dividedby_bbcq->Draw("E");
  h_prim_dividedby_bbcq->SetLineWidth(2);
  h_prim_dividedby_bbcq->SetLineColor(3);
  h_prim_dividedby_bbcq->Draw("Esame");
  
  Float_t entry_bbcq = h_bbcq_zcut->Integral();
  Float_t entry_seed = h_nseed_bbcqbin_zcut->Integral();
  Float_t entry_prim = h_nprimary_bbcqbin_zcut->Integral();
  
  cout << endl;
  cout << endl;
  cout << endl;
  cout << "seed vertex efficiency :  "<< entry_seed/entry_bbcq << endl;
  cout << "primary vertex efficiency : "<<  entry_prim/entry_bbcq << endl;
  leg2->AddEntry(h_seed_dividedby_bbcq,"seed(!=bbcz) vertex reconstruction efficiency with bbczcut (+-8cm)","lp");
  leg2->AddEntry(h_prim_dividedby_bbcq,"primary vertex reconstruction efficiency with bbczcut (+-8cm)","lp");
  leg2->SetFillColor(0);
  leg2->Draw();

  // seed and primary reconstruction efficiency in peripheral collision
  Makecanvas("vetex reconstruction efficiency in peripheral event");
  TLegend *leg3 = new TLegend(0.45,0.15,0.9,0.4);
  TH1F *h_bbcq_dividedby_bbcq_p =h_bbcq_zcut->Clone();
  TH1F *h_seed_dividedby_bbcq_p =h_nseed_bbcqbin_zcut->Clone();
  TH1F *h_prim_dividedby_bbcq_p =h_nprimary_bbcqbin_zcut->Clone();
  h_bbcq_dividedby_bbcq_p->Sumw2();
  h_seed_dividedby_bbcq_p->Sumw2();
  h_prim_dividedby_bbcq_p->Sumw2();
  h_bbcq_dividedby_bbcq_p->Divide(h_bbcq_zcut,h_bbcq_zcut,1.,1.,"");
  h_seed_dividedby_bbcq_p->Divide(h_nseed_bbcqbin_zcut,h_bbcq_zcut,1.,1.,"B");
  h_prim_dividedby_bbcq_p->Divide(h_nprimary_bbcqbin_zcut,h_bbcq_zcut,1.,1.,"B");
  h_bbcq_dividedby_bbcq_p->SetTitle("vertex reconstruction efficiency in peripheral event with bbcz cut (+-8cm) ");
  h_bbcq_dividedby_bbcq_p->SetLineWidth(2);
  h_bbcq_dividedby_bbcq_p->SetXTitle("bbc charge");
  h_bbcq_dividedby_bbcq_p->SetAxisRange(0,200,"X");
  h_bbcq_dividedby_bbcq_p->SetAxisRange(0,1.1,"Y");
  h_bbcq_dividedby_bbcq_p->Draw("E");
  h_seed_dividedby_bbcq_p->SetLineWidth(2);
  h_seed_dividedby_bbcq_p->SetLineColor(2);
  h_seed_dividedby_bbcq_p->Draw("Esame");
  h_prim_dividedby_bbcq_p->SetLineWidth(2);
  h_prim_dividedby_bbcq_p->SetLineColor(3);
  h_prim_dividedby_bbcq_p->Draw("Esame");
  leg3->AddEntry(h_seed_dividedby_bbcq_p,"seed(!=bbcz) vertex reconstruction efficiency with bbczcut (+-8cm)","lp");
  leg3->AddEntry(h_prim_dividedby_bbcq_p,"primary vertex reconstruction efficiency with bbczcut (+-8cm)","lp");
  leg3->SetFillColor(0);
  leg3->Draw();

  Float_t entry_bbcq_p = h_bbcq_zcut->Integral(0,20); // 1 bin is 10 bbcq
  Float_t entry_seed_p = h_nseed_bbcqbin_zcut->Integral(0,20);
  Float_t entry_prim_p = h_nprimary_bbcqbin_zcut->Integral(0,20);
  
  cout << "seed vertex efficiency (bbc charge < 200)    :  "<< entry_seed_p/entry_bbcq_p << endl;
  cout << "primary vertex efficiency (bbc charge < 200) :  "<<  entry_prim_p/entry_bbcq_p << endl;
   
  // bbcz cheack
  Makecanvas("bbcz - seedz" );
  h_bbcz_minus_seedz->SetXTitle("bbcz - seedz(!=bbcz) (cm)");
  h_bbcz_minus_seedz->Draw();
  
  cout << "bbcz - seedz(!=bbcz) mean : "    << h_bbcz_minus_seedz->GetMean()   << " cm " << endl; 
  

  Makecanvas("bbcz - primvtx" );
  h_bbcz_minus_primz->SetXTitle("bbcz - prim.z (cm)");
  h_bbcz_minus_primz->Draw();
  
  cout << "bbcz - primz mean : "    << h_bbcz_minus_primz->GetMean()   << " cm " << endl; 

  // pixel
  // 2D hit map
  Makecanvas("pixel cluster map B0");
  h2_cluster_zphi_0->SetXTitle("Z (cm)");
  h2_cluster_zphi_0->SetYTitle("phi(radian)");
  h2_cluster_zphi_0->Draw("colz");
  
  Makecanvas("pixel cluster map B1");
  h2_cluster_zphi_1->SetXTitle("Z (cm)");
  h2_cluster_zphi_1->SetYTitle("phi(radian)");
  h2_cluster_zphi_1->Draw("colz");
  
  // strip
  // 2D hit map
  Makecanvas("strip cluster map B2");
  h2_cluster_zphi_2->SetXTitle("Z (cm)");
  h2_cluster_zphi_2->SetYTitle("phi(radian)");
  h2_cluster_zphi_2->Draw("colz");
  
  Makecanvas("strip cluster map B3");
  h2_cluster_zphi_3->SetXTitle("Z (cm)");
  h2_cluster_zphi_3->SetYTitle("phi(radian)");
  h2_cluster_zphi_3->Draw("colz");
  

}
