
mass()
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  
  signal = new TFile("signal_new_eval.root");
  signal_tree = (TTree*)gDirectory->Get("dimu_reco");
  
  emb = new TFile("minb_new_eval.root");
  emb_tree = (TTree*)gDirectory->Get("dimu_reco");


  signal_tree->Draw("rmass>>count1","rmass!=0&&ghost==0&&rmass>2.6&&rmass<3.6");
  float signal_count = count1->GetEntries();
  emb_tree->Draw("rmass>>count2","rmass!=0&&ghost==0&&rmass>2.6&&rmass<3.6");
  float emb_count = count2->GetEntries();
  
  std::cout << signal_count << std::endl;
  std::cout << emb_count << std::endl;
  std::cout << emb_count/signal_count << std::endl;
  
  TH1F *frame = gPad->DrawFrame(0,0,5,180);
  frame->SetTitle("J/#psi Invariant Mass");
  TAxis *xaxis = frame->GetXaxis();
  xaxis->SetTitle("GeV");  
  frame->Draw();
  
  TH1F* mass = new TH1F("mass","mass",100,0,5);
  mass->SetLineWidth(2);
  mass->SetLineColor(2);
  signal_tree->Draw("rmass>>mass","rmass!=0&&ghost==0","same");
  
  TH1F* mass_emb = new TH1F("mass_emb","mass_emb",100,0,5);
  mass_emb->SetLineWidth(2);
  mass_emb->SetLineColor(4);
  emb_tree->Draw("rmass>>mass_emb","rmass!=0&&ghost==0","same");
  
  TH1F* bg_emb = new TH1F("bg_emb","bg_emb",100,0,5);
  bg_emb->SetLineWidth(2);
  bg_emb->SetLineColor(6);
  emb_tree->Draw("rmass>>bg_emb","rmass!=0&&ghost==1","same");  

  TLegend *leg = new TLegend(0.135057,0.675847,0.485632,0.862288,NULL,"brNDC");
  leg->SetFillColor(0);
  leg->AddEntry(mass,"pure J/#psi","l");
  leg->AddEntry(mass_emb,"embedded min bias Hijing","l");
  leg->AddEntry(bg_emb,"background","l");
  leg->Draw();

  TLatex *   tex = new TLatex(0.24237,116.976,"efficiency = 0.792");
  tex->SetTextSize(0.05);
  tex->SetLineWidth(2);
  tex->Draw();

}
