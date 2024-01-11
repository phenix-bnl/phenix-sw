plot() {

  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);

  c1 = new TCanvas("c1","c1",800,800);
  c1->SetLogy();
  c1->Divide(1,2);

  file = new TFile("errors.root");

  snoerr = new TH1F("snoerr","snoerr", 100, 0, 500);
  nnoerr = new TH1F("nnoerr","nnoerr", 100, 0, 500);
  south = new TH1F("south","south", 100, 0, 500);
  north = new TH1F("north","north", 100, 0, 500);
  snoerr->SetLineColor(2);
  snoerr->SetLineWidth(3);
  nnoerr->SetLineColor(2);
  nnoerr->SetLineWidth(3);
  south->SetLineColor(4);
  south->SetLineWidth(3);
  north->SetLineColor(4);
  north->SetLineWidth(3);

  tuple_event = (TTree*)gDirectory->Get("errors");

  c1->cd(1);
  tuple_event->Draw("nhit>>snoerr","type==0&&arm==0");
  tuple_event->Draw("nhit>>south","type!=0&&arm==0","same");
  c1->cd(2);
  tuple_event->Draw("nhit>>nnoerr","type==0&&arm==1");
  tuple_event->Draw("nhit>>north","type!=0&&arm==1","same");

  snoerr->Scale(1.0/snoerr->Integral());
  nnoerr->Scale(1.0/nnoerr->Integral());
  north->Scale(1.0/north->Integral());
  south->Scale(1.0/south->Integral());

  c1->cd(1);
  snoerr->Draw();
  south->Draw("same");

  TLegend *leg = new TLegend(0.527299,0.688559,0.877874,0.875,NULL,"brNDC");
  leg->SetFillColor(0);
  leg->AddEntry(snoerr,"No Error South","l");
  leg->AddEntry(south,"Error South","l");
  leg->Draw();

  TAxis* xaxis = snoerr->GetXaxis();
  xaxis->SetTitle("octant hit multiplicity");

  c1->cd(2);
  c1->SetLogy();
  nnoerr->Draw();
  north->Draw("same");

  TLegend *leg = new TLegend(0.527299,0.688559,0.877874,0.875,NULL,"brNDC");
  leg->SetFillColor(0);
  leg->AddEntry(snoerr,"No Error North","l");
  leg->AddEntry(north,"Error North","l");
  leg->Draw();
  
  TAxis* xaxis = nnoerr->GetXaxis();
  xaxis->SetTitle("octant hit multiplicity");
}
