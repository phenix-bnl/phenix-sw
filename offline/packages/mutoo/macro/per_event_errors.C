plot() {

  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);

  c1 = new TCanvas();
  c1->SetLogy();

  file = new TFile("errors.root");

  range = new TH2F("range","range", 2, 0, 70000, 2, 1e-1, 1e3);
  TAxis* xaxis = range->GetXaxis();
  range->SetTitle("execution time (ms)");

  noerr = new TH1F("noerr","noerr", 100, 0, 70000);
  south = new TH1F("south","south", 100, 0, 70000);
  north = new TH1F("north","north", 100, 0, 70000);
  noerr->SetLineColor(2);
  noerr->SetLineWidth(3);
  south->SetLineColor(4);
  south->SetLineWidth(3);
  north->SetLineColor(6);
  north->SetLineWidth(3);

  tuple_event = (TTree*)gDirectory->Get("event");
  range->Draw();
  tuple_event->Draw("time>>noerr","north_err==0&&south_err==0","same");
  tuple_event->Draw("time>>north","north_err!=0","same");
  tuple_event->Draw("time>>south","south_err!=0","same");

  std::cout << noerr->GetEntries()/1821.0 << std::endl;
  std::cout << north->GetEntries()/1821.0 << std::endl;
  std::cout << south->GetEntries()/1821.0 << std::endl;

  TLegend *leg = new TLegend(0.527299,0.688559,0.877874,0.875,NULL,"brNDC");
  leg->SetFillColor(0);
  leg->AddEntry(noerr,"No Error 90.2%","l");
  leg->AddEntry(north,"North Error 9.3%","l");
  leg->AddEntry(south,"South Error 6.8%","l");
  leg->Draw();

}
