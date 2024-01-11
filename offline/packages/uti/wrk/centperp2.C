{

// This macro is used to calibrate centrality "Perp" method.
// It uses outfile.root file produced by centperp1.C macro.

gStyle->SetPalette(1);
gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);
gStyle->SetOptFit(0);
gStyle->SetFrameBorderMode(0);
gStyle->SetDrawBorder(0);
gStyle->SetTitleColor(0);
gStyle->SetStatColor(0);
gStyle->SetCanvasBorderMode(0);
gStyle->SetCanvasColor(0);
gStyle->SetPadColor(0);
gStyle->SetPadBorderMode(0);

c1 = new TCanvas("c1","centrality",0,0,600,625);
c1->SetGridx();
c1->SetGridy();

float frac = 0.93;
int lastbin = int(frac*100.);

  f=new TFile("outfile1.root");

// Fill a histogram with cumulative sum
  float csum=0.;
  TH1F* sum = new TH1F("sum","sum",25000,0.,1.);
  for(int i=1; i<=25000; i++) {
    float a = hcalib1->GetBinContent(i);
    csum = csum + a;
    float bin = hcalib1->GetBinCenter(i);
    sum->Fill(bin,csum);
  }
  sum->Scale(1./csum*frac);
  sum->Draw();

cout << sum->GetBinContent(25000) << " " << sum->GetBinContent(25000-200) << endl;
cout << sum->GetBinContent(0) << " " << sum->GetBinContent(0+200) << endl;

// Calculate cuts

for(int j=0; j<=lastbin; j++) {
  for(int i=1; i<25000; i++) {
    if( sum->GetBinContent(i) >= (0.01*j) ) {
      cout << "    phiCut[" << j << "] = " << sum->GetBinCenter(i) << ";" << endl;
      break;
    }
  }
}


}


