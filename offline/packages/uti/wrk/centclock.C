{

// This macro is used to calibrate centrality "Clock" method.
// It requires an Ntuple containing bbc and zdc energies for
// minimum bias events (determined using TriggerHelper class)
// with vertex within +- 30cm. These energies must be called bbc1, bbc2, 
// zdc1 and zdc2. This macro rejects events where one (or both) side(s) 
// of bbc or zdc did not fire.

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
float bbcMax = 1700.;
float zdcMax = 4500.;
float bbc0 = bbcMax*0.15;

  f=new TFile("../Ana/test0.root");

// Fill a histogram with bbc/zdc angle
  hBbcZdcAngle = new TH1F("hBbcZdcAngle"," ",100000,-2.,2.);
  char fill[80];
  sprintf(fill,"atan2((bbc1+bbc2-%f)/%f, (zdc1+zdc2)/%f)>>hBbcZdcAngle",bbc0,bbcMax,zdcMax);
  cout << fill << endl;
  ntp0->Draw(fill,"bbc1>0 && bbc2>0 && zdc1>0 && zdc2>0");
  hBbcZdcAngle->Draw();

// Fill a histogram with cumulative sum
  float csum=0.;
  TH1F* sum = new TH1F("sum","sum",100000,-2.,2.);
  for(int i=1; i<=100000; i++) {
    float a = hBbcZdcAngle->GetBinContent(i);
    csum = csum + a;
    float bin = hBbcZdcAngle->GetBinCenter(i);
    sum->Fill(bin,csum);
  }
  sum->Scale(1./csum*frac);
  sum->Draw();

cout << sum->GetBinContent(100000) << " " << sum->GetBinContent(100000-1000) << endl;
cout << sum->GetBinContent(0) << " " << sum->GetBinContent(0+1000) << endl;

// Calculate cuts

for(int j=lastbin; j>=0; j--) {
  for(int i=100000; i>0; i--) {
    if( (sum->GetBinContent(i)-0.000001) <= (0.01*j) ) {
      cout << "    phiCut[" << lastbin-j << "] = " << sum->GetBinCenter(i) << ";" << endl; 
      break;
    }
  }
}


}

