{

// This macro produces outfile.root file for use in centperp2.C
// macro which calibrates "Perp" centrality method.
// This macro requires TNtuple which has bbc and zdc energies,
// which must be called bbc1, bbc2, zdc1 and zdc2.

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

TCanvas* c1 = new TCanvas("c1","centrality",0,0,800,600);
c1->SetGridx();
c1->SetGridy();

TNtuple* ntp0;
TChain chain("ntp0");

  chain.Add("../Ana/test0.root");

  int nentries = (int)chain.GetEntries();
  cout << "nentries = " << nentries << endl;

  float run;
  float bbc1;
  float bbc2;
  float zdc1;
  float zdc2;
  float npc1;
  float npc3;
  float ndch;

  chain.SetBranchAddress("run", &run);
  chain.SetBranchAddress("bbc1", &bbc1);
  chain.SetBranchAddress("bbc2", &bbc2);
  chain.SetBranchAddress("zdc1", &zdc1);
  chain.SetBranchAddress("zdc2", &zdc2);
  chain.SetBranchAddress("npc1", &npc1);
  chain.SetBranchAddress("npc3", &npc3);
  chain.SetBranchAddress("ndch", &ndch);

  TFile* outfile = new TFile("outfile.root","recreate");
  
  TGraphErrors* gr1;
  TF1* fit;

// Find mean Ezdc for different Ebbc

   TH2F* hhh1 = new TH2F("hhh1","",200,0.,1.0,200,0.,1.0);
    hhh1->SetXTitle("(pow(BBCE,0.25)-1.0)/5.6");
     hhh1->SetYTitle("ZDCE/4500");
   TProfile* prof1 = new TProfile("prof1"," ",80,0.22,0.92);
   prof1->SetMarkerStyle(20);

   TH2F* hhh2 = new TH2F("hhh2","",200,0.,1.0,200,0.,1.0);
    hhh2->SetXTitle("(26*((bbc1+bbc2)/820)-25*((bbc1+bbc2)/820)**2/(((bbc1+bbc2)/820)+0.14))/5.6");
     hhh2->SetYTitle("ZDCE/4500");
   TProfile* prof2 = new TProfile("prof2"," ",80,0.22,0.92);
   prof2->SetMarkerStyle(20);

     for(int i=0; i<nentries; i++) {

       chain.GetEvent(i);

       if(bbc1!=0 && bbc2!=0 && zdc1!=0 && zdc2!=0) {
         float x2 = (26*((bbc1+bbc2)/820)-25*((bbc1+bbc2)/820)**2/(((bbc1+bbc2)/820)+0.14))/5.6;
         float x1 = (pow((bbc1+bbc2),0.25)-1.0)/5.6;
         float y = (zdc1+zdc2)/4500.;
         hhh1->Fill(x1,y);
         prof1->Fill(x1,y); 
         hhh2->Fill(x2,y);
         prof2->Fill(x2,y); 
       }

     }

       prof1->Fit("pol4");
       hhh1->Draw("col");
       prof1->Draw("same");
       TF1* fit1 = prof1->GetFunction("pol4");
       fit1->SetRange(0.15,0.95);
       fit1->Draw("same");

//       prof2->Fit("pol4");
//       hhh2->Draw("col");
//       prof2->Draw("same");
//       TF1* fit2 = prof2->GetFunction("pol4");
//       fit2->SetRange(0.15,0.95);
//       fit2->Draw("same");

//--------------------------------------------------------

 TH1F* hcalib1 = new TH1F("hcalib1","",25000,0.,1.0);
 TH1F* hcalib2 = new TH1F("hcalib2","",11000,0.,1.1);

  for(int ii=0; ii<nentries; ii++) {
    chain.GetEvent(ii);
    if(ii%1000==0) cout << "entry # " << ii << endl;
    if(bbc1!=0 && bbc2!=0 && zdc1!=0 && zdc2!=0) {

      float xx = (pow(bbc1+bbc2,0.25)-1.0)/5.6;
//      float xx = (26*((bbc1+bbc2)/820)-25*((bbc1+bbc2)/820)**2/(((bbc1+bbc2)/820)+0.14))/5.6;
      float yy = (zdc1+zdc2)/4500.;

      float error = 9999.;
      int i=0;
      float X=0.,Y=-999.;
      if(xx>0.2) { X=xx-0.2; }
      while(i<400) {
        float value = yy - fit1->Eval(X);
//        float value = yy - fit2->Eval(X);
        value = (xx-X)*(xx-X)+value*value;
          if(value<error) {
            error=value;
            Y=X;
          }
          X += 0.001;
          i++;
      }
        hcalib1->Fill(1-Y);
//        hcalib2->Fill(1-Y);
    }

  }

//--------------------------------------------------------

  outfile->Write();
  outfile->Close();

}

