PlottingADC(char *infile="HbdADCOut.root")
{
gStyle->SetPalette(1);

char title[300];

TFile *fin = new TFile(infile);

TH2F *ADC[96];

for(int i=0;i<96;i++){
   sprintf(title,"ADCch%d",i);
   ADC[i] = (TH2F*)(gDirectory->Get(title))->Clone();
}

TCanvas *c1 = new TCanvas("c1","c1",0,0,1000,800);
c1->Divide(6,8);
for(int i=0;i<48;i++){
   c1->cd(i+1);
   ADC[i]->Draw("COLZ");
}
c1->Update();

TCanvas *c2 = new TCanvas("c2","c2",0,0,1000,800);
c2->Divide(6,8);
for(int i=0;i<48;i++){
   c2->cd(i+1);
   ADC[i+48]->Draw("COLZ");
}
c2->Update();
}
