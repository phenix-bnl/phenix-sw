ProjYOut()
{
  TH2F *tmp2f = (TH2F*)gDirectory->Get("ADCsum");
  TH1D *slice[12];
  char title[100];

  c1 = new TCanvas("c1","c1",1200,900);
  c1->Divide(3,4);

  for(int i=0;i<12;i++){
    sprintf(title,"py%d",i);  
    slice[i] = tmp2f->ProjectionY(title,i+1,i+1);
    slice[i]->SetAxisRange(1000,3000);
    slice[i]->Rebin(4);
    c1->cd(i+1);
    c1->GetPad(i+1)->SetLogy();
    slice[i]->Draw();
  }
}
