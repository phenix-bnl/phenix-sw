crkQAviewer(char *filename,unsigned int words, int qamode=0)
{
  TFile* fin = new TFile(filename);

  Int_t i;

  //
  // This is standard QA
  //
  TCanvas* c1 = new TCanvas("richqa","richqa",20,20,770,1070);
  c1->Draw();

  TPad* qawordpad = new TPad("QA_words","QA_words",0.0,0.75,0.35,1.0);
  qawordpad->Draw();

  TPad* nhitspad = new TPad("nhits_pad","nhits_pad",0.35,0.75,1.0,1.0);
  nhitspad->Draw();

  TH1F* crkpe[32];

  char title2[40];
  for(i=0;i<32;i++){
    sprintf(title2,"crkpe%d",i);
    crkpe[i]= (TH1F*)gDirectory->Get(title2);
    crkpe[i]->SetAxisRange(0,4);
  }

  TPad* gainp[32];

  for(i=0;i<32;i++){
    sprintf(title2,"gain%02dpad",i);
    gainp[i] = new TPad(title2,title2,0.125*(i%8),0.1875*(i/8),0.125*(i%8+1),0.1875*(i/8+1));
    gainp[i]->Draw();
  }

  nhitspad->cd();
  crkhit->Draw();

  TF1* f1= new TF1 ("f1","gaus",0.7,1.35);
  f1->SetLineColor(kBlue);
  f1->SetLineWidth(0.8);

  for(i=0;i<32;i++){
    gainp[i]->cd();
    f1->SetParameter(0,crkpe[i]->GetEntries()/3);
    f1->SetParameter(1,1.0);
    f1->SetParameter(2,0.3);
    crkpe[i]->Draw();
    crkpe[i]->Fit("f1","RQ");
  }
  qawordpad->cd();

  TPaveText *t1 = new TPaveText(0.05,0.6,0.95,0.9);
  t1->SetBorderSize(0);
  t1->SetFillColor(0);
  t1->AddText("RICH QA Standard Viewer"); 
  t1->Draw();
  char files[100];

  TPaveText *t2 = new TPaveText(0.02,0.1,0.98,0.6);
  t2->SetBorderSize(0);
  t2->SetFillColor(0);
  char stword[200];
  sprintf(stword,"Filename: %s",filename);
  t2->AddText(stword); 
  sprintf(stword,"Status word = %d",(words)&0x0f);
  t2->AddText(stword); 
  sprintf(stword,"Error in PMT hits = %d",(words>>4)&0x0ff);
  t2->AddText(stword); 
  sprintf(stword,"Error in Charge gain = %d",(words>>12)&0x03f);
  t2->AddText(stword); 
  t2->Draw();

  c1->cd(0);

  if(qamode==0) exit;


  //
  // From Here, expert QA mode
  //
  TCanvas* c2 = new TCanvas("richqa_ex","richqa_ex",20,20,770,1070);
  c2->Draw();

  TPad* qawordpad2 = new TPad("QA_words2","QA_words2",0.0,0.75,0.35,1.0);
  qawordpad2->Draw();

  qawordpad2->cd();

  TPaveText *t3 = new TPaveText(0.05,0.6,0.95,0.9);
  t3->SetBorderSize(0);
  t3->SetFillColor(0);
  t3->AddText("RICH QA Expert Viewer"); 
  t3->Draw();

  TPaveText *t4 = new TPaveText(0.02,0.1,0.98,0.6);
  t4->SetBorderSize(0);
  t4->SetFillColor(0);
  sprintf(stword,"Filename: %s",filename);
  t4->AddText(stword); 
  sprintf(stword,"Status word = %d",(words)&0x0f);
  t4->AddText(stword); 
  sprintf(stword,"Error in PMT hits = %d",(words>>4)&0x0ff);
  t4->AddText(stword); 
  sprintf(stword,"Error in Charge gain = %d",(words>>12)&0x03f);
  t4->AddText(stword); 
  t4->Draw();

  c1->cd(0);
}
