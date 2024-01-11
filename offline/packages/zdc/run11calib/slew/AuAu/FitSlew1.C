TGraphErrors *g[2][8];

void FitSlew1(char *rootfile){

  TCanvas *pict0 = new TCanvas("pict0","pict0",1200,400);
  //TCanvas *pict0 = new TCanvas("pict0","pict0",800,300);

  double InitPar[10] = {0};

  double Par[13];
  //****************for Run10 (Au Au)*************************//
  //  TF1 *fslew = new TF1("fslew","[0] + [1]/([2]-x) + [3]*x + [4]*x^2 + [5]*x^3");
  TF1 *f1 = new TF1("f1","[0]+ [1]/sqrt(x-[2])+exp([3]+[4]*(x-[2]))");
  //  TF1 *f1 = new TF1("f1","expo");
  TF1 *f2 = new TF1("f2","pol3");
  //TF1 *f3 = new TF1("f3","pol2");
  TF1 *f3 = new TF1("f3","pol3");

  //TF1 *fall = new TF1("fall","[0]+ [1]/sqrt(x-[2])+exp([3]+[4]*(x-[2]))+pol3(5)+pol2(9)");
  //  TF1 *fall = new TF1("fall","expo(0)+pol3(2)+pol3(6)");
  //TF1 *fall2 = new TF1("fall2","[0] + [1]/([2]-x) + [3]*x + [4]*x^2 + [5]*x^3+pol3(5)");

  f1 -> SetLineColor(2);
  f2 -> SetLineColor(2);
  f3 -> SetLineColor(2);
  //fall -> SetLineColor(4);
  //fall2 -> SetLineColor(4);

  //float FitArea1[8] = {500, 560, 580, 560, 540, 500, 500, 530 };
  //float FitArea1[8] = {480, 550, 570, 550, 510, 490, 490, 520 };
  float FitArea1[8] = {280, 280, 300, 300, 300, 320, 320, 300 };
  //float FitArea1[8] = {360, 330, 350, 300, 350, 310, 310, 320 };
  float ThreSlew[8] = {2450, 2900, 2200, 1700, 2500, 2400, 2300, 1900};

  TFile *hfile = new TFile(rootfile);

  float lut[8][4096] = {0};
  
  TH2F *h0[8];
  TH2F *h1[8];
  char hname[256];
  
  for(int i=0;i<8;i++){
    sprintf(hname,"h0_%d",i);
    h0[i] = (TH2F *)hfile -> Get(hname);
    sprintf(hname,"h1_%d",i);
    h1[i] = (TH2F *)hfile -> Get(hname);
  }

  for(int j=0; j<8; j++){
    //    GetBin(0, j, h0[j] );
    GetBin(1, j, h1[j] );
  }
  
  for(int j=0; j<8; j++){
    g[1][j] -> SetMarkerStyle(20);
  }
  
  pict0 -> Clear();
  //  pict0 -> Divide(4,2);
  //for(int i=0;i<4;i++){
  for(int i=0;i<8;i++){
    //    f1 -> SetParameters( InitPar );

    /*
    f1 -> SetParameters( InitPar );
    f2 -> SetParameters( InitPar );
    f3 -> SetParameters( InitPar )
    */
    pict0->Clear();

    g[1][i] -> Draw("AP");

    g[1][i] -> Fit("f1","","",0,FitArea1[i]);
    pict0 -> Update();
    getchar();
    for(int j=0;j<FitArea1[i];j++){
      lut[i][j] = f1 -> Eval( j );
      if( lut[i][j]<0 || lut[i][j]>20.0 ) lut[i][j] = 20.0; 
    }
   
    g[1][i] -> Fit("f2","","",FitArea1[i], FitArea1[i]+200);
    pict0 -> Update();
    getchar();
    for(int j=FitArea1[i];j<FitArea1[i]+200;j++) lut[i][j] = f2 -> Eval( j );

    g[1][i] -> Fit("f3","","",FitArea1[i]+200,4096);
    pict0 -> Update();
    getchar();
    //for(int j=FitArea1[i]+200;j<4096;j++) lut[i][j] = f3 -> Eval( j );
    for(int j=FitArea1[i]+200;j<4096;j++){
      lut[i][j] = f3 -> Eval( j );
      if( j>ThreSlew[i] ) lut[i][j] = lut[i][ThreSlew[i]]; 
    }

    h1[i]->Draw();
    //g[1][i] -> Draw("AP");
    g[1][i] -> Draw("P");
    f1 -> Draw("same");
    f2 -> Draw("same");
    f3 -> Draw("same");
    pict0 -> Update();
    getchar();

    if(i==0)  f1 -> GetParameters( &InitPar[0] );

  }

  FILE *fp;
  fp = fopen("ZdcCalib.tdc1lut","wt");
  for(int i=0;i<4096;i++){
    fprintf(fp,"%d ",i);
    for(int j=0;j<8;j++) fprintf(fp,"%4.3f ",lut[j][i]);
    fprintf(fp,"\n");
  }
  fclose(fp);
  
}

void GetBin(int TdcNum, int ZdcNum, TH2F *ha){

  TF1 *f = new TF1("f","gaus");
  f -> SetLineColor(2);

  int AddNum = 0;
  double x[800] = {0};
  double xE[800] = {0};
  double Mean[800] = {0};
  double MeanE[800] = {0};
 
  for(int i=0;i<200;i++){
    TH1F *htmp = (TH1F *)ha -> ProjectionY("htmp",i*20,(i+1)*20);
    float Center = htmp->GetMaximumBin() * htmp->GetBinWidth(0) - 20;

    //if( htmp->GetEntries() < 50 ) continue;
    if( htmp->GetEntries() < 100 ) continue;

    htmp -> Fit("f","NQ","",Center-4,Center+4);
    x[AddNum] = i * 20 + 10; 
    Mean[AddNum]  = f -> GetParameter(1);
    MeanE[AddNum] = f -> GetParameter(2) / sqrt( htmp->GetEntries() );
    AddNum++;
  }

  g[TdcNum][ZdcNum] = new TGraphErrors(AddNum,x,Mean,xE,MeanE);

}
