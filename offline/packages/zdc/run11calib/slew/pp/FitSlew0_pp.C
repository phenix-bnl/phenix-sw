TCanvas *pict = new TCanvas("pict","pict",1200,400);
TGraphErrors *g[8];

void FitSlew0_pp(){

  double f1InitPar[5] = {-6.09108e+01, 1.25106e+03, -4.88799e+02,
			 4.26252e+00,-9.14214e-04};

  double Par[13];
  TF1 *f1 = new TF1("f1","[0]+ [1]/sqrt(x-[2])+exp([3]+[4]*(x-[2]))");
  TF1 *f2 = new TF1("f2","pol3");
  TF1 *f3 = new TF1("f3","pol2");
  f1 -> SetLineColor(2);
  f2 -> SetLineColor(2);
  f3 -> SetLineColor(2);

  /*
    // Try to fit all region by 1 formula
  TF1 *fall = new TF1("fall","[0]+ [1]/sqrt(x-[2])+exp([3]+[4]*(x-[2]))+pol3(5)+pol2(9)");
  TF1 *fall2 = new TF1("fall2","[0] + [1]/([2]-x) + [3]*x + [4]*x^2 + [5]*x^3+pol3(5)");
  fall -> SetLineColor(4);
  fall2 -> SetLineColor(4);
  */

  float FitArea1[8] = {520, 500, 580, 540, 540, 500, 550, 530};

  // dAu only
  float ThreSlew[8] = {700, 750, 720, 660, 750, 800, 850, 720};

  TFile *hfile = new TFile("Slew_RUN8pp.root");

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

  for(int j=0; j<8; j++) GetBin(j, h0[j] );  
  for(int j=0; j<8; j++) g[j] -> SetMarkerStyle(20);
  
  for(int i=0;i<4;i++){

    pict -> Clear();
    g[i] -> Draw("AP");

    f1 -> SetParameters( f1InitPar );
    g[i] -> Fit("f1","","",0,FitArea1[i]);
    pict -> Update();
    getchar();
    for(int j=0;j<FitArea1[i];j++) lut[i][j] = f1 -> Eval( j );
   
    g[i] -> Fit("f2","","",FitArea1[i], 4096);
    pict -> Update();
    getchar();
    for(int j=FitArea1[i];j<4096;j++){
      lut[i][j] = f2 -> Eval( j );
      if( j>ThreSlew[i] ) lut[i][j] = lut[i][ThreSlew[i]]; 
    }

    g[i] -> Draw("AP");
    f1 -> Draw("same");
    f2 -> Draw("same");
    pict -> Update();
    getchar();

  }

  for(int i=4;i<8;i++){

    pict -> Clear();
    g[i] -> Draw("AP");

    f1 -> SetParameters( f1InitPar );
    g[i] -> Fit("f1","","",0,FitArea1[i]);
    pict -> Update();
    getchar();
    for(int j=0;j<FitArea1[i];j++) lut[i][j] = f1 -> Eval( j );
   
    g[i] -> Fit("f2","","",FitArea1[i], 4096);
    pict -> Update();
    getchar();
    for(int j=FitArea1[i];j<4096;j++){
      lut[i][j] = f2 -> Eval( j );
      if( j>ThreSlew[i] ) lut[i][j] = lut[i][ThreSlew[i-4]]; 
    }

    pict -> Clear();
    g[i] -> Draw("AP");
    f1 -> Draw("same");
    f2 -> Draw("same");
    pict -> Update();
    getchar();
  }

  FILE *fp;
  fp = fopen("ZdcCalib.tdc0lut","wt");
  for(int i=0;i<4096;i++){
    fprintf(fp,"%d ",i);
    for(int j=0;j<8;j++) fprintf(fp,"%4.3f ",lut[j][i]);
    fprintf(fp,"\n");
  }
  fclose(fp);
  
}

void GetBin(int ZdcNum, TH2F *ha){

  TF1 *f = new TF1("f","gaus");
  f -> SetLineColor(2);

  int AddNum = 0;
  double x[800] = {0};
  double xE[800] = {0};
  double Mean[800] = {0};
  double MeanE[800] = {0};
 
  for(int i=0;i<400;i++){
    TH1F *htmp = (TH1F *)ha -> ProjectionY("htmp",i*10,(i+1)*10);
    float Center = htmp->GetMaximumBin() * htmp->GetBinWidth(0) - 20;

    if( htmp->GetEntries() < 100 ) continue;
    if( AddNum<5 && htmp->GetEntries() < 200 ) continue;    

    htmp -> Fit("f","NQ","",Center-4,Center+4);
    x[AddNum] = i * 10 + 5; 
    Mean[AddNum]  = f -> GetParameter(1);
    MeanE[AddNum] = f -> GetParameter(2) / sqrt( htmp->GetEntries() );
    AddNum++;
  }

  g[ZdcNum] = new TGraphErrors(AddNum,x,Mean,xE,MeanE);

}
