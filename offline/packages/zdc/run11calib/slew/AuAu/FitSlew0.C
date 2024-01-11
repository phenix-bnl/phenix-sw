#include <stdlib.h>

TGraphErrors *g[2][8];

int FitSlew0(char *rootfile){

  TCanvas *pict0 = new TCanvas("pict0","pict0",1200,400);
  //TCanvas *pict0 = new TCanvas("pict0","pict0",800,300);
  double InitPar[10] = {0};

  double Par[13];

  TF1 *f1 = new TF1("f1","[0]+ [1]/sqrt(x-[2])+exp([3]+[4]*(x-[2]))");
  TF1 *f2 = new TF1("f2","pol3");
  //TF1 *f3 = new TF1("f3","pol2");
  TF1 *f3 = new TF1("f3","pol3");
  f1 -> SetLineColor(2);
  f2 -> SetLineColor(2);
  f3 -> SetLineColor(2);

    
  //float FitArea1[8] = {500, 560, 580, 560, 540, 500, 500, 530 };
  float FitArea1[8] = {520, 540, 600, 600, 560, 600, 580, 560 };
  //float FitArea1[8] = {480, 550, 570, 550, 510, 490, 490, 520 };
  //  float FitArea1[8] = {680, 680, 700, 700, 700, 690, 700, 800 };
  //float FitArea1[8] = {1040, 1080, 1200, 1200, 1120, 1240, 1080, 1120 };
  float ThreSlew[8] = {2500, 2900, 2300, 1750, 2500, 2500, 2400, 2050};

  //  int exclude;

  TFile *hfile = new TFile(rootfile);

  float lut[8][4096] = {0};
  
  TH2F *h0[8];
  TH2F *h1[8];
  char hname[256];
  char reply;
  float fitarea;

  for(int i=0;i<8;i++){
    sprintf(hname,"h0_%d",i);
    h0[i] = (TH2F *)hfile -> Get(hname);
    sprintf(hname,"h1_%d",i);
    h1[i] = (TH2F *)hfile -> Get(hname);
  }

  for(int j=0; j<8; j++) GetBin(0, j, h0[j] );  
  for(int j=0; j<8; j++) g[0][j] -> SetMarkerStyle(20);
  
  //for(int i=0;i<4;i++){
  for(int i=0;i<8;i++){
    reply='n';
    fitarea=470;
    while(reply!='y' && fitarea<800)
      {
	fitarea=fitarea+10;
	pict0 -> Clear();
	g[0][i] -> Draw("AP");
	//f1->SetParameters(-10,400,200,-70,0.1);
	//f1->SetParameters(0,0,0,0,0);
	g[0][i] -> Fit("f1","ML","",0,fitarea);
	pict0 -> Update();
	cout<<"Fitarea:"<<fitarea<<" is this good?(y,n,v=set value,e=exit):";
	cin>>reply;
	if(reply=='e') return 1;
	if(reply=='v')
	  {
	    cout<<"give value:";
	    cin>>fitarea;
	    pict0 -> Clear();
	    g[0][i] -> Draw("AP");
	    //f1->SetParameters(0,0,0,0,0);
	    g[0][i] -> Fit("f1","","",0,fitarea);
	    pict0 -> Update();
	    cout<<"You set the fitarea to:"<<fitarea<<" stop iterations?(y,n):";
	    cin>>reply;
	  }	
      }

    //for(int j=0;j<fitarea;j++) lut[i][j] = f1 -> Eval( j );
    for(int j=0;j<fitarea;j++){
      lut[i][j] = f1 -> Eval( j );
      if( lut[i][j]<0 || lut[i][j]>20.0 ) lut[i][j] = 20.0; 
    }
    
    g[0][i] -> Fit("f2","","",fitarea, fitarea+200);
    pict0 -> Update();
    getchar();
    for(int j=fitarea;j<fitarea+200;j++) lut[i][j] = f2 -> Eval( j );

    g[0][i] -> Fit("f3","","",fitarea+200,4096);
    pict0 -> Update();
    getchar();
    for(int j=fitarea+200;j<4096;j++){
      lut[i][j] = f3 -> Eval( j );
      if( j>ThreSlew[i] ) lut[i][j] = lut[i][ThreSlew[i]]; 
    }

    h0[i]->Draw();
    //g[0][i] -> Draw("AP");
    g[0][i] -> Draw("P");
    f1 -> Draw("same");
    f2 -> Draw("same");
    f3 -> Draw("same");
    pict0 -> Update();
    getchar();

    if(i==0)  f1 -> GetParameters( &InitPar[0] );

  }

  FILE *fp;
  fp = fopen("ZdcCalib.tdc0lut","wt");
  for(int i=0;i<4096;i++){
    fprintf(fp,"%d ",i);
    for(int j=0;j<8;j++) fprintf(fp,"%4.3f ",lut[j][i]);
    fprintf(fp,"\n");
  }
  fclose(fp);

  return 0;  
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

    if( htmp->GetEntries() < 300 ) continue;

    htmp -> Fit("f","NQ","",Center-4,Center+4);
    //x[AddNum] = i * 10 + 5; 
    x[AddNum] = i * 20 + 10; 
    Mean[AddNum]  = f -> GetParameter(1);
    MeanE[AddNum] = f -> GetParameter(2) / sqrt( htmp->GetEntries() );
    AddNum++;
  }

  g[TdcNum][ZdcNum] = new TGraphErrors(AddNum,x,Mean,xE,MeanE);

}
 
