#include <stdlib.h>

TGraphErrors *g[2][8];

int FitSlew0(char *rootfile){

  TCanvas *pict0 = new TCanvas("pict0","pict0",1200,400);
  //TCanvas *pict0 = new TCanvas("pict0","pict0",800,300);
  double InitPar[10] = {0};

  double Par[13];
  
    // Try to fit all region by 1 formula
  TF1 *f1 = new TF1("f1","[0]+ [1]/sqrt(x-[2])+exp([3]+[4]*(x-[2]))+pol3(5)+pol2(9)");
  TF1 *f2 = new TF1("f2","[0] + [1]/([2]-x) + [3]*x + [4]*x^2 + [5]*x^3+pol3(6)");
  f1 -> SetLineColor(4);
  f2 -> SetLineColor(4);
  
  float ThreSlew[8] = {2500, 2900, 2300, 1750, 2500, 2500, 2400, 2050};

  TFile *hfile = new TFile(rootfile);

  float lut[8][4096] = {0};
  
  TH2F *h0[8];
  TH2F *h1[8];
  char hname[256];
  char reply;
  int rep;

  for(int i=0;i<8;i++){
    sprintf(hname,"h0_%d",i);
    h0[i] = (TH2F *)hfile -> Get(hname);
    sprintf(hname,"h1_%d",i);
    h1[i] = (TH2F *)hfile -> Get(hname);
  }

  for(int j=0; j<8; j++) GetBin(0, j, h0[j] );  
  for(int j=0; j<8; j++) g[0][j] -> SetMarkerStyle(20);

  pict0->Print("Slew0_fit.pdf[","pdf");
  
  for(int i=0;i<8;i++){
    reply='r';
    while(reply!='y')
      {
	if(reply=='r')
	  {
	    pict0 -> Clear();
	    g[0][i] -> Draw("AP");
	    g[0][i] -> Fit("f1","M","",0,4096);
	    pict0 -> Update();
	    rep=1;
	  }
	else if(reply=='s')
	  {
	    pict0 -> Clear();
	    g[0][i] -> Draw("AP");
	    g[0][i] -> Fit("f2","M","",0,4096);
	    pict0 -> Update();
	    rep=2;
	  }
	cout<<i<<" is this good?(y,r=f1,s=f2,e=exit):";
	cin>>reply;
	if(reply=='e') return 1;
      }

    for(int j=0;j<4096;j++){
      if(rep==1)
	lut[i][j] = f1 -> Eval( j );
      else if(rep==2)
	lut[i][j] = f2 -> Eval( j );
      if( lut[i][j]<0 || lut[i][j]>20.0 ) lut[i][j] = 20.0; 
    }

    pict0->Print("Slew0_fit.pdf","pdf");
    h0[i]->Draw();
    g[0][i] -> Draw("P");
    if(rep==1)
      f1 -> Draw("same");
    else if(rep==2)
      f2 -> Draw("same");

    pict0 -> Update();
    getchar();

    //    if(i==0)  f1 -> GetParameters( &InitPar[0] );

  }


  pict0->Print("Slew0_fit.pdf]","pdf");
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
 
