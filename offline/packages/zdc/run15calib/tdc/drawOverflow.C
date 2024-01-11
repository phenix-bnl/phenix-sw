//const char* indir="/direct/phenix+spin/phnxsp01/minjung/nDSTOut/";
const char* indir="/data2/phnxzdc/minjung/offline/packages/zdc/run15calib/nDST/DSTOut/";
const float overflow=3800;
const int TDCNUM=2; //tdc0, tdc1
const int NZDC=8;
const int NSMD=32;
const int NCANVAS=3; //zdc, smd.s, smd.n
const int NBIN=300;
const int MINBIN=3800;
const int MAXBIN=4096;

void drawOverflow(const int runnumber=425296, const int segnum=1){

  gStyle->SetOptFit();
  gStyle->SetOptStat();

  char pdffile[64];
  sprintf(pdffile,"tdc%d.pdf",runnumber);
  TH1F* h[TDCNUM][40];
  TLine* ln[TDCNUM][40];
  const char inname[255];
  sprintf(inname,"%s/DST_ZDCSMDPOL-0000%d-%04d.root",indir,runnumber,segnum);
  if( runnumber==283353 )
    sprintf(inname,"/direct/phenix+prod03/phnxreco/DSTFiles/run9/run9pp_200GeV_pro83/run_0000283000_0000284000/DST_EVE_ALL/DST_EVE_ALL_run9pp_200GeV_pro83-0000283353-9001.root");

  TFile* fp = new TFile(inname);
  TTree* T=(TTree*)fp->Get("T");

  TCanvas* c[TDCNUM][NCANVAS];

  for(int i=0; i<TDCNUM; i++)
    {
      c[i][0]=new TCanvas();
      c[i][0]->Divide(4,2);
      c[i][1]=new TCanvas();
      c[i][1]->Divide(4,4);
      c[i][2]=new TCanvas();
      c[i][2]->Divide(4,4);
    }  
  
  FILE* out[2];
  out[0]=fopen("ZdcCalib.overflow0.temp","w");
  out[1]=fopen("ZdcCalib.overflow1.temp","w");
  

  char line[255];
  char cond[255];
  char hname[16];
  TF1* fit = new TF1("fit","gaus",3800,4096);
  for(int i=0; i<40; i++) 
    for(int tdc=0; tdc<TDCNUM; tdc++)
      {
	int sn, ncanvas, npad;
	if( i<4 )
	  {
	    sn=0; ncanvas=0; npad=i+1;
	  }
	else if( i>=4 && i<8 )
	  {
	    sn=1; ncanvas=0; npad=i+1;
	  }
	else if( i>=8 && i<24 )
	  {
	    sn=0; ncanvas=1; npad=i-7;
	  }
	else
	  {
	    sn=1; ncanvas=2; npad=i-23;
	  }
	      
	cout<<"tdc"<<tdc<<" ch"<<i<<endl;
	sprintf(hname,"t%d_%d",tdc,i);
	sprintf(line,"ZdcRawHits.Tdc%d[%d]>>%s(%d,%d,%d)",tdc,i,hname,NBIN,MINBIN,MAXBIN);  
	sprintf(cond,"ZdcRawHits.Tdc%d[%d]>3800",tdc,sn);

	c[tdc][ncanvas]->cd(npad);
	gPad->SetLogy();
	T->Draw(line);
	//T->Draw(line,cond);

	h[tdc][i] = (TH1F*)gDirectory->Get(hname);
	/*  I gave up this methode because there are some strange TDC channel which has 3 overflow (?) peaks
	  h[tdc][i]->Fit(fit);
	float min =  fit->GetParameter(1) - fit->GetParameter(2)*20 ;
	float max =  fit->GetParameter(1) + fit->GetParameter(2)*20 ;
	if( tdc==0 && ( i==18 || i==34 ) )
	  {
	    min =  fit->GetParameter(1) - fit->GetParameter(2)*40 ;
	    max =  fit->GetParameter(1) + fit->GetParameter(2)*40 ;
	  }

	*/
	float min=3500;
	float max=4097;
	h[tdc][i] ->GetXaxis()->SetRangeUser(min,max);
	
	float overflow = h[tdc][i]->GetMean();
	float rms = h[tdc][i]->GetRMS();
	if( rms <2 )
	  rms=2;
	float ycut = h[tdc][i]->GetEntries();
	if( tdc==0 && i==12 )
	  overflow = 4094;
	else if( tdc==0 && ( i==17 || i==21 || i==22 || i==23 || i==30 || i==32|| i==35 ) )
	  overflow = 4095;
	else if( tdc==1 && i==9 )
	  overflow = 3850;
	else if( tdc==1 && ( i==4 || i== 11 || i== 14 || i==16 || i==28 || i==36 ))
	  overflow = 4095;


	fprintf(out[tdc],"%.6f %.6f 0\n",overflow,rms);

	ln[tdc][i] = new TLine();
	ln[tdc][i] ->SetLineColor(4);
	ln[tdc][i] ->SetLineWidth(2);

	c[tdc][ncanvas]->cd(npad);
	ln[tdc][i]->DrawLine(overflow-6*rms,0,overflow-6*rms,ycut*10);
	c[tdc][ncanvas]->Update();
       
    }
  

  /*
  char pdfbegin[64];
  char pdfend[64];
  sprintf(pdfbegin,"%s(",pdffile);
  sprintf(pdfend,"%s)",pdffile);

  for(int i=0; i<TDCNUM; i++)
    for(int k=0; k<NCANVAS; k++)
      {
	if( i==0 && k==0 )
	  c[i][k]->Print(pdfbegin);
	else if( i==TDCNUM-1 && k==NCANVAS-1 )
	  c[i][k]->Print(pdfend);
	else
	  c[i][k]->Print(pdffile);
      }
  */ 
}
