const int NTDC=2;
const int NCH=8;
const int MAX_SEGNUM = 20;

//const string tscanfile[ NTDC ] = { "/data2/phnxzdc/minjung/offline/packages/zdc/calibrations/build/ZdcCalib.tdc0.Thu_Mar_12_11:43:55_2015", "/data2/phnxzdc/minjung/offline/packages/zdc/calibrations/build/ZdcCalib.tdc1.Thu_Mar_12_11:43:55_2015"};

const string tscanfile[ NTDC ] = {"/data2/phnxzdc/minjung/offline/packages/zdc/run11calib/slew/AuAu/ZdcCalib.tdc0.Wed_Jan_27_11:38:28_2016","/data2/phnxzdc/minjung/offline/packages/zdc/run11calib/slew/AuAu/ZdcCalib.tdc1.Wed_Jan_27_11:38:28_2016" };

const string ovffile[ NTDC ] = {"ZdcCalib.overflow0.Thu_Jan_1_00:00:00_2015-Wed_Jan_1_00:00:00_2020","ZdcCalib.overflow1.Thu_Jan_1_00:00:00_2015-Wed_Jan_1_00:00:00_2020"};

//const string ovffile[ NTDC ] = {"/data2/phnxzdc/minjung/offline/packages/zdc/calibrations/build/ZdcCalib.overflow0.Thu_Mar_12_16:45:55_2015", "/data2/phnxzdc/minjung/offline/packages/zdc/calibrations/build/ZdcCalib.overflow1.Thu_Mar_12_16:45:56_2015"};

const char* ndstfront = "/data2/phnxzdc/minjung/offline/packages/zdc/run15calib/make_nDST/DSTOut/DST_ZDCSMDPOL-0000442933";
//const string ndstfront = "/data2/phnxzdc/minjung/offline/packages/zdc/run15calib/make_nDST/DSTOut/DST_EVE_ZDCSMD-0000426225";


void drawSlew(){

  float tdc2ns[ NTDC ][ NCH ];
  float ovf[ NTDC ][ NCH ];
  float ovfrms[ NTDC ][ NCH ];
  
  for(int tt=0; tt<NTDC; tt++)
    {
      ifstream ifs(tscanfile[tt].c_str());
      float zero;
      int ich=0;
      
      while( !ifs.eof() )
	{
	  char line[128];
	  ifs.getline(line,128);
	  sscanf(line,"%f %f", &zero, &tdc2ns[tt][ich]);
	  cout<<"tdc"<<tt<<" ch"<<ich<<" tdc2ns= "<<tdc2ns[tt][ich]<<endl; 
	  ich++;
	  if( ich==NCH )
	    break;
	}
    }
  
  for(int tt=0; tt<NTDC; tt++)
    {
      ifstream ifs(ovffile[tt].c_str());
      int ich=0;

      while( !ifs.eof() )
	{
	  char line[128];
	  ifs.getline(line,128);
	  sscanf(line,"%f %f", &ovf[tt][ich], &ovfrms[tt][ich]);
	  cout<<"tdc"<<tt<<" ch"<<ich<<" overflow= "<<ovf[tt][ich]<<", rms= "<<ovfrms[tt][ich]<<endl; 
	  ich++;
	  if( ich==NCH )
	    break;
	}
    }

  FILE *fp;
  TChain *chain = new TChain("T");
 
  for(int segnum=0; segnum<MAX_SEGNUM; segnum++)
    {
      char fname[300];
      sprintf(fname,"%s-%04d.root", ndstfront, segnum);
      
      fp = fopen(fname,"r");
      if( fp == NULL ) continue;
      fclose(fp);
      chain -> Add(fname);
      cout<<"ADD: "<<fname<<endl;
    }
  

  TH2F *h[ NTDC ][ NCH ];

  char x_adc[256];
  char y_diff[256];
  char cond1[256];
  char cond2[256];
  char cond3[256];
  char cond4[256];
  char cond5[256];
  char cond6[256];

  for(int i=0;i<NCH;i++)
    for(int tt=0; tt<NTDC; tt++)
      {
	cout<<"tdc"<<tt<<" ch"<<i<<endl;

	char hname[256];
	char drawname[256];
	char cutname[256];
		
	sprintf(hname,"tdc%d_ch%d",tt,i);
	h[tt][i] = new TH2F(hname,"",5000,0,5000,500,-50,50);
	
	//drawing
	sprintf(x_adc,"ZdcRawHits.Adc[%d]",i);
	sprintf(y_diff,"ZdcRawHits.Tdc%d[%d]*%f-BbcNS.Timing[%d]", tt, i, tdc2ns[tt][i], i/4);

	//condition
	sprintf(cond1,"lvl1_trigscaled==0x00000400");
	sprintf(cond2,"ZdcRawHits.Adc[%d]>=0",i);
	sprintf(cond3,"ZdcRawHits.Adc[%d]<4096",i);
	sprintf(cond4,"ZdcRawHits.Tdc%d[%d] < %f", tt, i, ovf[tt][i]-6*ovfrms[tt][i]);
	sprintf(cond5,"ZdcRawHits.Tdc%d[%d] > %f", tt, i, 3*ovfrms[tt][i]);

	if( i<4 )
	  sprintf(cond6,"BbcNS.Timing[0]>10.27-1.25*2 && BbcNS.Timing[0]<10.27+1.25*2");
	else
	  sprintf(cond6,"BbcNS.Timing[1]>10.38-1.21*2 && BbcNS.Timing[1]<10.38+1.21*2");


	// sprintf(hname,"ZdcRawHits.Tdc0[%d]*%f-BbcNS.Timing[%d]:ZdcRawHits.Adc[%d]>>tdc0_%d",i,tdc0_to_ns[i],i/4,i,i);
	// sprintf(cutname,"ZdcHits.Time0[%d]>-100 && (lvl1_trigscaled&0x40000000)!=0x40000000 && ZdcRawHits.Tdc0[%d] < %f",i,i,ovf0[i]-6*ovfrms0[i]);
	sprintf(drawname,"%s : %s >> %s", y_diff, x_adc, hname);
	sprintf(cutname,"%s&& %s&& %s&& %s&& %s&& %s", cond1, cond2, cond3, cond4, cond5, cond6);
	
	chain -> Draw(drawname,cutname);

	h[tt][i]->SetTitle(cutname);
	h[tt][i]->GetXaxis()->SetTitle(x_adc);
	h[tt][i]->GetYaxis()->SetTitle(y_diff);
	
	printf("Filling %s is done.\n",hname);
      }
  
  

  /*
  TCanvas *pict[2];
  pict[0] = new TCanvas("pict0","pict0",1600,600);
  pict[1] = new TCanvas("pict1","pict1",1600,600);
  pict0 -> Clear();
  pict0 -> Divide(4,2);
  for(int i=0;i<8;i++)
    {
      pict0 -> cd(i+1);
      h0[i] -> Draw();
    }
  
  pict1 -> Clear();
  pict1 -> Divide(4,2);
  for(int i=0;i<8;i++)
    {
      pict1 -> cd(i+1);
      h1[i] -> Draw();
    }
  */

  TFile *hfile = new TFile("Slew.root","recreate");
  for(int tt=0; tt<NTDC; tt++)
    for(int i=0; i<NCH; i++) 
      h[tt][i] -> Write();
  hfile -> Close();
  
}
