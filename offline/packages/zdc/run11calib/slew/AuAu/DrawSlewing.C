{

  // Convert from TDC to TIME
  float tdc0g[8] = {0.00721,0.007201,0.007149,0.007143,
		    0.007232,0.007157,0.007309,0.007279};
  float tdc1g[8] = {0.007169,0.007177,0.007306,0.007263,
		    0.007134,0.007137,0.007122,0.007257};

  TCanvas *pict0 = new TCanvas("pict0","pict0",1600,600);
  TCanvas *pict1 = new TCanvas("pict1","pict1",1600,600);

  FILE *fp;
  TChain *chain = new TChain("T");
  
  //int runnum = 299372;
  //int runnum = 300106;
  int runnum = 310697;
  int max_segnum = 20;
  char hname[256];
  for(int segnum=0; segnum<max_segnum; segnum++)
    {
      sprintf(hname,"/data2/phnxzdc/run11/reco/run_0000343000_0000344000/DST_EVE/DST_EVE_run11ZdcCalib-0000343036-%04d.root", segnum);
      fp = fopen(hname,"r");
      if( fp == NULL ) continue;
      fclose(fp);
      chain -> Add(hname);
    }

  TH2F *h0[8];
  TH2F *h1[8];
  
  char hname[256];
  char cutname[256];
  for(int i=0;i<8;i++)
    {
      sprintf(hname,"h0_%d",i);
      h0[i] = new TH2F(hname,"",5000,0,5000,200,-20,20);
      sprintf(hname,"h1_%d",i);
      h1[i] = new TH2F(hname,"",5000,0,5000,200,-20,20);
      
      sprintf(hname,"ZdcRawHits.Tdc0[%d]*%f-BbcNS.Timing[%d]:ZdcRawHits.Adc[%d]>>h0_%d",
	      i,tdc0g[i],i/4,i,i);
      sprintf(cutname,"ZdcHits.Time0[%d]>-100&&(lvl1_trigscaled&0x40000000)!=0x40000000",i);
      chain -> Draw(hname,cutname);
      printf("Filling as %s is done.\n",hname);
      
      sprintf(hname,"ZdcRawHits.Tdc1[%d]*%f-BbcNS.Timing[%d]:ZdcRawHits.Adc[%d]>>h1_%d",
	      i,tdc1g[i],i/4,i,i);
      sprintf(cutname,"ZdcHits.Time1[%d]>-100&&(lvl1_trigscaled&0x40000000)!=0x40000000",i);
      chain -> Draw(hname,cutname);
      printf("Filling ss %s is done.\n",hname);
    }
  
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
  
  TFile *hfile = new TFile("Slew.root","recreate");
  for(int i=0;i<8;i++) h0[i] -> Write();
  for(int i=0;i<8;i++) h1[i] -> Write();
  hfile -> Close();

}
