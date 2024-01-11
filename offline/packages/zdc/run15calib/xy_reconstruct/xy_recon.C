const int NRUN=6;
const int SOUTHNORTH=2;
const int NCORD=6;
const int RUNNUMBER[NRUN]={363227, 387551, 421716, 422204, 422323, 422609};
const char* RUNMARK[NRUN]={"run12pp200", "run13pp510", "run15 before cable tweak", "ZDCLL1 only", "physics", "LED"};
const float ZDCSUMTHR[SOUTHNORTH]={500,500};
const float SMDTHR=30.0;
const char indir[255]="/direct/phenix+u/workarea/minjung/offline/packages/zdc/run15calib/make_pDST/macro/";

void xy_recon(){
 
  /* Read pDST */
  cout<<"Read pDST"<<endl;
  TFile* in[NRUN];
  char inname[255];
  for(int nrun=0; nrun<NRUN; nrun++)
    {
      sprintf(inname,"%s/pDST%d.root" ,indir, RUNNUMBER[nrun]);
      cout<<inname<<endl;
      in[nrun]=new TFile(inname);
    }

  /* Read Calibration File */
  cout<<"Read Calibration File"<<endl;
  //ifstream ifs_ped("/direct/phenix+u/workarea/minjung/offline/packages/zdc/run15calib/pedestal_gain_calib/pedestalfile/ZdcCalib.pedestal.Qscan"); //this pedestal file *u**s
  ifstream ifs_ped("/direct/phenix+u/workarea/minjung/offline/packages/zdc/run15calib/pedestal_gain_calib/pedestalfile/ZdcCalib.pedestal");

  float ped[40];
  int iped=0;
  while(!ifs_ped.eof())
    {
      char line[64];
      ifs_ped.getline(line,64);
      sscanf(line,"%f",&ped[iped]);
      iped++;
      if(iped==40)
	break;
    } 
  float smd_ped[32];
  for(int i=0; i<32; i++)
    smd_ped[i]=ped[i+8];
  float smd_gain[32];
  for(int i=0; i<32; i++)
    smd_gain[i]=1.0;

  /* Make histos */
  cout<<"Make histos"<<endl;
  TH1F* hcx[NRUN][SOUTHNORTH];
  TH1F* hcy[NRUN][SOUTHNORTH];
  TH1F* hx[NRUN][SOUTHNORTH];
  TH1F* hy[NRUN][SOUTHNORTH];
  TH2F* hxy[NRUN][SOUTHNORTH];
  TH1F* hx_trig[NRUN][SOUTHNORTH];
  TH1F* hy_trig[NRUN][SOUTHNORTH];
  TH2F* hxy_trig[NRUN][SOUTHNORTH];
  char hname[16];
  char htitle[255];
  for(int nrun=0; nrun<NRUN; nrun++)
    for(int sn=0; sn<SOUTHNORTH; sn++)
      {
	sprintf(hname,"%cx%d", sn==0 ? "s" : "n", nrun);
	sprintf(htitle,"%s vertical count run%d %s", sn==0 ? "South" : "North", RUNNUMBER[nrun], RUNMARK[nrun]);
	hcx[nrun][sn]=new TH1F(hname,htitle,7,0,7);
	sprintf(hname,"%cy%d", sn==0 ? "s" : "n", nrun);
	sprintf(htitle,"%s horizontal count run%d %s", sn==0 ? "South" : "North", RUNNUMBER[nrun], RUNMARK[nrun]);
	hcy[nrun][sn]=new TH1F(hname,htitle,8,0,8);

	sprintf(hname,"%sx%d", sn==0 ? "s" : "n", nrun);
	sprintf(htitle,"%s X position run%d %s", sn==0 ? "South" : "North", RUNNUMBER[nrun], RUNMARK[nrun]);
	hx[nrun][sn]=new TH1F(hname,htitle,100,-5,5);
	sprintf(hname,"%sy%d", sn==0 ? "s" : "n", nrun);
	sprintf(htitle,"%s Y position run%d %s", sn==0 ? "South" : "North", RUNNUMBER[nrun], RUNMARK[nrun]);
	hy[nrun][sn]=new TH1F(hname,htitle,100,-5,5);
	sprintf(hname,"%sxy%d", sn==0 ? "s" : "n", nrun);
	sprintf(htitle,"%s XY position run%d %s", sn==0 ? "South" : "North", RUNNUMBER[nrun], RUNMARK[nrun]);
	hxy[nrun][sn]=new TH2F(hname,htitle,100,-5,5,100,-5,5);
	
	sprintf(hname,"%sx%dt", sn==0 ? "s" : "n", nrun);
	sprintf(htitle,"%s X position run%d %s w/ trigger selection", sn==0 ? "South" : "North", RUNNUMBER[nrun], RUNMARK[nrun]);
	hx_trig[nrun][sn]=new TH1F(hname,htitle,100,-5,5);
	sprintf(hname,"%sy%dt", sn==0 ? "s" : "n", nrun);
	sprintf(htitle,"%s Y position run%d %s w/ trigger selection", sn==0 ? "South" : "North", RUNNUMBER[nrun], RUNMARK[nrun]);
	hy_trig[nrun][sn]=new TH1F(hname,htitle,100,-5,5);
	sprintf(hname,"%sxy%dt", sn==0 ? "s" : "n", nrun);
	sprintf(htitle,"%s XY position run%d %s w/ trigger selection", sn==0 ? "South" : "North", RUNNUMBER[nrun], RUNMARK[nrun]);
	hxy_trig[nrun][sn]=new TH2F(hname,htitle,100,-5,5,100,-5,5);
      }
  
      
  /* Make SMD Strip Positions */
  cout<<"Make SMD Strip Positions"<<endl;
  float xpos[7];
  float ypos[8];
  const float interval_x=10.0/7.0;
  const float interval_y=10.0/8.0;
  for(int i=0; i<7; i++)
    xpos[i]= interval_x*( i-3 );
  for(int i=0; i<8; i++)
    ypos[i]= interval_y*( i-3.5 );

  /* Coordinate tweak */
  cout<<"Coordinate tweak"<<endl;
  float temp[4]={0,0,0,0};
 
  const int tw=1;//0: no tweak, 1: tweak y, 2: tweak x

  if(tw==0)
    cout<<"no tweak"<<endl;
  else if(tw==1)
    {
      cout<<"tweak y coordinate"<<endl;
      for(int k=4; k<8; k++)
	temp[k-4]=ypos[k];
      for(int k=4; k<8; k++)
	ypos[k]=temp[7-k];

      for(int k=0; k<4; k++)
	temp[k]=ypos[k];
      for(int k=0; k<4; k++)
	ypos[k]=temp[3-k];

    }
  else if(tw==2)
    {      
      cout<<"tweak x coordinate"<<endl;
      for(int k=4; k<7; k++)
	temp[k-4]=xpos[k];
      for(int k=4; k<7; k++)
	xpos[k]=temp[7-k];
    }
  else
    cout<<"Wrong Tweak parameter"<<endl; 


  /* Fill Histos */
  cout<<"Fill Histos"<<endl;
  float x;
  float y;
  float wsum;
  float sum;
  float ene=0;
  int   nsmdx=0;
  int   nsmdy=0;

  unsigned int   trig;
  short zdc_adc[40];
  short zdc_tdc0[40];
  short zdc_tdc1[40];
  float smd_adc[32];
  for(int nrun=0; nrun<NRUN; nrun++)
    {
      TTree* T=(TTree*)in[nrun]->Get("T");
      T->SetBranchAddress("trig", &trig);
      T->SetBranchAddress("zdc_adc",  zdc_adc);
      T->SetBranchAddress("zdc_tdc0", zdc_tdc0);
      T->SetBranchAddress("zdc_tdc1", zdc_tdc1);
      const int NENTRIES=T->GetEntries();
      //const int NENTRIES=3;
       
      for(int evt=0; evt<NENTRIES; evt++)
	{
	  T->GetEntry(evt);
	  for(int i=0; i<32; i++)
	    smd_adc[i]=(float)zdc_adc[i+8];

	  for(int sn=0; sn<SOUTHNORTH; sn++)
	    {
	      if( zdc_adc[sn*4] >ZDCSUMTHR[sn] && zdc_tdc0[sn*4] <3800 )
		{
		  //Y
		  wsum=0;
		  sum=0;
		  nsmdy=0;
		  for(int j=0; j<8; j++)
		    {
		      int ch= j + 16*sn;
		      float ene = (smd_adc[ch]-smd_ped[ch])*smd_gain[ch];
		    
		      if( ene > SMDTHR )
			{
			  wsum+= ene*ypos[j];
			  sum += ene;
			  nsmdy++;
			  hcy[nrun][sn]->Fill(j);
			}
		    }	  
		  y=wsum/sum;
	  
		  //X
		  wsum=0;
		  sum=0;
		  nsmdx=0;
		  for(int j=0; j<7; j++)
		    {
		      int ch= 8 + j + 16*sn ;
		      float ene = (smd_adc[ch]-smd_ped[ch])*smd_gain[ch];
		    		  
		      if( ene > SMDTHR )
			{
			  wsum+= ene*xpos[j];
			  sum += ene;
			  nsmdx++;
			  hcx[nrun][sn]->Fill(j);
			}
		    }
		  x=wsum/sum;
		  
		  if( nsmdx>1 && nsmdy>1 )
		    {
		      hx[nrun][sn]->Fill(x);
		      hy[nrun][sn]->Fill(y);
		      hxy[nrun][sn]->Fill(x,y);
		      
		      if( trig >0x20000000 )
			{
			  hx_trig[nrun][sn]->Fill(x);
			  hy_trig[nrun][sn]->Fill(y);
			  hxy_trig[nrun][sn]->Fill(x,y);
			}
		    }
		}// if( zdc_adc[sn*4] >ZDCSUMTHR[sn] && zdc_tdc0[sn*4] <3800 )
	    }// for(int sn=0; sn<SOUTHNORTH; sn++)
	}// for(int evt=0; evt<NENTRIES; evt++)
      delete T;
    }// for(int nrun=0; nrun<NRUN; nrun++)
  
  /* Draw in Canvases */
  cout<<"Draw in Canvases"<<endl;
  TCanvas* c[NCORD][SOUTHNORTH];
  for(int sn=0; sn<SOUTHNORTH; sn++)
    for(int ncord=0; ncord<NCORD; ncord++)
      {
	c[ncord][sn]=new TCanvas();
	c[ncord][sn]->Divide((NRUN+1)/2,2);
      }	  
  for(int sn=0; sn<SOUTHNORTH; sn++)
    for(int nrun=0; nrun<NRUN; nrun++)
      {
	c[0][sn]->cd(nrun+1);
	hx[nrun][sn]->Draw();
	hx_trig[nrun][sn]->SetLineColor(2);
	hx_trig[nrun][sn]->Draw("SAME");

  	c[1][sn]->cd(nrun+1);
	hy[nrun][sn]->Draw();
	hy_trig[nrun][sn]->SetLineColor(2);
	hy_trig[nrun][sn]->Draw("SAME");
	
  	c[2][sn]->cd(nrun+1);
	hxy[nrun][sn]->Draw("colz");

	c[3][sn]->cd(nrun+1);
	hxy_trig[nrun][sn]->Draw("colz");

  	c[4][sn]->cd(nrun+1);
	hcx[nrun][sn]->Draw();

 	c[5][sn]->cd(nrun+1);
	hcy[nrun][sn]->Draw();
      }
}
