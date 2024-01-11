enum { NDST, PDST };

//const char* indir= getenv("NDSTOUT");

//const char* indir="/gpfs/mnt/gpfs02/phenix/spin3/minjung/taxi/Run8pp200MinBias/7917/data/";

const char fname_head[2][30]={"DST_ZDCSMDPOL-0000", ""};
const char fname_tail[2][30]={"-0001", ""};
const char adc_name[2][20]={"ZdcRawHits.Adc",  "zdc_rawadc"};
const char tdc_name[2][20]={"ZdcRawHits.Tdc0", "zdc_tdc0"};

const float overflow=3800;

void drawZdcSmdAdcHisto(const int runnumber= 259365// 259050
			, const Int_t dst_type=PDST
			, const Bool_t canv=true
			)
{
  
  char* indir=NULL;
  if( runnumber<323432 ) //before run11
    indir=getenv("PDSTOUT_run8pp");
  else if( runnumber<434604 )
    indir=getenv("PDSTOUT_pp");
  else
    indir=getenv("PDSTOUT_lp");
  
  const char inname[255];
  sprintf(inname,"%s/%s%d%s.root",indir,fname_head[dst_type],runnumber,fname_tail[dst_type]);
  cout<<endl<<"Open: "<<inname<<endl;

  TFile* fp = new TFile(inname);
  TTree* T=(TTree*)fp->Get("T");
  const char outname[255];
  sprintf(outname,"adchisto/adc%d.root",runnumber);
  TFile* out=new TFile(outname,"RECREATE");

  char line[255];
  char cond[255];
  char hname[32];

  TCanvas* c = new TCanvas("c","c",1900,1000);
  c->Divide(8,6);

  for(int i=0; i<44; i++)
    {	        
      cout<<"ch"<<i<<endl;
      sprintf(hname,"h%d",i);

      sprintf(line,"%s[%d]>>%s(1000,0,4000)",adc_name[dst_type],i,hname);
      if( i==0 || i==4 ) 
	sprintf(cond,"");
      else if( i<8 )
	sprintf(cond,"%s[%d]>490 && %s[%d]<3800", adc_name[dst_type], 4*(i/4), tdc_name[dst_type], 4*(i/4));
      else if( i<40 )
	sprintf(cond,"%s[%d]>490 && %s[%d]<3800", adc_name[dst_type], 4*( (i-8)/16), tdc_name[dst_type], 4*( (i-8)/16));
      else
	{
	  sprintf(line,"%s[%d]>>%s(1000,-1000,3000)",adc_name[dst_type],i,hname);
	  sprintf(cond,"");
	}
      
      c->cd(i+1);
     
      T->Draw(line,cond);
      TH1F* h=(TH1F*)gDirectory->Get(hname);
      out->ReOpen("UPDATE");
      h->Write(hname);
      
      if( canv )
	{	  
	  h->DrawCopy();
	  gPad->SetLogy();
	  c->Update();
	}

      delete h; h=NULL;

      if( i==0 || i==4 )
	{
	  sprintf(hname,"hh%d",i);
	  sprintf(line,"%s[%d]:%s[%d]+%s[%d]+%s[%d]>>%s(1000,0,4000,1000,0,4000)",adc_name[dst_type],i, adc_name[dst_type],i+1, adc_name[dst_type],i+2, adc_name[dst_type],i+3, hname);
	  
	  c->cd(45+i/4);
	   
	  T->Draw(line,cond);
	  TH2F* hh=(TH2F*)gDirectory->Get(hname);
	  out->ReOpen("UPDATE");
	  hh->Write(hname);

	  if( canv )
	    {	  
	      hh->DrawCopy();
	      gPad->SetLogy(0);
	      gPad->SetLogz();
	      c->Update();
	    }

	  delete hh; hh=NULL;
      	}      
    }
  
  out->Close();
}
