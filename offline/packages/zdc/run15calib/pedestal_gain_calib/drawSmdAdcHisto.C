const char* indir="/direct/phenix+spin/phnxsp01/minjung/DSTOut/";
const float overflow=3800;

void drawSmdAdcHisto(const int runnumber){
  
  const char inname[255];
  sprintf(inname,"%s/DST_EVE_ZDCSMD-0000%d-0001.root",indir,runnumber);

  TFile* fp = new TFile(inname);
  TTree* T=(TTree*)fp->Get("T");
  const char outname[255];
  sprintf(outname,"adchisto/adc%d.root",runnumber);
  TFile* out=new TFile(outname,"RECREATE");

  TH1F* hs[32];
  TH1F* hz[8];
  char line[255];
  char cond[255];
 
  char s[32];
  for(int i=0; i<32; i++)
    {	  
 
      cout<<"ch"<<i<<endl;
      sprintf(line,"ZdcRawHits.Adc[%d]>>s%d(1000,0,4000)",i+8,i);      
      //sprintf(cond,"ZdcRawHits.Tdc0[%d]<3800 && ZdcRawHits.Tdc0[%d]<3800 && lvl1_trigscaled<0x10000000 ",4*(i/16), i+8);
      sprintf(cond,"ZdcRawHits.Adc[%d]>500 && ZdcRawHits.Tdc0[%d]<3800",4*(i/16),4*(i/16));
	
      
      T->Draw(line,cond);
      sprintf(s,"s%d",i);
      hs[i]=(TH1F*)gDirectory->Get(s);
      out->ReOpen("UPDATE");
      hs[i]->Write(s);
    }

  for(int i=0; i<8; i++)
    {	  
 
      cout<<"ch"<<i<<endl;
      sprintf(line,"ZdcRawHits.Adc[%d]>>z%d(1000,0,4000)",i+8,i);      
      //sprintf(cond,"ZdcRawHits.Tdc0[%d]<3800 && ZdcRawHits.Tdc0[%d]<3800 && lvl1_trigscaled<0x10000000 ",4*(i/16), i+8);
      sprintf(cond,"ZdcRawHits.Adc[%d]>500 && ZdcRawHits.Tdc0[%d]<3800",4*(i/16),4*(i/16));
	
      
      T->Draw(line,cond);
      sprintf(s,"z%d",i);
      hs[i]=(TH1F*)gDirectory->Get(s);
      out->ReOpen("UPDATE");
      hz[i]->Write(s);
    }
  out->Close();
}
