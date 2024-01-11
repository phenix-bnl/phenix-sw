
#include <cstring>
#include <fstream>
#include <iostream>
#include <TString.h>
#include <MpcWarnMap.h>
#include <TVector3.h>
#include <TCanvas.h>
#include <TAxis.h>
#include <TMath.h>

//#include "phool.h"
using namespace std;

static const int HALF_MAXCH = 288;
static const int MAXCH = 576;

MpcWarnMap::MpcWarnMap()
{
  mpcmap = MpcMap::instance();
  mpcnoise = new MpcNoise();
  //Initialize(runNumber);
  FitString="ROB=0.7";
  FitArg=0.7;
  SigmaCut=4.0;
  FitCorr = new TF1("FitCorr","pol5",0,1);
  FitRMS = new TF1("FitRMS","gausn(0)*x*x",-15,15);
  FitRMS->SetParameter(0,1); //coefficient
  FitRMS->SetParameter(1,0); //mean
  FitRMS->SetParameter(2,1); //stdev
  float param[6] = {-0.08681,0.8596,0.4325,-0.4954,-0.6398,0.9069};
  for(int iparam=0;iparam<6;iparam++) FitCorr->SetParameter(iparam,param[iparam]);

  N_towers[0] = 0;
  N_towers[1] = 0;
  for(int ich=0;ich<MAXCH;ich++)
    if(mpcmap->getGridX(ich)>=0)
      {
	int mpc=(ich/HALF_MAXCH)%2;
	N_towers[mpc]++;
      }
}



MpcWarnMap::MpcWarnMap(int runNumber)
{
  mpcmap = MpcMap::instance();
  mpcnoise = new MpcNoise();
  Initialize(runNumber);
  FitString="ROB=0.7";
  FitArg=0.7;
  SigmaCut=4.0;
  FitCorr = new TF1("FitCorr","pol5",0,1);
  FitRMS = new TF1("FitRMS","gausn(0)*x*x",-15,15);
  FitRMS->SetParameter(0,1); //coefficient
  FitRMS->SetParameter(1,0); //mean
  FitRMS->SetParameter(2,1); //stdev
  float param[6] = {-0.08681,0.8596,0.4325,-0.4954,-0.6398,0.9069};
  for(int iparam=0;iparam<6;iparam++) FitCorr->SetParameter(iparam,param[iparam]);

  N_towers[0] = 0;
  N_towers[1] = 0;
  for(int ich=0;ich<MAXCH;ich++)
    if(mpcmap->getGridX(ich)>=0)
      {
	int mpc=(ich/HALF_MAXCH)%2;
	N_towers[mpc]++;
      }
}


MpcWarnMap::MpcWarnMap(const string infile)
{
  mpcmap = MpcMap::instance();
  mpcnoise = new MpcNoise();
  Initialize(infile);
  FitString="ROB=0.7";
  FitArg=0.7;
  SigmaCut=4.0;
  FitCorr = new TF1("FitCorr","pol5",0,1);
  FitRMS = new TF1("FitRMS","gausn(0)*x*x",-15,15);
  FitRMS->SetParameter(0,1); //coefficient
  FitRMS->SetParameter(1,0); //mean
  FitRMS->SetParameter(2,1); //stdev
  float param[6] = {-0.08681,0.8596,0.4325,-0.4954,-0.6398,0.9069};
  for(int iparam=0;iparam<6;iparam++) FitCorr->SetParameter(iparam,param[iparam]);

  N_towers[0] = 0;
  N_towers[1] = 0;
  for(int ich=0;ich<MAXCH;ich++)
    if(mpcmap->getGridX(ich)>=0)
      {
	int mpc=(ich/HALF_MAXCH)%2;
	N_towers[mpc]++;
      }
}



MpcWarnMap::MpcWarnMap(int runNumber, float chisqcut, float fitarg)
{
  mpcmap = MpcMap::instance();
  mpcnoise = new MpcNoise();
  FitCorr = new TF1("FitCorr","pol5",0,1);
  FitRMS = new TF1("FitRMS","gausn(0)*x*x",-15,15);
  FitRMS->SetParameter(0,1); //coefficient
  FitRMS->SetParameter(1,0); //mean
  FitRMS->SetParameter(2,1); //stdev

  float param[6] = {-0.08681,0.8596,0.4325,-0.4954,-0.6398,0.9069};
  for(int iparam=0;iparam<6;iparam++) FitCorr->SetParameter(iparam,param[iparam]);
  Initialize(runNumber);
  FitArg=fitarg;
  SigmaCut=chisqcut;
  for(int ich=0;ich<MAXCH;ich++)
    if(mpcmap->getGridX(ich)>=0)
      {
	int mpc=(ich/HALF_MAXCH)%2;
	N_towers[mpc]++;
      }
}

void MpcWarnMap::Initialize(int runNumber)
{
  InitBadList();
  SetEta(3.0,4.0); //defaults
  norm_graphs = 0;
  mpcnoise->Download_Noise(runNumber);
  for( int impc=0;impc<NMPC;impc++ )
    {
      TString ts_mpc = impc ? "N" : "S";
      for( int ithr=0;ithr<NTHRESH;ithr++ )
	{
	  PercentNoisy[impc][ithr]=0;
	  
	  NvsEta[impc][ithr] = 0;//new TGraphErrors();
	  
	  TString name = "FitPol1_"; name+= ts_mpc; name+=(ithr+1);
	  FitPol1[impc][ithr] = 0;//new TF1(name,"pol1(0)",3,4);
	  
	  name = "FitPol1_lo_"; name+= ts_mpc; name+=(ithr+1);
	  FitPol1Bounds[impc][ithr][0] = new TF1(name,"pol1(0)",3,4);
	  
	  name = "FitPol1_hi_"; name+= ts_mpc; name+=(ithr+1);
	  FitPol1Bounds[impc][ithr][1] = new TF1(name,"pol1(0)",3,4);
	}
    }
}


void MpcWarnMap::Initialize(const string infile)
{
  InitBadList();
  SetEta(3.0,4.0); //defaults
  norm_graphs = 0;
  mpcnoise->Download_Noise(infile);
  for( int impc=0;impc<NMPC;impc++ )
    {
      TString ts_mpc = impc ? "N" : "S";
      for( int ithr=0;ithr<NTHRESH;ithr++ )
	{
	  PercentNoisy[impc][ithr]=0;
	  
	  NvsEta[impc][ithr] = 0;//new TGraphErrors();
	  
	  TString name = "FitPol1_"; name+= ts_mpc; name+=(ithr+1);
	  FitPol1[impc][ithr] = 0;//new TF1(name,"pol1(0)",3,4);
	  
	  name = "FitPol1_lo_"; name+= ts_mpc; name+=(ithr+1);
	  FitPol1Bounds[impc][ithr][0] = new TF1(name,"pol1(0)",3,4);
	  
	  name = "FitPol1_hi_"; name+= ts_mpc; name+=(ithr+1);
	  FitPol1Bounds[impc][ithr][1] = new TF1(name,"pol1(0)",3,4);
	}
    }
}

void MpcWarnMap::InitBadList()
{

  for(int impc=0;impc<NMPC;impc++)
    for(int ix=0;ix<18;ix++)
      for(int iy=0;iy<18;iy++){
	badlist[impc][ix][iy] = 0;
      }
  
  ifstream inFile1;
  TString mpcWarn2FN = "mpcbadlist.txt";
  
  inFile1.open(mpcWarn2FN.Data(),ios::in);
  
  if(inFile1.is_open())
    {
      while(!inFile1.eof() )
        {
          int inmpc = -1;int inx = -1; int iny = -1;
          inFile1 >> inmpc >> inx >> iny;
          cout << "Debugging: " << inmpc << "\t" << inx << "\t" << iny << endl;
          if(inx>=0 && iny>=0 && inx<18 && iny<18 && (inmpc == 0 || inmpc == 1)){
            badlist[inmpc][inx][iny]=-1;
            cout << "Warnmapvalue is " << badlist[inmpc][inx][iny];
          }
        }
    }
  else cout << "Unable to open Mpc extra Warnmap file"; 
  return;
  
}

void MpcWarnMap::Reset()
{
  for( int impc=0;impc<NMPC;impc++ )
    for( int ithr=0;ithr<NTHRESH;ithr++ )
      {
	delete FitPol1[impc][ithr]; //delete NvsEta[impc][ithr];
	FitPol1[impc][ithr]=0; //NvsEta[impc][ithr]=0;
	for(int ibound=0;ibound<2;ibound++) 
	  {
	    delete FitPol1Bounds[impc][ithr][ibound];
	    FitPol1Bounds[impc][ithr][ibound]=0;
	  }
      }
  //N_towers[0]=0; N_towers[1]=0;
  mpcnoise->Reset();
}

float MpcWarnMap::PseudoRapidity(int ch)
{
  float pseudorapidity = -9999;
  if(mpcmap != 0)
    {
      if( mpcmap->getGridX(ch) >= 0 )
	{
	  TVector3 v3d( mpcmap->getX(ch),mpcmap->getY(ch),mpcmap->getZ(ch) );
	  pseudorapidity = v3d.PseudoRapidity(); 
	}
    }
  else cout << "Need to initialize MpcMap object" << endl;
  return pseudorapidity;
}

float MpcWarnMap::GetCorrection(float fitarg, int mpc, int thr)
{
  if( thr<0 || thr>=4 || mpc<0 || mpc>1)
    {cout << PHWHERE << "Error in GetCorrection\n"; return -999999;}
  else if(fitarg<=0 || fitarg >=1) 
    {
      cout << PHWHERE << "Bad input to GetCorrection\n";
      return -9999;
    }

  //correct for noisy data which is not distributed in "gaussian"
  //manner about line. Increases the % of data used by LTS regression
  float input = fitarg;
  if(PercentNoisy[mpc][thr]<1 && PercentNoisy[mpc][thr]>=-0.00001)
    {
      input = fitarg/(1.0-PercentNoisy[mpc][thr]); // we don't want input to be > 1
      if(input > 1) input = fitarg;
    }
  //x= # of sigmas away from mean
  float x = TMath::ErfInverse(input)*sqrt(2);
  //cout << "# of sigmas away from mean is: " << x << endl;
  return sqrt(FitRMS->Integral(-x,x)/input);
}

float MpcWarnMap::GetCorrection_fast(float fitarg, int mpc, int thr)
{
  if( thr<0 || thr>=4 || mpc<0 || mpc>1)
    {cout << PHWHERE << "Error in GetCorrection_fast\n"; return -999999;}
  else if(fitarg<=0 || fitarg >=1) 
    {
      cout << PHWHERE << "Bad input to GetCorrection\n";
      return -9999;
    }
  float input = fitarg;
  if(PercentNoisy[mpc][thr]<1 && PercentNoisy[mpc][thr]>=-0.00001)
    {
      input = fitarg/(1.0-PercentNoisy[mpc][thr]);
      if(input > 1) input = fitarg;
    }
  return FitCorr->Eval(input);
}
  

int MpcWarnMap::CreateGraphs(int debug) //returns -1 if there is a problem, 0 otherwise
{
  float eta[NMPC][HALF_MAXCH];
  float N_scaled[NMPC][NTHRESH][HALF_MAXCH];
  int graph_size[NMPC];
    //if(debug) cout << "Made it to " << PHWHERE << endl;
  int N_tot = mpcnoise->getNtot();
  int N_hits[NTHRESH];
  int impc=0;
  if( N_tot <= 0)
    {
      cout << "Error: N_tot <= 0" << PHWHERE << endl;
      return -9999;
    }
  int graph_ch = 0;
  for( int ich=0;ich<MAXCH;ich++ )
    {
      if(ich == HALF_MAXCH) {impc = 1; graph_ch = 0;}
      if(mpcmap->getGridX(ich)>=0)// && fabs(PseudoRapidity(ich)) < eta_hi )// && fabs(PseudoRapidity(ich)) > eta_lo )
	{
	 
	  int gridx = mpcmap->getGridX(ich);
	  int gridy = mpcmap->getGridY(ich);
	  int whichmpc = (ich<288)?0:1;

	  if(badlist[whichmpc][gridx][gridy] < 0) continue;
	  
	  
	  for(int ithr=0;ithr<NTHRESH;ithr++)
	    {
	      N_hits[ithr] = mpcnoise->getN(ich,ithr);
	      if(N_hits[ithr] < 0)
		{
		  cout << "Error: Either MpcNoise put in database with different map"
		       << "or MpcNoise object not found for given run number."
		       << PHWHERE << endl;
		  return -1;
		}
	      N_scaled[impc][ithr][graph_ch] = (float)N_hits[ithr]/(float)N_tot;
	    }
	  
	  eta[impc][graph_ch] = fabs(PseudoRapidity(ich));
	  if( ich == HALF_MAXCH-1 || ich == MAXCH-1)
	    {
	      graph_size[impc] = graph_ch+1;
	    }
	  graph_ch++;
	}
    }
  
  for(int impc=0;impc<NMPC;impc++)
    {
      for(int ithr=0;ithr<NTHRESH;ithr++) 
	{
	  TString name = "NvsEta_"; TString ts_mpc = impc ? "N_" : "S_";
	  name+=ts_mpc; name+=(ithr+1);
	  NvsEta[impc][ithr] = new TGraph(graph_size[impc],eta[impc],N_scaled[impc][ithr]);
	  NvsEta[impc][ithr]->SetTitle(name);
	  NvsEta[impc][ithr]->SetName(name);
	  NvsEta[impc][ithr]->GetXaxis()->SetTitle("#eta");
	  NvsEta[impc][ithr]->GetYaxis()->SetTitle("N/N_{thr}");
	  NvsEta[impc][ithr]->SetMarkerStyle(20);
	  NvsEta[impc][ithr]->SetMarkerColor(4);
	  
	  NvsEta[impc][ithr]->GetXaxis()->SetTitleSize(0.06);  // Sets size for xaxis title
	  NvsEta[impc][ithr]->GetXaxis()->SetTitleOffset(0.7); // Sets offset for xaxis title
	  NvsEta[impc][ithr]->GetYaxis()->SetTitleSize(0.06);  // Sets size for yaxis title
	  NvsEta[impc][ithr]->GetYaxis()->SetTitleOffset(1.05);
	}
    }
  return 0;
}

int MpcWarnMap::FitGraphs()
{
  for(int impc=0;impc<NMPC;impc++)
    { 
      TString ts_mpc = impc ? "N" : "S";
      for(int ithr=0;ithr<NTHRESH;ithr++) 
	{
	  if(!NvsEta[impc][ithr]) return -9999;
	  TString name = "FitPol1_"; name+= ts_mpc; name+=(ithr+1);
	  NvsEta[impc][ithr]->Fit("pol1",FitString,"",eta_lo,eta_hi );
	  if(norm_graphs){
	    TF1* fit_temp = NvsEta[impc][ithr]->GetFunction("pol1");
	    int npts = NvsEta[impc][ithr]->GetN();
	    
	    for(int ipt=0;ipt<npts;ipt++){
	      double eta,val;
	      NvsEta[impc][ithr]->GetPoint(ipt,eta,val);
	      double fitresult = fit_temp->Eval(eta);
	      double newval = 0;
	      if(fitresult > 0){
		double tempval = val/fitresult;
		double diff = tempval -1.0;
		diff=diff*sqrt(fitresult)*sqrt( mpcnoise->getNtot() )*0.05;
		newval=1+diff;
	      }
	      NvsEta[impc][ithr]->SetPoint(ipt,eta,newval);
	    }
	  }
	  
	  NvsEta[impc][ithr]->Fit("pol1",FitString,"",eta_lo,eta_hi );
	  FitPol1[impc][ithr] = NvsEta[impc][ithr]->GetFunction("pol1");
	  FitPol1[impc][ithr]->SetName(name);
	  for(int ibound=0;ibound<2;ibound++)
	    {
	      FitPol1Bounds[impc][ithr][ibound]->SetParameter(1,GetFitSlope(impc,ithr)); //slope
	      float intercept = GetFitIntercept(impc,ithr)+SigmaCut*GetSigma(impc,ithr)*(-1.0+2.0*(float)ibound);//GetSigma(impc,ithr) sqrt(GetChiSqPerDOF(impc,ithr))
	      FitPol1Bounds[impc][ithr][ibound]->SetParameter(0,intercept); //y-intercept
	    }
	}
    }
  return 0;
}

void MpcWarnMap::DrawGraphs(const char* file_name, int mpc, int save) //I use a .png extension in file_name
{
  if(mpc<0 || mpc>1) {cout << PHWHERE << "Bad mpc integer\n"; return;}
  TCanvas* c1 = new TCanvas("Noise_canvas","Noise Canvas",1000,1000);
  c1->Divide(2,2);
  
  for(int ithr=0;ithr<NTHRESH;ithr++)
    {
      c1->cd(ithr+1);
      if(NvsEta[mpc][ithr] != 0)  NvsEta[mpc][ithr]->Draw("AP");
    }
  if(save)
    {
      c1->SaveAs(file_name);
      delete c1;
    }
}

void MpcWarnMap::DrawGraphsBounds(const char* file_name, int mpc, int save) //I use a .png extension in file_name
{
  if(mpc<0 || mpc>1) {cout << PHWHERE << "Bad mpc integer\n"; return;}
  TCanvas* c1 = new TCanvas("Noise_canvas","Noise Canvas",1000,1000);
  c1->Divide(2,2);
  for(int ithr=0;ithr<NTHRESH;ithr++)
    {
      c1->cd(ithr+1);
      if(NvsEta[mpc][ithr] != 0 && FitPol1Bounds[mpc][ithr][0] !=0 &&  FitPol1Bounds[mpc][ithr][1] != 0)
	{
	  NvsEta[mpc][ithr]->Draw("AP");
	  FitPol1Bounds[mpc][ithr][0]->Draw("SAMEL");
	  FitPol1Bounds[mpc][ithr][1]->Draw("SAMEL");
	}
      
    }
  if(save)
    {
      c1->SaveAs(file_name);
      delete c1;
    }
}

//-------------Non-inline Set/Get/Calc for private data members-----------------------------------	

void MpcWarnMap::SetFitArg( float fitarg  )
{
  if(fitarg > 1.0 || fitarg < 0.5)
    {
      cout << PHWHERE << "Bad Fit Arg, use 0.5 < FitArg < 1.0: FitArg not set" << endl;
     }
  else
    {
      FitArg=fitarg;
      char temp[256]; 
      sprintf(temp,"ROB=%.2f",fitarg);
      FitString=temp;
    }
}

void MpcWarnMap::SetPercentNoisy(float pct, int mpc, int thr)
{
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && pct <1 && pct>=-0.0000001)
    {
      //cout << "NBad: " << GetNbad(mpc,thr) << endl;
      //cout << "Ntot: " << GetNtowers(mpc) << endl;
      PercentNoisy[mpc][thr] = pct;
    }
  else {cout << PHWHERE << "Error in SetPercentNoisy\n";}
}


void MpcWarnMap::CalcPercentNoisy(int mpc, int thr)
{
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1))
    {
      //cout << "NBad: " << GetNbad(mpc,thr) << endl;
      //cout << "Ntot: " << GetNtowers(mpc) << endl;
      float pct = (float)GetNbad(mpc,thr)/(float)GetNtowers(mpc);
      if(pct<1 && pct>=-0.000001)
	PercentNoisy[mpc][thr] = pct;
    }
  else {cout << PHWHERE << "Error in CalcPercentNoisy\n";}
}

void MpcWarnMap::CalcAllPercentNoisy()
{
  for(int impc=0;impc<NMPC;impc++)
    for(int ithr=0;ithr<NTHRESH;ithr++)
      {
	CalcPercentNoisy(impc,ithr);
      }
}

float MpcWarnMap::GetPercentNoisy(int mpc, int thr)
{
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1)){ return PercentNoisy[mpc][thr];}
  else {cout << PHWHERE << "Error in GetPercentNoisy\n"; return -999999;}
}

//--------Get Fit Parameter Functions-------------------


float MpcWarnMap::GetChiSq(int mpc, int thr)
{
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && FitPol1[mpc][thr]!=0) return FitPol1[mpc][thr]->GetChisquare();
  else {cout << PHWHERE << "Error in GetChiSq\n"; return -999999;}
}

float MpcWarnMap::GetNDF(int mpc, int thr)
{
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && FitPol1[mpc][thr]!=0) return FitPol1[mpc][thr]->GetNDF();
  else {cout << PHWHERE << "Error in GetNDF\n"; return -999999;}
}

float MpcWarnMap::GetChiSqPerDOF(int mpc, int thr)
{ 
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && FitPol1[mpc][thr]!=0) return FitPol1[mpc][thr]->GetChisquare()/((float)FitPol1[mpc][thr]->GetNDF());
  else {cout << PHWHERE << "Error in GetChiSqPerDOF\n"; return -999999;}
}

float MpcWarnMap::GetSigma(int mpc, int thr)
{
  if(thr>=0 && thr<4 && (mpc==0 || mpc==1) && FitPol1[mpc][thr]!=0) {
    float sigma = sqrt(GetChiSqPerDOF(mpc,thr))/GetCorrection(FitArg,mpc,thr);
    return sigma;
  }
  else {cout << PHWHERE << "Error in GetSigma\n"; return -999999;}
}

float MpcWarnMap::GetFitSlope(int mpc, int thr)
{ 
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && FitPol1[mpc][thr]!=0) return FitPol1[mpc][thr]->GetParameter(1);
  else {cout << PHWHERE << "Error in GetFitSlope\n"; return -999999;}
}

float MpcWarnMap::GetFitIntercept(int mpc, int thr)
{ 
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && FitPol1[mpc][thr]!=0) return FitPol1[mpc][thr]->GetParameter(0);
  else {cout << PHWHERE << "Error in GetFitIntercept\n"; return -999999;}
}

//I only have this so people can plot the graphs. Note that if you
//modify this object then it will mess up the fits once you call
//FitGraphs().  Maybe the best usage is to say
// TGraph* a = warnmap->GetGraph(0,0)->Clone("newname");
//This way you will not affect the object

const TGraph* MpcWarnMap::GetGraph(int mpc, int thr)
{
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && NvsEta[mpc][thr]!=0) return NvsEta[mpc][thr];
  else {cout << PHWHERE << "Error in GetGraph\n"; return 0;}
}

const TF1* MpcWarnMap::GetFit(int mpc, int thr)
{
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && FitPol1[mpc][thr]!=0) return FitPol1[mpc][thr];
  else {cout << PHWHERE << "Error in GetFit\n"; return 0;}
}

const TF1* MpcWarnMap::GetFitBound(int mpc, int thr, int bound)
{
  if( thr>=0 && thr<4 && (mpc==0 || mpc==1) && bound >=0 && bound <2 && FitPol1Bounds[mpc][thr][bound]!=0) return FitPol1Bounds[mpc][thr][bound];
  else {cout << PHWHERE << "Error in GetFitBound\n"; return 0;}
}


//-------Get Functions used in Print/FilePrint--------------------------

float MpcWarnMap::GetPointVal(int thr,int ch)
{
  if(mpcmap->getGridX(ch) >=0 && mpcnoise->getNtot()>=0 && thr>=0 && thr<4)
    return (float)mpcnoise->getN(ch,thr)/(float)mpcnoise->getNtot();
  else
    { 
      cout << PHWHERE << "Bad Channel or Ntot<=0" << endl;
      return -999999;
    }
}

float MpcWarnMap::GetLineDiff(int thr, int ch)
{
  int mpc = (ch<288)?0:1;
  if(mpcmap->getGridX(ch)>=0 && thr>=0 && thr<4 && FitPol1[mpc][thr]!=0)
    {
      float fitPol1_val = FitPol1[mpc][thr]->Eval(fabs(PseudoRapidity(ch)));
      float point_val = GetPointVal(thr,ch);//(float)mpcnoise->getN(ch,thr)/(float)mpcnoise->getNtot();
      return (point_val - fitPol1_val);
    }
  else
    {
      cout << PHWHERE << "Error: Mpc Channel " << ch << " is not used or fit not performed" << endl;
      return -999999;
    }
}

int MpcWarnMap::IsOk(int thr, int ch)
{
  if(GetMpc(ch) >= 0 && thr>=0 && thr<4 && FitPol1[GetMpc(ch)][thr] != 0 )
    {
      if(fabs(GetLineDiff(thr,ch)/GetSigma(GetMpc(ch),thr))<SigmaCut) return 1; //sqrt(GetChiSqPerDOF(GetMpc(ch),thr)))  GetSigma(GetMpc(ch),thr))<
    }
  return 0;
}

//----------------------------------------------------

int MpcWarnMap::GetMpc(int ch)
{
  if(ch < 0 || ch>575) {cout << PHWHERE << "Incorrect channel\n";  return -999999;}
  else if( ch<288) return 0;
  else return 1;
}

int MpcWarnMap::GetNbad(int mpc,int thr) //all channels are returned as bad with bad arguments
{
  int N_bad=0;
  int ch_min = (mpc == 0)?0:HALF_MAXCH;
  int ch_max = (mpc == 0)?HALF_MAXCH:MAXCH;
  for(int ich=ch_min;ich<ch_max;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  if(!IsOk(thr,ich)) N_bad++;
	}
    }
  return N_bad;
}

//--------------Summary/print/fileprinting functions------------------

void MpcWarnMap::Summarize()
{
  for(int impc=0;impc<2;impc++)
    {
      std::string mpc_str = (impc == 0)?"SOUTH MPC: N_towers= ":"NORTH MPC: N_towers= "; 
    
      cout << mpc_str << GetNtowers(impc) << endl;
      cout << "N_bad Towers for each threshold: ";
      for(int ithr=0;ithr<4;ithr++)
	{
	  cout << GetNbad(impc,ithr) << "\t";
	}
      cout << endl;
      cout << "% of bad Towers for each threshold: ";
      for(int ithr=0;ithr<4;ithr++)
	{
	  cout << (float)GetNbad(impc,ithr)/(float)GetNtowers(impc) << "\t";
	}
      cout << endl;
    }
}

void MpcWarnMap::PrintLineDiff()
{
  // cout << "SigmaCut is " << SigmaCut << endl;
  
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  cout << ich << "\t";
	  cout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) cout << setprecision(3) << GetLineDiff(ithr,ich) << "\t";
	  cout << endl;
	}
    }
}

void MpcWarnMap::PrintIsOk()
{
  cout << "SigmaCut is " << SigmaCut << endl;
  
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  cout << ich << "\t";
	  cout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) cout << setprecision(3) << IsOk(ithr,ich) << "\t";
	  cout << endl;
	}
    }
}

void MpcWarnMap::PrintLineDiffRatio()
{
  //cout << "SigmaCut is " << SigmaCut << endl;
  
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  int impc = (ich<288)?0:1;
	  cout << ich << "\t";
	  cout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) cout << setprecision(3) << (GetLineDiff(ithr,ich)/sqrt(GetChiSqPerDOF(impc,ithr))) << "\t";
	  cout << endl;
	}
    }
}

void MpcWarnMap::PrintLineDiffRatioCorrected()
{
  //cout << "SigmaCut is " << SigmaCut << endl;
  
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  int impc = (ich<288)?0:1;
	  cout << ich << "\t";
	  cout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) cout << setprecision(3) << (GetLineDiff(ithr,ich)/GetSigma(impc,ithr)) << "\t";
	  cout << endl;
	}
    }
}

void MpcWarnMap::PrintPointVal()
{
  cout << "SigmaCut is " << SigmaCut << endl;
  
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  cout << ich << "\t";
	  cout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) cout << setprecision(3) << GetPointVal(ithr,ich) << "\t";
	  cout << endl;
	}
    }
}

void MpcWarnMap::FilePrintLineDiff(const char* fname)
{
  // cout << "SigmaCut is " << SigmaCut << endl;
  ofstream dout;
  dout.open(fname);
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  dout << ich << "\t";
	  dout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) dout << setprecision(3) << GetLineDiff(ithr,ich) << "\t";
	  dout << endl;
	}
    }
  dout.close();
}

void MpcWarnMap::FilePrintIsOk(const char* fname)
{
  //cout << "SigmaCut is " << SigmaCut << endl;
  ofstream dout;
  dout.open(fname);
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  dout << ich << "\t";
	  dout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) dout << setprecision(3) << IsOk(ithr,ich) << "\t";
	  dout << endl;
	}
    }
  dout.close();
}

void MpcWarnMap::FilePrintLineDiffRatio(const char* fname)
{
  //cout << "SigmaCut is " << SigmaCut << endl;
  ofstream dout;
  dout.open(fname);
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  int impc = (ich<288)?0:1;
	  dout << ich << "\t";
	  dout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) dout << setprecision(3) << (GetLineDiff(ithr,ich)/sqrt(GetChiSqPerDOF(impc,ithr))) << "\t";
	  dout << endl;
	}
    }
  dout.close();
}

void MpcWarnMap::FilePrintLineDiffRatioCorrected(const char* fname)
{
  //cout << "SigmaCut is " << SigmaCut << endl;
  ofstream dout;
  dout.open(fname);
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  int impc = (ich<288)?0:1;
	  dout << ich << "\t";
	  dout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) dout << setprecision(3) << (GetLineDiff(ithr,ich)/GetSigma(impc,ithr)) << "\t";
	  dout << endl;
	}
    }
  dout.close();
}

void MpcWarnMap::FilePrintPointVal(const char* fname)
{
  //cout << "SigmaCut is " << SigmaCut << endl;
  ofstream dout;
  dout.open(fname);
  for(int ich=0;ich<MAXCH;ich++)
    {
      if(mpcmap->getGridX(ich)>=0)
	{
	  dout << ich << "\t";
	  dout << fixed;
	  for(int ithr=0;ithr<NTHRESH;ithr++) dout << setprecision(3) << GetPointVal(ithr,ich) << "\t";
	  dout << endl;
	}
    }
  dout.close();
}
