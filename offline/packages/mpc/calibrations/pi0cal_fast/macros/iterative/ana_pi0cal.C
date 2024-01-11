/*
Beau Meredith 11-11-2010

This class takes output from the Subsysreco class mpcPi0TTree and uses
it to improve the calibrations for the MPC. The idea is to match the
pi0 mass value in each tower to that determined from running 200 GeV
p+p pythia through GEANT.  The matching file is found at
sim_means_combined.txt in this directory.  The output (pictures and
gain files) can be found in the directory ./recal.


The value of the mass is changed by taking

gain_new = gain x expected mass/reconstructed mass

I use a simplified procedure to find the value of the mass in each
bin.  There is no subtraction, but rather a Gaussian is fit a small
region (+/- 25 MeV) around the maximum of the mass peak.  The mean
value and standard deviation are then calculated from this fit; the
stdv is still a good measure of the width of the peak, but is not an
exact measure.

The procedure used has many of the tools used in reconstruction, and
hence if the cluster reconstructoin changes then this code must be
changed accordingly.  These include the x,y shifts from C.G
coordinates to actual hit positoins, calculating Ecore, and using the
shower shape.

8 iterations are performed to get the procedure to converge.  The cuts
made (some are made in mpcPi0TTree) are:

fabs(zvtx) < 30.0

Eclus > 2
chi2/dof < 3
Max(dispx, dispy) < 4.0

pt_pair > 0.45
9 < E_pair < 17
Easymm < 0.6

*/


#include <iostream>
#include <TGraphErrors.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TCanvas.h>
#include <fstream>
#include <string>
#include <TTree.h>
#include <TChain.h>
#include <THmulf.h>
#include <cmath>
#include <TFile.h>
#include <TF1.h>
#include <TStyle.h>
#include <TSystem.h>
#include <MpcMap.h>
#include <algorithm>
#include <vector>
#include <deque>
#include <TPad.h>
#include <TPaveText.h>
#include <TText.h>

using namespace std;

extern TStyle *gStyle;
extern TSystem *gSystem;

int verbosity = 0;

TChain *ttree;
TString DeadTowerList;
int tower[2][18][18];
MpcMap* mpcmap;
bool loaded = 0;


TH1D* hmean[8][2];
TH1D* hstdv[8][2];

TH1D* hpi[2][18][18];
TH1D* hperm[8][2][18][18];

float sim_mass[2][18][18];
float gain[9][2][18][18];
float data_mass[9][2][18][18];

int event; 
int run;
float pi0_z;
float x1;  
float yt1;
float z1;
float x2;
float y2;
float z2;
float px1;  
float py1;
float pz1;
float px2;
float py2;
float pz2;  
float energy1;
float energy2;
float pi0_px; 
float pi0_py; 
float pi0_pz;
float pi0_energy;
float mass;
float pt;
int arm1;
int arm2;



int driver1;
int driver2;
int fee1;
int fee2;

float etot1;
float etot2;
float cdisp;
float disp;

  
float ldisp;
float lcdisp;
float corrdispx1;
float corrdispy1;
float corrdispx2;
float corrdispy2;
float logcorrdispx1;
float logcorrdispy1;
float logcorrdispx2;
float logcorrdispy2;
float dispx1;
float dispy1;
float dispx2;
float dispy2;
float logdispx1;
float logdispy1;
float logdispx2;
float logdispy2;


float eta;
float phi;
float theta;

float chi1;
float chi2;
float pi0_chi;

int ixpos1;
int iypos1;
int ixpos2;
int iypos2;

float etow1[220];
float ch1[220];
float etow2[220];
float ch2[220];
int ntow1;
int ntow2;



float fShowPar2[6][4]; //exp(-r)+exp^(-r^3)+exp^(-r^5) used in sharing
float fShow2[6]; //parameters used in sharing

//copied over from MpcSectorRecV2.C
void SetProfileParametersV2_2(float Energy);
float EvalShowPar2(int ipar, float e);
float PredictEnergyV2_2(float x, float y, float Energy);
float PredictEnergyV2_2(float r,float Energy);
float GetXCG(float x, float y,int arm);
float GetYCG(float y);
bool GetEcoreStatus(int feech, float etot, float xval, float yval);
float GetEtot(int nch, float et[]);
float GetEcore(int nt, float et[], float ch[], float x, float y);
void Verbosity(int v){ verbosity = v;}
//void set_new_gains(int itr, int nt, float et[], int chs[]);

void get_sim_mass(char* infile);
void chain(char* name, char* list);
void init_cal();
void InitWM();
void set_new_gains(int itr, int nt, float et[], float chs[]);

void run_fitter(int itr);
void run_loop(int itr,long nent);

float GetXCG(float x, float y,int arm){
  float ycg = GetYCG(y);
  float s = 0.4642; //arm = 1 defaults
  float c1 = 3.26463e-02;
  
  if ( arm==0 && fabs(ycg)>=9.3575 )	// TOP_SMPC
    {
      s = 0.15875;	// 1/16"
      c1 = 2.070245e-02;
      }
  else if ( arm==0 && fabs(ycg)<9.3575 )	// MID_SMPC
    {
      s = 0.4642;
      c1 = 2.070245e-02;
    }
  //i have no idea why this is like it is
  //needs to be changed desperately think
  float sign = (x >=0)?1:-1;
  return sign*(fabs(x)-c1*s)/(1-c1);
}

float GetYCG(float y){
  float s = 2.0*0.15875;	// used to get back onto the rectilinear grid, = 2*1/16"
  float c1 = 3.58578e-2;
  float sign = (y >=0)?1:-1;
  return sign*(fabs(y)-c1*s)/(1-c1);

}

float GetEcore(int nt, float et[], float ch[], float x, float y){

  float etot = GetEtot(nt,et);
  if(verbosity > 0) cout << "etot is: " << etot << endl;
  SetProfileParametersV2_2(etot);
  int ntowers = nt;
  int ncoretwrs = 0;
  float myecore = 0;
  if(verbosity > 0) cout << "ntow is: " << ntowers << endl;
  for (int it=0; it<ntowers; it++)
    {
      int twrid = ch[it];
      if( (ch[it]- twrid) > 0.05) twrid++;
      if(verbosity > 0) cout << "ch/F , ch/I: " << ch[it] << ", " << twrid << endl;
      
      if(verbosity > 0) cout << "x,y: " << x << ", " << y << endl;
      bool status = GetEcoreStatus(twrid,etot,x,y);
      if(verbosity > 0) cout << "ecore status is: " << status << endl;
      if( status == 0) continue;
      
      ncoretwrs++;
      myecore+=et[it]/0.918;
    }

   const float p0 = 0.9759;
   const float p1 = 0.03201;
   const float p2 = 0.09286;
   float ec2 = myecore;
   float ep0 = ec2/0.98;
   
   float ratio = p0+p1*exp(-p2*ep0);
   if(ratio < 0.9 || ratio > 1.1){
     
     static int nwarns = 0;
     nwarns++;
     if(nwarns <=5){
       std::cout << ratio << " warning...ratio is out of expected bounds\n"  << endl;
     }
     ratio = 1.0;
   }
   float ec_final = ec2/ratio;

   
   return ec_final;
}



//copied over from MpcSectorRecV2.C

void SetProfileParametersV2_2(float Energy)
{
  //shower shape parameters
  for(int ipar=0;ipar<6;ipar++){
    fShow2[ipar] = EvalShowPar2(ipar,Energy); //used for energy sharing in clusters
  }
  return;
}


float EvalShowPar2(int ipar, float e){
  float par = 0;
  if(e > 60) e = 60;
  par = fShowPar2[ipar][0]+fShowPar2[ipar][1]*e+fShowPar2[ipar][2]*e*e+fShowPar2[ipar][3]*e*e*e;
  return par;
}

float PredictEnergyV2_2(float x, float y, float Energy)
{
  float ret = 0;
  float r = sqrt(x*x+y*y);
  ret = fShow2[0]*exp(-fShow2[1]*r)+fShow2[2]*exp(-fShow2[3]*r*r*r)+fShow2[4]*exp(-fShow2[5]*r*r*r*r*r);
  
  return ret;
}

float PredictEnergyV2_2(float r,float Energy)
{
  float ret = 0;
  ret = fShow2[0]*exp(-fShow2[1]*r)+fShow2[2]*exp(-fShow2[3]*r*r*r)+fShow2[4]*exp(-fShow2[5]*r*r*r*r*r);
  
  return ret;
}
 

float GetEtot(int nch, float et[]){
  float etot = 0;
  for(int i=0;i<nch;i++){
    etot+=et[i];
  }
  return etot;
}


bool GetEcoreStatus(int feech, float etot, float xval, float yval)
{

   //use logxcg() b/c x() has angle dependent correction
  int arm = mpcmap->getArm(feech);
  float logxcg = GetXCG(xval,yval,arm);;
  float logycg = GetYCG(yval);;

  float tow_x  = mpcmap->getX(feech);
  float tow_y  = mpcmap->getY(feech);

  //have to convert sizes to module units
  float dr = sqrt( pow( (logxcg-tow_x) ,2) + pow( (logycg-tow_y) ,2) )/2.26;
  float efrac = PredictEnergyV2_2(dr,etot);
  if(efrac < 0.02) return 0;
  else return 1;
}



void get_sim_mass(char* infile){
  
  ifstream din(infile);
  while(din){
    cout << "getting sim mass\n";
    int dummy;
    int arm, ix, iy;
    float msim;
    din >> arm;
    if(arm < 0 || arm > 1) break;
    din >> ix;
    if(ix < 0 || ix > 17) break;

    din >> iy;
    if(iy < 0 || iy > 17) break;

    din  >> msim;
    cout << "arm, ix,iy,mass: " << arm << ", " << ix << ", " << iy << ", " << msim << endl;
    if(msim < 0.09 || msim > 0.18) break;
    cout << "arm, ix,iy,mass: " << arm << ", " << ix << ", " << iy << ", " << msim << endl;
    sim_mass[arm][ix][iy] = msim;
  }
  return;
}



void init_cal(){
  for(int iarm=0;iarm<2;iarm++){
    for(int ix=0;ix<18;ix++){
      for(int iy=0;iy<18;iy++){
	sim_mass[iarm][ix][iy] = -1;
      }
    }
  }

  for(int itr=0;itr<8;itr++){
    for(int iarm=0;iarm<2;iarm++){
      for(int ix=0;ix<18;ix++){
	for(int iy=0;iy<18;iy++){
	  data_mass[itr][iarm][ix][iy] = -1;
	}
      }
    }
  }
  
  for(int itr=0;itr<8;itr++){
    for(int iarm=0;iarm<2;iarm++){
      for(int ix=0;ix<18;ix++){
	for(int iy=0;iy<18;iy++){
	  gain[itr][iarm][ix][iy] = 1; //default behavior...no modification
	}
      }
    }
  }

  fShowPar2[0][0]=0.109076;  
  fShowPar2[0][1]=-0.0020524;  
  fShowPar2[0][2]=4.83446e-05;  
  fShowPar2[0][3]=-3.58957e-07;  
  
  fShowPar2[1][0]=1.6985;  
  fShowPar2[1][1]=-0.00639118;  
  fShowPar2[1][2]=0.000153674;  
  fShowPar2[1][3]=-1.17523e-06;  
  
  fShowPar2[2][0]=0.190027;  
  fShowPar2[2][1]=-0.00752877;  
  fShowPar2[2][2]=0.000176111;  
  fShowPar2[2][3]=-1.28594e-06;  
  
  fShowPar2[3][0]=2.42983;  
  fShowPar2[3][1]=-0.0417858;  
  fShowPar2[3][2]=0.000893255;  
  fShowPar2[3][3]=-6.32293e-06;  
  
  fShowPar2[4][0]=0.516242;  
  fShowPar2[4][1]=0.010746;  
  fShowPar2[4][2]=-0.000251936;  
  fShowPar2[4][3]=1.84155e-06;  
  
  fShowPar2[5][0]=15.0943;  
  fShowPar2[5][1]=-0.0981804;  
  fShowPar2[5][2]=0.00261719;  
  fShowPar2[5][3]=-1.90878e-05;  




  return;
}


void chain(char* name, char* list){
  ttree = new TChain(name);
  std::ifstream din;
  din.open(list);
  while(din){
    std::string temp;
    din >> temp;
    TString t_temp = temp;
    if(t_temp.Contains(".root"))
      {
        ttree->Add(t_temp.Data());
      }
  }
  return;
}

void InitWM(){
  for(int iarm=0;iarm<2;iarm++){
    for(int ix=0;ix<18;ix++){
      for(int iy=0;iy<18;iy++){
	tower[iarm][ix][iy] = 0;
      }
    }
  }
  ifstream inFile;
  DeadTowerList = "/direct/phenix+hp/data06/bmeredi2/../bmeredi2_taxi/warnmap/mpcpp2.txt";
  //this is the run8 dead tower list
  inFile.open(DeadTowerList.Data(),ios::in);
  if (inFile.is_open())
    {
      while (! inFile.eof() )
	{
          //int count=0;
          int arm, x,y;
          inFile>>arm >>x>>y;
          tower[arm][y][x] = -1;
          if(arm < 0 || arm > 1) {cout << "exiting wm\n"; break;}
        }
      inFile.close();
      for(int impc=0;impc<2;impc++)
        for(int iy=0;iy<18;iy++)
	  for(int ix=0;ix<18;ix++)
	    {std::cout << mpcmap->getFeeCh(ix,iy,impc) << ", " << tower[impc][iy][ix] << endl;}
    }
  else cout << "Unable to open file";
  return;
}
  




void set_new_gains(int itr, int nt, float et[], float chs[]){
  for(int i=0;i<nt;i++){
    int twrid = chs[i];
    if( (chs[i]-twrid) > 0.05) twrid++;
    int ix = mpcmap->getGridX(twrid);
    int iy = mpcmap->getGridY(twrid);
    int arm = mpcmap->getArm(twrid);
    if(ix < 0){ cout << "something is wrong in set_new_gains\n"; continue;}
    if(gain[itr][arm][ix][iy] <= 0){ cout << "something wrong in set_new_gains..gain < 0 for ch: " << ix << ", " << iy << endl; continue;}
    et[i] = et[i]*gain[itr][arm][ix][iy];
  }
  return;
}




void run_fitter(int itr){
  
  int driver_colors[20] = {30,4,5,6,7,8,9,10,11,12,30,4,5,6,7,8,9,10,11,12};
  
  ofstream dout, dout_mass;
  TString name = "recal/gains_data_"; name+=(itr);
  dout.open(name.Data());

  name = "recal/mass_data_"; name+=(itr);
  dout_mass.open(name.Data());
  
  TCanvas* cbig[2];
  cbig[0]= new TCanvas("cbigS","cbigS",4000,4000);
  cbig[0]->Divide(18,18,0.000001,0.000001);
  cbig[1]= new TCanvas("cbigN","cbigN",4000,4000);
  cbig[1]->Divide(18,18,0.000001,0.000001);
  
  for(int iarm=0;iarm<2;iarm++){
    for(int ix=0;ix<18;ix++){
      for(int iy=0;iy<18;iy++){

      // Dead area in north bottom rows
      // Should put into dead towers list, not hard-code here
      //if(iarm == 1 && iy == 1 && (ix >=5 && ix <= 12)) continue;
      //if(iarm == 1 && iy == 0 && (ix >=6 && ix <= 11)) continue;

      int feech= mpcmap->getFeeCh(ix,iy,iarm);
      if(ix == 0 && iy == 7) cout << "ix = 0, iy = 7, feech = " << feech << endl;
      int driver = mpcmap->getDriver(feech);
      
      if(ix == 0 && iy == 7) cout << "ix = 0, iy = 7, feech = " << feech << " driver is: " << driver << endl;
      
      if(feech < 0 || feech > 576) continue;
      if(mpcmap->getGridX(feech) < 0) continue;
      
      TH1* h = hperm[itr][iarm][ix][iy];
      int ipad = (17-iy)*18+ix;
      cbig[iarm]->cd(ipad+1);
      cbig[iarm]->GetPad(ipad+1)->SetFrameFillColor(driver_colors[(driver-1)]);
      
      TF1* fit = new TF1("fgaus","gaus",0,1);
      fit->SetParameter(0,1);
      fit->SetParameter(1,0.135);
      fit->SetParameter(2,0.15);
      
      int maxbin = h->GetMaximumBin();
      float val = h->GetBinCenter(maxbin);
      float val_lo = val-0.025;
      float val_hi = val+0.025;
      
      
      h->Fit(fit,"Q","",val_lo,val_hi);
      float mean = fit->GetParameter(1);
      float std = fit->GetParameter(2);
      float mean_err = fit->GetParError(1);
      float std_err = fit->GetParError(2);
      
      float maxmassyield = val;
      
      data_mass[itr][iarm][ix][iy] = mean;
      if(mean <= 0) data_mass[itr][iarm][ix][iy] = -1;
      if(itr <7){
      float sm = sim_mass[iarm][ix][iy];
      if(sm < 0 || data_mass[itr][iarm][ix][iy] <=0){
      gain[itr+1][iarm][ix][iy] = 1;
      cout << "ch: " << ix << ", " << iy << " has problem: sim gain is: " << sm << ", mean mass is: " << mean << endl;
	  
	  
	  
	}
	else{

	  if(itr == 0) gain[itr+1][iarm][ix][iy] = sm/mean;
	  else gain[itr+1][iarm][ix][iy] = sm/mean*gain[itr][iarm][ix][iy];
	  
	  cout << "itr, ix, iy, mean, simmass, new gain: " << itr << ", " << ix << ", " << iy << ", " << gain[itr+1][iarm][ix][iy] <<  endl;
	}

	dout  << iarm << "\t" << ix << "\t" << iy << "\t" << gain[itr+1][iarm][ix][iy] <<  endl;
	dout_mass  << iarm << "\t" << ix << "\t" << iy << "\t" << sm << "\t" << mean << "\t" << std <<  endl;


      }
      
      hmean[itr][iarm]->Fill(mean);
      hstdv[itr][iarm]->Fill(std);
      
      TPaveText *ptyield = new TPaveText(0.55,0.6,0.89,0.89,"brNDC");
      ptyield->SetLineColor(1);
      char m1[1024];
      char m2[1024];
      char m3[1024];
      sprintf(m1,"mean = %.3f +/- %.3f",mean, mean_err);
      sprintf(m2,"max = %.3f",maxmassyield);
      sprintf(m3,"std = %.3f +/- %.3f",std, std_err);
      
      TText* ty1 = ptyield->AddText(m1);
      TText* ty2 = ptyield->AddText(m2);
      TText* ty3 = ptyield->AddText(m3);
      
      if(mean > 0.18 || std>0.05){
        h->SetFillColor(2);
      }
      
      h->SetFillColor(46);
      h->Draw();
      ptyield->Draw("same");
      fit->Draw("same");
      }
    }
  }
  TString fname = "recal/data_mass_itr"; fname+=itr; fname+="_N.png";
  cbig[1]->SaveAs(fname.Data());
  fname.ReplaceAll("N.png","S.png");
  cbig[0]->SaveAs(fname.Data());

  TCanvas* c_mass = new TCanvas("c_mass","c_mass",800,800);
  c_mass->Divide(2,2);
  
  hmean[itr][0]->SetTitle("South Mpc Pi0 Mean Distribution;Mean Mass (GeV/c^{2}");
  hmean[itr][1]->SetTitle("North Mpc Pi0 Mean Distribution;Mean Mass (GeV/c^{2}");
  hstdv[itr][0]->SetTitle("South Mpc Pi0 Stdv Distribution;sigma mass (GeV/c^{2}");
  hstdv[itr][1]->SetTitle("North Mpc Pi0 Stdv Distribution;sigma mass (GeV/c^{2}");
  

  c_mass->cd(1);
  hmean[itr][0]->Draw();
  c_mass->cd(2);
  hstdv[itr][0]->Draw();
  c_mass->cd(3);
  hmean[itr][1]->Draw();
  c_mass->cd(4);
  hstdv[itr][1]->Draw();
  
  fname = "recal/data_distributions_itr"; fname+=itr; fname+=".png";
  c_mass->SaveAs(fname.Data());

}


void run_loop(int itr, long nent){
  
  Long64_t nentries = ttree->GetEntries();
  if(nent <= 0 || nent > nentries) nent = nentries; 
  for (Long64_t i=0; i<nent;i++) 
    {
      
      if( i%10000 == 0 ) cout << i << endl;
      ttree->GetEntry(i);
      
      //      if(pi0_energy < 10) continue;
      //      if(arm1 != 1) continue;
      //if(arm1 == 0) continue;
      if(pi0_chi > 4) continue;
      if(disp > 4) continue;
      //      if(pi0_energy > 17) continue;

      
      float r1 = sqrt(x1*x1+yt1*yt1);
      //      if(r1 < 11 || r1 > 19) continue;
      float r2 = sqrt(x2*x2+y2*y2);
      //      if(r2 < 11 || r2 > 19) continue;
      eta = -log(tan(theta/2.));
      //      if( fabs(eta) > 3.8 || fabs(eta) < 3.1) continue;
      float zvtx = pi0_z - 220;
      if(arm1 == 0) zvtx = pi0_z + 220;
      if(fabs(zvtx) > 30) continue;

      //we use the same cuts above all the time

      float old_ec1 = etow1[0];
      float old_ec2 = etow2[0];

      if(itr > 0) set_new_gains(itr,ntow1,etow1,ch1);
      if(itr > 0) set_new_gains(itr,ntow2,etow2,ch2);
      
      if(i%10000 == 0){
        float e1 = GetEcore(ntow1,etow1,ch1, x1,yt1);
        float e2 = GetEcore(ntow2,etow2,ch2, x2,y2);
        cout << "itr, e1, e2 vs en1, en2: " << itr << ", " << e1 << ", " << e2
             << "      vs     " << energy1 << ", " << energy2 << endl;
      }
      
      float old_e1 = energy1;
      float old_e2 = energy2;

      if(itr > 0) energy1 = GetEcore(ntow1,etow1,ch1, x1,yt1);
      if(itr > 0) energy2 = GetEcore(ntow2,etow2,ch2, x2,y2);
      if(itr > 0) pi0_energy = energy1+energy2;
      
      if(pi0_energy < 9) continue;
      if(pi0_energy > 17) continue;
      if(pt < 0.45) continue;

      //m = sqrt(e1e2)*sqrt((1-cos theta)) or something close
      if(energy1 <= 0 || energy2 <= 0 || old_e1 <=0 || old_e2 <= 0){
        cout << "something is terribly wrong in loop...an energy < 0\n";
        continue;
      }

      float mass_factor = sqrt(energy1*energy2/old_e1/old_e2);

      //      cout << "here1\n";
      int ix1 = mpcmap->getGridX(fee1);
      int iy1 = mpcmap->getGridY(fee1);
      //      if(tower[1][iy1][ix1] < 0) continue;

      int ix2 = mpcmap->getGridX(fee2);
      int iy2 = mpcmap->getGridY(fee2);
      ixpos1 = ix1;
      ixpos2 = ix2;
      iypos1 = iy1;
      iypos2 = iy2;

      // Put dead towers in dead tower list!!!
      //if(fee1 == 95 || fee2 == 95) continue;
      //if(iy1 == 0 && arm1 == 1) continue; 
      //if(iy2 == 0 && arm1 == 1) continue; 
      //if(iy1 == 1 && ix1 <=12 && ix1 >=5  && arm1 == 1) continue; 
      //if(iy2 == 1 && ix2 <=12 && ix2 >=5  && arm1 == 1) continue; 

      bool fill1 = 1; 
      bool fill2 = 1; 
      
      int driver1 = mpcmap->getDriver(fee1); 
      int driver2 = mpcmap->getDriver(fee2); 
      int edge1 = mpcmap->isEdge(fee1); 
      int edge2 = mpcmap->isEdge(fee2); 
      

      if(edge2 == 1) fill1 = 0; 
      //if(driver2 == 18 && driver1 != 18) fill1 = 0; 
      
      if(edge1 == 1) fill2 = 0; 
      //if(driver1 == 18 && driver2 != 18) fill2 = 0; 

      if(i%10000 == 0){
	
        cout << "ix1,iy1, ratio new/old energy1, gain1 : " << ix1 << ", " << iy1 << ", " << energy1/old_e1 << ", " << gain[itr][arm1][ix1][iy1] << endl;
        cout << "ec1, ec1_old: " << etow1[0] << ", " << old_ec1 << ", " << (etow1[0]/old_ec1) << endl; 
        cout << "ix2,iy2, ratio new/old energy2, gain2 : " << ix2 << ", " << iy2 << ", " << energy2/old_e2 << ", " << gain[itr][arm1][ix2][iy2] << endl;
        cout << "ec2, ec2_old: " << etow2[0] << ", " << old_ec2 << ", " << (etow2[0]/old_ec2) << endl; 
        cout << "massfactor, mass, mass*massfactor: " << mass << ", " << mass_factor << ", " << (mass*mass_factor) << endl;
      }

      if(i%1000 == 0 && ( (ix1 == 16 && iy1 == 7) || (ix2 == 16 && iy2 == 7) ) ){
	
        cout << "======================look at me==========================\n";
        cout << "ix1,iy1, ratio new/old energy1, gain1 : " << ix1 << ", " << iy1 << ", " << energy1/old_e1 << ", " << gain[itr][arm1][ix1][iy1] << endl;
        cout << "ec1, ec1_old: " << etow1[0] << ", " << old_ec1 << ", " << (etow1[0]/old_ec1) << endl; 
        cout << "ix2,iy2, ratio new/old energy2, gain2 : " << ix2 << ", " << iy2 << ", " << energy2/old_e2 << ", " << gain[itr][arm1][ix2][iy2] << endl;
        cout << "ec2, ec2_old: " << etow2[0] << ", " << old_ec2 << ", " << (etow2[0]/old_ec2) << endl; 
        cout << "massfactor, mass, mass*massfactor: " << mass << ", " << mass_factor << ", " << (mass*mass_factor) << endl;
        cout << "---------------------done looking------------------------------n";
      }




      float weight1 = energy1/pi0_energy;
      float weight2 = energy2/pi0_energy;
      //      cout << "here2\n" << "ix1,iy1, ix2, iy2: " << ix1 << ", " << iy1 << ", " << ix2 << ", " << iy2 << endl;
      //      cout << hperm[itr][ix1][iy1] << endl;
      TH1* h1 = hperm[itr][arm1][ix1][iy1];
      TH1* h2 = hperm[itr][arm1][ix2][iy2];
      //      cout << "here3\n";
      if(fill1) h1->Fill(mass_factor*mass,weight1);
      if(fill2) h2->Fill(mass_factor*mass,weight2);
      //      cout << "here4\n";
      
    }
}




void ana_pi0cal(char* inlist = "list_ttree.txt", char* outfile="out_ana.root", int nent = 10000000){
  //here list is the input list of files from the ttree analysis
  //nent is the number of entries that will be processed in each iteration
  
  Verbosity(0);
  
  for(int itr =0;itr<8;itr++){
    for(int iarm=0;iarm<2;iarm++){
      TString mname = "hmean_arm"; mname+=iarm; mname+="_"; mname+=itr;
      hmean[itr][iarm] = new TH1D(mname.Data(),mname.Data(),100,0.05,0.25);
      mname.ReplaceAll("mean","stdv");
      hstdv[itr][iarm] = new TH1D(mname.Data(),mname.Data(),100,0.0,0.04);
      
      for(int ix=0;ix<18;ix++){
        for(int iy=0;iy<18;iy++){
          TString name = "hpi_itr"; name+=itr; name+="_arm"; name+=iarm; name+="_x"; 
          name+=ix; name+="_y"; name+=iy;
          hperm[itr][iarm][ix][iy] = new TH1D(name.Data(),name.Data(),200,0,0.5);
	  //      name.ReplaceAll("hpi","hpibg");
	  //      hpibg[ix][iy] = new TH1D(name.Data(),name.Data(),200,0,0.5);
        }
      }
    }
  }
    
 
  mpcmap = MpcMap::instance();
  
  TH2F* h2_m_phi = new TH2F("mvphi","mass vs phi",100,0,1,50,-3.14159,3.14159);
  
  InitWM();
  TString name = "pi0_ttree";
  //TString name = "T";
  chain( const_cast<char*> (name.Data()),inlist);
  
  
  init_cal();
  get_sim_mass("sim_means_combined.txt");
  
  ttree->SetBranchAddress("event",&event);
  ttree->SetBranchAddress("x1",&x1);
  ttree->SetBranchAddress("y1",&yt1);
  ttree->SetBranchAddress("x2",&x2);
  ttree->SetBranchAddress("y2",&y2);
  //  ttree->SetBranchAddress("pi0_x",&pi0_x,"pi0_x/F"); 
  //ttree->SetBranchAddress("pi0_y",&pi0_y,"pi0_y/F"); 
  ttree->SetBranchAddress("pi0_z",&pi0_z);
  

  ttree->SetBranchAddress("disp",&disp);
  ttree->SetBranchAddress("cdisp",&cdisp);
  ttree->SetBranchAddress("ldisp",&ldisp);
  ttree->SetBranchAddress("lcdisp",&lcdisp);
  
  ttree->SetBranchAddress("chi2",&chi2);
  ttree->SetBranchAddress("chi1",&chi1);
  ttree->SetBranchAddress("pi0_chi",&pi0_chi);
  
  

  ttree->SetBranchAddress("pi0_px",&pi0_px);
  ttree->SetBranchAddress("pi0_py",&pi0_py);
  ttree->SetBranchAddress("pi0_pz",&pi0_pz);
  ttree->SetBranchAddress("phi",&phi);
  ttree->SetBranchAddress("theta",&theta);
  ttree->SetBranchAddress("px1",&px1);
  ttree->SetBranchAddress("py1",&py1);
  ttree->SetBranchAddress("pz1",&pz1);
  ttree->SetBranchAddress("px2",&px2);
  ttree->SetBranchAddress("py2",&py2);
  ttree->SetBranchAddress("pz2",&pz2);
  ttree->SetBranchAddress("energy1",&energy1);
  ttree->SetBranchAddress("energy2",&energy2);
  ttree->SetBranchAddress("pi0_energy",&pi0_energy);
  ttree->SetBranchAddress("mass",&mass);
  ttree->SetBranchAddress("pt",&pt);
  ttree->SetBranchAddress("arm1",&arm1);
  ttree->SetBranchAddress("fee1",&fee1);
  ttree->SetBranchAddress("fee2",&fee2);
  

  ttree->SetBranchAddress("ntow1",&ntow1);
  ttree->SetBranchAddress("etow1",etow1);
  ttree->SetBranchAddress("ch1",ch1);
  
  ttree->SetBranchAddress("ntow2",&ntow2);
  ttree->SetBranchAddress("etow2",etow2);
  ttree->SetBranchAddress("ch2",ch2);
  

  for(int itr=0;itr<8;itr++){
    run_loop(itr,nent);
    run_fitter(itr);
  }  
  
  TFile *fout = new TFile(outfile,"recreate");
  
  
  for(int itr=0;itr<8;itr++){
    for(int iarm=0;iarm<2;iarm++){
      for(int ix=0;ix<18;ix++){
        for(int iy=0;iy<18;iy++){
          hperm[itr][iarm][ix][iy]->Write();
	  
        }
      }
    }
  }
  
  
}



