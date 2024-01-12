#include <TofwrecalReco.h>

#include <TofwCalib.h>
#include <RunHeader.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>
#include <RunToTime.hh>

#include <PHCompositeNode.h>
#include <gsl/gsl_const.h>

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

using namespace std;


TofwrecalReco::TofwrecalReco(const string &name): Recalibrator(name)
{
    //baseclasses.insert("PHCentralTrackv23");
    //baseclasses.insert("PHCentralTrackv24");
    baseclasses.insert("PHCentralTrack");
    tofwcalib = 0;
    return;
}


TofwrecalReco::~TofwrecalReco()
{
    delete tofwcalib;
    return;
}

int TofwrecalReco::isValidRun(const int runno) const
{
 
    if(runno >= 227016 && runno <= 240121){   return 1;   }   //  Run7                     1
    if(runno >= 246214 && runno <= 253701){   return 1;   }   //  Run8dAu200GeV            2
    if(runno >= 256450 && runno <= 259575){   return 1;   }   //  Run8pp200GeV             3
    if(runno >= 276324 && runno <= 280240){   return 1;   }   //  Run9pp500GeV             4
    if(runno >= 281911 && runno <= 291515){   return 1;   }   //  Run9pp200GeV             5
    if(runno >= 300475 && runno <= 310454){   return 1;   }   //  Run10AuAu200             6
    if(runno >= 310698 && runno <= 313322){   return 1;   }   //  Run10AuAu62              7
    if(runno >= 313591 && runno <= 314994){   return 1;   }   //  Run10AuAu39              8
    if(runno >= 315450 && runno <= 318944){   return 1;   }   //  Run10AuAu7               9
    if(runno >= 331130 && runno <= 340515){   return 1;   }   //  Run11pp500              10
    if(runno >= 341270 && runno <= 342345){   return 1;   }   //  Run11AuAu19GeV          11
    if(runno >= 343031 && runno <= 349680){   return 1;   }   //  Run11AuAu200GeV         12
    if(runno >= 349830 && runno <= 350577){   return 1;   }   //  Run11AuAu27GeV          13
    if(runno >= 358629 && runno <= 363228){   return 1;   }   //  Run12pp200GeV           14
    if(runno >= 364822 && runno <= 368798){   return 1;   }   //  Run12pp510GeV           15
    if(runno >= 369327 && runno <= 371908){   return 1;   }   //  Run12UU192GeV  (193)    16
    if(runno >= 372403 && runno <= 377310){   return 1;   }   //  Run12CuAu200GeV         17
    if(runno >= 386773 && runno <= 398149){   return 1;   }   //  Run13pp510GeV           18

    if(runno >= 415751 && runno <= 416892){   return 1;   }   //  Run14HeAu200GeV         19

    return 0;
    //  	(runNumber>= 358629 && runNumber <= 363228) || // Run12pp200                      14
}

int TofwrecalReco::Init(PHCompositeNode *topNode)
{
    if(verbosity>0)
    {
	cout<<"Hello! Welcome to Shengli's (and Brennan's) Tofw Recalibraiton Module"<<endl;
	cout<<"It is still in processing"<<endl<<endl<<endl;
    }

    fetchstripoffflag=false;
    
    nrun=0; //using for fetching from file
    
    tofwcalib = new TofwCalib();
    
    //fun_dz      = new TF1("fun_dz",  "[0]+[1]/x+[2]/sqrt(x)+[3]*x",0.2,4.0);
    //fun_dphi    = new TF1("fun_dphi","[0]+[1]/x+[2]/sqrt(x)+[3]*x",0.2,4.0);
    //fun_slewing = new TF1("fun_slewing","[0]+[1]/x^0.4",0,2000);
    
    return 0;
}

int TofwrecalReco::InitRun(PHCompositeNode *topNode)
{
    RunHeader * d_runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");
  
    if (!d_runhdr)
    {
	cout << PHWHERE << "TofwrecalReco:: runhdr not in Node Tree" << endl;
	cout << PHWHERE << "You get zeroes for your Acc Recalibrations" << endl;
	return -1;
    }
 
    //  Use the run header to fetch the run number...
    //int runnumber = d_runhdr->get_RunNumber();
    runNumber = d_runhdr->get_RunNumber();

    //fetch run-by-run offset
    SetDeltaT(0.0);
    if(isValidRun(runNumber)>0) 
    {
	if(tofwcalib->fetchrunoff(runNumber))
	{
	    float runoffset=tofwcalib->get_runoffset();
	    SetDeltaT(runoffset);
	    //cout<<runNumber<<" "<<DeltaTRun<<endl;
	}
    }
    
    //fetch stripe-by-stripe offset
    if(isValidRun(runNumber)>0)
      {
	fetchstripoffflag = tofwcalib->fetchstripoff(runNumber);
      }
    
    if(fetchstripoffflag)
      {
	for(int ipar=0; ipar<4; ipar++)
	  {
	    sigma_dphi_par[ipar]=tofwcalib->get_sigma_dphi(ipar);
	    sigma_dz_par[ipar]=tofwcalib->get_sigma_dz(ipar);
	    
	    //fun_dphi->SetParameters(ipar, sigma_dphi_par[ipar]);
	    //fun_dz->SetParameters(ipar, sigma_dz_par[ipar]);
	  } 
	
	//if(runNumber<= 259575)
	//{
	for(int istrip=0; istrip<TOFW_NSTRIP_TOTAL; istrip++)
	  {
	    DeltaT[istrip]          = tofwcalib->get_DeltaT(istrip);
	    Slewing_A[istrip]       = tofwcalib->get_Slewing_A(istrip);
	    Slewing_B[istrip]       = tofwcalib->get_Slewing_B(istrip);
	    Mean_Dz_Plus[istrip]    = tofwcalib->get_Mean_Dz_Plus(istrip);
	    Mean_Dz_Minus[istrip]   = tofwcalib->get_Mean_Dz_Minus(istrip);
	    Mean_Dphi_Plus[istrip]  = tofwcalib->get_Mean_Dphi_Plus(istrip);
	    Mean_Dphi_Minus[istrip] = tofwcalib->get_Mean_Dphi_Minus(istrip);      
	  }  
	//}
	//else  // get current timing and get old slewing parameters
	    //{
	//using the old run for the slewing between run9 to run14
	if(runNumber<416892){
	  fetchstripoffflag = tofwcalib->fetchstripoff(253701);
	  if(fetchstripoffflag)
	    {
	      for(int istrip=0; istrip<TOFW_NSTRIP_TOTAL; istrip++)
		{
		  Slewing_A[istrip]   = tofwcalib->get_Slewing_A(istrip);
		  Slewing_B[istrip]   = tofwcalib->get_Slewing_B(istrip);
		} 
	    }
	}
      }
    
    
    return 0;
}

int TofwrecalReco::process_event(PHCompositeNode *topNode)
{

    static const float c = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9; // cm/ns

    PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

    if (d_cnt && isValidRun(runNumber)>0)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
      {
	PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
	int strip = sngltrk->get_striptofw();
	
	if (strip >= 0 && strip < TOFW_NSTRIP_TOTAL)
	{
	  //matching
	  if((runNumber >= 246214 && runNumber <= 253701)|| // Run8 dAu 200 GeV
	     (runNumber >= 256450 && runNumber <= 259575))  // Run8 pp 200 GeV
	    {
	      int   charge   = sngltrk->get_charge();
	      float tofwdz   = sngltrk->get_tofwdz();
	      float tofwdphi = sngltrk->get_tofwdphi();
	      if(charge>0) 
		{
		  tofwdz = tofwdz - Mean_Dz_Plus[strip];
		  tofwdphi = tofwdphi - Mean_Dphi_Plus[strip];
		}
	      else if(charge<0)
		{
		  tofwdz = tofwdz - Mean_Dz_Minus[strip];
		  tofwdphi = tofwdphi - Mean_Dphi_Minus[strip]; 
		}
	      sngltrk->set_tofwdz(tofwdz);
	      sngltrk->set_tofwdphi(tofwdphi);
	    }
	  
	  //timing
	  float p        = sngltrk->get_mom();
	  float ttofw    = sngltrk->get_ttofw();
	  float qtofw    = 0.5*(sngltrk->get_tofwadcup()+sngltrk->get_tofwadcdw());
	  float SlewingT = CalSlewing(strip,qtofw);
	  
	  float t = 0.0;
	  
	  t = ttofw-(DeltaTRun + 25.0 + DeltaT[strip]+SlewingT);
	  
	  
	  
	  float L = sngltrk->get_pltofw();
	  
	  int ichamber = int(strip/4)%32;
	  if(ichamber<16&&ichamber%2==0) L  = L + 0.361;//east face bottom
	  if(ichamber<16&&ichamber%2==1) L  = L + 3.511;//west face bottom
	  
	  if(ichamber>=16&&ichamber%2==1) L = L + 0.361;//east face top
	  if(ichamber>=16&&ichamber%2==0) L = L + 3.511;//west face top
	  
	  sngltrk->set_pltofw(L);
	  
	  float m2 = 0;
	  if (L > 0)
	  {
	    m2 = p * p * ( t * t * c * c / (L * L) - 1);
	  }
	  sngltrk->set_ttofw(t);
	  sngltrk->set_m2tofw(m2);
	}
      }
    }
    return 0;
}

void TofwrecalReco::fetchrunFromFile(const char * filename)
{
    ifstream file(filename);
    if (!file)
    {
	cout << "!!!  TofwrecalReco:: Could not open input file " << filename << " !!" << endl<<endl<<endl;
	return ;
    }

    //  Each line of the text file contains the following:
    //
    //  b   /   i
    //  d   \    DeltaT
    //

    if (verbosity > 0)
	cout << "Run-by-run DeltaT" << endl;
    while (!file.eof())
    {
	file >> run_deltat[nrun][0] >> run_deltat[nrun][1];
	if (verbosity > 0)
	{
	    cout << run_deltat[nrun][0] << " ";
	    cout << run_deltat[nrun][1] << " ";
	    cout << endl;
	}
	nrun++;
    }

    file.close();
    return ;
}

void TofwrecalReco::fetchFromFile(const char * filename)
{
    ifstream file(filename);
    if (!file)
    {
	cout << "TofwrecalReco:: Could not open input file " << filename << " !!" << endl<<endl<<endl;
	return ;
    }

    //  Each line of the text file contains the following:
    //
    //  b   /   i
    //  d   \    DeltaT
    //

    int istrip;
    if (verbosity > 0)
	cout << "Strip DeltaT" << endl;
    while (!file.eof())
    {
	file >> istrip;
	file >> DeltaT[istrip] >> Slewing_A[istrip] >> Slewing_B[istrip]
	     >> Mean_Dz_Plus[istrip] >> Mean_Dz_Minus[istrip] 
	     >> Mean_Dphi_Plus[istrip] >> Mean_Dphi_Minus[istrip];
      
	if (verbosity > 0)
	{
	    cout << istrip << " ";
	    cout << DeltaT[istrip] << " ";
	    cout << Slewing_A[istrip] << " ";
	    cout << Slewing_B[istrip] << " ";
	    cout << Mean_Dz_Plus[istrip] << " " << Mean_Dz_Minus[istrip] <<" ";
	    cout << Mean_Dphi_Plus[istrip] << " " << Mean_Dphi_Minus[istrip] <<" ";
	    cout << endl;
	}
    }

    file.close();
    return ;
}

float TofwrecalReco::CalSigmaZ(float mom)
{
    // TF1 *fun_dz   = new TF1("fun_dz",  "[0]+[1]/x+[2]/sqrt(x)+[3]*x",0.2,4.0);
//   for(int ipar=0; ipar<4; ipar++){
//     sigma_dz_par[ipar]=tofwcalib->get_sigma_dz(ipar);
//     fun_dz->SetParameters(ipar, sigma_dz_par[ipar]);
//   } 
  
//   float sigma = -9999;
//   if(mom<3.0&&mom>=0.2) sigma = fun_dz->Eval(mom);
//   else sigma = fun_dz->Eval(2.8);
//   fun_dz->Delete();
    float sigma = -9999;
    if(mom<3.0&&mom>=0.2) 
    {
	sigma = sigma_dz_par[0]+sigma_dz_par[1]/mom
	    +sigma_dz_par[2]/sqrt(mom)+sigma_dz_par[3]*mom;
    }
    else
    {
	sigma = sigma_dz_par[0]+sigma_dz_par[1]/2.8
	    +sigma_dz_par[2]/sqrt(2.8)+sigma_dz_par[3]*2.8;
    }
    return sigma;
}

float TofwrecalReco::CalSigmaPhi(float mom)
{
//   TF1 *fun_dphi = new TF1("fun_dphi","[0]+[1]/x+[2]/sqrt(x)+[3]*x",0.2,4.0);
//   for(int ipar=0; ipar<4; ipar++){
//     sigma_dphi_par[ipar]=tofwcalib->get_sigma_dphi(ipar);  
//     fun_dphi->SetParameters(ipar, sigma_dphi_par[ipar]);
//   } 
//   float sigma = -9999;
//   if(mom<4.0&&mom>=0.2) sigma = fun_dphi->Eval(mom);
//   else sigma = sigma = fun_dphi->Eval(4.0);
//   fun_dphi->Delete();

    float sigma = -9999;
    if(mom<4.0&&mom>=0.2) 
    {
	sigma = sigma_dphi_par[0]+sigma_dphi_par[1]/mom
	    +sigma_dphi_par[2]/sqrt(mom)+sigma_dphi_par[3]*mom;
    }
    else
    {
	sigma = sigma_dphi_par[0]+sigma_dphi_par[1]/4.0
	    +sigma_dphi_par[2]/sqrt(4.0)+sigma_dphi_par[3]*4.0;
    }
    return sigma;
}

float TofwrecalReco::CalSlewing(int strip, float adc)
{
    //TF1 *fun_slewing =new TF1("fun_slewing","[0]+[1]/x^0.4",0,2000);
    //fun_slewing->SetParameter(0,Slewing_A[strip]);
    //fun_slewing->SetParameter(1,Slewing_B[strip]);
    //float offset = fun_slewing->Eval(adc);
    //fun_slewing->Delete();
    float par = 0.4;
    float offset = Slewing_A[strip]+Slewing_B[strip]/pow(adc,par);
    return offset;  
}

