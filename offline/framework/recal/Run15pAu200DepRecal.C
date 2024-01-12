#include "Run15pAu200DepRecal.h"

#include <Fun4AllReturnCodes.h>
#include "Fun4AllHistoManager.h"
#include "Fun4AllServer.h"

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>

#include <PHGlobal.h>

#include <PHCompositeNode.h>
#include <recoConsts.h>

#include <PHString.h>
#include <TH2.h>
#include <TMath.h>

#include <iostream>

using namespace std;
using namespace findNode;

Run15pAu200DepRecal::Run15pAu200DepRecal(const string &name):
Recalibrator("Run15pAu200DepRecal"),
runNumber(-9999),
d_cnt(NULL),
d_global(NULL)
{
  baseclasses.insert("PHCentralTrack");

  initialize_parameters();
  initializeBins();
}

int Run15pAu200DepRecal::Init(PHCompositeNode *topNode)
{
  cout << "Run15pAu200DepRecal::Init()" << endl;

  return 0;
}

int Run15pAu200DepRecal::InitRun(PHCompositeNode *topNode)
{
  RunHeader *d_run = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (!d_run)
    {
      cout << PHWHERE << " RunHeader not found" << endl;
      return 0;
    }
  
  runNumber = d_run->get_RunNumber();
  
  return 0;
}

int Run15pAu200DepRecal::isValidRun(const int runno) const
{
  if(runno >= 432637 && runno <= 436647)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

void Run15pAu200DepRecal::help()
{
    cout << "===================================================================" << endl;
    cout << "Run15pAu200DepRecal::help method output" << endl;
    cout << "Author:  Zhiyan Wang (zywang999@gmail.com)" << endl;
    cout << "         This recalibrator updates the Dep values for PHCentralTrack   "             << endl;
    cout << "                  mom range is 0.24 < mom < 10 GeV/c                                "              << endl;
    cout << "==================================================================="                              << endl;
    cout << "===================================================================" << endl;
}

int Run15pAu200DepRecal::process_event(PHCompositeNode *topNode)
{
  if(isValidRun(runNumber) == 0)
    {
      return 0;
    }

  static int evtno = 0;
  //if(evtno%1000==0) std::cout << "Run15pAu200DepRecal::process_event() "<< evtno << std::endl;

  evtno++;

  //Get central tracks
  d_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if (!d_cnt || !d_global)
    {
      return 0;
    }

  Calibrate_Run15pAu200GeV();
  
  return EVENT_OK;
}

int Run15pAu200DepRecal::Calibrate_Run15pAu200GeV()
{
  //Iterate over tracks
  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {
      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();

      /*
      if (
	  sngltrk->isImplemented(sngltrk->get_mom()) &&
	  sngltrk->isImplemented(sngltrk->get_charge()) &&
	  sngltrk->isImplemented(sngltrk->get_dcarm()) &&
	  sngltrk->isImplemented(sngltrk->get_sect()) &&
	  sngltrk->isImplemented(sngltrk->get_ecore())
	  )
        {
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
        }
      else
        {
	  sngltrk->ShutUp(1);
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
	  continue;
        }

      sngltrk->ShutUp(1); // enable virtual warnings again

      if (!isfinite(sngltrk->get_mom()))
        {
	  continue;
        }
      */

      //Extract track parameters
      float mom  = 0;
      float ecore = 0;
      //float secore = 0;
      float eop = 0;
      //float seop = 0;
      float dep = 0;
      //float sdep = 0;
      //float alpha = 0;
      int dcarm = 0;
      int emcsect = 0;
      int sect = 0;
      float charge = 0;

      mom  = sngltrk->get_mom();
      ecore = sngltrk->get_ecore();
      //secore = sngltrk->get_secore();
      //alpha = sngltrk->get_alpha();
      dcarm = (int) sngltrk->get_dcarm();
      emcsect = (int)sngltrk->get_sect();
      sect = dcarm * 4 + emcsect;
      charge = sngltrk->get_charge();

      eop = ecore/mom;

      /*
      if(ecore < 0 || mom < 0)
	{
	  continue;
	}
      */

      if(mom < 0.24)
	{
	  continue;
	}

      if(mom > 10)
	{
	  mom = 10;
	}
      
      if(!(sect==0||sect==1||sect==2||sect==3||sect==4||sect==5||sect==6||sect==7))
	{
	  continue;
	}
      
      
      //Standardize E/p
      dep = calculate_dep(sect,charge,mom,eop);
      //sdep = calculate_dep(sect,charge,mom,seop);
      
      //Set the new variables
      sngltrk->set_dep(dep);
      //sngltrk->set_sdep(sdep);
    }

    return EVENT_OK;
}

int Run15pAu200DepRecal:: getBin(float val)
{ 
  for(int i=0; i<NMOM_BINS; i++)
    {
      if(val >= mom_bin_lo[i] && val < mom_bin_hi[i])
	{
	  return i;
	}
    }
  
  return 0;
}

float Run15pAu200DepRecal::meanFunc(float x = -9999, int sect_f = -9999, float charge_f = -9999)
{
  float p0 = 0;
  float p1 = 0;
  float p2 = 0;
  float p3 = 0;
  float p4 = 0;

  if(charge_f > 0)
    {
      p0 = mean_params_pos[sect_f][0];
      p1 = mean_params_pos[sect_f][1];
      p2 = mean_params_pos[sect_f][2];
      p3 = mean_params_pos[sect_f][3];
      p4 = mean_params_pos[sect_f][4];
    }
  else if(charge_f < 0)
    {
      p0 = mean_params_neg[sect_f][0];
      p1 = mean_params_neg[sect_f][1];
      p2 = mean_params_neg[sect_f][2];
      p3 = mean_params_neg[sect_f][3];
      p4 = mean_params_neg[sect_f][4];
    }
  
  float mean = p0*(1/x)-p1*(1/(x*x))-p2*(1/(x*x*x))-p3*(1/(x*x*x*x))+p4;
  
  //Account for sector 7 anomaly for positive tracks
  // if(sect == 7 && x > 0.3 && x < 0.65 && charge > 0)
  //   {
  //     float dm = 1.28*x-1.032;
  //     mean = mean - dm;
  //   }
  
  return mean;
}

float Run15pAu200DepRecal::sigmaFunc(float x = -9999, int sect_f = -9999, float charge_f = -9999)
{
  float p0 = 0;
  float p1 = 0;
  float p2 = 0;
  float p3 = 0; 
  
  if(charge_f > 0)
    {
      p0 = sigma_params_pos[sect_f][0];
      p1 = sigma_params_pos[sect_f][1];
      p2 = sigma_params_pos[sect_f][2];
      p3 = sigma_params_pos[sect_f][3];
    }
  else if(charge_f < 0)
    {
      p0 = sigma_params_neg[sect_f][0];
      p1 = sigma_params_neg[sect_f][1];
      p2 = sigma_params_neg[sect_f][2];
      p3 = sigma_params_neg[sect_f][3];
    }
  
  float sigma = p0+p1*x+p2*(1/x)+p3*(1/(x*x));
  return sigma;
}

float Run15pAu200DepRecal::calculate_dep(int sector_f = -9999, float charge_f = -9999, float mom_f = -9999, float eop_f = -9999)
{ 

  float mean = 0;
  float sigma = 0;
 	  
  mean = meanFunc(mom_f,sector_f,charge_f);
  sigma = sigmaFunc(mom_f,sector_f,charge_f);
 	
  float ret = 0;
  ret = (eop_f-mean)/sigma;

  return ret;

}

void Run15pAu200DepRecal::initializeBins()
{
  mom_bin_lo[0] = 0.24;
  mom_bin_lo[1] = 0.28;
  mom_bin_lo[2] =  0.32;
  mom_bin_lo[3] = 0.36; 
  mom_bin_lo[4] = 0.40; 
  mom_bin_lo[5] = 0.44; 
  mom_bin_lo[6] = 0.48; 
  mom_bin_lo[7] = 0.52; 
  mom_bin_lo[8] = 0.56; 
  mom_bin_lo[9] = 0.60; 
  mom_bin_lo[10] = 0.64; 
  mom_bin_lo[11] = 0.68; 
  mom_bin_lo[12] = 0.72;
  mom_bin_lo[13] = 0.76; 
  mom_bin_lo[14] = 0.80;
  mom_bin_lo[15] = 0.85;
  mom_bin_lo[16] =  0.90; 
  mom_bin_lo[17] = 0.95; 
  mom_bin_lo[18] = 1.00; 
  mom_bin_lo[19] = 1.50; 
  mom_bin_lo[20] = 2.00; 
  mom_bin_lo[21] = 2.50; 
  mom_bin_lo[22] = 3.00;

  mom_bin_hi[0] = 0.28;
  mom_bin_hi[1] = 0.32;
  mom_bin_hi[2] = 0.36;
  mom_bin_hi[3] = 0.40;
  mom_bin_hi[4] = 0.44;
  mom_bin_hi[5] = 0.48;
  mom_bin_hi[6] = 0.52;
  mom_bin_hi[7] = 0.56;
  mom_bin_hi[8] = 0.60;
  mom_bin_hi[9] = 0.64;
  mom_bin_hi[10] = 0.68;
  mom_bin_hi[11] = 0.72;
  mom_bin_hi[12] = 0.76;
  mom_bin_hi[13] = 0.80;
  mom_bin_hi[14] = 0.85;
  mom_bin_hi[15] = 0.90;
  mom_bin_hi[16] = 0.95;
  mom_bin_hi[17] = 1.00;
  mom_bin_hi[18] = 1.50;
  mom_bin_hi[19] = 2.00;
  mom_bin_hi[20] = 2.50;
  mom_bin_hi[21] = 3.00;
  mom_bin_hi[22] = 5.00;
}

void Run15pAu200DepRecal::initialize_parameters()
{
  mean_params_pos[0][0] = -0.078557;
  mean_params_pos[0][1] = 0.004251;
  mean_params_pos[0][2] = -0.008800;
  mean_params_pos[0][3] = 0.001508;
  mean_params_pos[0][4] = 1.008180;

  mean_params_pos[1][0] = -0.038031;
  mean_params_pos[1][1] = 0.036484;
  mean_params_pos[1][2] = -0.016990;
  mean_params_pos[1][3] = 0.002188;
  mean_params_pos[1][4] = 0.989836;

  mean_params_pos[2][0] = 0.198176;
  mean_params_pos[2][1] = 0.248969;
  mean_params_pos[2][2] = -0.099376;
  mean_params_pos[2][3] = 0.013135;
  mean_params_pos[2][4] = 0.949002;

  mean_params_pos[3][0] = 0.266111;
  mean_params_pos[3][1] = 0.292383;
  mean_params_pos[3][2] = -0.100164;
  mean_params_pos[3][3] = 0.011261;
  mean_params_pos[3][4] = 0.895871;

  mean_params_pos[4][0] = 0.266873;
  mean_params_pos[4][1] = 0.269287;
  mean_params_pos[4][2] = -0.090452;
  mean_params_pos[4][3] = 0.010101;
  mean_params_pos[4][4] = 0.882080;

  mean_params_pos[5][0] = 0.281619;
  mean_params_pos[5][1] = 0.331408;
  mean_params_pos[5][2] = -0.123716;
  mean_params_pos[5][3] = 0.015245;
  mean_params_pos[5][4] = 0.875680;

  mean_params_pos[6][0] = 0.316153;
  mean_params_pos[6][1] = 0.274607;
  mean_params_pos[6][2] = -0.087142;
  mean_params_pos[6][3] = 0.009478;
  mean_params_pos[6][4] = 0.820585;

  mean_params_pos[7][0] = 0.220822;
  mean_params_pos[7][1] = 0.231182;
  mean_params_pos[7][2] = -0.080498;
  mean_params_pos[7][3] = 0.009320;
  mean_params_pos[7][4] = 0.892673;

  sigma_params_pos[0][0] = 0.046208;
  sigma_params_pos[0][1] = 0.010480;
  sigma_params_pos[0][2] = 0.036441;
  sigma_params_pos[0][3] = -0.001768;

  sigma_params_pos[1][0] = 0.051691;
  sigma_params_pos[1][1] = 0.005383;
  sigma_params_pos[1][2] = 0.033362;
  sigma_params_pos[1][3] = -0.001462;

  sigma_params_pos[2][0] = 0.034165;
  sigma_params_pos[2][1] = 0.006691;
  sigma_params_pos[2][2] = 0.057083;
  sigma_params_pos[2][3] = -0.009021;

  sigma_params_pos[3][0] = 0.034005;
  sigma_params_pos[3][1] = 0.007588;
  sigma_params_pos[3][2] = 0.039581;
  sigma_params_pos[3][3] = -0.000416;

  sigma_params_pos[4][0] = 0.032284;
  sigma_params_pos[4][1] = 0.009880;
  sigma_params_pos[4][2] = 0.050645;
  sigma_params_pos[4][3] = -0.006095;

  sigma_params_pos[5][0] = 0.044226;
  sigma_params_pos[5][1] = 0.005704;
  sigma_params_pos[5][2] = 0.050203;
  sigma_params_pos[5][3] = -0.005082;

  sigma_params_pos[6][0] = 0.069831;
  sigma_params_pos[6][1] = 0.007457;
  sigma_params_pos[6][2] = 0.019499;
  sigma_params_pos[6][3] = 0.000751;

  sigma_params_pos[7][0] = 0.047642;
  sigma_params_pos[7][1] = 0.004285;
  sigma_params_pos[7][2] = 0.038113;
  sigma_params_pos[7][3] = -0.003273;

  mean_params_neg[0][0] = 0.140057;
  mean_params_neg[0][1] = 0.210066;
  mean_params_neg[0][2] = -0.081051;
  mean_params_neg[0][3] = 0.010046;
  mean_params_neg[0][4] = 0.935517;

  mean_params_neg[1][0] = -0.031654;
  mean_params_neg[1][1] = 0.049643;
  mean_params_neg[1][2] = -0.023914;
  mean_params_neg[1][3] = 0.003234;
  mean_params_neg[1][4] = 0.985317;

  mean_params_neg[2][0] = 0.200718;
  mean_params_neg[2][1] = 0.221549;
  mean_params_neg[2][2] = -0.077601;
  mean_params_neg[2][3] = 0.008958;
  mean_params_neg[2][4] = 0.939435;

  mean_params_neg[3][0] = 0.282006;
  mean_params_neg[3][1] = 0.280819;
  mean_params_neg[3][2] = -0.096008;
  mean_params_neg[3][3] = 0.010969;
  mean_params_neg[3][4] = 0.874890;

  mean_params_neg[4][0] = 0.304462;
  mean_params_neg[4][1] = 0.296461;
  mean_params_neg[4][2] = -0.102265;
  mean_params_neg[4][3] = 0.011859;
  mean_params_neg[4][4] = 0.853068;

  mean_params_neg[5][0] = 0.284784;
  mean_params_neg[5][1] = 0.348402;
  mean_params_neg[5][2] = -0.133293;
  mean_params_neg[5][3] = 0.016540;
  mean_params_neg[5][4] = 0.872856;

  mean_params_neg[6][0] = 0.199854;
  mean_params_neg[6][1] = 0.241463;
  mean_params_neg[6][2] = -0.089025;
  mean_params_neg[6][3] = 0.010677;
  mean_params_neg[6][4] = 0.910547;

  mean_params_neg[7][0] = 0.244177;
  mean_params_neg[7][1] = 0.256228;
  mean_params_neg[7][2] = -0.081527;
  mean_params_neg[7][3] = 0.008313;
  mean_params_neg[7][4] = 0.892983;

  sigma_params_neg[0][0] = 0.050088;
  sigma_params_neg[0][1] = 0.009444;
  sigma_params_neg[0][2] = 0.036305;
  sigma_params_neg[0][3] = -0.002810;

  sigma_params_neg[1][0] = 0.060029;
  sigma_params_neg[1][1] = 0.003191;
  sigma_params_neg[1][2] = 0.027452;
  sigma_params_neg[1][3] = -0.000525;

  sigma_params_neg[2][0] = 0.042124;
  sigma_params_neg[2][1] = 0.005255;
  sigma_params_neg[2][2] = 0.044024;
  sigma_params_neg[2][3] = -0.004246;

  sigma_params_neg[3][0] = 0.050047;
  sigma_params_neg[3][1] = 0.003861;
  sigma_params_neg[3][2] = 0.032014;
  sigma_params_neg[3][3] = -0.001485;

  sigma_params_neg[4][0] = 0.033592;
  sigma_params_neg[4][1] = 0.010269;
  sigma_params_neg[4][2] = 0.047160;
  sigma_params_neg[4][3] = -0.004960;

  sigma_params_neg[5][0] = 0.048011;
  sigma_params_neg[5][1] = 0.004404;
  sigma_params_neg[5][2] = 0.051347;
  sigma_params_neg[5][3] = -0.006304;

  sigma_params_neg[6][0] = 0.062804;
  sigma_params_neg[6][1] = 0.005129;
  sigma_params_neg[6][2] = 0.029412;
  sigma_params_neg[6][3] = -0.001799;

  sigma_params_neg[7][0] = 0.029623;
  sigma_params_neg[7][1] = 0.008118;
  sigma_params_neg[7][2] = 0.060851;
  sigma_params_neg[7][3] = -0.006710;
}
