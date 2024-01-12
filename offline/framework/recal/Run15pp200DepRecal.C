#include "Run15pp200DepRecal.h"

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

Run15pp200DepRecal::Run15pp200DepRecal(const string &name):
Recalibrator("Run15pp200DepRecal"),
runNumber(-9999),
d_cnt(NULL),
d_global(NULL)
{
  baseclasses.insert("PHCentralTrack");

  initialize_parameters();
  initializeBins();
}

int Run15pp200DepRecal::Init(PHCompositeNode *topNode)
{
  cout << "Run15pp200DepRecal::Init()" << endl;

  return 0;
}

int Run15pp200DepRecal::InitRun(PHCompositeNode *topNode)
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

int Run15pp200DepRecal::isValidRun(const int runno) const
{
  if(runno >= 421707 && runno <= 432008)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

void Run15pp200DepRecal::help()
{
    cout << "===================================================================" << endl;
    cout << "Run15pp200DepRecal::help method output" << endl;
    cout << "Author:  Javier Orjuela-Koop (javier.orjuelakoop@colorado.edu)" << endl;
    cout << "         This recalibrator updates the Dep values for PHCentralTrack   "             << endl;
    cout << "                  mom range is 0.24 < mom < 4 GeV/c                                "              << endl;
    cout << "==================================================================="                              << endl;
    cout << "===================================================================" << endl;
    cout << "Run15pp200DepRecal::help method output" << endl;
    cout << "Author 2:  Timothy Rinn (trinn@iastate.edu)" << endl;
    cout << "         This recalibrator updates the Dep values for PHCentralTrack   "             << endl;
    cout << "                  mom range is 0.2 < mom < 8 GeV/c                                "              << endl;
    cout << "==================================================================="                              << endl;
}

int Run15pp200DepRecal::process_event(PHCompositeNode *topNode)
{
  if(isValidRun(runNumber) == 0)
    {
      return 0;
    }

  static int evtno = 0;
  //if(evtno%1000==0) std::cout << "Run15pp200DepRecal::process_event() "<< evtno << std::endl;

  evtno++;

  //Get central tracks
  d_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if (!d_cnt || !d_global)
    {
      return 0;
    }

  Calibrate_Run15pp200GeV();
  
  return EVENT_OK;
}

int Run15pp200DepRecal::Calibrate_Run15pp200GeV()
{
  //Iterate over tracks
  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {
      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();

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

      //Extract track parameters
      float mom  = 0;
      float ecore = 0;
      float eop = 0;
      float dep = 0;
      float alpha = 0;
      int dcarm = 0;
      int emcsect = 0;
      int sect = 0;

      mom  = sngltrk->get_mom();
      ecore = sngltrk->get_ecore();
      alpha = sngltrk->get_alpha();
      dcarm = (int) sngltrk->get_dcarm();
      emcsect = (int)sngltrk->get_sect();
      sect = dcarm * 4 + emcsect;

      if(ecore < 0 || mom < 0)
	{
	  continue;
	}

      if(mom < 0.24 || mom > 10)
	{
	  continue;
	}      

      eop = ecore/mom;

      if(!(sect==0||sect==1||sect==2||sect==3||sect==4||sect==5||sect==6||sect==7))
	{
	  continue;
	}

      //Standardize E/p
      dep = calculate_dep(sect,alpha,mom,eop);

      //Set the new variables
      sngltrk->set_dep(dep);
    }

    return EVENT_OK;
}

int Run15pp200DepRecal:: getBin(float val)
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

float Run15pp200DepRecal::meanFunc(float x, int sect, float alpha)
{
  float p0 = 0;
  float p1 = 0;
  float p2 = 0;
  float p3 = 0;
  float p4 = 0;

  if(alpha > 0)
    {
      p0 = mean_params_pos[sect][0];
      p1 = mean_params_pos[sect][1];
      p2 = mean_params_pos[sect][2];
      p3 = mean_params_pos[sect][3];
      p4 = mean_params_pos[sect][4];
    }
  else if(alpha < 0)
    {
      p0 = mean_params_neg[sect][0];
      p1 = mean_params_neg[sect][1];
      p2 = mean_params_neg[sect][2];
      p3 = mean_params_neg[sect][3];
      p4 = mean_params_neg[sect][4];
    }

  float mean = p0*(1/x)-p1*(1/(x*x))-p2*(1/(x*x*x))-p3*(1/(x*x*x*x))+p4;

  //Account for sector 7 anomaly for positive tracks
  // if(sect == 7 && x > 0.3 && x < 0.65 && alpha > 0)
  //   {
  //     float dm = 1.28*x-1.032;
  //     mean = mean - dm;
  //   }

  return mean;
}

float Run15pp200DepRecal::sigmaFunc(float x, int sect, float alpha)
{
  float p0 = 0;
  float p1 = 0;
  float p2 = 0;
  float p3 = 0; 

if(alpha > 0)
  {
    p0 = sigma_params_pos[sect][0];
    p1 = sigma_params_pos[sect][1];
    p2 = sigma_params_pos[sect][2];
    p3 = sigma_params_pos[sect][3];
  }
 else if(alpha < 0)
   {
     p0 = sigma_params_neg[sect][0];
     p1 = sigma_params_neg[sect][1];
     p2 = sigma_params_neg[sect][2];
     p3 = sigma_params_neg[sect][3];
   }
 
 float sigma = p0+p1*x+p2*(1/x)+p3*(1/(x*x));
 return sigma;
}

float Run15pp200DepRecal::calculate_dep(int sector, float alpha, float mom, float eop)
{ 
  float mean = 0;
  float sigma = 0;
 	  
  mean = meanFunc(mom,sector,alpha);
  sigma = sigmaFunc(mom,sector,alpha);
 	
  float ret = 0;
  ret = (eop-mean)/sigma;
 	
  return ret;
}

void Run15pp200DepRecal::initializeBins()
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

void Run15pp200DepRecal::initialize_parameters()
{
  mean_params_pos[0][0] = -2.82631e-02;
  mean_params_pos[0][1] = 3.39819e-02 ;
  mean_params_pos[0][2] = -1.81570e-02 ;
  mean_params_pos[0][3] = 2.41540e-03 ;
  mean_params_pos[0][4] = 9.43204e-01;
  
  mean_params_pos[1][0] = -8.02177e-02;
  mean_params_pos[1][1] = -1.52689e-02;
  mean_params_pos[1][2] = 6.22960e-03;
  mean_params_pos[1][3] = -1.77044e-03;
  mean_params_pos[1][4] = 9.65675e-01;

  mean_params_pos[2][0] = -7.94925e-02;
  mean_params_pos[2][1] = -3.50006e-02;
  mean_params_pos[2][2] = 1.21065e-02;
  mean_params_pos[2][3] = -1.96864e-03;
  mean_params_pos[2][4] = 9.69810e-01;

  mean_params_pos[3][0] = -1.43181e-01;
  mean_params_pos[3][1] = -1.20492e-01;
  mean_params_pos[3][2] =  6.02404e-02;
  mean_params_pos[3][3] = -1.03605e-02;
  mean_params_pos[3][4] = 9.96244e-01;

  mean_params_pos[4][0] = 8.94016e-02;
  mean_params_pos[4][1] = 1.00510e-01;
  mean_params_pos[4][2] = -3.51577e-02;
  mean_params_pos[4][3] = 4.18059e-03;
  mean_params_pos[4][4] = 8.87674e-01;

  mean_params_pos[5][0] = 2.40716e-01;
  mean_params_pos[5][1] = 4.01464e-01;
  mean_params_pos[5][2] = -2.41404e-01;
  mean_params_pos[5][3] = 5.12176e-02 ;
  mean_params_pos[5][4] = 8.50863e-01 ;

  mean_params_pos[6][0] = 2.52863e-01;
  mean_params_pos[6][1] =2.71311e-01;
  mean_params_pos[6][2] = -1.06806e-01 ;
  mean_params_pos[6][3] = 1.45396e-02;
  mean_params_pos[6][4] = 8.32378e-01;

  mean_params_pos[7][0] = 1.33246e-01;
  mean_params_pos[7][1] = 1.49070e-01;
  mean_params_pos[7][2] = -5.92087e-02;
  mean_params_pos[7][3] = 8.05176e-03 ;
  mean_params_pos[7][4] = 8.77056e-01;

  mean_params_neg[0][0] = 2.26485e-02;
  mean_params_neg[0][1] = 7.73225e-02;
  mean_params_neg[0][2] = -3.12657e-02;
  mean_params_neg[0][3] = 3.89691e-03;
  mean_params_neg[0][4] = 9.17834e-01;

  mean_params_neg[1][0] = 4.44138e-02;
  mean_params_neg[1][1] = 1.15034e-01;
  mean_params_neg[1][2] = -5.31088e-02;
  mean_params_neg[1][3] = 7.62481e-03;
  mean_params_neg[1][4] = 9.09303e-01;

  mean_params_neg[2][0] = 1.58874e-01;
  mean_params_neg[2][1] = 1.98741e-01;
  mean_params_neg[2][2] = -8.60830e-02;
  mean_params_neg[2][3] = 1.26128e-02;
  mean_params_neg[2][4] = 8.69768e-01;

  mean_params_neg[3][0] = 1.97178e-01;
  mean_params_neg[3][1] = 2.28502e-01;
  mean_params_neg[3][2] = -9.76525e-02;
  mean_params_neg[3][3] = 1.42676e-02;
  mean_params_neg[3][4] = 8.57517e-01;

  mean_params_neg[4][0] = 6.76384e-02;
  mean_params_neg[4][1] = 9.74832e-02;
  mean_params_neg[4][2] = -4.21513e-02;
  mean_params_neg[4][3] = 6.06505e-03;
  mean_params_neg[4][4] = 9.04282e-01;

  mean_params_neg[5][0] = 1.84373e-02;
  mean_params_neg[5][1] = 1.08096e-01; 
  mean_params_neg[5][2] = -5.96857e-02;
  mean_params_neg[5][3] =9.65965e-03 ;
  mean_params_neg[5][4] = 9.22475e-01;

  mean_params_neg[6][0] = -2.73323e-02;
  mean_params_neg[6][1] = 2.27871e-03;
  mean_params_neg[6][2] = 2.67971e-03;
  mean_params_neg[6][3] = -1.25733e-03;
  mean_params_neg[6][4] = 9.49101e-01;

  mean_params_neg[7][0] = 1.03238e-01;
  mean_params_neg[7][1] = 1.53703e-01;
  mean_params_neg[7][2] = -6.77711e-02;
  mean_params_neg[7][3] = 1.00298e-02 ;
  mean_params_neg[7][4] = 9.15568e-01;

  sigma_params_pos[0][0] = 2.75423e-02;
  sigma_params_pos[0][1] = 9.05943e-03;
  sigma_params_pos[0][2] = 5.79866e-02;
  sigma_params_pos[0][3] = -9.73659e-03;

  sigma_params_pos[1][0] = 2.75423e-02;
  sigma_params_pos[1][1] = 9.05943e-03;
  sigma_params_pos[1][2] = 5.79866e-02;
  sigma_params_pos[1][3] = -9.73659e-03;

  sigma_params_pos[2][0] = 2.75423e-02;
  sigma_params_pos[2][1] = 9.05943e-03;
  sigma_params_pos[2][2] = 5.79866e-02;
  sigma_params_pos[2][3] = -9.73659e-03;

  sigma_params_pos[3][0] = 2.75423e-02;
  sigma_params_pos[3][1] = 9.05943e-03;
  sigma_params_pos[3][2] = 5.79866e-02;
  sigma_params_pos[3][3] = -9.73659e-03;

  sigma_params_pos[4][0] = 2.75423e-02;
  sigma_params_pos[4][1] = 9.05943e-03;
  sigma_params_pos[4][2] = 5.79866e-02;
  sigma_params_pos[4][3] = -9.73659e-03;

  sigma_params_pos[5][0] = 2.75423e-02;
  sigma_params_pos[5][1] = 9.05943e-03;
  sigma_params_pos[5][2] = 5.79866e-02;
  sigma_params_pos[5][3] = -9.73659e-03;

  sigma_params_pos[6][0] = 2.75423e-02;
  sigma_params_pos[6][1] = 9.05943e-03;
  sigma_params_pos[6][2] = 5.79866e-02;
  sigma_params_pos[6][3] = -9.73659e-03;

  sigma_params_pos[7][0] = 2.75423e-02;
  sigma_params_pos[7][1] = 9.05943e-03;
  sigma_params_pos[7][2] = 5.79866e-02;
  sigma_params_pos[7][3] = -9.73659e-03;


  sigma_params_neg[0][0] = 2.75423e-02;
  sigma_params_neg[0][1] = 9.05943e-03;
  sigma_params_neg[0][2] = 5.79866e-02;
  sigma_params_neg[0][3] = -9.73659e-03;

  sigma_params_neg[1][0] = 2.75423e-02;
  sigma_params_neg[1][1] = 9.05943e-03;
  sigma_params_neg[1][2] = 5.79866e-02;
  sigma_params_neg[1][3] = -9.73659e-03;

  sigma_params_neg[2][0] = 2.75423e-02;
  sigma_params_neg[2][1] = 9.05943e-03;
  sigma_params_neg[2][2] = 5.79866e-02;
  sigma_params_neg[2][3] = -9.73659e-03;

  sigma_params_neg[3][0] = 2.75423e-02;
  sigma_params_neg[3][1] = 9.05943e-03;
  sigma_params_neg[3][2] = 5.79866e-02;
  sigma_params_neg[3][3] = -9.73659e-03;

  sigma_params_neg[4][0] = 2.75423e-02;
  sigma_params_neg[4][1] = 9.05943e-03;
  sigma_params_neg[4][2] = 5.79866e-02;
  sigma_params_neg[4][3] = -9.73659e-03;

  sigma_params_neg[5][0] = 2.75423e-02;
  sigma_params_neg[5][1] = 9.05943e-03;
  sigma_params_neg[5][2] = 5.79866e-02;
  sigma_params_neg[5][3] = -9.73659e-03;

  sigma_params_neg[6][0] = 2.75423e-02;
  sigma_params_neg[6][1] = 9.05943e-03;
  sigma_params_neg[6][2] = 5.79866e-02;
  sigma_params_neg[6][3] = -9.73659e-03;

  sigma_params_neg[7][0] = 2.75423e-02;
  sigma_params_neg[7][1] = 9.05943e-03;
  sigma_params_neg[7][2] = 5.79866e-02;
  sigma_params_neg[7][3] = -9.73659e-03;






}
