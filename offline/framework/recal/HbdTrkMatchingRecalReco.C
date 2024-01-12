#include "HbdTrkMatchingRecalReco.h"

#include <PHGlobal.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <getClass.h>
#include <recoConsts.h>
#include <PHCompositeNode.h>

#include <HbdMiniCellList.h>
#include <hbdAdcCalib.hh>
#include <RunHeader.h>

#include <gsl/gsl_math.h>

#include <cstdlib>
#include <iostream>
#include <RunNumberRanges.h>

using namespace std;


HbdTrkMatchingRecalReco::HbdTrkMatchingRecalReco(const string &name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  calib = new hbdAdcCalib();

  runnum = 300000;
  mc_flag = 0;
  clst_flag = 0;

  recoConsts *rc = recoConsts::instance();

  // Set the default Clusterizer
  runnum = rc->get_IntFlag("RUNNUMBER");

  //Run-9
  if (runnum > BEGIN_OF_RUN9 && runnum < BEGIN_OF_RUN10)
    { //Weizmann Clusterizer
      clst_flag = rc->get_IntFlag("HBD_CLUSTERIZER", 1);
    }
  //Run-10
  else if (runnum >= BEGIN_OF_RUN10 && runnum < BEGIN_OF_RUN10_7GEV)
    { //MinPad Clusterizer
      clst_flag = rc->get_IntFlag("HBD_CLUSTERIZER", 3);
    }
  else
    { //Weizmann Clusterizer
      clst_flag = rc->get_IntFlag("HBD_CLUSTERIZER", 1);
    }

}

HbdTrkMatchingRecalReco::~HbdTrkMatchingRecalReco()
{
  delete calib;
  return;
}

int 
HbdTrkMatchingRecalReco::isValidRun(const int runno) const
{
  
  //Run-9
  if (runno > BEGIN_OF_RUN9 && runno < BEGIN_OF_RUN10)
    {
      return 1;
    }
  
  //Run-10
  if (runno >= BEGIN_OF_RUN10 && runno < BEGIN_OF_RUN10_7GEV)
    {
      calib->fetch(runno);
      
      // Check if the run is calibrated
      if ( calib->isRunCalibrated() ){
	//cout << "HbdTrkMatchingRecalReco: Gain Calibration for run " << runno << "........OK"  << endl;

	//Don't need to run the recalibrator for HbdMinPadClusterizer
	if ( clst_flag == 3 ){
	  //cout << PHWHERE<< "HbdTrkMatchingRecalReco not needed for HbdMinPadClusterizer"  << endl;
	  return 0;
	}
	
	return 1;
      }
      
      else{
	//cout << PHWHERE<< "HbdTrkMatchingRecalReco: Gain Calibration for run " << runno << " does not exist!"  << endl;
	return 0;
      } 
    }

  return 0;
}


int 
HbdTrkMatchingRecalReco::InitRun(PHCompositeNode *topNode)
{

  recoConsts *rc = recoConsts::instance();
  mc_flag = rc->get_IntFlag("HBD_MC",0);
  skip_flag=0;
  if (mc_flag==0)
    {
      HbdMiniCellList* mincell = NULL;
      mincell = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
      if(!mincell) {
	std::cerr << "No HbdMiniCellList!::InitRun"<< std::endl;
	std::cerr << "We don't process HBD Trk Matching Recal::InitRun"<< std::endl;
	skip_flag=1;
	return 0;
      }
      
      RunHeader *run = findNode::getClass<RunHeader>(topNode,"RunHeader");
      runnum = run->get_RunNumber();
    }
  
  return 0;
}


int 
HbdTrkMatchingRecalReco::process_event(PHCompositeNode *topNode)
{

  //
  // If there is no HbdMiniCell, skip process
  //
  if(skip_flag) return 0;
  
  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
        {
	  PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
	  
	  float mom     = sngltrk->get_mom();
	  float hbddz   = sngltrk->get_hbddz();
	  float hbddphi = sngltrk->get_hbddphi();
	  int charge    = (int)sngltrk->get_charge();
	  
	  if (runnum > BEGIN_OF_RUN9 && runnum < BEGIN_OF_RUN10){
	    sngltrk->set_hbdsdz   (get_sdz   (hbddz,   mom) );
	    sngltrk->set_hbdsdphi (get_sdphi (hbddphi, mom) );
	  }
	  else if (runnum >= BEGIN_OF_RUN10 && runnum <= 306685){ //+/- field
	    sngltrk->set_hbdsdz   (get_sdz_Run10(hbddz,mom));
	    sngltrk->set_hbdsdphi (get_sdphi_Run10(hbddphi,mom,charge));
	  }
   	  else if (runnum > 306685){ //-/+ field
	    sngltrk->set_hbdsdz   (get_sdz_Run10(hbddz,mom));
	    sngltrk->set_hbdsdphi (get_sdphi_Run10(hbddphi,mom,-charge));
	  }	
	}
     } // if(d_cnt)

  return 0;
}


float
HbdTrkMatchingRecalReco::get_sdz(float hbddz, float mom)
{
  
  float mean    = -999.;
  float sigma   = -999.;
  float sdz     = -999.;
  float pars[4] = {0.1369, 0.1464, -5.117e-10, 1.248};
  float par[4]  = {0.2407, 0.05889, 1.758,     0.01237};
  
  sigma = sqrt(pars[0]*pars[0]+(pars[1]*pars[1])/mom
              +pars[2]*pars[2]*mom*mom)
              +pars[3];

  mean  = par[0]+par[1]/(1+exp(par[2]*mom))+par[3]*mom;

  sdz   = (hbddz-mean)/(sigma);

//  cout << "DZ: " << hbddz <<", SDZ: " << sdz<<endl;
  return sdz;
}


float
HbdTrkMatchingRecalReco::get_sdphi(float hbddphi, float mom)
{
  
  float mean    = -999.;
  float sigma   = -999.;
  float sdphi   = -999.;
  float pars[4] = {0.003347,  0.00292, 4.535e-11,  0.006221};
  float par[4]  = {0.002589, -0.00598, 2.202,     -0.0005902};
  
  sigma = sqrt(pars[0]*pars[0]+(pars[1]*pars[1])/mom
              +pars[2]*pars[2]*mom*mom)
              +pars[3];

  mean  = par[0]+par[1]/(1+exp(par[2]*mom))+par[3]*mom;

  sdphi = (hbddphi-mean)/(sigma);

//  cout << "DPHI: " << hbddphi <<", SDPHI: " << sdphi<<endl;
  return sdphi;
}

double 
HbdTrkMatchingRecalReco::get_sdphi_Run10(float hbddphi, float mom, int charge)
{

if (hbddphi<=-999) return -9999;    
 
 Double_t dphi_sigma[3];
 Double_t dphi_mean[2];
  
 //positrons
 if (charge == 1){ 
   dphi_sigma[0] = -4.969191e+00;
   dphi_sigma[1] = -6.645718e+00;
   dphi_sigma[2] = 1.247421e-02;
   dphi_mean[0] = 2.629165e-05;
   dphi_mean[1] = 0.0000;
 }
  
 //electrons
 else{
   dphi_sigma[0] = -4.694584e+00;
   dphi_sigma[1] = -7.351502e+00;
   dphi_sigma[2] = 1.247350e-02;
   dphi_mean[0] = -3.223977e-05;
   dphi_mean[1] = 0.0000;
 }
 
 Double_t offset,sigma;
 
 offset = dphi_mean[0]/(mom*mom) + dphi_mean[1];
 sigma = exp(dphi_sigma[0]+dphi_sigma[1]*mom)+dphi_sigma[2];
 
 return (hbddphi - offset) / sigma;
 
}

double 
HbdTrkMatchingRecalReco::get_sdz_Run10(float hbddz, float mom)
{
  
  if (hbddz <= -999) return -9999; 
  
  Double_t dz_sigma[3];
  Double_t dz_mean;
  
  dz_sigma[0] = -7.150788e-01;
  dz_sigma[1] = -6.606366e+00;
  dz_sigma[2] = 7.826932e-01;
  dz_mean = 0.0000;
  
  Double_t offset,sigma;
  
  offset = dz_mean;
  sigma = exp(dz_sigma[0]+dz_sigma[1]*mom)+dz_sigma[2];
  
  return (hbddz - offset) / sigma;

}
