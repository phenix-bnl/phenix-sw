#include "EmcElMatchingRecalReco.h"

#include <DepObj.h>
#include <DepObjv1.h>
#include <DepObjv2.h>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <PdbEmcTrackMatch.hh>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>

#include <RunNumberRanges.h>
#include <getClass.h>
#include <recoConsts.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>

#include <boost/scoped_ptr.hpp>

#include <cmath>
#include <fstream>
#include <iostream>
#include <string>



using namespace std;

static const string description[16] =
  {"emcal phimatch pos mean", "emcal phimatch pos sigma",
   "emcal phimatch neg mean", "emcal phimatch neg sigma",
   "emcal zmatch pos mean", "emcal zmatch pos sigma",
   "emcal zmatch neg mean", "emcal zmatch neg sigma",
   "emcal depv1 pos mean", "emcal depv1 pos sigma",
   "emcal depv1 neg mean", "emcal depv1 neg sigma",
   "emcal depv2 pos mean", "emcal depv2 pos sigma",
   "emcal depv2 neg mean", "emcal depv2 neg sigma"};

EmcElMatchingRecalReco::EmcElMatchingRecalReco(const string &name): 
  Recalibrator(name),
  depversion(1),
  run10auau200version(0),
  run(0),
  run13pp510(false)
{
  memset(phimatch_pos_mean,0,sizeof(phimatch_pos_mean));
  memset(phimatch_pos_sigma,0,sizeof(phimatch_pos_sigma));
  memset(phimatch_neg_mean,0,sizeof(phimatch_neg_mean));
  memset(phimatch_neg_sigma,0,sizeof(phimatch_neg_sigma));
  memset(zmatch_pos_mean,0,sizeof(zmatch_pos_mean));
  memset(zmatch_pos_sigma,0,sizeof(zmatch_pos_sigma));
  memset(zmatch_neg_mean,0,sizeof(zmatch_neg_mean));
  memset(zmatch_neg_sigma,0,sizeof(zmatch_neg_sigma));

  baseclasses.insert("PHCentralTrack");
}

int 
EmcElMatchingRecalReco::isValidRun(const int runno) const
{

  if (runno<BEGIN_OF_RUN4)
    {
      return 0;
    }
  else if (runno > BEGIN_OF_RUN11)
    {
      if(runno >= (int)BEGIN_OF_RUN13PP510 && runno <= (int)END_OF_RUN13PP510 ) // Sasha Lebedev 11/13/2013
        {
          return 1; // run13pp510
        }
      else { return 0; }
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      cerr << PHWHERE << "ERROR: failed to start application to check run" << endl;
      application->abort();
      return 0;
    }

  PdbBankID bankID(0);
  PdbCalBank *emcMatchEBank = bankManager->fetchBank("PdbEmcTrackMatchBank",
					 bankID,
					 "calib.emcmatch.e",
					 runno);
  if (emcMatchEBank)
    {
      delete emcMatchEBank;
      return 1;
    }
  return  0;
}

int
EmcElMatchingRecalReco::InitRun(PHCompositeNode *topNode)
{
  recoConsts* rc = recoConsts::instance();
  run = rc->get_IntFlag("RUNNUMBER");

  //set dep version number
  // version 2 implemented only for run8
  // all other runs use version 1
  if (run >= BEGIN_OF_RUN8 && run < BEGIN_OF_RUN9)
    {
      depversion = 2;
    }

  //set flag for run10
  //concerns sdz/2 and +- / -+ field separation
  if (run >= 300105 && run <= 310454)
    {
      run10auau200version = 1;
    }

  if(run >= (int)BEGIN_OF_RUN13PP510 && run <= (int)END_OF_RUN13PP510 )
    {
      run13pp510 = true;
      return FetchMatch();
    }

  return Fetch();

}


int 
EmcElMatchingRecalReco::process_event(PHCompositeNode *topNode)
{
  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
	{
	  PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
	  sngltrk->ShutUp();
	  // make calibration only for electron candidates
	  if ( d_cnt->get_n0(i) < 0 && d_cnt->get_sn0(i) < 0)
	    {
	       continue;
	    }
	  
	  float emcdz = d_cnt->get_emcdz (i);
	  float emcdphi = d_cnt->get_emcdphi (i);
	  
	  if ( emcdz < -100 || emcdphi < -100)
	    {
	      continue;
	    }
	
	  float mom = d_cnt->get_mom (i);
	  float pt = d_cnt->get_pt (i);
	  float alpha = d_cnt->get_alpha(i);
	  float theta = d_cnt->get_beta (i);
	  float zed = d_cnt->get_zed (i);
	  int charge = d_cnt->get_charge (i);
	  int emcsector = 4 * (d_cnt->get_dcarm (i)) + d_cnt->get_sect (i);
	 	       	  
	  float new_emcsdz_e = -9999;
	  float new_emcsdphi_e = -9999;

	  if (run10auau200version==0)
	  {

            if ((alpha<0 && charge<0) || (alpha>0 && charge>0))
	    {
	      new_emcsdz_e = emcsdz_cor(emcdz, charge, mom, theta, emcsector);
	      new_emcsdphi_e = emcsdphi_cor(emcdphi, charge, mom, zed, emcsector);
	    }
	    else
	    {
	      new_emcsdz_e = emcsdz_cor(emcdz, -charge, mom, theta, emcsector);
	      new_emcsdphi_e = emcsdphi_cor(emcdphi, -charge, mom, zed, emcsector);	      
	    }
          }

	  if (run10auau200version==1)
	  {
              new_emcsdz_e = emcsdz_cor(emcdz, charge, mom*sin(d_cnt->get_the0(i)), theta, emcsector)/2.5;
	      new_emcsdphi_e = emcsdphi_cor(emcdphi, charge, mom*sin(d_cnt->get_the0(i)), zed, emcsector);
	  }

          if(run13pp510) {
            new_emcsdz_e   = emcsdz_cor_run13pp510  (emcdz,   charge, pt, zed, emcsector);
            new_emcsdphi_e = emcsdphi_cor_run13pp510(emcdphi, charge, pt, zed, emcsector);
          }

//	  if (new_emcsdphi_e<-10 || new_emcsdphi_e>10)
//	    {
//	      //cout<<emcdphi<<" "<<alpha<<" "<<charge<<" "<<mom<<" "<<zed<<" "<<emcsector<<" "<<new_emcsdphi_e<<endl;
//	    }

	  d_cnt->set_emcsdz_e(i, new_emcsdz_e);
	  d_cnt->set_emcsdphi_e(i, new_emcsdphi_e);
	  //	  cout << "EmcElMatching on " << inputnodename << " " << emcdphi << " " << new_emcsdphi_e << " " << d_cnt->get_emcsdphi_e(i) << endl;
	}
    } // if(d_cnt)
  
  return 0;
}



float
EmcElMatchingRecalReco::emcsdphi_cor_run13pp510(const float emcdphi, const int charge, const float mom, const float zed, const short emcsector) const
{
    if (emcsector<0 || emcsector>7) return emcdphi;
    if (zed<-1000) return emcdphi;
    if (mom<=0) return emcdphi;

    float p0_mean, p1_mean, p2_mean, p0_sigma, p1_sigma, p2_sigma, offset, sigma;
    offset = 0;
    sigma = 1.0;
    if (charge>0)
    {
        p0_mean = phimatch_pos_mean[emcsector][0][0] + 
                  phimatch_pos_mean[emcsector][0][1]/pow(mom,phimatch_pos_mean[emcsector][0][2]);
        p1_mean = phimatch_pos_mean[emcsector][1][0] + 
                  phimatch_pos_mean[emcsector][1][1]/pow(mom,phimatch_pos_mean[emcsector][1][2]);
        p2_mean = phimatch_pos_mean[emcsector][2][0] + 
                  phimatch_pos_mean[emcsector][2][1]/pow(mom,phimatch_pos_mean[emcsector][2][2]);
        offset = p0_mean + p1_mean*zed + p2_mean*zed*zed;

        p0_sigma = phimatch_pos_sigma[emcsector][0][0] +
                   phimatch_pos_sigma[emcsector][0][1]/pow(mom,phimatch_pos_sigma[emcsector][0][2]);
        p1_sigma = phimatch_pos_sigma[emcsector][1][0] +
                   phimatch_pos_sigma[emcsector][1][1]/pow(mom,phimatch_pos_sigma[emcsector][1][2]);
        p2_sigma = phimatch_pos_sigma[emcsector][2][0] +
                   phimatch_pos_sigma[emcsector][2][1]/pow(mom,phimatch_pos_sigma[emcsector][2][2]);
        sigma = p0_sigma + p1_sigma*zed + p2_sigma*zed*zed;
    }
    else if (charge<0)
    {
        p0_mean = phimatch_neg_mean[emcsector][0][0] +
                  phimatch_neg_mean[emcsector][0][1]/pow(mom,phimatch_neg_mean[emcsector][0][2]);
        p1_mean = phimatch_neg_mean[emcsector][1][0] +
                  phimatch_neg_mean[emcsector][1][1]/pow(mom,phimatch_neg_mean[emcsector][1][2]);
        p2_mean = phimatch_neg_mean[emcsector][2][0] +
                  phimatch_neg_mean[emcsector][2][1]/pow(mom,phimatch_neg_mean[emcsector][2][2]);
        offset = p0_mean + p1_mean*zed + p2_mean*zed*zed;

        p0_sigma = phimatch_neg_sigma[emcsector][0][0] +
                   phimatch_neg_sigma[emcsector][0][1]/pow(mom,phimatch_neg_sigma[emcsector][0][2]);
        p1_sigma = phimatch_neg_sigma[emcsector][1][0] +
                   phimatch_neg_sigma[emcsector][1][1]/pow(mom,phimatch_neg_sigma[emcsector][1][2]);
        p2_sigma = phimatch_neg_sigma[emcsector][2][0] +
                   phimatch_neg_sigma[emcsector][2][1]/pow(mom,phimatch_neg_sigma[emcsector][2][2]);
        sigma = p0_sigma + p1_sigma*zed + p2_sigma*zed*zed;
    }
    if (verbosity > 10) {cout << "emcdphi: " << offset*1000. << " " << sigma*1000. << " mrad" << endl;}

    return (emcdphi - offset) / sigma;
}


float
EmcElMatchingRecalReco::emcsdz_cor_run13pp510(const float emcdz, const int charge, const float mom, const float zed, const short emcsector) const
{
    if (emcsector<0 || emcsector>7) return emcdz;
    if (zed<-1000) return emcdz;
    if (mom<=0) return emcdz;

    float p0_mean = 0;
    float  p1_mean = 0;
    float p0_sigma = 0;
    float p1_sigma = 0;
    float sigma;
    float offset;
    if (charge>0)
      {
        p0_mean = zmatch_pos_mean[emcsector][0][0] +
                  zmatch_pos_mean[emcsector][0][1]/pow(mom,zmatch_pos_mean[emcsector][0][2]);
        p1_mean = zmatch_pos_mean[emcsector][1][0] +
                  zmatch_pos_mean[emcsector][1][1]/pow(mom,zmatch_pos_mean[emcsector][1][2]);

        p0_sigma = zmatch_pos_sigma[emcsector][0][0] +
                   zmatch_pos_sigma[emcsector][0][1]/pow(mom,zmatch_pos_sigma[emcsector][0][2]);
        p1_sigma = zmatch_pos_sigma[emcsector][1][0] +
                   zmatch_pos_sigma[emcsector][1][1]/pow(mom,zmatch_pos_sigma[emcsector][1][2]);
    }
    else if (charge<0)
    {
        p0_mean = zmatch_neg_mean[emcsector][0][0] +
                  zmatch_neg_mean[emcsector][0][1]/pow(mom,zmatch_pos_mean[emcsector][0][2]);
        p1_mean = zmatch_neg_mean[emcsector][1][0] +
                  zmatch_neg_mean[emcsector][1][1]/pow(mom,zmatch_pos_mean[emcsector][1][2]);

        p0_sigma = zmatch_neg_sigma[emcsector][0][0] +
                   zmatch_neg_sigma[emcsector][0][1]/pow(mom,zmatch_neg_sigma[emcsector][0][2]);
        p1_sigma = zmatch_neg_sigma[emcsector][1][0] +
                   zmatch_neg_sigma[emcsector][1][1]/pow(mom,zmatch_neg_sigma[emcsector][1][2]);
    }
    offset = p0_mean  + p1_mean*zed;
    sigma  = p0_sigma + p1_sigma*zed;
    if (verbosity > 10) {cout << "emcdz: " << offset << " " << sigma << " cm" << endl;}

    return (emcdz - offset) / sigma;
}

void
EmcElMatchingRecalReco::test(const short emcsector, const float pt, const float zed, const short charge) const
{
  emcsdz_cor_run13pp510  (1.0,   charge, pt, zed, emcsector);
  emcsdphi_cor_run13pp510(0.001, charge, pt, zed, emcsector);
}



float
EmcElMatchingRecalReco::emcsdphi_cor(const float emcdphi, const int charge, const float mom, const float zed, const short emcsector) const
{
  if (emcsector<0 || emcsector>7) return emcdphi;
  if (zed<-1000) return emcdphi;
  if (mom<=0) return emcdphi;

  float p0_mean;
  float p1_mean;
  float  p2_mean;
  float  p0_sigma;
  float  p1_sigma;
  float  p2_sigma;
  float offset = 0.;
  float sigma = 1.0;
  if (charge>0)
    {
      p0_mean = phimatch_pos_mean[emcsector][0][0] +
	phimatch_pos_mean[emcsector][0][1]/mom +
	phimatch_pos_mean[emcsector][0][2]/mom/mom;
      p1_mean = phimatch_pos_mean[emcsector][1][0] +
	phimatch_pos_mean[emcsector][1][1]/mom +
	phimatch_pos_mean[emcsector][1][2]/mom/mom;
      p2_mean = phimatch_pos_mean[emcsector][2][0] +
	phimatch_pos_mean[emcsector][2][1]/mom +
	phimatch_pos_mean[emcsector][2][2]/mom/mom;
      offset = p0_mean + p1_mean*zed + p2_mean*zed*zed;
	
      p0_sigma = phimatch_pos_sigma[emcsector][0][0] +
	phimatch_pos_sigma[emcsector][0][1]/mom +
	phimatch_pos_sigma[emcsector][0][2]/mom/mom;
      p1_sigma = phimatch_pos_sigma[emcsector][1][0] +
	phimatch_pos_sigma[emcsector][1][1]/mom +
	phimatch_pos_sigma[emcsector][1][2]/mom/mom;
      p2_sigma = phimatch_pos_sigma[emcsector][2][0] +
	phimatch_pos_sigma[emcsector][2][1]/mom +
	phimatch_pos_sigma[emcsector][2][2]/mom/mom;
      sigma = p0_sigma + p1_sigma*zed + p2_sigma*zed*zed;
    }
  else if (charge<0)
    {
      p0_mean = phimatch_neg_mean[emcsector][0][0] +
	phimatch_neg_mean[emcsector][0][1]/mom +
	phimatch_neg_mean[emcsector][0][2]/mom/mom;
      p1_mean = phimatch_neg_mean[emcsector][1][0] +
	phimatch_neg_mean[emcsector][1][1]/mom +
	phimatch_neg_mean[emcsector][1][2]/mom/mom;
      p2_mean = phimatch_neg_mean[emcsector][2][0] +
	phimatch_neg_mean[emcsector][2][1]/mom +
	phimatch_neg_mean[emcsector][2][2]/mom/mom;
      offset = p0_mean + p1_mean*zed + p2_mean*zed*zed;
	
      p0_sigma = phimatch_neg_sigma[emcsector][0][0] +
	phimatch_neg_sigma[emcsector][0][1]/mom +
	phimatch_neg_sigma[emcsector][0][2]/mom/mom;
      p1_sigma = phimatch_neg_sigma[emcsector][1][0] +
	phimatch_neg_sigma[emcsector][1][1]/mom +
	phimatch_neg_sigma[emcsector][1][2]/mom/mom;
      p2_sigma = phimatch_neg_sigma[emcsector][2][0] +
	phimatch_neg_sigma[emcsector][2][1]/mom +
	phimatch_neg_sigma[emcsector][2][2]/mom/mom;
      sigma = p0_sigma + p1_sigma*zed + p2_sigma*zed*zed;
    }
  return (emcdphi - offset) / sigma;
}

float
EmcElMatchingRecalReco::emcsdz_cor(const float emcdz, const int charge, const float mom, const float theta, const short emcsector) const
{
  if (emcsector<0 || emcsector>7) return emcdz;
  if (theta<-1000) return emcdz;
  if (mom<=0) return emcdz;

  float p0_mean = 0;
  float p1_mean = 0;
  float p0_sigma = 0;
  float p1_sigma = 0;
  float offset;
  float sigma;
  if (charge>0)
    {
      p0_mean = zmatch_pos_mean[emcsector][0][0] +
	zmatch_pos_mean[emcsector][0][1]/mom +
	zmatch_pos_mean[emcsector][0][2]/mom/mom +
	zmatch_pos_mean[emcsector][0][3]/mom/mom/mom;
      p1_mean = zmatch_pos_mean[emcsector][1][0] +
	zmatch_pos_mean[emcsector][1][1]/mom +
	zmatch_pos_mean[emcsector][1][2]/mom/mom +
	zmatch_pos_mean[emcsector][1][3]/mom/mom/mom;
	
      p0_sigma = zmatch_pos_sigma[emcsector][0][0] +
	zmatch_pos_sigma[emcsector][0][1]/mom +
	zmatch_pos_sigma[emcsector][0][2]/mom/mom +
	zmatch_pos_sigma[emcsector][0][3]/mom/mom/mom;
      p1_sigma = zmatch_pos_sigma[emcsector][1][0] +
	zmatch_pos_sigma[emcsector][1][1]/mom +
	zmatch_pos_sigma[emcsector][1][2]/mom/mom +
	zmatch_pos_sigma[emcsector][1][3]/mom/mom/mom;
    }
  else if (charge<0)
    {
      p0_mean = zmatch_neg_mean[emcsector][0][0] +
	zmatch_neg_mean[emcsector][0][1]/mom +
	zmatch_neg_mean[emcsector][0][2]/mom/mom +
	zmatch_neg_mean[emcsector][0][3]/mom/mom/mom;
      p1_mean = zmatch_neg_mean[emcsector][1][0] +
	zmatch_neg_mean[emcsector][1][1]/mom +
	zmatch_neg_mean[emcsector][1][2]/mom/mom +
	zmatch_neg_mean[emcsector][1][3]/mom/mom/mom;
	
      p0_sigma = zmatch_neg_sigma[emcsector][0][0] +
	zmatch_neg_sigma[emcsector][0][1]/mom +
	zmatch_neg_sigma[emcsector][0][2]/mom/mom +
	zmatch_neg_sigma[emcsector][0][3]/mom/mom/mom;
      p1_sigma = zmatch_neg_sigma[emcsector][1][0] +
	zmatch_neg_sigma[emcsector][1][1]/mom +
	zmatch_neg_sigma[emcsector][1][2]/mom/mom +
	zmatch_neg_sigma[emcsector][1][3]/mom/mom/mom;
    }
  offset = p0_mean + p1_mean*tan(theta-M_PI/2.);
  sigma = p0_sigma*cos(theta-M_PI/2.) + p1_sigma;
  return (emcdz - offset) / sigma;
}

int
EmcElMatchingRecalReco::FetchFromFile_run13pp510(const string &filename)
{

  ifstream fin(filename.c_str());

  if (!fin) 
    { 
      cout << PHWHERE << " File " << filename << " not found" << endl; 
      return 1; 
    }
  else 
    { 
      cout << "Loading EMCal Track Matching Parameters from file " << filename <<endl; 
    }

  for(int i=0; i<NSECT; i++) 
    {    // sector = dcharm*4 + sector
      //cout << "--------- sector " << i << endl;
      fin >> phimatch_pos_mean[i][0][0] >> phimatch_pos_mean[i][0][1] >> phimatch_pos_mean[i][0][2]; 
      fin >> phimatch_pos_mean[i][1][0] >> phimatch_pos_mean[i][1][1] >> phimatch_pos_mean[i][1][2]; 
      fin >> phimatch_pos_mean[i][2][0] >> phimatch_pos_mean[i][2][1] >> phimatch_pos_mean[i][2][2]; 
      //cout << "phimatch_pos_mean: " << endl;
      //for(int j=0; j<3; j++) {
      //for(int k=0; k<3; k++) {
      //cout << phimatch_pos_mean[i][j][k] << " ";
      //} cout << endl;
      //} cout << endl;

      fin >> phimatch_pos_sigma[i][0][0] >> phimatch_pos_sigma[i][0][1] >> phimatch_pos_sigma[i][0][2]; 
      fin >> phimatch_pos_sigma[i][1][0] >> phimatch_pos_sigma[i][1][1] >> phimatch_pos_sigma[i][1][2]; 
      fin >> phimatch_pos_sigma[i][2][0] >> phimatch_pos_sigma[i][2][1] >> phimatch_pos_sigma[i][2][2]; 

      fin >> phimatch_neg_mean[i][0][0] >> phimatch_neg_mean[i][0][1] >> phimatch_neg_mean[i][0][2];
      fin >> phimatch_neg_mean[i][1][0] >> phimatch_neg_mean[i][1][1] >> phimatch_neg_mean[i][1][2];
      fin >> phimatch_neg_mean[i][2][0] >> phimatch_neg_mean[i][2][1] >> phimatch_neg_mean[i][2][2];
      //cout << "phimatch_neg_mean: " << endl;
      //for(int j=0; j<3; j++) {
      //for(int k=0; k<3; k++) {
      //cout << phimatch_neg_mean[i][j][k] << " ";
      //} cout << endl;
      //} cout << endl;

      fin >> phimatch_neg_sigma[i][0][0] >> phimatch_neg_sigma[i][0][1] >> phimatch_neg_sigma[i][0][2];
      fin >> phimatch_neg_sigma[i][1][0] >> phimatch_neg_sigma[i][1][1] >> phimatch_neg_sigma[i][1][2];
      fin >> phimatch_neg_sigma[i][2][0] >> phimatch_neg_sigma[i][2][1] >> phimatch_neg_sigma[i][2][2];

      fin >> zmatch_pos_mean[i][0][0] >> zmatch_pos_mean[i][0][1] >> zmatch_pos_mean[i][0][2];
      fin >> zmatch_pos_mean[i][1][0] >> zmatch_pos_mean[i][1][1] >> zmatch_pos_mean[i][1][2];
      zmatch_pos_mean[i][0][3]=0.;
      zmatch_pos_mean[i][1][3]=0.;

      fin >> zmatch_pos_sigma[i][0][0] >> zmatch_pos_sigma[i][0][1] >> zmatch_pos_sigma[i][0][2];
      fin >> zmatch_pos_sigma[i][1][0] >> zmatch_pos_sigma[i][1][1] >> zmatch_pos_sigma[i][1][2];
      zmatch_pos_sigma[i][0][3]=0.;
      zmatch_pos_sigma[i][1][3]=0.;

      fin >> zmatch_neg_mean[i][0][0] >> zmatch_neg_mean[i][0][1] >> zmatch_neg_mean[i][0][2];
      fin >> zmatch_neg_mean[i][1][0] >> zmatch_neg_mean[i][1][1] >> zmatch_neg_mean[i][1][2];
      zmatch_neg_mean[i][0][3]=0.;
      zmatch_neg_mean[i][1][3]=0.;

      fin >> zmatch_neg_sigma[i][0][0] >> zmatch_neg_sigma[i][0][1] >> zmatch_neg_sigma[i][0][2];
      fin >> zmatch_neg_sigma[i][1][0] >> zmatch_neg_sigma[i][1][1] >> zmatch_neg_sigma[i][1][2];
      zmatch_neg_sigma[i][0][3]=0.;
      zmatch_neg_sigma[i][1][3]=0.;

    }
  fin.close();
  return 0;
}

int
EmcElMatchingRecalReco::FetchFromFile(const string &filename, const int depver)
{
  /*
    Data structure in filename should be
    float phimatch_pos_mean[8][3][3] 
    float phimatch_pos_sigma[8][3][3]  
    float phimatch_neg_mean[8][3][3]  
    float phimatch_neg_sigma[8][3][3] 
    float zmatch_pos_mean[8][2][4] 
    float zmatch_pos_sigma[8][2][4]  
    float zmatch_neg_mean[8][2][4]  
    float zmatch_neg_sigma[8][2][4]  
    float dep_pos_mean[8][3]  
    float dep_pos_sigma[8][3]  
    float dep_neg_mean[8][3]  
    float dep_neg_sigma[8][3]  
  */


  ifstream fin(filename.c_str());

  if (!fin)
    {
      cout << PHWHERE << " File " << filename << " not found" << endl;
      return 1;
    }
  else
    {
      cout << "Loading EMCal Track Matching Parameters from file " << filename <<endl;
    }
  ////////// PHI match constants ///////////
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<3; ipar++)
	{     
	  for (int iipar=0; iipar<3; iipar++)
	    {
	      fin >> phimatch_pos_mean[isect][ipar][iipar];
	    }
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<3; ipar++)
	{
	  for (int iipar=0; iipar<3; iipar++)
	    {
	      fin >> phimatch_pos_sigma[isect][ipar][iipar];
	    }
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<3; ipar++)
	{
	  for (int iipar=0; iipar<3; iipar++)
	    {
	      fin >> phimatch_neg_mean[isect][ipar][iipar];
	    }
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<3; ipar++)
	{
	  for (int iipar=0; iipar<3; iipar++)
	    {
	      fin >> phimatch_neg_sigma[isect][ipar][iipar];
	    }
	}
    }
  ////////// Z match constants ///////////
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<2; ipar++)
	{
	  for (int iipar=0; iipar<4; iipar++)
	    {
	      fin >> zmatch_pos_mean[isect][ipar][iipar];
	    }
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<2; ipar++)
	{
	  for (int iipar=0; iipar<4; iipar++)
	    {
	      fin >> zmatch_pos_sigma[isect][ipar][iipar];
	    }
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<2; ipar++)
	{
	  for (int iipar=0; iipar<4; iipar++)
	    {
	      fin >> zmatch_neg_mean[isect][ipar][iipar];
	    }
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<2; ipar++)
	{
	  for (int iipar=0; iipar<4; iipar++)
	    {
	      fin >> zmatch_neg_sigma[isect][ipar][iipar];
	    }
	}
    }
  ////////// DEP constants ///////////
  float temp;

  //pick dep version
  DepObj* dep;
  if (depver == 1) 
    {
      dep = DepObjv1::instance();
    }
  else if (depver == 2) 
    {
      dep = DepObjv2::instance();
    }
  else 
    {
      cout << PHWHERE << " Dep Version #" << depver << " not implemented! " << endl;
      return 1;
    }

  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<dep->get_N_meanpar(); ipar++)
	{
	  fin >> temp;
	  dep->set_meanpar(1, isect, ipar, temp);
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<dep->get_N_sigpar(); ipar++)
	{
	  fin >> temp;
	  dep->set_sigpar(1, isect, ipar, temp);
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<dep->get_N_meanpar(); ipar++)
	{
	  fin >> temp;
	  dep->set_meanpar(-1, isect, ipar, temp);
	}
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      for (int ipar=0; ipar<dep->get_N_sigpar(); ipar++)
	{
	  fin >> temp;
	  dep->set_sigpar(-1, isect, ipar, temp);
	}
    }
  fin.close();
  return 0;
}

int
EmcElMatchingRecalReco::UpdateDB(const int run_beg, const int run_end, const int depver)
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      cerr << PHWHERE << "ERROR: failed to start application for update" << endl;
      application->abort();
      return 1;
    }

  int iret = 0;

  for (int bankid=0; bankid<16; bankid++)
    {
      //check dep version number
      if (depver == 1 && bankid > 11) 
	{
	  continue;
	}
      if (depver == 2 && bankid > 7 && bankid < 12) 
	{
	  continue;
	}
      //update Parameter DB
      iret += UpdateParDB(bankid, run_beg, run_end);
    }
  return iret;
}

int
EmcElMatchingRecalReco::UpdateMatchDB(const int run_beg, const int run_end)
{
  int iret = 0;

  for (int bankid=0; bankid<8; bankid++) 
    {
      iret += UpdateParDB(bankid, run_beg, run_end); 
    }
  return iret;
}


int
EmcElMatchingRecalReco::UpdateParDB(const int bankid, const int run_beg, const int run_end)
{

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startUpdate())
    {
      cerr << PHWHERE << "ERROR: failed to start application for update" << endl;
      application->abort();
      return 1;
    }

  PdbBankID bankID(bankid);
  PdbCalBank *emcMatchEBank =
    bankManager->createBank(run_beg, run_end,
			    "PdbEmcTrackMatchBank",
			    bankID,
			    description[bankid].c_str(),
			    "calib.emcmatch.e");
  emcMatchEBank->setLength(NSECT);
 
  int npar = 3;
  int nmompar = 3;
  if (bankid>3)
    {
      npar = 2;
      nmompar = 4;
    }
  if (bankid>7)
    {
      npar = 3;
      nmompar = 1;
    }
  if (bankid == 12 || bankid == 14)
    {
      npar = 3;
      nmompar = 2;
    }
  if (bankid == 13 || bankid == 15)
    {
      npar = 2;
      nmompar = 2;
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      PdbEmcTrackMatch *achan = (PdbEmcTrackMatch*) & (emcMatchEBank->getEntry(isect));
      for (int ipar=0; ipar<npar; ipar++)
	{
	  for (int iipar=0; iipar<nmompar; iipar++)
	    {
	      DepObj* dep;
	      switch (bankid)
		{  
		case 0 : achan->setParameter(ipar,iipar,phimatch_pos_mean[isect][ipar][iipar]); break;
		case 1 : achan->setParameter(ipar,iipar,phimatch_pos_sigma[isect][ipar][iipar]); break;
		case 2 : achan->setParameter(ipar,iipar,phimatch_neg_mean[isect][ipar][iipar]); break;
		case 3 : achan->setParameter(ipar,iipar,phimatch_neg_sigma[isect][ipar][iipar]); break;
		case 4 : achan->setParameter(ipar,iipar,zmatch_pos_mean[isect][ipar][iipar]); break;
		case 5 : achan->setParameter(ipar,iipar,zmatch_pos_sigma[isect][ipar][iipar]); break;
		case 6 : achan->setParameter(ipar,iipar,zmatch_neg_mean[isect][ipar][iipar]); break;
		case 7 : achan->setParameter(ipar,iipar,zmatch_neg_sigma[isect][ipar][iipar]); break;
		case 8 : 
		  dep = DepObjv1::instance();
		  achan->setParameter(ipar,iipar,dep->get_meanpar(1, isect, ipar)); 
		  break;
		case 9 : 
		  dep = DepObjv1::instance();
		  achan->setParameter(ipar,iipar,dep->get_sigpar(1, isect, ipar)); 
		  break;
		case 10 : 
		  dep = DepObjv1::instance();
		  achan->setParameter(ipar,iipar,dep->get_meanpar(-1, isect, ipar)); 
		  break;
		case 11 : 
		  dep = DepObjv1::instance();
		  achan->setParameter(ipar,iipar,dep->get_sigpar(-1, isect, ipar)); 
		  break;
		case 12 : 
		  dep = DepObjv2::instance();
		  achan->setParameter(ipar,iipar,dep->get_meanpar(1, isect, nmompar * ipar + iipar));
		  break;
		case 13 : 
		  dep = DepObjv2::instance();
		  achan->setParameter(ipar,iipar,dep->get_sigpar(1, isect, nmompar * ipar + iipar)); 
		  break;
		case 14 : 
		  dep = DepObjv2::instance();
		  achan->setParameter(ipar,iipar,dep->get_meanpar(-1, isect, nmompar * ipar + iipar)); 
		  break;
		case 15 : 
		  dep = DepObjv2::instance();
		  achan->setParameter(ipar,iipar,dep->get_sigpar(-1, isect, nmompar * ipar + iipar)); 
		  break;
		}
	    }
	}
    }
  application->commit();
  return 0;
}

int
EmcElMatchingRecalReco::FetchMatch()
{
  int iret = 0;
  for (int bankid=0; bankid<8; bankid++) 
    { 
      iret += FetchParDB(bankid); 
    }
  return iret;
}

int
EmcElMatchingRecalReco::FetchMatch(const int runno)
{
  run = runno;
  return FetchMatch();
}


int 
EmcElMatchingRecalReco::Fetch()
{
  int iret = 0;
  for (int bankid=0; bankid<16; bankid++)
    {
      //check dep version number
      if (depversion == 1 && bankid > 11) 
	{
	  continue;
	}
      if (depversion == 2 && bankid > 7 && bankid < 12)
	{
	  continue;
	}
      //update Parameter DB
      iret += FetchParDB(bankid);
    }
  return iret;
}

int 
EmcElMatchingRecalReco::FetchParDB(const int bankid)
{
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startRead())
    {
      cerr << PHWHERE << "ERROR: failed to start application to read database" << endl;
      application->abort();
      return 1;
    }

  PdbBankID bankID(bankid);
  PdbCalBank *emcMatchEBank =
    bankManager->fetchBank("PdbEmcTrackMatchBank",
			   bankID,
			   "calib.emcmatch.e",
			   run);
  boost::scoped_ptr<PdbCalBank> tmp(emcMatchEBank); // facilitates exception-safe cleanup

  if (!emcMatchEBank)
    {
      cout << PHWHERE << "ERROR: Constants not found for run " << run << endl;
      return 1;
    }

  int npar = 3;
  int nmompar = 3;
  if (bankid>3)
    {
      npar = 2;
      nmompar = 4;
    }
  if (bankid>7)
    {
      npar = 3;
      nmompar = 1;
    }
  if (bankid == 12 || bankid == 14)
    {
      npar = 3;
      nmompar = 2;
    }
  if (bankid == 13 || bankid == 15)
    {
      npar = 2;
      nmompar = 2;
    }
  for (int isect=0; isect<NSECT; isect++)
    {
      PdbEmcTrackMatch *achan = (PdbEmcTrackMatch*) & (emcMatchEBank->getEntry(isect));
      for (int ipar=0; ipar<npar; ipar++)
	{
	  for (int iipar=0; iipar<nmompar; iipar++)
	    {
              DepObj* dep;
	      switch (bankid)
		{  
		case 0 :  
		  phimatch_pos_mean[isect][ipar][iipar]  = achan->getParameter(ipar,iipar); 
		  break;
		case 1 :
		  phimatch_pos_sigma[isect][ipar][iipar] = achan->getParameter(ipar,iipar); 
		  break;
		case 2 :
		  phimatch_neg_mean[isect][ipar][iipar]  = achan->getParameter(ipar,iipar); 
		  break;
		case 3 :
		  phimatch_neg_sigma[isect][ipar][iipar] = achan->getParameter(ipar,iipar); 
		  break;
		case 4 :
		  zmatch_pos_mean[isect][ipar][iipar]    = achan->getParameter(ipar,iipar); 
		  break;
		case 5 :
		  zmatch_pos_sigma[isect][ipar][iipar]   = achan->getParameter(ipar,iipar); 
		  break;
		case 6 :
		  zmatch_neg_mean[isect][ipar][iipar]    = achan->getParameter(ipar,iipar); 
		  break;
		case 7 :
		  zmatch_neg_sigma[isect][ipar][iipar]   = achan->getParameter(ipar,iipar); 
		  break;
		case 8 :  
		  dep = DepObjv1::instance();
		  dep->set_meanpar(1, isect, ipar, achan->getParameter(ipar,iipar)); 
		  break;
		case 9 :  
		  dep = DepObjv1::instance();
		  dep->set_sigpar(1, isect, ipar, achan->getParameter(ipar,iipar)); 
		  break;
		case 10 : 
		  dep = DepObjv1::instance();
		  dep->set_meanpar(-1, isect, ipar, achan->getParameter(ipar,iipar)); 
		  break;
		case 11 : 
		  dep = DepObjv1::instance();
		  dep->set_sigpar(-1, isect, ipar, achan->getParameter(ipar,iipar)); 
		  break;
		case 12 : 
		  dep = DepObjv2::instance();
		  dep->set_meanpar(1, isect, nmompar * ipar + iipar, achan->getParameter(ipar,iipar)); 
		  break;
		case 13 : 
		  dep = DepObjv2::instance();
		  dep->set_sigpar(1, isect, nmompar * ipar + iipar, achan->getParameter(ipar,iipar)); 
		  break;
		case 14 : 
		  dep = DepObjv2::instance();
		  dep->set_meanpar(-1, isect, nmompar * ipar + iipar, achan->getParameter(ipar,iipar)); 
		  break;
		case 15 : 
		  dep = DepObjv2::instance();
		  dep->set_sigpar(-1, isect, nmompar * ipar + iipar, achan->getParameter(ipar,iipar)); 
		  break;
		}
	    }
	}
    }
  application->commit();
  return 0;
}

float
EmcElMatchingRecalReco::getMatchPar(const int bankid, const int isect, const int ipar, const int iipar) const
{
  DepObj* dep;
  switch (bankid)
    {  
    case 0 :
      return phimatch_pos_mean[isect][ipar][iipar];
    case 1 :
      return phimatch_pos_sigma[isect][ipar][iipar];
    case 2 :
      return phimatch_neg_mean[isect][ipar][iipar];
    case 3 :
      return phimatch_neg_sigma[isect][ipar][iipar];
    case 4 :
      return zmatch_pos_mean[isect][ipar][iipar];
    case 5 :
      return zmatch_pos_sigma[isect][ipar][iipar];
    case 6 :
      return zmatch_neg_mean[isect][ipar][iipar];
    case 7 :
      return zmatch_neg_sigma[isect][ipar][iipar];
    case 8 :  
      dep = DepObjv1::instance();
      return dep->get_meanpar(1, isect, ipar);
    case 9 :  
      dep = DepObjv1::instance();
      return dep->get_sigpar(1, isect, ipar);
    case 10 : 
      dep = DepObjv1::instance();
      return dep->get_meanpar(-1, isect, ipar);
    case 11 : 
      dep = DepObjv1::instance();
      return dep->get_sigpar(-1, isect, ipar);
    case 12 : 
      dep = DepObjv2::instance();
      return dep->get_meanpar(1, isect, ipar);
    case 13 : 
      dep = DepObjv2::instance();
      return dep->get_sigpar(1, isect, ipar);
    case 14 : 
      dep = DepObjv2::instance();
      return dep->get_meanpar(-1, isect, ipar);
    case 15 : 
      dep = DepObjv2::instance();
      return dep->get_sigpar(-1, isect, ipar);
    default : 
      return -9999.;
    }
}

string
EmcElMatchingRecalReco::bankDescription(const int  bankid) const
{
  if (bankid<16)
    {
      return description[bankid];
    }
  return "Do not exist.";
}
