#include "EmcTrkMatchingRecalReco.h"

#include "RunHeader.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"

#include "getClass.h"
#include "recoConsts.h"
#include "PHCompositeNode.h"

#include "gsl/gsl_math.h"

#include <cstdlib>
#include <iostream>

using namespace std;

///////////////// emcdz vs. zed ////////////////////////////////////

const float p00_z_zdep_alpha0[8] = {2.234, 3.633, -3.815, -4.846, -1.937, -2.386,
                                -3.61, -5.575};
const float p01_z_zdep_alpha0[8] = {3.21, 3.119, 2.718, 2.36, 2.776, 2.673, 2.603,
                                2.374};
const float p02_z_zdep_alpha0[8] = {3.756, 4.208, 6.527, 4.612, 4.005, 3.812, 3.956,
                                2.336};

const float p10_z_zdep_alpha0[8] = { -0.9593, -0.2842, -0.5355, -0.1574, 0.153, 0.133,
                                 0.00659, 0.2797 };
const float p11_z_zdep_alpha0[8] = {0.1669, -0.2068, -0.5057, -0.7972, 0.1464,
                                0.3332, -0.006248, 0.1868};
const float p12_z_zdep_alpha0[8] = {6.119, 2.393, 2.496, 0.7665, 4.838, 4.733,
                                5.0, 6.691};

const float p00_z_zdep_alpha1[8] = {3.171, 3.824, -3.492, -5.044, -2.71, -2.786,
                                -3.962, -5.104};
const float p01_z_zdep_alpha1[8] = {3.185, 3.39, 2.417, 1.806, 2.139, 2.325, 2.381,
                                2.618 };
const float p02_z_zdep_alpha1[8] = {3.376, 4.075, 4.487, 1.363, 2.656, 3.907,
                                4.518, 4.749};

const float p10_z_zdep_alpha1[8] = { -1.025, -0.09608, -0.539, -0.1389, 0.05511,
                                 0.1086, 0.06584, 0.4247};
const float p11_z_zdep_alpha1[8] = { -0.681, -0.3163, 0.3432, -0.4, -0.3132, 0.03951,
                                 0.2556, -0.1535 };
const float p12_z_zdep_alpha1[8] = {1.445, 1.13, 10, 6.637, 5.114, 5, 4.198, 3.141};


/////////////// emcdphi vs zed ////////////////////////////
/////////////// p0(p)+p1(p)*z+p2(p)*z*z //////////////////

const float p00_phi_zdep_alpha0[8] = { -4.46626e-05 , 0.00310239 , 0.00196224 ,
                                   0.00377458 , 0.000200786 , 0.000825099,
                                   0.000193834 , 0.00172665};
const float p01_phi_zdep_alpha0[8] = { -4.02811 , -3.96068 , -4.70901 , -4.54684 ,
                                   -4.61126 , -4.68186 , -4.56256 , -4.62788 };
const float p02_phi_zdep_alpha0[8] = {1.17497 , 1.80882 , 1.72788 , 1.98724 ,
                                  1.89352, 1.79634 , 1.64974 , 1.57086 };

const float p10_phi_zdep_alpha0[8] = { -2.66822e-06 , -3.89081e-07 , 7.00516e-06 ,
                                   9.03977e-06 , 5.81513e-06 ,
                                   -2.93204e-06 , -7.1674e-06 , -1.51518e-05};
const float p11_phi_zdep_alpha0[8] = {2.13768 , 5.96307 , 2.33754 , 3.30025 ,
                                  8.2803 , 4.76946 , 10.6271 , 9.34343};
const float p12_phi_zdep_alpha0[8] = { -5.30034 , 1.18273 , -5.21966 , -3.44888 ,
                                   5.29774 , -1.71301 , 8.86846 , 6.17113};
const float p13_phi_zdep_alpha0[8] = { 7.9672 , 15.9258 , 8.04821 , 10.4561 ,
                                   23.1505, 13.4153 , 27.0812 , 23.0896};

const float p20_phi_zdep_alpha0[8] = { -1.26368e-10 , -7.7857e-11 , -1.07021e-07 ,
                                   -2.46441e-08 , -1.03109e-07 ,
                                   -1.60113e-07 , -1.50529e-07 ,
                                   -1.16975e-07 };
const float p21_phi_zdep_alpha0[8] = {2.75217 , 2.41514 , 3.92455 , 2.50036 , 4.01303
                                  , 4.15227 , 3.56525 , 3.62977};
const float p22_phi_zdep_alpha0[8] = { -7.56946 , -8.23842 , -5.61019 , -8.32195 ,
                                   -5.56197 , -5.56151 , -6.64458 ,
                                   -6.34143};
const float p23_phi_zdep_alpha0[8] = { 8.05893 , 7.15336 , 10.5734 , 7.17104 ,
                                   10.9137 , 10.6604 , 9.25672 , 9.54438};

const float p00_phi_zdep_alpha1[8] = { -0.00300888 , -0.00180435 , -0.000773268 ,
                                   0.000930627 , -0.0027538 ,
                                   -0.00261698 , -0.00248665 , -0.000516773};
const float p01_phi_zdep_alpha1[8] = { -3.95423 , -3.88611 , -4.51782 , -4.74471 ,
                                   -4.73107 , -4.617 , -4.59688 ,
                                   -4.60441 };
const float p02_phi_zdep_alpha1[8] = {1.88831 , 1.92182 , 1.86893 , 1.70811 , 1.52772
                                  , 1.71638 , 1.97933 , 1.76012};

const float p10_phi_zdep_alpha1[8] = {2.21982e-06 , 5.41898e-06 , 1.00653e-05 ,
                                  8.97318e-06 , 4.92694e-06 , -2.70464e-06 ,
                                  -6.2955e-06 , -1.52739e-05 };
const float p11_phi_zdep_alpha1[8] = {4.24683 , 5.10613 , 2.89785 , 0.219565 ,
                                  1.31833 , 6.10604 , 8.55313 , 5.46758};
const float p12_phi_zdep_alpha1[8] = { -1.6614 , 0.186794 , -4.21042 , -9.24182 ,
                                   -9.03092 , 1.02342 , 5.49237 ,
                                   -0.196356};
const float p13_phi_zdep_alpha1[8] = {12.5567 , 15.7439 , 9.65902 , 4.10944 ,
                                  3.30571, 16.4029 , 22.979 , 15.0725};

const float p20_phi_zdep_alpha1[8] = {1.46854e-07 , 2.94343e-08 , 1.67688e-07 ,
                                  2.16578e-07 , 1.31472e-07 ,
                                  2.33215e-08 , 6.16967e-08 , 1.12839e-08};
const float p21_phi_zdep_alpha1[8] = {1.86572 , 2.32985 , 3.10595 , 6.67174 ,
                                  5.58368, 4.0693 , 4.85927 , 3.32835};
const float p22_phi_zdep_alpha1[8] = { -8.33686 , -8.08774 , -6.88703 , -0.635805 ,
                                   -2.90888 , -5.66033 , -4.29148 , -7.00161};
const float p23_phi_zdep_alpha1[8] = {8.30421 , 7.65684 , 9.28462 , 17.056 ,
                                  13.9675 , 10.3327 , 12.1806 , 8.71976};

////////////// emcdphi vs phi //////////////////////////////////////////////
///////////// [0]*(phi-[1])+[2] ////////////////////////////////////////////
// [0] is constant but separete p<0.25, 0.25<p<0.35 , 0.35<p<0.45 //////////
////////////// [1], [2] is also constant

const float p0_dphi_phidep[8] = { -0.00281995 , -0.00552116 , 0.00530438 ,
                              0.00745806 , 0.00775731 , 0.0082891 ,
                              0.00698566 , 0.00876341};
const float p1_dphi_phidep[8] = {3.5, 3.1, 2.75, 2.35, -0.4, 0.0, 0.4, 0.8};
const float p2_dphi_phidep[8] = {3.08155e-05 , 0.000364292 , 0.000148534 ,
                             -6.48976e-05 , -0.000213735 ,
                             -2.93642e-05 , 4.00721e-05 , 0.000491016 };
/// p<0.25///
const float p0_dphi_phidep_p025[8] = { -0.0308288, -0.0139367, 0.00453213, -0.0520709,
                                   -0.0257264, 0.00280599, -0.00169268 , -0.0188224};
/// 0.25<p<0.35 /////////
const float p0_dphi_phidep_p035[8] = { -0.014611, -0.00898977, 0.00235281, -0.0122337,
                                   -0.0073260, 0.00660768,
                                   0.000120616, -0.00357492};
/// 0.35<p<0.45 ////////
const float p0_dphi_phidep_p045[8] = { -0.007343, -0.00664223, 0.00347036,
                                   0.000906985, 0.000572535, 0.00975385,
                                   0.00197031, 0.00449536};

// p<0.25 ////
const float p2_dphi_phidep_p025[8] = { -0.00091201, -0.000200544, -0.000718563,
                                   0.00356448, 0.00366262, 0.000428262,
                                   0.000290308, -0.00251981 };
/// 0.25<p<0.35 ////
const float p2_dphi_phidep_p035[8] = { -0.000434777, 0.000248691, 4.04409e-05,
                                   0.000624687, 0.000888538, 0.00025472,
                                   9.79467e-05, -0.000507778};
/// 0.35<p<0.45 ////
const float p2_dphi_phidep_p045[8] = { -0.000103498, 0.000364474, 0.000154854,
                                   3.46074e-05, -1.48952e-05, 7.35835e-05,
                                   8.15696e-06, 0.000296314};


////////////////// emcdz sigma //////////////////////////////////////////
//// sigma = sqrt(pow(p0,2)+pow(p1*sin(theta-3.1514/2.0),2)) ////////////
//// p0 = sqrt(pow([0],2)+pow([1],2)/pow(p,[2])) ////////////////////////
////  p1 = [0]+exp([1]-[2]*p) ///////////////////////////////////////////

const float p0_p0_dz_sigma[8] = {0.5097, 0.6173, 1.143e-08, -1.222e-08, 1.754e-07,
                             3.229e-08, 2.151e-10, 5.357e-10};
const float p0_p1_dz_sigma[8] = {1.371, 1.319, 1.525, 1.432, 1.542, 1.603,
                             1.544, 1.544};
const float p0_p2_dz_sigma[8] = {1.391, 1.574, 1.264, 1.12, 1.082, 1.191,
                             1.138, 1.15};

const float p1_p0_dz_sigma[8] = {2.737, 2.737, 2.42, 2.42, 2.19, 2.19, 2.661, 2.661};
const float p1_p1_dz_sigma[8] = {1.898, 1.898, 1.657, 1.657, 2.286, 2.286,
                             2.457, 2.457};
const float p2_p1_dz_sigma[8] = {1.768, 1.769, 1.85, 1.85, 2.911, 2.911,
                             3.588, 3.588};

/////////////// emcdphi sigma //////////////////////////////////////////
////// sigma = sqrt(pow(p0,2)+pow(p1*zed,2))////////////////
//////  p0 = sqrt(pow([0],2)+pow([1],2)/pow(p,[2]))/////////
//////  p1 = [0]+exp([1]-[2]*p)  for p<0.8 && |zed|<60 /////////
/////// p1 = const for p>0.8 && (|zed|<60 || |zed|>60) ////////////
/// p1 =  sqrt(pow(p0,2)+pow(p1*60,2)) (p0, p1 is sqrt and exp) for p<0.45 |zed|>60////////////////
/// p1 =  sqrt(pow(p0,2)+pow(p1*70,2)) (p0, p1 is sqrt and exp) for p>0.45 && p<0.8 |zed|>60////////

///////////////////////////////////////////////////////////

const float p0_p0_dphi_sigma[8] = {9.52327e-04, 1.25862e-03, 8.47077e-04,
                               -1.44379e-10, -1.41468e-13,
                               -1.35340e-13, -1.66304e-13, -1.42630e-13};
const float p0_p1_dphi_sigma[8] = {2.20661e-03, 2.24091e-03, 2.67938e-03,
                               3.00325e-03, 3.11979e-03,
                               3.44985e-03, 2.96211e-03, 2.87930e-03};
const float p0_p2_dphi_sigma[8] = {1.90349e+00, 1.95949e+00, 1.33110e+00,
                               1.24210e+00, 1.18735e+00,
                               9.78407e-01, 1.13259e+00 , 1.10854e+00};

const float p1_p0_dphi_sigma[8] = {1.22574e-05, 0, 0, 0, 0, 1.07741e-05, 0, 0.00000e+00};
const float p1_p1_dphi_sigma[8] = { -5.41510e+00, -8, -6.2, -6.2, -6.2, -5.93812e+00,
                                -5.7, -7.64680e+00};
const float p1_p2_dphi_sigma[8] = {9.77790e+00, 4, 7.5, 7.5, 7.5, 8.40166e+00 ,
                               8.5, 4.17990e+00};

const float p1_p0_dphi_sigma_p08_zed60 = -1;
const float p1_p1_dphi_sigma_p08_zed60 = 0;
const float p1_p2_dphi_sigma_p08_zed60 = 0;

float ad_hoc_emcsdphi_e_scale(float pt)
{
	float value = ((4.58435e-01 - 2.02492e+01*exp(-1.09881e+02*(pt-1.57264e-01))) + (-6.57944e-01*pt*pt + 1.17498e+00*pt));
	return value;
}

EmcTrkMatchingRecalReco::EmcTrkMatchingRecalReco(const string &name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
}

int 
EmcTrkMatchingRecalReco::isValidRun(const int runno) const
{
  if (runno >= 107445 && runno <= 122223)
    {
      return 1;
    }
  return 0;
}

int 
EmcTrkMatchingRecalReco::InitRun(PHCompositeNode *topNode)
{
  haverich = 2;
  return 0;
}

int 
EmcTrkMatchingRecalReco::process_event(PHCompositeNode *topNode)
{
  if (!haverich)
    {
      return 0;
    }
  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
	{
	  PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
	  if (haverich == 2)
	    {
	      sngltrk->ShutUp();
	      if (
		  sngltrk->isImplemented(sngltrk->get_mom()) &&
		  sngltrk->isImplemented(sngltrk->get_alpha()) &&
		  sngltrk->isImplemented(sngltrk->get_beta()) &&
		  sngltrk->isImplemented(sngltrk->get_zed()) &&
		  sngltrk->isImplemented(sngltrk->get_charge()) &&
		  sngltrk->isImplemented(sngltrk->get_dcarm()) &&
		  sngltrk->isImplemented(sngltrk->get_sect()) &&
		  sngltrk->isImplemented(sngltrk->get_pemcy()) &&
		  sngltrk->isImplemented(sngltrk->get_pemcx()) &&
		  sngltrk->isImplemented(sngltrk->get_emcsdz_e()) &&
		  sngltrk->isImplemented(sngltrk->get_emcsdphi_e()) &&
		  sngltrk->isImplemented(sngltrk->get_emcdz()) &&
		  sngltrk->isImplemented(sngltrk->get_emcdphi()) &&
		  sngltrk->isImplemented(sngltrk->get_the0()) &&
		  sngltrk->isImplemented(sngltrk->get_phi0()) )
		{
		  haverich = 1;
		  sngltrk->ShutUp(0);
		}
	      else
		{
		  haverich = 0;
		  sngltrk->ShutUp(0);
		  return 0;
		}
	    }

	  if ( d_cnt->get_n1(i) < 0 && d_cnt->get_sn1(i) < 0)
	    {
	      continue;
	    }


	  float mom = d_cnt->get_mom (i);
	  float alpha = d_cnt->get_alpha(i);
	  float theta = d_cnt->get_beta (i);
	  float zed = d_cnt->get_zed (i);
	  int charge = d_cnt->get_charge (i);
	  int emcsector = 4 * (d_cnt->get_dcarm (i)) + d_cnt->get_sect (i);
	  float emcphi = atan2(d_cnt->get_pemcy (i), d_cnt->get_pemcx (i));
	  float emcsdz_e = d_cnt->get_emcsdz_e (i);
	  float emcsdphi_e = d_cnt->get_emcsdphi_e (i);

	  float emcdz = d_cnt->get_emcdz (i);
	  float emcdphi = d_cnt->get_emcdphi (i);
	  float Px = d_cnt->get_mom(i) * sin(d_cnt->get_the0(i)) * cos(d_cnt->get_phi0(i));
	  float Py = d_cnt->get_mom(i) * sin(d_cnt->get_the0(i)) * sin(d_cnt->get_phi0(i));


	  float new_emcsdz_e = Emcsdz_e_Match(emcsdz_e, emcdz, alpha, mom,
					      theta, emcsector);

	  d_cnt->set_emcsdz_e(i, new_emcsdz_e);



	  if (emcphi < -M_PI*0.5 && emcphi > -M_PI)
	    {
	      emcphi = 2.0 * M_PI + emcphi;
	    }

	  float new_emcsdphi_e = Emcsdphi_e_Match(Px, Py, emcsdphi_e, emcdphi, charge, alpha, mom,
                                                zed, emcphi, emcsector);

        d_cnt->set_emcsdphi_e(i, new_emcsdphi_e);

      }
  } // if(d_cnt)

return 0;
}


float 
EmcTrkMatchingRecalReco::Emcsdz_e_Match(const float emcsdz_e, const float emcdz, const float alpha, const float mom, const float beta, const int sector)
{

  if (sector < 0 || sector > 7)
    {
      return emcsdz_e;
    }

  float dzecor = 0;
  float norm_emcdz = 1;
  float emcsdz_e_new;

  if ( alpha > 0)
    {
      if (sector < 4)
        {
          dzecor = (p00_z_zdep_alpha1[sector] + exp(p01_z_zdep_alpha1[sector] -
						    p02_z_zdep_alpha1[sector] * mom))
	    * tan(beta - 3.141592 / 2.0) + (p10_z_zdep_alpha1[sector] - exp(p11_z_zdep_alpha1[sector] - p12_z_zdep_alpha1[sector] * mom));
        }
      else
        {
          dzecor = (p00_z_zdep_alpha1[sector] + exp(p01_z_zdep_alpha1[sector] -
						    p02_z_zdep_alpha1[sector] * mom))
	    * tan(beta - M_PI / 2.0) + (p10_z_zdep_alpha1[sector] + exp(p11_z_zdep_alpha1[sector] - p12_z_zdep_alpha1[sector] * mom));
        }
    }
  else if (alpha < 0)
    {
      if (sector < 4)
        {
          dzecor = (p00_z_zdep_alpha0[sector] + exp(p01_z_zdep_alpha0[sector] -
						    p02_z_zdep_alpha0[sector] * mom))
	    * tan(beta - M_PI / 2.0) + (p10_z_zdep_alpha0[sector] - exp(p11_z_zdep_alpha0[sector] - p12_z_zdep_alpha0[sector] * mom));
        }
      else
        {
          dzecor = (p00_z_zdep_alpha0[sector] + exp(p01_z_zdep_alpha0[sector] -
						    p02_z_zdep_alpha0[sector] * mom))
	    * tan(beta - M_PI / 2.0) + (p10_z_zdep_alpha0[sector] + exp(p11_z_zdep_alpha0[sector] - p12_z_zdep_alpha0[sector] * mom));
        }
    }

  norm_emcdz = sqrt( (pow(p0_p0_dz_sigma[sector], 2) + pow(p0_p1_dz_sigma[sector], 2) / pow(mom, p0_p2_dz_sigma[sector])) + pow((p1_p0_dz_sigma[sector] + exp(p1_p1_dz_sigma[sector] - p2_p1_dz_sigma[sector] * mom)) * sin(beta - M_PI / 2.0), 2));

  emcsdz_e_new = (emcdz - dzecor) / norm_emcdz;


  return emcsdz_e_new;



}

float 
EmcTrkMatchingRecalReco::Emcsdphi_e_Match(const float Px, const float Py, const float emcsdphi_e, const float emcdphi, const int charge, const float alpha, const float mom, const float zed, const float emcphi, const int sector)
{

  if (sector < 0 || sector > 7)
    {
      return emcsdphi_e;
    }

  float dphiecor = 0;
  float dphie_phicor = 0;
  float norm_emcdphi = 1;
  float emcsdphi_e_new;

  if (alpha > 0)
    {
      dphiecor = (p00_phi_zdep_alpha1[sector] - exp(p01_phi_zdep_alpha1[sector] -
						    p02_phi_zdep_alpha1[sector] * mom)) + (p10_phi_zdep_alpha1[sector] + pow(mom, p11_phi_zdep_alpha1[sector]) * exp(p12_phi_zdep_alpha1[sector] - p13_phi_zdep_alpha1[sector] * mom)) * zed + (p20_phi_zdep_alpha1[sector] + pow(mom, p21_phi_zdep_alpha1[sector]) * exp(p22_phi_zdep_alpha1[sector] - p23_phi_zdep_alpha1[sector] * mom)) * pow(zed, 2);
    }
  else if (alpha < 0)
    {
      dphiecor = (p00_phi_zdep_alpha0[sector] + exp(p01_phi_zdep_alpha0[sector] -
                  p02_phi_zdep_alpha0[sector] * mom)) + (p10_phi_zdep_alpha0[sector] - pow(mom, p11_phi_zdep_alpha0[sector]) * exp(p12_phi_zdep_alpha0[sector] - p13_phi_zdep_alpha0[sector] * mom)) * zed + (p20_phi_zdep_alpha0[sector] - pow(mom, p21_phi_zdep_alpha0[sector]) * exp(p22_phi_zdep_alpha0[sector] - p23_phi_zdep_alpha0[sector] * mom)) * pow(zed, 2);
    }


  if ( mom < 0.25)
    {
      dphie_phicor = p0_dphi_phidep_p025[sector] * (emcphi - p1_dphi_phidep[sector]) + p2_dphi_phidep_p025[sector];
    }
  else if (mom >= 0.25 && mom < 0.35)
    {
      dphie_phicor = p0_dphi_phidep_p035[sector] * (emcphi - p1_dphi_phidep[sector]) + p2_dphi_phidep_p035[sector];
    }
  else if (mom >= 0.35 && mom < 0.45)
    {
      dphie_phicor = p0_dphi_phidep_p045[sector] * (emcphi - p1_dphi_phidep[sector]) + p2_dphi_phidep_p045[sector];
    }
  else
    {
      dphie_phicor = p0_dphi_phidep[sector] * (emcphi - p1_dphi_phidep[sector]) + p2_dphi_phidep[sector];
    }


  if (mom < 0.8 && abs(zed) < 60)
    {

      norm_emcdphi = sqrt((pow(p0_p0_dphi_sigma[sector], 2) + pow(p0_p1_dphi_sigma[sector], 2) / pow(mom, p0_p2_dphi_sigma[sector])) + pow((p1_p0_dphi_sigma[sector] + exp(p1_p1_dphi_sigma[sector] - p1_p2_dphi_sigma[sector] * mom)) * zed, 2));

    }
  else if (mom > 0.8 && (abs(zed) >= 60 || abs(zed < 60)))
    {

      norm_emcdphi = sqrt((pow(p0_p0_dphi_sigma[sector], 2) + pow(p0_p1_dphi_sigma[sector], 2) / pow(mom, p0_p2_dphi_sigma[sector])) + pow((p1_p0_dphi_sigma_p08_zed60 + exp(p1_p1_dphi_sigma_p08_zed60 - p1_p2_dphi_sigma_p08_zed60 * mom)) * zed, 2));

    }
  else if (mom < 0.8 && mom > 0.45 && abs(zed) > 60)
    {

      norm_emcdphi = sqrt((pow(p0_p0_dphi_sigma[sector], 2) + pow(p0_p1_dphi_sigma[sector], 2) / pow(mom, p0_p2_dphi_sigma[sector])) + pow((p1_p0_dphi_sigma[sector] + exp(p1_p1_dphi_sigma[sector] - p1_p2_dphi_sigma[sector] * mom)) * 70, 2));

    }
  else if (mom <= 0.45 && abs(zed) > 60)
    {

      norm_emcdphi = sqrt((pow(p0_p0_dphi_sigma[sector], 2) + pow(p0_p1_dphi_sigma[sector], 2) / pow(mom, p0_p2_dphi_sigma[sector])) + pow((p1_p0_dphi_sigma[sector] + exp(p1_p1_dphi_sigma[sector] - p1_p2_dphi_sigma[sector] * mom)) * 60, 2));

    }
  else
    {
      norm_emcdphi = 1.0;
    }
    
    float pt = sqrt(Px*Px + Py*Py);

    if(pt>.86)
    {
	    emcsdphi_e_new = (emcdphi - dphiecor - dphie_phicor) / norm_emcdphi;
   }
    else
    {
	   emcsdphi_e_new = ((emcdphi - dphiecor - dphie_phicor) / norm_emcdphi)/(ad_hoc_emcsdphi_e_scale(pt));
   }

  return emcsdphi_e_new ;

}
