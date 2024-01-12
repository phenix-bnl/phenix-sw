#include <Pidrecal_dAu_Reco.h>

#include <PHCompositeNode.h>

#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <Fun4AllServer.h>

#include <getClass.h>
#include <recoConsts.h>
#include <TH2.h>

#include <gsl/gsl_const.h>

#include <string>
#include <fstream>

//==============================================================================
// T O F - P I D   P A R A M E T E R S for dAu Run3
// based on tofHelper code written by Felix Matathias and in CVS under
// offline/analysis/tofHelper
// _____________________________________________________________________________
//
static const double light_v  = GSL_CONST_CGS_SPEED_OF_LIGHT / 1e9; // cm/ns

// values from tofHelper
static const int NRUNS = 27;
static const int NOffsets = 767;

static const double globalTzero[27][2] = { { 69502, -0.0791016 },  //phase1
				    { 69650, -0.0807463 },
				    { 69654, -0.0822935 },
				    { 70087, -0.0822203 }, 
				    { 72361, -0.0921035 },
				    { 74857, -0.0175869 },
				    { 74865, -0.0161134 },
				    { 74868, -0.0134772 },
				    { 75622, -0.0210526 }, 
				    { 75631, -0.0175799 },  //phase2
				    { 75796, -0.0192538 },   
				    { 75800, -0.0173683 },
				    { 76053, -0.0387799 },
				    { 76692, -0.0191795 },
				    { 76693, -0.0222647 },
				    { 76797, -0.0249733 },
				    { 76864, -0.0249456 },
				    { 76985, -0.0252891 },
				    { 76995, -0.026099  },
				    { 77380, -0.0298642 },
				    { 77391, -0.0308699 },
				    { 77392, -0.0250669 },
				    { 78033, -0.0303641 },
				    { 78035, -0.0303932 },
				    { 78182, -0.0409016 },
				    { 78210, -0.0371338 },
				    { 78435, -0.0353625 }  };
using namespace std;

Pidrecal_dAu_Reco::Pidrecal_dAu_Reco(const string &name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  return ;
}

int
Pidrecal_dAu_Reco::isValidRun(const int runno) const
{
// Run3 dAu runs used in Felix's analysis according to an289
  if (runno < globalTzero[0][0] || runno > globalTzero[NRUNS-1][0]) 
    {
      return 0;
    }

//  bool matching_Run=false;
//  for (int i=0; i<NRUNS; i++)
//    {
//      if (runno==globalTzero[i][0])
//	{
//	  matching_Run=true;
//	}
//    }
//
//  if (matching_Run)
//    {
      return 1;
//    }
//  else
//    {
//      return 0;
//    }
}

int 
Pidrecal_dAu_Reco::Init(PHCompositeNode *topNode)
{
  //initialize calib constants
  InitializeDeadSlats();
  SetTimingOffsets();

  //make histos
  Fun4AllServer *se = Fun4AllServer::instance();
  string Histoname = Name();
  Histoname += "_TOFmomtof";
  TOFmomtof = new TH2F(Histoname.c_str(),"1./momentum vs tof",100,15,40,100,-10,10);
  se->registerHisto(TOFmomtof);

  Histoname = Name();
  Histoname += "_TOFmomtofP";
  TOFmomtofP = new TH2F(Histoname.c_str(),"1./momentum vs tof Proton",100,15,40,100,-10,10);
  se->registerHisto(TOFmomtofP);

  Histoname = Name();
  Histoname += "_TOFmomtofK";
  TOFmomtofK = new TH2F(Histoname.c_str(),"1./momentum vs tof Kaon",100,15,40,100,-10,10);
  se->registerHisto(TOFmomtofK);

  Histoname = Name();
  Histoname += "_TOFmomtofPi";
  TOFmomtofPi = new TH2F(Histoname.c_str(),"1./momentum vs tof Pion",100,15,40,100,-10,10);
  se->registerHisto(TOFmomtofPi);

  return 0;
}

int
Pidrecal_dAu_Reco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int runnumber = rc->get_IntFlag("RUNNUMBER");
  if (verbosity > 0)
    {
      cout << "Fetching for run number: " << runnumber << endl;
    }
  bool found=false;
  float sum=0.0;
  for (int i=0; i<NRUNS; i++) 
    {
      if (globalTzero[i][0]==runnumber) 
	{
	  this_globalTzero = globalTzero[i][1];
	  found=true;
	}
      sum= globalTzero[i][1] + sum;
    }

  // if not found use average
  if (!found)
    {
      this_globalTzero= sum/NRUNS;
    }
  return 0;
}

int
Pidrecal_dAu_Reco::process_event(PHCompositeNode *topNode)
{
  d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
        {
          PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);

	  // do not run if m2tof is not valid (track did not hit tof)
	  if (!sngltrk->isImplemented(sngltrk->get_m2tof()) || !sngltrk->isImplemented(sngltrk->get_slat()))
	    {
	      continue;
	    }

	  if (!(sngltrk->isValid(sngltrk->get_dcarm())) &&
	      (sngltrk->isValid(sngltrk->get_sect ())) &&
	      (sngltrk->isValid(sngltrk->get_ysect())) &&
	      (sngltrk->isValid(sngltrk->get_zsect())) )
	    {
	      if (verbosity > 0) cout << PHWHERE << "missing valid values" << endl;
	      continue;
	    }

	  int dcarm = sngltrk->get_dcarm();
	  int sect  = sngltrk->get_sect();

	  if ( dcarm != 0 || sect > 1 ) continue;

	  // Much of the implementation of the tofHelper subprograms 
	  // is taken from spectraMaker.C by Felix Matathias
	  // it's located at offline/analysis/hadronSelect/SpectraMaker/
	  // or workarea/felice/run03/pid/offline/analysis/hadronSelect/spectraMaker

	  // remove dead slats
	  int slat = sngltrk->get_slat();
	  if(slat < 0 || (slat < 800 && dead_slat[slat])) continue;
	  if (slat >= 768) continue;  // JTM 5/14/13, to fix an insure error

          float mom   = sngltrk->get_mom();
          float ttof  = sngltrk->get_ttof();
          float pltof = sngltrk->get_pltof();

	  //correct ttof and mom2tof
	  //	  cout << i << " oldttof: " << ttof;
	  ttof = ttof - t0_offset[slat] - this_globalTzero ;
	  float m2tof = calcMeasM2(ttof, pltof , mom);
	  sngltrk->set_ttof(ttof);
	  sngltrk->set_m2tof(m2tof);
// 	  cout << " newttof: " << ttof << endl;
// 	  cout << " newm2tof: " << m2tof << endl;

          short charge = sngltrk->get_charge();
          float zed = sngltrk->get_zed();
          float tofsdz = sngltrk->get_tofsdz();
          float tofsdphi = sngltrk->get_tofsdphi();

	  // burning of the dz matching for real data Run03
	  float tofsdz_corrected   = d_TOF_z_match_new(   charge*mom, tofsdz, zed);
	  float tofsdphi_corrected = d_TOF_phi_match_new( charge*mom, tofsdphi);
	  sngltrk->set_tofsdz(tofsdz_corrected);
	  sngltrk->set_tofsdphi(tofsdphi_corrected);

	  //set isPi, isK, isP
          float nPi = IsPion(m2tof, mom);
          float nK  = IsKaon(m2tof, mom);
          float nP  = IsProton(m2tof, mom);

	  sngltrk->ShutUp();
          sngltrk->set_isPi(nPi);
          sngltrk->set_isK(nK);
          sngltrk->set_isP(nP);
	  sngltrk->ShutUp(0);

	  //fill histos
	  TOFmomtof->Fill(sngltrk->get_ttof(),1./mom*charge);
	  if (fabs(nPi)<1)
	    {
	      TOFmomtofPi->Fill(sngltrk->get_ttof(),1./mom*charge);
	    }
	  if (fabs(nP)<1)
	    {
	      TOFmomtofP->Fill(sngltrk->get_ttof(),1./mom*charge);
	    }
	  if (fabs(nK)<1)
	    {
	      TOFmomtofK->Fill(sngltrk->get_ttof(),1./mom*charge);
	    }
        }
    }
  else if (verbosity)
    {
      cout << PHWHERE << "no PHCentralTrack "<< endl;
    }


  return 0;
}

void Pidrecal_dAu_Reco::Print(const string &what) const
{
  cout << PHWHERE << "Currently Pidrecal_dAu_Reco is not set up to print the parameters." << endl;
  return ;
}


// ==== isPi, isK, isP ===

double //taken directly from tofHelper.C
Pidrecal_dAu_Reco::IsPion(double m2, double p)
{
  double centroid = meanPi2(p);
  double nPi = 9999.9;

  nPi = (m2-centroid)/sigmaM2_analytical( m2, p );

  return nPi;
}

double //taken directly from tofHelper.C
Pidrecal_dAu_Reco::IsKaon(double m2, double p)
{

  double centroid = meanK2(p);
  double nK = 9999.9;
  
  nK = (m2-centroid)/sigmaM2_analytical( m2, p );
  
  return nK;  
}

double //taken directly from tofHelper.C
Pidrecal_dAu_Reco::IsProton(double m2, double p)
{
  double centroid = meanP2(p);
  double  nP = 9999.9;

  nP = (m2-centroid)/sigmaM2_analytical( m2, p );
  
  return nP;
}

double //taken directly from tofHelper.C
Pidrecal_dAu_Reco::sigmaM2_analytical(double m2, double p)
{
  //analytical form of sigma_M2, the traditional functional form used by Phenix
  if(p==0) return 9999;
  
  //parameters by Anuj Purwar, run#74865, averaged between positives and negatives
  double param[3];
  param[0] = 1.095/87; // angular resolution
  param[1] = 0.7272/87; // multiple scattering
  param[2] = 0.130 ;   // tof resolution 

  double Sigma  = sqrt(4*pow(param[0]*p*m2,2) + 4*pow(m2*param[1],2)*(1+m2/(p*p)) + 
			4*pow(param[2]/17,2)*(m2+p*p)*p*p);

  return Sigma;  
}

double //taken directly from tofHelper.C
Pidrecal_dAu_Reco::meanPi2(double mom)
{
  //  float retvalue = 0.0194795;
  //return retvalue;
  double par[3];
  par[0] = 0.01982;
  par[1] = -0.0001051;
  par[2] = -0.00206;

  double ret_val = par[0]+par[1]*mom+par[2]*mom*mom;
  return ret_val;
 
}

double //taken directly from tofHelper.C
Pidrecal_dAu_Reco::meanK2(double mom)
{
  float retvalue = 0.2437357;
  return retvalue;
}

double //taken directly from tofHelper.C
Pidrecal_dAu_Reco::meanP2(double mom)
{
  float retvalue = 0.8803525;
  return retvalue;
}

float  //taken directly from tofHelper.C
Pidrecal_dAu_Reco::calcMeasM2(float t, float L, float p)
{
  float m2 = 0.0;

  if (L > 0) 
    {
      m2 =(pow(t*light_v/L,2.0)-1)*p*p;
    } 

  return m2;
}

float  //taken directly from tofHelper.C
Pidrecal_dAu_Reco::d_TOF_z_match_new(float p, float dv, float zed)
{
  if(p==0.0) return 10000;

  float tofz_mean[2] = {0.0 , 0.0};
  if(zed>0.)
    {
      tofz_mean[0] = -0.91960; 
      tofz_mean[1] =  0.00439;
    }
  else
    {
      tofz_mean[0] = 0.5281;
      tofz_mean[1] = 0.1029;
    }
  float offset   = (tofz_mean[0]*exp(-0.7*fabs(p))) + tofz_mean[1]; 
  

  float tofz_rms[2] = {0.0,0.0};
  if(zed>0.0) 
    { 
      tofz_rms[0] = 0.7171; 
      tofz_rms[1]=  1.258; 
    }
  else
    {
      tofz_rms[0] = 0.6865; 
      tofz_rms[1]=  1.189; 
    }
  float sigma_width = sqrt(tofz_rms[0]*tofz_rms[0]/(p*p) +  tofz_rms[1]*tofz_rms[1]);
  
  return (dv-offset)/sigma_width;
}

float  //taken directly from tofHelper.C
Pidrecal_dAu_Reco::d_TOF_phi_match_new(float p, float dv)
{
  if(p==0.0) return 10000;

  float tofphi_mean[2] = {0.0 , 0.0};
  if(p>0.)
    {
      tofphi_mean[0] =  0.003443; 
      tofphi_mean[1] =  0.001826;
    }
  else
    {
      tofphi_mean[0] = -0.004191;
      tofphi_mean[1] =  0.001031;
    }
  float offset   = (tofphi_mean[0]*exp(-1.679*fabs(p))) + tofphi_mean[1]; 

  float tofphi_rms[2] = {0.0,0.0};
  if(p>0.0) 
    { 
      tofphi_rms[0] = 0.01263; 
      tofphi_rms[1]=  0.00410; 
    }
  else
    {
      tofphi_rms[0] = 0.01219; 
      tofphi_rms[1]=  0.00411; 
    }
  float sigma_width = (tofphi_rms[0]*exp(-4.94*fabs(p))) + tofphi_rms[1];
  
  return (dv-offset)/sigma_width;
}

void //this is taken from tofHelper
Pidrecal_dAu_Reco::InitializeDeadSlats()
{
  for(int i=0; i<800; i++)
    dead_slat[i]=0;

  //even though the timing has been fixed
  //for these slats, their dphi is way off.
  //until we correct for it, these slats
  //have to be removed from real and montecarlo

  //FELIX--have these been fixed by matchingRecal???

  dead_slat[5]=1;
  dead_slat[8]=1;
  dead_slat[180]=1;
  dead_slat[248]=1;
  dead_slat[249]=1;
  dead_slat[250]=1;
  dead_slat[251]=1;
  dead_slat[252]=1;
  dead_slat[253]=1;
  dead_slat[254]=1;
  dead_slat[255]=1;
  dead_slat[304]=1;
  dead_slat[305]=1;
  dead_slat[306]=1;
  dead_slat[307]=1;
  dead_slat[308]=1;
  dead_slat[309]=1;
  dead_slat[310]=1;
  dead_slat[311]=1;
  dead_slat[483]=1;
  dead_slat[706]=1;

  return;
}

void //values from tofHelper
Pidrecal_dAu_Reco::SetTimingOffsets()
{
  // tofHelper called on the files slat_times_corrections_pos.txt, slat_times_corrections_neg.txt
  // and read the following values

  t0_offset_pos[0]= -0.775043; 
  t0_offset_pos[1]= 1.8635; 
  t0_offset_pos[2]= 0.487225; 
  t0_offset_pos[3]= 0.0651787; 
  t0_offset_pos[4]= 0.00771462; 
  t0_offset_pos[5]= -0.0705223; 
  t0_offset_pos[6]= -0.0416898; 
  t0_offset_pos[7]= 0.132742; 
  t0_offset_pos[8]= 0.00127785; 
  t0_offset_pos[9]= 0.0973018; 
  t0_offset_pos[10]= 0.0496238; 
  t0_offset_pos[11]= 0.0859395; 
  t0_offset_pos[12]= 0.00280185; 
  t0_offset_pos[13]= 0.092743; 
  t0_offset_pos[14]= 0.0468865; 
  t0_offset_pos[15]= 0.0371427; 
  t0_offset_pos[16]= 0.20784; 
  t0_offset_pos[17]= 0.454672; 
  t0_offset_pos[18]= -0.0234481; 
  t0_offset_pos[19]= 0.144293; 
  t0_offset_pos[20]= 0.190474; 
  t0_offset_pos[21]= 0.0892023; 
  t0_offset_pos[22]= 0.0445325; 
  t0_offset_pos[23]= -0.0406512; 
  t0_offset_pos[24]= 0.0221793; 
  t0_offset_pos[25]= -0.0204567; 
  t0_offset_pos[26]= -0.00687498; 
  t0_offset_pos[27]= 0.0481857; 
  t0_offset_pos[28]= 0.00700004; 
  t0_offset_pos[29]= 0.0718341; 
  t0_offset_pos[30]= 0.0836372; 
  t0_offset_pos[31]= 0.0565784; 
  t0_offset_pos[32]= 0.156165; 
  t0_offset_pos[33]= 0.267299; 
  t0_offset_pos[34]= 0.117138; 
  t0_offset_pos[35]= 0.177653; 
  t0_offset_pos[36]= -0.0202704; 
  t0_offset_pos[37]= 0.06402; 
  t0_offset_pos[38]= 0.0338084; 
  t0_offset_pos[39]= 0.0938984; 
  t0_offset_pos[40]= 0.0148394; 
  t0_offset_pos[41]= 0.0425057; 
  t0_offset_pos[42]= 0.0731534; 
  t0_offset_pos[43]= 0.0732763; 
  t0_offset_pos[44]= 0.0554489; 
  t0_offset_pos[45]= 0.00507335; 
  t0_offset_pos[46]= -0.057257; 
  t0_offset_pos[47]= -0.026399; 
  t0_offset_pos[48]= 0; 
  t0_offset_pos[49]= 0.379201; 
  t0_offset_pos[50]= 0; 
  t0_offset_pos[51]= 0.281734; 
  t0_offset_pos[52]= 0.164529; 
  t0_offset_pos[53]= 0.084479; 
  t0_offset_pos[54]= 0.103339; 
  t0_offset_pos[55]= 0.136271; 
  t0_offset_pos[56]= 0.00806435; 
  t0_offset_pos[57]= -0.0268875; 
  t0_offset_pos[58]= -0.00607366; 
  t0_offset_pos[59]= -0.0325095; 
  t0_offset_pos[60]= 0.00160878; 
  t0_offset_pos[61]= -0.0469917; 
  t0_offset_pos[62]= -0.0240039; 
  t0_offset_pos[63]= 0.00313477; 
  t0_offset_pos[64]= 0.225821; 
  t0_offset_pos[65]= 0.136236; 
  t0_offset_pos[66]= 0.0202127; 
  t0_offset_pos[67]= 0.0454435; 
  t0_offset_pos[68]= 0.0639418; 
  t0_offset_pos[69]= 0.0672736; 
  t0_offset_pos[70]= -0.00184136; 
  t0_offset_pos[71]= -0.0320139; 
  t0_offset_pos[72]= 0.0238965; 
  t0_offset_pos[73]= 0.0230459; 
  t0_offset_pos[74]= -0.0139873; 
  t0_offset_pos[75]= 0.0740844; 
  t0_offset_pos[76]= 0.0297561; 
  t0_offset_pos[77]= 0.0402551; 
  t0_offset_pos[78]= 0.0340657; 
  t0_offset_pos[79]= 0.029748; 
  t0_offset_pos[80]= 0.0246181; 
  t0_offset_pos[81]= 0.151199; 
  t0_offset_pos[82]= -0.000387304; 
  t0_offset_pos[83]= -0.00845912; 
  t0_offset_pos[84]= 0.177795; 
  t0_offset_pos[85]= 0; 
  t0_offset_pos[86]= -0.0372865; 
  t0_offset_pos[87]= -0.017413; 
  t0_offset_pos[88]= -0.00459967; 
  t0_offset_pos[89]= 0.000476618; 
  t0_offset_pos[90]= -0.00356928; 
  t0_offset_pos[91]= 0.0918785; 
  t0_offset_pos[92]= 0.0783247; 
  t0_offset_pos[93]= 0.0221764; 
  t0_offset_pos[94]= 0.0539294; 
  t0_offset_pos[95]= 0.0325938; 
  t0_offset_pos[96]= 0.00669392; 
  t0_offset_pos[97]= 0.0916734; 
  t0_offset_pos[98]= 0.0628771; 
  t0_offset_pos[99]= 0.0730027; 
  t0_offset_pos[100]= 0.0428244; 
  t0_offset_pos[101]= 0.00427434; 
  t0_offset_pos[102]= 0.068444; 
  t0_offset_pos[103]= 0.0449951; 
  t0_offset_pos[104]= 0.123244; 
  t0_offset_pos[105]= 0.0897522; 
  t0_offset_pos[106]= 0.118255; 
  t0_offset_pos[107]= 0.0884384; 
  t0_offset_pos[108]= 0.0819311; 
  t0_offset_pos[109]= 0.0587172; 
  t0_offset_pos[110]= 0.00398434; 
  t0_offset_pos[111]= 0.0289235; 
  t0_offset_pos[112]= 0.0718121; 
  t0_offset_pos[113]= 0.0910131; 
  t0_offset_pos[114]= 0.0283696; 
  t0_offset_pos[115]= -0.000190656; 
  t0_offset_pos[116]= 0.0548372; 
  t0_offset_pos[117]= 0.0242348; 
  t0_offset_pos[118]= 0.0636756; 
  t0_offset_pos[119]= 0.0631888; 
  t0_offset_pos[120]= 0.04814; 
  t0_offset_pos[121]= 0.0481252; 
  t0_offset_pos[122]= 0.0745214; 
  t0_offset_pos[123]= 0.0465477; 
  t0_offset_pos[124]= 0.0297402; 
  t0_offset_pos[125]= 0.084526; 
  t0_offset_pos[126]= 0.0335634; 
  t0_offset_pos[127]= 0.0800027; 
  t0_offset_pos[128]= 0.0513666; 
  t0_offset_pos[129]= 0.0480379; 
  t0_offset_pos[130]= 0.0612275; 
  t0_offset_pos[131]= 0.038613; 
  t0_offset_pos[132]= 0.0130769; 
  t0_offset_pos[133]= 0.0441; 
  t0_offset_pos[134]= 0.0246693; 
  t0_offset_pos[135]= 0.0335485; 
  t0_offset_pos[136]= 0.433172; 
  t0_offset_pos[137]= 0.47695; 
  t0_offset_pos[138]= 0.508077; 
  t0_offset_pos[139]= 0.456249; 
  t0_offset_pos[140]= 0.377036; 
  t0_offset_pos[141]= 0.436111; 
  t0_offset_pos[142]= 0; 
  t0_offset_pos[143]= 0.394824; 
  t0_offset_pos[144]= 0.0824962; 
  t0_offset_pos[145]= -0.0019975; 
  t0_offset_pos[146]= 0.0582486; 
  t0_offset_pos[147]= 0.01718; 
  t0_offset_pos[148]= -0.00335192; 
  t0_offset_pos[149]= 0.0622946; 
  t0_offset_pos[150]= 0.0320746; 
  t0_offset_pos[151]= 0; 
  t0_offset_pos[152]= 0.0519333; 
  t0_offset_pos[153]= 0.0180218; 
  t0_offset_pos[154]= -0.00799901; 
  t0_offset_pos[155]= 0.0136856; 
  t0_offset_pos[156]= 0.0583056; 
  t0_offset_pos[157]= -0.00075084; 
  t0_offset_pos[158]= -0.011085; 
  t0_offset_pos[159]= 0.0287493; 
  t0_offset_pos[160]= 0.0166329; 
  t0_offset_pos[161]= 0.13575; 
  t0_offset_pos[162]= 0.015369; 
  t0_offset_pos[163]= 4.27054; 
  t0_offset_pos[164]= 0.0752872; 
  t0_offset_pos[165]= 0.614979; 
  t0_offset_pos[166]= 0.0662697; 
  t0_offset_pos[167]= 0.0658874; 
  t0_offset_pos[168]= -0.00694692; 
  t0_offset_pos[169]= 0.000108533; 
  t0_offset_pos[170]= 0.0151218; 
  t0_offset_pos[171]= 0.0608349; 
  t0_offset_pos[172]= 0.0295363; 
  t0_offset_pos[173]= 0.00234624; 
  t0_offset_pos[174]= 0.0583044; 
  t0_offset_pos[175]= 0.0561778; 
  t0_offset_pos[176]= 0.0309986; 
  t0_offset_pos[177]= 0.0509221; 
  t0_offset_pos[178]= 0.0316189; 
  t0_offset_pos[179]= -0.0144848; 
  t0_offset_pos[180]= -0.005967; 
  t0_offset_pos[181]= 0.0612169; 
  t0_offset_pos[182]= 0.0356497; 
  t0_offset_pos[183]= 0.0276387; 
  t0_offset_pos[184]= 0.00914944; 
  t0_offset_pos[185]= 0.053852; 
  t0_offset_pos[186]= 0.0326473; 
  t0_offset_pos[187]= 0.0486587; 
  t0_offset_pos[188]= 0.014781; 
  t0_offset_pos[189]= -0.0106255; 
  t0_offset_pos[190]= -0.0201865; 
  t0_offset_pos[191]= 0.0161116; 
  t0_offset_pos[192]= 0.0512866; 
  t0_offset_pos[193]= 0.0812154; 
  t0_offset_pos[194]= 0.107883; 
  t0_offset_pos[195]= 0.0915534; 
  t0_offset_pos[196]= -0.0288578; 
  t0_offset_pos[197]= -0.0208005; 
  t0_offset_pos[198]= 0.0189647; 
  t0_offset_pos[199]= 0.0423883; 
  t0_offset_pos[200]= 0.113889; 
  t0_offset_pos[201]= 0.159007; 
  t0_offset_pos[202]= 0.117223; 
  t0_offset_pos[203]= 0.22834; 
  t0_offset_pos[204]= 0.107134; 
  t0_offset_pos[205]= 0.148597; 
  t0_offset_pos[206]= 0.0779793; 
  t0_offset_pos[207]= 0.0552059; 
  t0_offset_pos[208]= 0.0834392; 
  t0_offset_pos[209]= 0.0198872; 
  t0_offset_pos[210]= 0.0355365; 
  t0_offset_pos[211]= -0.0232847; 
  t0_offset_pos[212]= 0.0952515; 
  t0_offset_pos[213]= 0.0961298; 
  t0_offset_pos[214]= 0; 
  t0_offset_pos[215]= 0.0455989; 
  t0_offset_pos[216]= 0.103014; 
  t0_offset_pos[217]= 0.198763; 
  t0_offset_pos[218]= 0.291548; 
  t0_offset_pos[219]= 0.102218; 
  t0_offset_pos[220]= 0.0449288; 
  t0_offset_pos[221]= 0.00331925; 
  t0_offset_pos[222]= 0.0110179; 
  t0_offset_pos[223]= 0.0819665; 
  t0_offset_pos[224]= 0.0721132; 
  t0_offset_pos[225]= 0.0176823; 
  t0_offset_pos[226]= 0.105387; 
  t0_offset_pos[227]= 0.0584094; 
  t0_offset_pos[228]= 0.0408671; 
  t0_offset_pos[229]= 0.0691361; 
  t0_offset_pos[230]= 0.0914713; 
  t0_offset_pos[231]= 0.0674032; 
  t0_offset_pos[232]= 0.0359993; 
  t0_offset_pos[233]= 0.0190858; 
  t0_offset_pos[234]= 0.0435599; 
  t0_offset_pos[235]= 0.0242781; 
  t0_offset_pos[236]= 0.0941687; 
  t0_offset_pos[237]= 0.0625041; 
  t0_offset_pos[238]= 0.166061; 
  t0_offset_pos[239]= 0.130616; 
  t0_offset_pos[240]= 0.0501871; 
  t0_offset_pos[241]= 0.0340767; 
  t0_offset_pos[242]= 0.0451448; 
  t0_offset_pos[243]= 0.0419763; 
  t0_offset_pos[244]= 0.0445891; 
  t0_offset_pos[245]= 0.0359986; 
  t0_offset_pos[246]= 0.0469406; 
  t0_offset_pos[247]= 0.0607298; 
  t0_offset_pos[248]= 1.53436; 
  t0_offset_pos[249]= 2.284; 
  t0_offset_pos[250]= -1.57061; 
  t0_offset_pos[251]= -1.20839; 
  t0_offset_pos[252]= -0.950469; 
  t0_offset_pos[253]= -1.88275; 
  t0_offset_pos[254]= -2.07345; 
  t0_offset_pos[255]= -0.807353; 
  t0_offset_pos[256]= -0.039415; 
  t0_offset_pos[257]= 0.016584; 
  t0_offset_pos[258]= -0.0199945; 
  t0_offset_pos[259]= -0.0525731; 
  t0_offset_pos[260]= 0.0256319; 
  t0_offset_pos[261]= 0.000364566; 
  t0_offset_pos[262]= -0.0488114; 
  t0_offset_pos[263]= 0.0205697; 
  t0_offset_pos[264]= 0.0410198; 
  t0_offset_pos[265]= -0.0860787; 
  t0_offset_pos[266]= 0.0189595; 
  t0_offset_pos[267]= -0.0126911; 
  t0_offset_pos[268]= 0.141659; 
  t0_offset_pos[269]= 0.0445789; 
  t0_offset_pos[270]= 0.0765691; 
  t0_offset_pos[271]= 0.061119; 
  t0_offset_pos[272]= 0.00613935; 
  t0_offset_pos[273]= 0.0297272; 
  t0_offset_pos[274]= -0.0290907; 
  t0_offset_pos[275]= -0.0169413; 
  t0_offset_pos[276]= 0.015685; 
  t0_offset_pos[277]= -0.0211236; 
  t0_offset_pos[278]= 0.0299116; 
  t0_offset_pos[279]= 0.0251034; 
  t0_offset_pos[280]= -0.0137262; 
  t0_offset_pos[281]= -0.00611969; 
  t0_offset_pos[282]= 0.0607834; 
  t0_offset_pos[283]= 0.00710032; 
  t0_offset_pos[284]= 0.00000; 
  t0_offset_pos[285]= 0.0482852; 
  t0_offset_pos[286]= 0.0373742; 
  t0_offset_pos[287]= 0.048155; 
  t0_offset_pos[288]= 0.0774659; 
  t0_offset_pos[289]= 0.196144; 
  t0_offset_pos[290]= 0.175562; 
  t0_offset_pos[291]= 0.00000; 
  t0_offset_pos[292]= 0.0810659; 
  t0_offset_pos[293]= 0.00000; 
  t0_offset_pos[294]= 0.128478; 
  t0_offset_pos[295]= 0.136728; 
  t0_offset_pos[296]= 0.101106; 
  t0_offset_pos[297]= 0.129543; 
  t0_offset_pos[298]= 0.171855; 
  t0_offset_pos[299]= 0.164024; 
  t0_offset_pos[300]= 0.0960998; 
  t0_offset_pos[301]= 0.168636; 
  t0_offset_pos[302]= 0.18736; 
  t0_offset_pos[303]= 0.0946729; 
  t0_offset_pos[304]= 1.38651; 
  t0_offset_pos[305]= 1.82032; 
  t0_offset_pos[306]= 0.249289; 
  t0_offset_pos[307]= 2.03869; 
  t0_offset_pos[308]= -1.09813; 
  t0_offset_pos[309]= -2.77081; 
  t0_offset_pos[310]= -2.64402; 
  t0_offset_pos[311]= -2.4958; 
  t0_offset_pos[312]= 0.212311; 
  t0_offset_pos[313]= 0.161278; 
  t0_offset_pos[314]= 0.0875769; 
  t0_offset_pos[315]= 0.119434; 
  t0_offset_pos[316]= 0.0660616; 
  t0_offset_pos[317]= 0.356395; 
  t0_offset_pos[318]= 0.181305; 
  t0_offset_pos[319]= 0.198138; 
  t0_offset_pos[320]= -8.93452e-05; 
  t0_offset_pos[321]= 0.0349623; 
  t0_offset_pos[322]= 0.0927469; 
  t0_offset_pos[323]= 0.0191236; 
  t0_offset_pos[324]= 0.0292391; 
  t0_offset_pos[325]= 0.0629082; 
  t0_offset_pos[326]= -0.00866894; 
  t0_offset_pos[327]= 0.0502192; 
  t0_offset_pos[328]= 0.169804; 
  t0_offset_pos[329]= 0.145083; 
  t0_offset_pos[330]= 0.127928; 
  t0_offset_pos[331]= 0.248705; 
  t0_offset_pos[332]= 0.0312046; 
  t0_offset_pos[333]= 0.200418; 
  t0_offset_pos[334]= 0.101842; 
  t0_offset_pos[335]= 2.85267; 
  t0_offset_pos[336]= 0.0661593; 
  t0_offset_pos[337]= 0.095097; 
  t0_offset_pos[338]= 0.0679776; 
  t0_offset_pos[339]= 0; 
  t0_offset_pos[340]= -0.00225738; 
  t0_offset_pos[341]= 0.0225122; 
  t0_offset_pos[342]= 0.0504538; 
  t0_offset_pos[343]= 0.0907912; 
  t0_offset_pos[344]= 0.103972; 
  t0_offset_pos[345]= 0.060928; 
  t0_offset_pos[346]= 0.114562; 
  t0_offset_pos[347]= 0.314617; 
  t0_offset_pos[348]= 0.0774254; 
  t0_offset_pos[349]= 0.180959; 
  t0_offset_pos[350]= 0.0514109; 
  t0_offset_pos[351]= -0.0171981; 
  t0_offset_pos[352]= 0.118461; 
  t0_offset_pos[353]= 0.0458456; 
  t0_offset_pos[354]= 1.335; 
  t0_offset_pos[355]= 0.0338974; 
  t0_offset_pos[356]= 0.0378717; 
  t0_offset_pos[357]= -0.0538847; 
  t0_offset_pos[358]= 0.0178856; 
  t0_offset_pos[359]= 0.291521; 
  t0_offset_pos[360]= 0.126757; 
  t0_offset_pos[361]= 0.0765081; 
  t0_offset_pos[362]= 0; 
  t0_offset_pos[363]= 0.107665; 
  t0_offset_pos[364]= 0.026952; 
  t0_offset_pos[365]= 0.084699; 
  t0_offset_pos[366]= 0.00878777; 
  t0_offset_pos[367]= -0.073909; 
  t0_offset_pos[368]= 0.0724446; 
  t0_offset_pos[369]= 0.0560566; 
  t0_offset_pos[370]= 0.0875224; 
  t0_offset_pos[371]= 0.0872163; 
  t0_offset_pos[372]= 0; 
  t0_offset_pos[373]= 0.0581932; 
  t0_offset_pos[374]= 0; 
  t0_offset_pos[375]= 0.105054; 
  t0_offset_pos[376]= 0.0550361; 
  t0_offset_pos[377]= 0.033371; 
  t0_offset_pos[378]= 0.0112813; 
  t0_offset_pos[379]= -0.0503206; 
  t0_offset_pos[380]= 0.124408; 
  t0_offset_pos[381]= 0.028889; 
  t0_offset_pos[382]= 0.0898824; 
  t0_offset_pos[383]= 0.167862; 
  t0_offset_pos[384]= 0.19384; 
  t0_offset_pos[385]= 0; 
  t0_offset_pos[386]= 0.22079; 
  t0_offset_pos[387]= 0.206107; 
  t0_offset_pos[388]= 0; 
  t0_offset_pos[389]= 0.328918; 
  t0_offset_pos[390]= 0.237189; 
  t0_offset_pos[391]= 0.188614; 
  t0_offset_pos[392]= 0.236294; 
  t0_offset_pos[393]= 0.216113; 
  t0_offset_pos[394]= 0.256754; 
  t0_offset_pos[395]= 0.199833; 
  t0_offset_pos[396]= 0.286295; 
  t0_offset_pos[397]= 0.197613; 
  t0_offset_pos[398]= 0.181907; 
  t0_offset_pos[399]= 0.237008; 
  t0_offset_pos[400]= 0.334898; 
  t0_offset_pos[401]= 0; 
  t0_offset_pos[402]= 0.124251; 
  t0_offset_pos[403]= 0.342219; 
  t0_offset_pos[404]= 0.216392; 
  t0_offset_pos[405]= 0.405925; 
  t0_offset_pos[406]= 0.249533; 
  t0_offset_pos[407]= 0.243236; 
  t0_offset_pos[408]= 0.208007; 
  t0_offset_pos[409]= 0.116478; 
  t0_offset_pos[410]= 0.215133; 
  t0_offset_pos[411]= 0.199299; 
  t0_offset_pos[412]= 0.196702; 
  t0_offset_pos[413]= 0.196153; 
  t0_offset_pos[414]= 0.156213; 
  t0_offset_pos[415]= 0.153872; 
  t0_offset_pos[416]= 0.274623; 
  t0_offset_pos[417]= 0.269502; 
  t0_offset_pos[418]= 0.279477; 
  t0_offset_pos[419]= 0.23936; 
  t0_offset_pos[420]= 0.131211; 
  t0_offset_pos[421]= 0.268778; 
  t0_offset_pos[422]= 0.205048; 
  t0_offset_pos[423]= 0.238002; 
  t0_offset_pos[424]= 0.118024; 
  t0_offset_pos[425]= 0.251083; 
  t0_offset_pos[426]= 0.141046; 
  t0_offset_pos[427]= 0.183179; 
  t0_offset_pos[428]= 0.182277; 
  t0_offset_pos[429]= 0.229805; 
  t0_offset_pos[430]= 0.232598; 
  t0_offset_pos[431]= 0.206046; 
  t0_offset_pos[432]= 0.239932; 
  t0_offset_pos[433]= -0.00706128; 
  t0_offset_pos[434]= 0.124584; 
  t0_offset_pos[435]= 0.269778; 
  t0_offset_pos[436]= 0.175794; 
  t0_offset_pos[437]= 0.142975; 
  t0_offset_pos[438]= 0.0765216; 
  t0_offset_pos[439]= 0.139588; 
  t0_offset_pos[440]= 0.122909; 
  t0_offset_pos[441]= 0.156313; 
  t0_offset_pos[442]= 0.146191; 
  t0_offset_pos[443]= 0.17052; 
  t0_offset_pos[444]= 0.197895; 
  t0_offset_pos[445]= 0.132305; 
  t0_offset_pos[446]= 0.136073; 
  t0_offset_pos[447]= 0.105101; 
  t0_offset_pos[448]= 0.129099; 
  t0_offset_pos[449]= 0.17066; 
  t0_offset_pos[450]= 0.156831; 
  t0_offset_pos[451]= 0.140512; 
  t0_offset_pos[452]= 0.0899657; 
  t0_offset_pos[453]= 0.113442; 
  t0_offset_pos[454]= 0.106251; 
  t0_offset_pos[455]= 0.182033; 
  t0_offset_pos[456]= 0.0419671; 
  t0_offset_pos[457]= 1.71639; 
  t0_offset_pos[458]= 0.0367288; 
  t0_offset_pos[459]= 0.101109; 
  t0_offset_pos[460]= 0.0141198; 
  t0_offset_pos[461]= 0.0250168; 
  t0_offset_pos[462]= -0.0511718; 
  t0_offset_pos[463]= 0.0493415; 
  t0_offset_pos[464]= 0.157217; 
  t0_offset_pos[465]= 0; 
  t0_offset_pos[466]= 0.129255; 
  t0_offset_pos[467]= 0.12301; 
  t0_offset_pos[468]= 0.103011; 
  t0_offset_pos[469]= 0.292576; 
  t0_offset_pos[470]= 0.113382; 
  t0_offset_pos[471]= 0.106406; 
  t0_offset_pos[472]= 0.225642; 
  t0_offset_pos[473]= 0.065485; 
  t0_offset_pos[474]= 0.117275; 
  t0_offset_pos[475]= 0.104256; 
  t0_offset_pos[476]= 0.0417537; 
  t0_offset_pos[477]= 0.0612808; 
  t0_offset_pos[478]= 0.147455; 
  t0_offset_pos[479]= 0.0791631; 
  t0_offset_pos[480]= 0.258471; 
  t0_offset_pos[481]= 0.137304; 
  t0_offset_pos[482]= 0.161272; 
  t0_offset_pos[483]= 0.367223; 
  t0_offset_pos[484]= 0.00000; 
  t0_offset_pos[485]= 0.140285; 
  t0_offset_pos[486]= 0.220273; 
  t0_offset_pos[487]= 0.0909579; 
  t0_offset_pos[488]= 0.171937; 
  t0_offset_pos[489]= 0.157677; 
  t0_offset_pos[490]= 0.109509; 
  t0_offset_pos[491]= 0.153339; 
  t0_offset_pos[492]= 0.167644; 
  t0_offset_pos[493]= 0.133968; 
  t0_offset_pos[494]= 0.122136; 
  t0_offset_pos[495]= 0.15076; 
  t0_offset_pos[496]= 0.405483; 
  t0_offset_pos[497]= 0.267015; 
  t0_offset_pos[498]= 0.186462; 
  t0_offset_pos[499]= 0.229823; 
  t0_offset_pos[500]= 0.264882; 
  t0_offset_pos[501]= 0; 
  t0_offset_pos[502]= 0.194708; 
  t0_offset_pos[503]= 0.2619; 
  t0_offset_pos[504]= 0.114908; 
  t0_offset_pos[505]= 0.112379; 
  t0_offset_pos[506]= 0.162323; 
  t0_offset_pos[507]= 0.0889292; 
  t0_offset_pos[508]= 0.15486; 
  t0_offset_pos[509]= 0.218; 
  t0_offset_pos[510]= 0.14686; 
  t0_offset_pos[511]= 0.148321; 
  t0_offset_pos[512]= 0.0965183; 
  t0_offset_pos[513]= 0.134008; 
  t0_offset_pos[514]= 0.129341; 
  t0_offset_pos[515]= 0.118121; 
  t0_offset_pos[516]= 0.097598; 
  t0_offset_pos[517]= 0.135499; 
  t0_offset_pos[518]= 0.132113; 
  t0_offset_pos[519]= 0.130496; 
  t0_offset_pos[520]= 0.148632; 
  t0_offset_pos[521]= 0.133837; 
  t0_offset_pos[522]= 0.111906; 
  t0_offset_pos[523]= 0.111103; 
  t0_offset_pos[524]= 0.135514; 
  t0_offset_pos[525]= 0.204175; 
  t0_offset_pos[526]= 0.215744; 
  t0_offset_pos[527]= 0.154252; 
  t0_offset_pos[528]= 0; 
  t0_offset_pos[529]= 0.165858; 
  t0_offset_pos[530]= 0.165996; 
  t0_offset_pos[531]= 0.170963; 
  t0_offset_pos[532]= 0.125911; 
  t0_offset_pos[533]= 0.106519; 
  t0_offset_pos[534]= 0.0304274; 
  t0_offset_pos[535]= 0.161529; 
  t0_offset_pos[536]= 0.189166; 
  t0_offset_pos[537]= 0.222963; 
  t0_offset_pos[538]= 0.189701; 
  t0_offset_pos[539]= 0.13879; 
  t0_offset_pos[540]= 0.155135; 
  t0_offset_pos[541]= 0.179323; 
  t0_offset_pos[542]= 0.143865; 
  t0_offset_pos[543]= 0.188218; 
  t0_offset_pos[544]= 0.108648; 
  t0_offset_pos[545]= 0.109636; 
  t0_offset_pos[546]= 0.121308; 
  t0_offset_pos[547]= 0.080965; 
  t0_offset_pos[548]= 0.101083; 
  t0_offset_pos[549]= 0.133183; 
  t0_offset_pos[550]= 0.173907; 
  t0_offset_pos[551]= 0.151574; 
  t0_offset_pos[552]= 0.140526; 
  t0_offset_pos[553]= 0.151423; 
  t0_offset_pos[554]= 0.125271; 
  t0_offset_pos[555]= 0.146108; 
  t0_offset_pos[556]= 0.132278; 
  t0_offset_pos[557]= 0.134329; 
  t0_offset_pos[558]= 0.161348; 
  t0_offset_pos[559]= 0.151749; 
  t0_offset_pos[560]= 0.108038; 
  t0_offset_pos[561]= 0.0937513; 
  t0_offset_pos[562]= 0.13038; 
  t0_offset_pos[563]= 0.101261; 
  t0_offset_pos[564]= 0.125578; 
  t0_offset_pos[565]= 0.327714; 
  t0_offset_pos[566]= 0.0861963; 
  t0_offset_pos[567]= 0.0973786; 
  t0_offset_pos[568]= 0.15763; 
  t0_offset_pos[569]= 0.172415; 
  t0_offset_pos[570]= 0.139193; 
  t0_offset_pos[571]= 0.172353; 
  t0_offset_pos[572]= 0.0917635; 
  t0_offset_pos[573]= 0.0656631; 
  t0_offset_pos[574]= 0.157408; 
  t0_offset_pos[575]= 0.125282; 
  t0_offset_pos[576]= 0.205437; 
  t0_offset_pos[577]= 0.177907; 
  t0_offset_pos[578]= 0.196059; 
  t0_offset_pos[579]= 0.17096; 
  t0_offset_pos[580]= 0.172443; 
  t0_offset_pos[581]= 0.323022; 
  t0_offset_pos[582]= 0.12797; 
  t0_offset_pos[583]= 0.139447; 
  t0_offset_pos[584]= 0.145741; 
  t0_offset_pos[585]= 0.133669; 
  t0_offset_pos[586]= 0.157767; 
  t0_offset_pos[587]= 0.0735984; 
  t0_offset_pos[588]= 0.171996; 
  t0_offset_pos[589]= 0.113851; 
  t0_offset_pos[590]= 0.114438; 
  t0_offset_pos[591]= 0.0945942; 
  t0_offset_pos[592]= 0.174921; 
  t0_offset_pos[593]= 0.236402; 
  t0_offset_pos[594]= 0.134646; 
  t0_offset_pos[595]= 0.102844; 
  t0_offset_pos[596]= 0.19477; 
  t0_offset_pos[597]= 0.203483; 
  t0_offset_pos[598]= 0.191004; 
  t0_offset_pos[599]= 0.201356; 
  t0_offset_pos[600]= 0.112492; 
  t0_offset_pos[601]= 0.124026; 
  t0_offset_pos[602]= 0; 
  t0_offset_pos[603]= 0.207723; 
  t0_offset_pos[604]= 0.163817; 
  t0_offset_pos[605]= 0.172364; 
  t0_offset_pos[606]= 0.120018; 
  t0_offset_pos[607]= 0.19711; 
  t0_offset_pos[608]= 0.174589; 
  t0_offset_pos[609]= 0.178757; 
  t0_offset_pos[610]= 0.112145; 
  t0_offset_pos[611]= 0.1821; 
  t0_offset_pos[612]= 0.200128; 
  t0_offset_pos[613]= 0.12856; 
  t0_offset_pos[614]= 0.145027; 
  t0_offset_pos[615]= 0.119099; 
  t0_offset_pos[616]= 0.122709; 
  t0_offset_pos[617]= 0.100769; 
  t0_offset_pos[618]= 0.139298; 
  t0_offset_pos[619]= 0.11041; 
  t0_offset_pos[620]= 0.125336; 
  t0_offset_pos[621]= 0.118545; 
  t0_offset_pos[622]= 0.149128; 
  t0_offset_pos[623]= 0.148341; 
  t0_offset_pos[624]= 0.0629895; 
  t0_offset_pos[625]= 0.11782; 
  t0_offset_pos[626]= 0.0864454; 
  t0_offset_pos[627]= 0.103888; 
  t0_offset_pos[628]= 0; 
  t0_offset_pos[629]= 0.135557; 
  t0_offset_pos[630]= 0.11938; 
  t0_offset_pos[631]= 0.114846; 
  t0_offset_pos[632]= 0.0840339; 
  t0_offset_pos[633]= 0.160333; 
  t0_offset_pos[634]= 0.131122; 
  t0_offset_pos[635]= 0.13116; 
  t0_offset_pos[636]= 0.146072; 
  t0_offset_pos[637]= 0.171202; 
  t0_offset_pos[638]= 0.101803; 
  t0_offset_pos[639]= 0.206762; 
  t0_offset_pos[640]= 0.16752; 
  t0_offset_pos[641]= 0.102697; 
  t0_offset_pos[642]= 0.100054; 
  t0_offset_pos[643]= 0.145844; 
  t0_offset_pos[644]= 0.150081; 
  t0_offset_pos[645]= 0.116933; 
  t0_offset_pos[646]= 0.172735; 
  t0_offset_pos[647]= 0.116796; 
  t0_offset_pos[648]= 0.183107; 
  t0_offset_pos[649]= 0.217599; 
  t0_offset_pos[650]= 0.193189; 
  t0_offset_pos[651]= 0.234397; 
  t0_offset_pos[652]= 0.169302; 
  t0_offset_pos[653]= 0.181793; 
  t0_offset_pos[654]= 0.257962; 
  t0_offset_pos[655]= 0.212916; 
  t0_offset_pos[656]= 0.139155; 
  t0_offset_pos[657]= 0.18268; 
  t0_offset_pos[658]= 0.0853628; 
  t0_offset_pos[659]= 0.140557; 
  t0_offset_pos[660]= 0.030607; 
  t0_offset_pos[661]= 0.0945224; 
  t0_offset_pos[662]= 0.0670999; 
  t0_offset_pos[663]= 0.0846366; 
  t0_offset_pos[664]= 0.134417; 
  t0_offset_pos[665]= 0.167964; 
  t0_offset_pos[666]= 0.133501; 
  t0_offset_pos[667]= 0.0792982; 
  t0_offset_pos[668]= 0.0980506; 
  t0_offset_pos[669]= 0.139602; 
  t0_offset_pos[670]= 0.0552783; 
  t0_offset_pos[671]= 0.12658; 
  t0_offset_pos[672]= 0.221862; 
  t0_offset_pos[673]= 0.18904; 
  t0_offset_pos[674]= 0.258433; 
  t0_offset_pos[675]= 0.18265; 
  t0_offset_pos[676]= 0.105289; 
  t0_offset_pos[677]= 0.10351; 
  t0_offset_pos[678]= 0.133242; 
  t0_offset_pos[679]= 0.0851738; 
  t0_offset_pos[680]= 0.129584; 
  t0_offset_pos[681]= 0; 
  t0_offset_pos[682]= 0.195616; 
  t0_offset_pos[683]= 0.440505; 
  t0_offset_pos[684]= -0.0742037; 
  t0_offset_pos[685]= 1.05658; 
  t0_offset_pos[686]= 0.25329; 
  t0_offset_pos[687]= -5.34744; 
  t0_offset_pos[688]= 0.0946887; 
  t0_offset_pos[689]= 0.131494; 
  t0_offset_pos[690]= 0.127026; 
  t0_offset_pos[691]= 0.148544; 
  t0_offset_pos[692]= 0.130653; 
  t0_offset_pos[693]= 0.140219; 
  t0_offset_pos[694]= 0.149788; 
  t0_offset_pos[695]= 0.17955; 
  t0_offset_pos[696]= 0.0873086; 
  t0_offset_pos[697]= 0.160023; 
  t0_offset_pos[698]= 0.0826252; 
  t0_offset_pos[699]= 0.0880649; 
  t0_offset_pos[700]= 0.22621; 
  t0_offset_pos[701]= 0.242965; 
  t0_offset_pos[702]= 0.232883; 
  t0_offset_pos[703]= -2.43492; 
  t0_offset_pos[704]= 0.144437; 
  t0_offset_pos[705]= 0.0625203; 
  t0_offset_pos[706]= 0.13202; 
  t0_offset_pos[707]= 0.176184; 
  t0_offset_pos[708]= 0.111349; 
  t0_offset_pos[709]= 0.116798; 
  t0_offset_pos[710]= 0.136334; 
  t0_offset_pos[711]= 0.175339; 
  t0_offset_pos[712]= 0.17457; 
  t0_offset_pos[713]= 0.138358; 
  t0_offset_pos[714]= 0.135733; 
  t0_offset_pos[715]= 0.453354; 
  t0_offset_pos[716]= 0.281286; 
  t0_offset_pos[717]= 0.3623; 
  t0_offset_pos[718]= 0.262104; 
  t0_offset_pos[719]= 0.366709; 
  t0_offset_pos[720]= 0.119621; 
  t0_offset_pos[721]= 0.152709; 
  t0_offset_pos[722]= 0.144317; 
  t0_offset_pos[723]= 0.0637079; 
  t0_offset_pos[724]= 0.219741; 
  t0_offset_pos[725]= 0.156311; 
  t0_offset_pos[726]= 0.196017; 
  t0_offset_pos[727]= 0.163437; 
  t0_offset_pos[728]= -0.601267; 
  t0_offset_pos[729]= -0.922887; 
  t0_offset_pos[730]= -0.656076; 
  t0_offset_pos[731]= 0.376086; 
  t0_offset_pos[732]= 1.78716; 
  t0_offset_pos[733]= -0.0643161; 
  t0_offset_pos[734]= -0.165354; 
  t0_offset_pos[735]= 0.522719; 
  t0_offset_pos[736]= 0.125103; 
  t0_offset_pos[737]= 0.188729; 
  t0_offset_pos[738]= 0.174251; 
  t0_offset_pos[739]= 0; 
  t0_offset_pos[740]= 0.103363; 
  t0_offset_pos[741]= 0.144968; 
  t0_offset_pos[742]= 0.117278; 
  t0_offset_pos[743]= 0.0738371; 
  t0_offset_pos[744]= 0.0979109; 
  t0_offset_pos[745]= 0.181768; 
  t0_offset_pos[746]= 0.141113; 
  t0_offset_pos[747]= 0.127918; 
  t0_offset_pos[748]= 0.186046; 
  t0_offset_pos[749]= 0.42822; 
  t0_offset_pos[750]= 0.789373; 
  t0_offset_pos[751]= -0.0317346; 
  t0_offset_pos[752]= 0.178777; 
  t0_offset_pos[753]= 0.134838; 
  t0_offset_pos[754]= 0.121064; 
  t0_offset_pos[755]= 0.141242; 
  t0_offset_pos[756]= 0.0942179; 
  t0_offset_pos[757]= -0.033037; 
  t0_offset_pos[758]= 0.092923; 
  t0_offset_pos[759]= 0.0966526; 
  t0_offset_pos[760]= 0.107136; 
  t0_offset_pos[761]= 0.0960795; 
  t0_offset_pos[762]= 0.0996791; 
  t0_offset_pos[763]= 0.203678; 
  t0_offset_pos[764]= 0.350057; 
  t0_offset_pos[765]= 0.0206813; 
  t0_offset_pos[766]= 0.287737; 

  t0_offset_neg[0]= -0.202928; 
  t0_offset_neg[1]= -0.811575; 
  t0_offset_neg[2]= 0.886129; 
  t0_offset_neg[3]= 0.0347071; 
  t0_offset_neg[4]= -0.0311838; 
  t0_offset_neg[5]= -0.0349496; 
  t0_offset_neg[6]= -0.00265704; 
  t0_offset_neg[7]= 0.121622; 
  t0_offset_neg[8]= -0.0503178; 
  t0_offset_neg[9]= 0.0614185; 
  t0_offset_neg[10]= 0.0328687; 
  t0_offset_neg[11]= 0.0646635; 
  t0_offset_neg[12]= -0.000564671; 
  t0_offset_neg[13]= 0.0956252; 
  t0_offset_neg[14]= 0.0377338; 
  t0_offset_neg[15]= 0.038623; 
  t0_offset_neg[16]= 0.180942; 
  t0_offset_neg[17]= 0.276989; 
  t0_offset_neg[18]= -0.106406; 
  t0_offset_neg[19]= 0.290974; 
  t0_offset_neg[20]= 0.187499; 
  t0_offset_neg[21]= 0.0278179; 
  t0_offset_neg[22]= 0.0465124; 
  t0_offset_neg[23]= -0.0473125; 
  t0_offset_neg[24]= 0.0298973; 
  t0_offset_neg[25]= 0.0252138; 
  t0_offset_neg[26]= -0.0574447; 
  t0_offset_neg[27]= 0.0742266; 
  t0_offset_neg[28]= 0.0494756; 
  t0_offset_neg[29]= 0.0961233; 
  t0_offset_neg[30]= 0.0921587; 
  t0_offset_neg[31]= 0.061537; 
  t0_offset_neg[32]= 0.239231; 
  t0_offset_neg[33]= 0.163387; 
  t0_offset_neg[34]= 0.073389; 
  t0_offset_neg[35]= 0.00337342; 
  t0_offset_neg[36]= 0.0367242; 
  t0_offset_neg[37]= 0.0793017; 
  t0_offset_neg[38]= 0.0416129; 
  t0_offset_neg[39]= 0.0584683; 
  t0_offset_neg[40]= 0.0642055; 
  t0_offset_neg[41]= 0.0694436; 
  t0_offset_neg[42]= 0.0628971; 
  t0_offset_neg[43]= 0.0984387; 
  t0_offset_neg[44]= 0.0450359; 
  t0_offset_neg[45]= 0.017874; 
  t0_offset_neg[46]= -0.0339291; 
  t0_offset_neg[47]= -0.0406138; 
  t0_offset_neg[48]= 0.000000; 
  t0_offset_neg[49]= 0.212546; 
  t0_offset_neg[50]= 0.000000; 
  t0_offset_neg[51]= 0.595537; 
  t0_offset_neg[52]= 0.177425; 
  t0_offset_neg[53]= 0.150665; 
  t0_offset_neg[54]= 0.0570314; 
  t0_offset_neg[55]= 0.128909; 
  t0_offset_neg[56]= 0.00945602; 
  t0_offset_neg[57]= 0.00618476; 
  t0_offset_neg[58]= 0.00679892; 
  t0_offset_neg[59]= -0.00417819; 
  t0_offset_neg[60]= 0.0203144; 
  t0_offset_neg[61]= -0.0516636; 
  t0_offset_neg[62]= 0.039654; 
  t0_offset_neg[63]= 0.00765709; 
  t0_offset_neg[64]= 0.489069; 
  t0_offset_neg[65]= 0.136572; 
  t0_offset_neg[66]= 0.132044; 
  t0_offset_neg[67]= 0.0847031; 
  t0_offset_neg[68]= 0.0250563; 
  t0_offset_neg[69]= 0.0523176; 
  t0_offset_neg[70]= 0.0791836; 
  t0_offset_neg[71]= -0.0174344; 
  t0_offset_neg[72]= 0.00562187; 
  t0_offset_neg[73]= -0.0041315; 
  t0_offset_neg[74]= 0.0531684; 
  t0_offset_neg[75]= 0.0598797; 
  t0_offset_neg[76]= 0.0192582; 
  t0_offset_neg[77]= 0.0236982; 
  t0_offset_neg[78]= 0.0218592; 
  t0_offset_neg[79]= 0.0341971; 
  t0_offset_neg[80]= -0.070331; 
  t0_offset_neg[81]= 0.048657; 
  t0_offset_neg[82]= 0.223857; 
  t0_offset_neg[83]= -0.0227982; 
  t0_offset_neg[84]= 0.0822309; 
  t0_offset_neg[85]= 0.000000; 
  t0_offset_neg[86]= 0.0292149; 
  t0_offset_neg[87]= 0.0482535; 
  t0_offset_neg[88]= 0.0614651; 
  t0_offset_neg[89]= 0.0624097; 
  t0_offset_neg[90]= 0.0072042; 
  t0_offset_neg[91]= 0.0760596; 
  t0_offset_neg[92]= 0.0332517; 
  t0_offset_neg[93]= 0.00944582; 
  t0_offset_neg[94]= 0.0464102; 
  t0_offset_neg[95]= 0.0838335; 
  t0_offset_neg[96]= -0.0138837; 
  t0_offset_neg[97]= 0.0704751; 
  t0_offset_neg[98]= 0.0232053; 
  t0_offset_neg[99]= 0.0175303; 
  t0_offset_neg[100]= 0.013971; 
  t0_offset_neg[101]= 0.0270657; 
  t0_offset_neg[102]= 0.0317573; 
  t0_offset_neg[103]= 0.0596805; 
  t0_offset_neg[104]= 0.108416; 
  t0_offset_neg[105]= 0.129089; 
  t0_offset_neg[106]= 0.129499; 
  t0_offset_neg[107]= 0.0656352; 
  t0_offset_neg[108]= 0.0659442; 
  t0_offset_neg[109]= 0.0615; 
  t0_offset_neg[110]= 0.0142521; 
  t0_offset_neg[111]= -0.00593942; 
  t0_offset_neg[112]= 0.00515515; 
  t0_offset_neg[113]= 0.0817671; 
  t0_offset_neg[114]= 0.048156; 
  t0_offset_neg[115]= 0.0379763; 
  t0_offset_neg[116]= 0.0976084; 
  t0_offset_neg[117]= 0.0597883; 
  t0_offset_neg[118]= 0.0534323; 
  t0_offset_neg[119]= 0.049075; 
  t0_offset_neg[120]= 0.0410981; 
  t0_offset_neg[121]= 0.0319972; 
  t0_offset_neg[122]= 0.0625196; 
  t0_offset_neg[123]= 0.0640375; 
  t0_offset_neg[124]= 0.0334681; 
  t0_offset_neg[125]= 0.0776886; 
  t0_offset_neg[126]= 0.0452348; 
  t0_offset_neg[127]= 0.0707938; 
  t0_offset_neg[128]= 0.0302609; 
  t0_offset_neg[129]= 0.0529925; 
  t0_offset_neg[130]= 0.0822578; 
  t0_offset_neg[131]= 0.0647996; 
  t0_offset_neg[132]= 0.0389658; 
  t0_offset_neg[133]= 0.045598; 
  t0_offset_neg[134]= 0.000386834; 
  t0_offset_neg[135]= 0.0308016; 
  t0_offset_neg[136]= 0.444167; 
  t0_offset_neg[137]= 0.467953; 
  t0_offset_neg[138]= 0.481674; 
  t0_offset_neg[139]= 0.433899; 
  t0_offset_neg[140]= 0.404531; 
  t0_offset_neg[141]= 0.448424; 
  t0_offset_neg[142]= 0.000000; 
  t0_offset_neg[143]= 0.430177; 
  t0_offset_neg[144]= 0.049916; 
  t0_offset_neg[145]= 0.0243982; 
  t0_offset_neg[146]= 0.0723879; 
  t0_offset_neg[147]= 0.0391771; 
  t0_offset_neg[148]= 0.0297879; 
  t0_offset_neg[149]= 0.0610252; 
  t0_offset_neg[150]= 0.0536976; 
  t0_offset_neg[151]= 0.000000; 
  t0_offset_neg[152]= 0.0892276; 
  t0_offset_neg[153]= 0.0202479; 
  t0_offset_neg[154]= 0.0205138; 
  t0_offset_neg[155]= 0.0352846; 
  t0_offset_neg[156]= 0.0489477; 
  t0_offset_neg[157]= 7.48223e-05; 
  t0_offset_neg[158]= 0.0302789; 
  t0_offset_neg[159]= 0.0711707; 
  t0_offset_neg[160]= 0.0320986; 
  t0_offset_neg[161]= 0.127187; 
  t0_offset_neg[162]= 0.0150095; 
  t0_offset_neg[163]= 3.85501; 
  t0_offset_neg[164]= 0.105116; 
  t0_offset_neg[165]= 0.000000; 
  t0_offset_neg[166]= 0.114435; 
  t0_offset_neg[167]= 0.118381; 
  t0_offset_neg[168]= 0.0130336; 
  t0_offset_neg[169]= 0.0501052; 
  t0_offset_neg[170]= 0.0224068; 
  t0_offset_neg[171]= 0.0645116; 
  t0_offset_neg[172]= 0.0948275; 
  t0_offset_neg[173]= 0.0534794; 
  t0_offset_neg[174]= 0.139855; 
  t0_offset_neg[175]= 0.0650896; 
  t0_offset_neg[176]= 0.298252; 
  t0_offset_neg[177]= 0.0828797; 
  t0_offset_neg[178]= 0.0809423; 
  t0_offset_neg[179]= 0.0328991; 
  t0_offset_neg[180]= 0.0825153; 
  t0_offset_neg[181]= 0.0550936; 
  t0_offset_neg[182]= 0.0885027; 
  t0_offset_neg[183]= 0.031252; 
  t0_offset_neg[184]= 0.0272126; 
  t0_offset_neg[185]= 0.0837757; 
  t0_offset_neg[186]= 0.0453006; 
  t0_offset_neg[187]= 0.0856504; 
  t0_offset_neg[188]= 0.0562686; 
  t0_offset_neg[189]= -0.0511361; 
  t0_offset_neg[190]= 0.0119286; 
  t0_offset_neg[191]= 0.0134702; 
  t0_offset_neg[192]= 0.0800134; 
  t0_offset_neg[193]= 0.0736173; 
  t0_offset_neg[194]= 0.113212; 
  t0_offset_neg[195]= 0.0926071; 
  t0_offset_neg[196]= 0.0071175; 
  t0_offset_neg[197]= -0.0326795; 
  t0_offset_neg[198]= -0.0146094; 
  t0_offset_neg[199]= 0.0153688; 
  t0_offset_neg[200]= 0.0914377; 
  t0_offset_neg[201]= 0.124822; 
  t0_offset_neg[202]= 0.0957761; 
  t0_offset_neg[203]= 0.0718948; 
  t0_offset_neg[204]= 0.0966812; 
  t0_offset_neg[205]= 0.146769; 
  t0_offset_neg[206]= 0.0527705; 
  t0_offset_neg[207]= -0.00960812; 
  t0_offset_neg[208]= 0.0792642; 
  t0_offset_neg[209]= 0.0424959; 
  t0_offset_neg[210]= 0.0535214; 
  t0_offset_neg[211]= -0.00782394; 
  t0_offset_neg[212]= 0.0618317; 
  t0_offset_neg[213]= 0.111574; 
  t0_offset_neg[214]= 0.000000; 
  t0_offset_neg[215]= 0.0427934; 
  t0_offset_neg[216]= 0.0900895; 
  t0_offset_neg[217]= 0.203593; 
  t0_offset_neg[218]= 0.147798; 
  t0_offset_neg[219]= 0.113363; 
  t0_offset_neg[220]= 0.0624755; 
  t0_offset_neg[221]= 0.000377281; 
  t0_offset_neg[222]= -0.0268307; 
  t0_offset_neg[223]= 0.0498076; 
  t0_offset_neg[224]= 0.0704996; 
  t0_offset_neg[225]= 0.0727823; 
  t0_offset_neg[226]= 0.100287; 
  t0_offset_neg[227]= 0.0662741; 
  t0_offset_neg[228]= -0.000382242; 
  t0_offset_neg[229]= 0.0556838; 
  t0_offset_neg[230]= 0.0977235; 
  t0_offset_neg[231]= 0.0991303; 
  t0_offset_neg[232]= 0.0120465; 
  t0_offset_neg[233]= 0.0370419; 
  t0_offset_neg[234]= 0.0970885; 
  t0_offset_neg[235]= 0.0454203; 
  t0_offset_neg[236]= 0.106646; 
  t0_offset_neg[237]= 0.0965206; 
  t0_offset_neg[238]= 0.169918; 
  t0_offset_neg[239]= 0.134694; 
  t0_offset_neg[240]= 0.0812943; 
  t0_offset_neg[241]= 0.0774625; 
  t0_offset_neg[242]= 0.0489486; 
  t0_offset_neg[243]= 0.0454348; 
  t0_offset_neg[244]= 0.0426563; 
  t0_offset_neg[245]= 0.0388612; 
  t0_offset_neg[246]= 0.044224; 
  t0_offset_neg[247]= 0.0732382; 
  t0_offset_neg[248]= 1.52846; 
  t0_offset_neg[249]= 2.33311; 
  t0_offset_neg[250]= -1.61158; 
  t0_offset_neg[251]= -1.21449; 
  t0_offset_neg[252]= -0.953639; 
  t0_offset_neg[253]= -1.87618; 
  t0_offset_neg[254]= -2.09496; 
  t0_offset_neg[255]= -0.809999; 
  t0_offset_neg[256]= -0.0298516; 
  t0_offset_neg[257]= 0.0955913; 
  t0_offset_neg[258]= -0.0117619; 
  t0_offset_neg[259]= -0.0377566; 
  t0_offset_neg[260]= 0.050433; 
  t0_offset_neg[261]= -0.0185227; 
  t0_offset_neg[262]= 0.0152099; 
  t0_offset_neg[263]= -0.00526205; 
  t0_offset_neg[264]= 0.0683043; 
  t0_offset_neg[265]= -0.0291712; 
  t0_offset_neg[266]= -0.039036; 
  t0_offset_neg[267]= 0.0229581; 
  t0_offset_neg[268]= 0.140139; 
  t0_offset_neg[269]= 0.110144; 
  t0_offset_neg[270]= 0.126484; 
  t0_offset_neg[271]= 0.0655974; 
  t0_offset_neg[272]= 0.109378; 
  t0_offset_neg[273]= 0.0413047; 
  t0_offset_neg[274]= 0.025138; 
  t0_offset_neg[275]= 0.0118587; 
  t0_offset_neg[276]= 0.0330141; 
  t0_offset_neg[277]= 0.0699209; 
  t0_offset_neg[278]= 0.0652302; 
  t0_offset_neg[279]= 0.051983; 
  t0_offset_neg[280]= 0.0438144; 
  t0_offset_neg[281]= 0.00999339; 
  t0_offset_neg[282]= 0.0710074; 
  t0_offset_neg[283]= -0.0184494; 
  t0_offset_neg[284]= 0.0504162; 
  t0_offset_neg[285]= 0.0878854; 
  t0_offset_neg[286]= 0.0934095; 
  t0_offset_neg[287]= 0.11304; 
  t0_offset_neg[288]= 0.0540711; 
  t0_offset_neg[289]= 0.198849; 
  t0_offset_neg[290]= 0.177584; 
  t0_offset_neg[291]= 0.0500618; 
  t0_offset_neg[292]= 0.0775438; 
  t0_offset_neg[293]= 0.122497; 
  t0_offset_neg[294]= 0.126008; 
  t0_offset_neg[295]= 0.03266; 
  t0_offset_neg[296]= 0.0935628; 
  t0_offset_neg[297]= 0.09328; 
  t0_offset_neg[298]= 0.152748; 
  t0_offset_neg[299]= 0.130623; 
  t0_offset_neg[300]= 0.123114; 
  t0_offset_neg[301]= 0.131318; 
  t0_offset_neg[302]= 0.177974; 
  t0_offset_neg[303]= -0.00189004; 
  t0_offset_neg[304]= 1.3153; 
  t0_offset_neg[305]= 1.76138; 
  t0_offset_neg[306]= 0.234797; 
  t0_offset_neg[307]= 2.01731; 
  t0_offset_neg[308]= -1.08397; 
  t0_offset_neg[309]= -2.78349; 
  t0_offset_neg[310]= -2.64653; 
  t0_offset_neg[311]= -2.43529; 
  t0_offset_neg[312]= 0.155939; 
  t0_offset_neg[313]= 0.166204; 
  t0_offset_neg[314]= 0.0842554; 
  t0_offset_neg[315]= 0.126275; 
  t0_offset_neg[316]= 0.0643897; 
  t0_offset_neg[317]= 0.166196; 
  t0_offset_neg[318]= 0.0757328; 
  t0_offset_neg[319]= 0.067929; 
  t0_offset_neg[320]= 0.00722969; 
  t0_offset_neg[321]= 0.00997948; 
  t0_offset_neg[322]= 0.0584226; 
  t0_offset_neg[323]= 0.0352013; 
  t0_offset_neg[324]= 0.051345; 
  t0_offset_neg[325]= 0.0497869; 
  t0_offset_neg[326]= 0.0380693; 
  t0_offset_neg[327]= 0.0545637; 
  t0_offset_neg[328]= 0.14161; 
  t0_offset_neg[329]= 0.100087; 
  t0_offset_neg[330]= 0.13091; 
  t0_offset_neg[331]= 0.11171; 
  t0_offset_neg[332]= 0.0429793; 
  t0_offset_neg[333]= 0.104006; 
  t0_offset_neg[334]= 0.0920228; 
  t0_offset_neg[335]= 2.78651; 
  t0_offset_neg[336]= 0.0761137; 
  t0_offset_neg[337]= 0.104521; 
  t0_offset_neg[338]= 0.0863308; 
  t0_offset_neg[339]= 0.000000; 
  t0_offset_neg[340]= 0.0288114; 
  t0_offset_neg[341]= 0.0734203; 
  t0_offset_neg[342]= 0.0709742; 
  t0_offset_neg[343]= 0.0359359; 
  t0_offset_neg[344]= 0.181893; 
  t0_offset_neg[345]= 0.0989208; 
  t0_offset_neg[346]= 0.115713; 
  t0_offset_neg[347]= 0.129377; 
  t0_offset_neg[348]= 0.123701; 
  t0_offset_neg[349]= 0.0194561; 
  t0_offset_neg[350]= 0.0502232; 
  t0_offset_neg[351]= 0.0528991; 
  t0_offset_neg[352]= 0.114235; 
  t0_offset_neg[353]= 0.135503; 
  t0_offset_neg[354]= 0.000000; 
  t0_offset_neg[355]= 0.09384; 
  t0_offset_neg[356]= 0.0525297; 
  t0_offset_neg[357]= 0.0330653; 
  t0_offset_neg[358]= 0.0741238; 
  t0_offset_neg[359]= 0.0685894; 
  t0_offset_neg[360]= 0.17146; 
  t0_offset_neg[361]= 0.0817918; 
  t0_offset_neg[362]= 0.000000; 
  t0_offset_neg[363]= 0.197217; 
  t0_offset_neg[364]= 0.0161045; 
  t0_offset_neg[365]= 0.105705; 
  t0_offset_neg[366]= 0.0322; 
  t0_offset_neg[367]= 0.137682; 
  t0_offset_neg[368]= 0.129435; 
  t0_offset_neg[369]= 0.143098; 
  t0_offset_neg[370]= 0.149295; 
  t0_offset_neg[371]= 0.150342; 
  t0_offset_neg[372]= 0.165527; 
  t0_offset_neg[373]= 0.0955986; 
  t0_offset_neg[374]= 0.168702; 
  t0_offset_neg[375]= 0.133713; 
  t0_offset_neg[376]= 0.0775043; 
  t0_offset_neg[377]= 0.0451195; 
  t0_offset_neg[378]= 0.0386124; 
  t0_offset_neg[379]= -0.0390743; 
  t0_offset_neg[380]= 0.170997; 
  t0_offset_neg[381]= 0.0745578; 
  t0_offset_neg[382]= 0.0952902; 
  t0_offset_neg[383]= 0.138065; 
  t0_offset_neg[384]= 0.197797; 
  t0_offset_neg[385]= 0.174464; 
  t0_offset_neg[386]= 0.20066; 
  t0_offset_neg[387]= 0.225246; 
  t0_offset_neg[388]= 0.440614; 
  t0_offset_neg[389]= 0.255576; 
  t0_offset_neg[390]= 0.222783; 
  t0_offset_neg[391]= 0.207276; 
  t0_offset_neg[392]= 0.194503; 
  t0_offset_neg[393]= 0.205526; 
  t0_offset_neg[394]= 0.186376; 
  t0_offset_neg[395]= 0.199347; 
  t0_offset_neg[396]= 0.285651; 
  t0_offset_neg[397]= 0.163751; 
  t0_offset_neg[398]= 0.198404; 
  t0_offset_neg[399]= 0.225206; 
  t0_offset_neg[400]= 0.181101; 
  t0_offset_neg[401]= 0.000000; 
  t0_offset_neg[402]= 0.135775; 
  t0_offset_neg[403]= 0.229835; 
  t0_offset_neg[404]= 0.162487; 
  t0_offset_neg[405]= 0.183891; 
  t0_offset_neg[406]= 0.174869; 
  t0_offset_neg[407]= 0.244235; 
  t0_offset_neg[408]= 0.156131; 
  t0_offset_neg[409]= 0.170656; 
  t0_offset_neg[410]= 0.152361; 
  t0_offset_neg[411]= 0.178968; 
  t0_offset_neg[412]= 0.141527; 
  t0_offset_neg[413]= 0.197662; 
  t0_offset_neg[414]= 0.13542; 
  t0_offset_neg[415]= 0.127343; 
  t0_offset_neg[416]= 0.216005; 
  t0_offset_neg[417]= 0.257393; 
  t0_offset_neg[418]= 0.332076; 
  t0_offset_neg[419]= 0.233681; 
  t0_offset_neg[420]= 0.139904; 
  t0_offset_neg[421]= 0.224403; 
  t0_offset_neg[422]= 0.244464; 
  t0_offset_neg[423]= 0.272487; 
  t0_offset_neg[424]= 0.163827; 
  t0_offset_neg[425]= 0.183614; 
  t0_offset_neg[426]= 0.161385; 
  t0_offset_neg[427]= 0.205845; 
  t0_offset_neg[428]= 0.16425; 
  t0_offset_neg[429]= 0.215822; 
  t0_offset_neg[430]= 0.186112; 
  t0_offset_neg[431]= 0.224884; 
  t0_offset_neg[432]= 0.150822; 
  t0_offset_neg[433]= 0.227031; 
  t0_offset_neg[434]= 0.139082; 
  t0_offset_neg[435]= 0.196223; 
  t0_offset_neg[436]= 0.126958; 
  t0_offset_neg[437]= 0.119419; 
  t0_offset_neg[438]= 0.0922568; 
  t0_offset_neg[439]= 0.136867; 
  t0_offset_neg[440]= 0.155716; 
  t0_offset_neg[441]= 0.146383; 
  t0_offset_neg[442]= 0.167491; 
  t0_offset_neg[443]= 0.149587; 
  t0_offset_neg[444]= 0.170351; 
  t0_offset_neg[445]= 0.138623; 
  t0_offset_neg[446]= 0.159941; 
  t0_offset_neg[447]= 0.141728; 
  t0_offset_neg[448]= 0.134596; 
  t0_offset_neg[449]= 0.211257; 
  t0_offset_neg[450]= 0.200738; 
  t0_offset_neg[451]= 0.17062; 
  t0_offset_neg[452]= 0.217028; 
  t0_offset_neg[453]= 0.143572; 
  t0_offset_neg[454]= 0.174202; 
  t0_offset_neg[455]= 0.162525; 
  t0_offset_neg[456]= 0.0486109; 
  t0_offset_neg[457]= 4.32403; 
  t0_offset_neg[458]= 0.0660751; 
  t0_offset_neg[459]= 0.164896; 
  t0_offset_neg[460]= 0.0413495; 
  t0_offset_neg[461]= 0.0526889; 
  t0_offset_neg[462]= 0.0300265; 
  t0_offset_neg[463]= 0.0885426; 
  t0_offset_neg[464]= 0.137814; 
  t0_offset_neg[465]= 0.000000; 
  t0_offset_neg[466]= 0.165804; 
  t0_offset_neg[467]= 0.147485; 
  t0_offset_neg[468]= 0.186356; 
  t0_offset_neg[469]= 0.173032; 
  t0_offset_neg[470]= 0.258438; 
  t0_offset_neg[471]= 0.15942; 
  t0_offset_neg[472]= 0.170832; 
  t0_offset_neg[473]= 0.207609; 
  t0_offset_neg[474]= 0.146341; 
  t0_offset_neg[475]= 0.234552; 
  t0_offset_neg[476]= 0.0954332; 
  t0_offset_neg[477]= 0.187931; 
  t0_offset_neg[478]= 0.125303; 
  t0_offset_neg[479]= 0.0888329; 
  t0_offset_neg[480]= 0.146814; 
  t0_offset_neg[481]= 0.107862; 
  t0_offset_neg[482]= 0.135373; 
  t0_offset_neg[483]= 0.340784; 
  t0_offset_neg[484]= 0.128817; 
  t0_offset_neg[485]= 0.14964; 
  t0_offset_neg[486]= 0.139228; 
  t0_offset_neg[487]= 0.122284; 
  t0_offset_neg[488]= 0.162115; 
  t0_offset_neg[489]= 0.114003; 
  t0_offset_neg[490]= 0.12312; 
  t0_offset_neg[491]= 0.138377; 
  t0_offset_neg[492]= 0.14188; 
  t0_offset_neg[493]= 0.120624; 
  t0_offset_neg[494]= 0.109961; 
  t0_offset_neg[495]= 0.159142; 
  t0_offset_neg[496]= 0.206128; 
  t0_offset_neg[497]= 0.221267; 
  t0_offset_neg[498]= 0.217226; 
  t0_offset_neg[499]= 0.213737; 
  t0_offset_neg[500]= 0.250931; 
  t0_offset_neg[501]= 0.000000; 
  t0_offset_neg[502]= 0.198615; 
  t0_offset_neg[503]= 0.22422; 
  t0_offset_neg[504]= 0.143226; 
  t0_offset_neg[505]= 0.104631; 
  t0_offset_neg[506]= 0.130807; 
  t0_offset_neg[507]= 0.13175; 
  t0_offset_neg[508]= 0.158305; 
  t0_offset_neg[509]= 0.162381; 
  t0_offset_neg[510]= 0.137805; 
  t0_offset_neg[511]= 0.154847; 
  t0_offset_neg[512]= 0.0897936; 
  t0_offset_neg[513]= 0.0997495; 
  t0_offset_neg[514]= 0.125594; 
  t0_offset_neg[515]= 0.116767; 
  t0_offset_neg[516]= 0.105849; 
  t0_offset_neg[517]= 0.151285; 
  t0_offset_neg[518]= 0.111532; 
  t0_offset_neg[519]= 0.143172; 
  t0_offset_neg[520]= 0.103863; 
  t0_offset_neg[521]= 0.101266; 
  t0_offset_neg[522]= 0.102052; 
  t0_offset_neg[523]= 0.0775386; 
  t0_offset_neg[524]= 0.151501; 
  t0_offset_neg[525]= 0.189257; 
  t0_offset_neg[526]= 0.269822; 
  t0_offset_neg[527]= 0.14556; 
  t0_offset_neg[528]= 0.154748; 
  t0_offset_neg[529]= 0.147874; 
  t0_offset_neg[530]= 0.137197; 
  t0_offset_neg[531]= 0.1145; 
  t0_offset_neg[532]= 0.111296; 
  t0_offset_neg[533]= 0.105698; 
  t0_offset_neg[534]= 0.0803089; 
  t0_offset_neg[535]= 0.170522; 
  t0_offset_neg[536]= 0.2271; 
  t0_offset_neg[537]= 0.149206; 
  t0_offset_neg[538]= 0.163923; 
  t0_offset_neg[539]= 0.161513; 
  t0_offset_neg[540]= 0.141341; 
  t0_offset_neg[541]= 0.192699; 
  t0_offset_neg[542]= 0.146415; 
  t0_offset_neg[543]= 0.174942; 
  t0_offset_neg[544]= 0.130923; 
  t0_offset_neg[545]= 0.127139; 
  t0_offset_neg[546]= 0.154839; 
  t0_offset_neg[547]= 0.113036; 
  t0_offset_neg[548]= 0.139953; 
  t0_offset_neg[549]= 0.142141; 
  t0_offset_neg[550]= 0.216387; 
  t0_offset_neg[551]= 0.155938; 
  t0_offset_neg[552]= 0.188111; 
  t0_offset_neg[553]= 0.154742; 
  t0_offset_neg[554]= 0.155607; 
  t0_offset_neg[555]= 0.189171; 
  t0_offset_neg[556]= 0.168493; 
  t0_offset_neg[557]= 0.153275; 
  t0_offset_neg[558]= 0.151809; 
  t0_offset_neg[559]= 0.159709; 
  t0_offset_neg[560]= 0.139791; 
  t0_offset_neg[561]= 0.144001; 
  t0_offset_neg[562]= 0.165023; 
  t0_offset_neg[563]= 0.0968922; 
  t0_offset_neg[564]= 0.164411; 
  t0_offset_neg[565]= 0.174406; 
  t0_offset_neg[566]= 0.0909885; 
  t0_offset_neg[567]= 0.125567; 
  t0_offset_neg[568]= 0.184489; 
  t0_offset_neg[569]= 0.161325; 
  t0_offset_neg[570]= 0.186782; 
  t0_offset_neg[571]= 0.145719; 
  t0_offset_neg[572]= 0.146979; 
  t0_offset_neg[573]= 0.170426; 
  t0_offset_neg[574]= 0.1441; 
  t0_offset_neg[575]= 0.107391; 
  t0_offset_neg[576]= 0.153233; 
  t0_offset_neg[577]= 0.230656; 
  t0_offset_neg[578]= 0.206819; 
  t0_offset_neg[579]= 0.134017; 
  t0_offset_neg[580]= 0.145668; 
  t0_offset_neg[581]= 0.36185; 
  t0_offset_neg[582]= 0.146155; 
  t0_offset_neg[583]= 0.137498; 
  t0_offset_neg[584]= 0.160063; 
  t0_offset_neg[585]= 0.110258; 
  t0_offset_neg[586]= 0.110923; 
  t0_offset_neg[587]= 0.0850859; 
  t0_offset_neg[588]= 0.146759; 
  t0_offset_neg[589]= 0.113472; 
  t0_offset_neg[590]= 0.109179; 
  t0_offset_neg[591]= 0.120941; 
  t0_offset_neg[592]= 0.181219; 
  t0_offset_neg[593]= 0.207981; 
  t0_offset_neg[594]= 0.13577; 
  t0_offset_neg[595]= 0.089483; 
  t0_offset_neg[596]= 0.177264; 
  t0_offset_neg[597]= 0.19939; 
  t0_offset_neg[598]= 0.177255; 
  t0_offset_neg[599]= 0.203189; 
  t0_offset_neg[600]= 0.0948019; 
  t0_offset_neg[601]= 0.140662; 
  t0_offset_neg[602]= 0.000000; 
  t0_offset_neg[603]= 0.137517; 
  t0_offset_neg[604]= 0.121543; 
  t0_offset_neg[605]= 0.19159; 
  t0_offset_neg[606]= 0.109885; 
  t0_offset_neg[607]= 0.154535; 
  t0_offset_neg[608]= 0.13026; 
  t0_offset_neg[609]= 0.163734; 
  t0_offset_neg[610]= 0.127366; 
  t0_offset_neg[611]= 0.174159; 
  t0_offset_neg[612]= 0.165994; 
  t0_offset_neg[613]= 0.163689; 
  t0_offset_neg[614]= 0.13603; 
  t0_offset_neg[615]= 0.106385; 
  t0_offset_neg[616]= 0.120683; 
  t0_offset_neg[617]= 0.128601; 
  t0_offset_neg[618]= 0.164734; 
  t0_offset_neg[619]= 0.127475; 
  t0_offset_neg[620]= 0.125899; 
  t0_offset_neg[621]= 0.132188; 
  t0_offset_neg[622]= 0.136846; 
  t0_offset_neg[623]= 0.115416; 
  t0_offset_neg[624]= 0.0456414; 
  t0_offset_neg[625]= 0.109937; 
  t0_offset_neg[626]= 0.0836865; 
  t0_offset_neg[627]= 0.121064; 
  t0_offset_neg[628]= 0.000000; 
  t0_offset_neg[629]= 0.148148; 
  t0_offset_neg[630]= 0.120145; 
  t0_offset_neg[631]= 0.113919; 
  t0_offset_neg[632]= 0.0890862; 
  t0_offset_neg[633]= 0.13338; 
  t0_offset_neg[634]= 0.141842; 
  t0_offset_neg[635]= 0.143904; 
  t0_offset_neg[636]= 0.0904604; 
  t0_offset_neg[637]= 0.161964; 
  t0_offset_neg[638]= 0.109922; 
  t0_offset_neg[639]= 0.146188; 
  t0_offset_neg[640]= 0.172268; 
  t0_offset_neg[641]= 0.0747936; 
  t0_offset_neg[642]= 0.120953; 
  t0_offset_neg[643]= 0.144263; 
  t0_offset_neg[644]= 0.136003; 
  t0_offset_neg[645]= 0.12142; 
  t0_offset_neg[646]= 0.1718; 
  t0_offset_neg[647]= 0.0985561; 
  t0_offset_neg[648]= 0.160568; 
  t0_offset_neg[649]= 0.249887; 
  t0_offset_neg[650]= 0.205039; 
  t0_offset_neg[651]= 0.197808; 
  t0_offset_neg[652]= 0.136419; 
  t0_offset_neg[653]= 0.124089; 
  t0_offset_neg[654]= 0.214353; 
  t0_offset_neg[655]= 0.195074; 
  t0_offset_neg[656]= 0.101034; 
  t0_offset_neg[657]= 0.131507; 
  t0_offset_neg[658]= 0.0884252; 
  t0_offset_neg[659]= 0.139729; 
  t0_offset_neg[660]= 0.0675273; 
  t0_offset_neg[661]= 0.106052; 
  t0_offset_neg[662]= 0.10895; 
  t0_offset_neg[663]= 0.109332; 
  t0_offset_neg[664]= 0.117165; 
  t0_offset_neg[665]= 0.156783; 
  t0_offset_neg[666]= 0.129482; 
  t0_offset_neg[667]= 0.130345; 
  t0_offset_neg[668]= 0.102657; 
  t0_offset_neg[669]= 0.123014; 
  t0_offset_neg[670]= 0.0518004; 
  t0_offset_neg[671]= 0.1488; 
  t0_offset_neg[672]= 0.229891; 
  t0_offset_neg[673]= 0.235338; 
  t0_offset_neg[674]= 0.273839; 
  t0_offset_neg[675]= 0.191854; 
  t0_offset_neg[676]= 0.152209; 
  t0_offset_neg[677]= 0.126198; 
  t0_offset_neg[678]= 0.069124; 
  t0_offset_neg[679]= 0.0761242; 
  t0_offset_neg[680]= 0.169728; 
  t0_offset_neg[681]= 0.000000; 
  t0_offset_neg[682]= 0.159941; 
  t0_offset_neg[683]= 0.219842; 
  t0_offset_neg[684]= -0.0640665; 
  t0_offset_neg[685]= -0.149739; 
  t0_offset_neg[686]= 0.152252; 
  t0_offset_neg[687]= -5.87675; 
  t0_offset_neg[688]= 0.134338; 
  t0_offset_neg[689]= 0.166003; 
  t0_offset_neg[690]= 0.127302; 
  t0_offset_neg[691]= 0.131305; 
  t0_offset_neg[692]= 0.102562; 
  t0_offset_neg[693]= 0.106377; 
  t0_offset_neg[694]= 0.168407; 
  t0_offset_neg[695]= 0.12299; 
  t0_offset_neg[696]= 0.0892638; 
  t0_offset_neg[697]= 0.154491; 
  t0_offset_neg[698]= 0.152576; 
  t0_offset_neg[699]= 0.117009; 
  t0_offset_neg[700]= 0.108599; 
  t0_offset_neg[701]= 0.230317; 
  t0_offset_neg[702]= 0.136758; 
  t0_offset_neg[703]= 0.210648; 
  t0_offset_neg[704]= 0.143603; 
  t0_offset_neg[705]= 0.103862; 
  t0_offset_neg[706]= 0.117074; 
  t0_offset_neg[707]= 0.120872; 
  t0_offset_neg[708]= 0.0852557; 
  t0_offset_neg[709]= 0.111273; 
  t0_offset_neg[710]= 0.135117; 
  t0_offset_neg[711]= 0.150463; 
  t0_offset_neg[712]= 0.170416; 
  t0_offset_neg[713]= 0.0923537; 
  t0_offset_neg[714]= 0.330354; 
  t0_offset_neg[715]= 0.124015; 
  t0_offset_neg[716]= 0.399959; 
  t0_offset_neg[717]= 0.412097; 
  t0_offset_neg[718]= 0.180251; 
  t0_offset_neg[719]= 0.293915; 
  t0_offset_neg[720]= 0.163672; 
  t0_offset_neg[721]= 0.0950251; 
  t0_offset_neg[722]= 0.128483; 
  t0_offset_neg[723]= 0.063589; 
  t0_offset_neg[724]= 0.230492; 
  t0_offset_neg[725]= 0.109687; 
  t0_offset_neg[726]= 0.183456; 
  t0_offset_neg[727]= 0.157586; 
  t0_offset_neg[728]= -0.61768; 
  t0_offset_neg[729]= -1.10247; 
  t0_offset_neg[730]= -0.597582; 
  t0_offset_neg[731]= 0.458212; 
  t0_offset_neg[732]= 0.0; 
  t0_offset_neg[733]= -0.274473; 
  t0_offset_neg[734]= -0.258899; 
  t0_offset_neg[735]= -0.0572905; 
  t0_offset_neg[736]= 0.13102; 
  t0_offset_neg[737]= 0.175371; 
  t0_offset_neg[738]= 0.131409; 
  t0_offset_neg[739]= 0.000000; 
  t0_offset_neg[740]= 0.0872651; 
  t0_offset_neg[741]= 0.126326; 
  t0_offset_neg[742]= 0.0855046; 
  t0_offset_neg[743]= 0.0684523; 
  t0_offset_neg[744]= 0.129783; 
  t0_offset_neg[745]= 0.137375; 
  t0_offset_neg[746]= 0.148527; 
  t0_offset_neg[747]= 0.350194; 
  t0_offset_neg[748]= 0.163681; 
  t0_offset_neg[749]= 0.314643; 
  t0_offset_neg[750]= 0.384989; 
  t0_offset_neg[751]= 0.547813; 
  t0_offset_neg[752]= 0.162278; 
  t0_offset_neg[753]= 0.106944; 
  t0_offset_neg[754]= 0.105568; 
  t0_offset_neg[755]= 0.147995; 
  t0_offset_neg[756]= 0.0563629; 
  t0_offset_neg[757]= 0.0659149; 
  t0_offset_neg[758]= 0.0664489; 
  t0_offset_neg[759]= 0.0746442; 
  t0_offset_neg[760]= 0.145583; 
  t0_offset_neg[761]= 0.102123; 
  t0_offset_neg[762]= 0.05786; 
  t0_offset_neg[763]= 0.205534; 
  t0_offset_neg[764]= -2.27813; 
  t0_offset_neg[765]= -0.0145329; 
  t0_offset_neg[766]= 0.296451; 

  int badcounter_pos=0;
  int badcounter_neg=0;
  for(int i=0; i<NOffsets; i++)
    {
      //check okay offsets
      if( fabs(t0_offset_pos[i])>4 )
	{ 
	  badcounter_pos++;
	  if (verbosity) 
	    {
	      cout << "[" << i << "] :" << t0_offset_pos[i] << "  " << badcounter_pos << endl;
	    }
	  t0_offset_pos[i]=0.;
	}
      if( fabs(t0_offset_neg[i])>4 )
	{ 
	  badcounter_neg++;
	  if (verbosity) 
	    {
	      cout << "[" << i << "] :" << t0_offset_neg[i] << "  " << badcounter_neg << endl;
	    }
	  t0_offset_neg[i]=0.;
	}

      //set average
      t0_offset[i] = (t0_offset_pos[i] + t0_offset_neg[i]) / 2.;
    }
  return;
}
