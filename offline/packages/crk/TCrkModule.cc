#include <PHIODataNode.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_const_cgs.h>

// PRDF
#include <Event.h>
#include <packetConstants.h>
typedef PHDataNode<Event> EventNode_t;

// Wrapper classes
// Data classes for RICH
#include <crkghitWrapper.h>
#include <dCrkRawWrapper.h>
#include <dCrkFEMWrapper.h>
#include <dCrkDCMWrapper.h>
#include <dCrkHitWrapper.h>
#include <dCrkPidWrapper.h>

// calibration, geometry classes
#include <dCrkGeoWrapper.h>
#include <dCrkCalWrapper.h>
#include <dCrkUcalWrapper.h>

// class for calibration database access
#include <PdbRichADC.hh>
#include <PdbRichPar.hh>
#include <CrkCal.h>

// DAO --- address mapping
#include <CrkDAO.h>

// class for Ghit<-->Raw relations
#include <dCrkRel2sWrapper.h>

// parameter classes
#include <dCrkGhitRawParWrapper.h>
#include <dCrkRawFEMparWrapper.h>
#include <dCrkDCMparWrapper.h>
#include <dCrkRawHitParWrapper.h>
#include <dCrkProjPidParWrapper.h>

// Header of this class itself
#include <TCrkModule.h>

// Headers of other Crk classes
#include <getClass.h>
#include <TCrkProjector.h>

// other
#include <pidcode.h>
#include <RunToTime.hh>

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace std;

// Fixed parameters for RICH geometry
#define N_CRK_PMT    5120
#define NCELL_SECT   1280
#define NPMT_FEM      320 /* # of ch in one FEM */
#define NPMT_FEM2     160 /* # of ch in 1/2 FEM...read-out unit */
#define NPMT_AMUCARD2  32 /* # of ch in 1/2 AMUADC card */
#define NCARD_FEM       5 /* # of AMUADC card in one FEM */
#define NPHI_FEM2      10 /* # of phi rows in 1/2 FEM */
#define NCPHI          80 /* # of rows in one sector */
#define NCPMT          32 /* # of PMTs in one SM */     
#define NCPMT_2        16 /* # of PMTs in one row */

#define FEM_PRE_OFFSET  0
#define FEM_POST_OFFSET 1
#define FEM_TAC_OFFSET  2

#define DCM_PRE_OFFSET  0
#define DCM_POST_OFFSET 1
#define DCM_TAC_OFFSET  2

static const float degree = M_PI / 180.0;  /* radian per degree */
static const float c_light = GSL_CONST_CGS_SPEED_OF_LIGHT/1.e9; /* cm / nsec */
static gsl_rng *rng = 0;

//===================================================
// Forward declarations of helper static functions
//===================================================
//
// Geometry functions
//
static bool pmt2cell(int pmt, int& sector, int& iz, int& iphi);
static void pmt2fem(int pmt, int *pfem, int *paddr);
static int  fem2pmt(int fem, int addr);
static void set_default_geom(DCRKGEO_ST *geo);
static void pmt_axis(DCRKGEO_ST *geo, int pmt, float uax[]);
static void pmt_position(DCRKGEO_ST *geo, int pmt, float x[]);

//
// helper functions for ghitRaw
//
static float calc_sinT(DCRKGEO_ST *geo, CRKGHIT_ST *h);
static void add_hit(DCRKGHITRAWPAR_ST *par, DCRKGEO_ST *geo, DCRKUCAL_ST *ucal,
		    DCRKRAW_ST *raw, DCRKRAW_ST **raw_end_p, CRKGHIT_ST *ghit,
		    map<int,int> &MapRawParent);
static void add_noise(DCRKGHITRAWPAR_ST *par,DCRKUCAL_ST *ucal, DCRKRAW_ST *raw,
		      DCRKRAW_ST **raw_end_p, map<int,int> &MapRawParent);
static void fill_ghitraw(dCrkRel2sWrapper *rel_wrap,
			 map<int,int> &MapRawParent);
//
//  helper functions for rawFem
//
static void make_empty_fem(DCRKFEM_ST *fem, int fem_id, int mplex);
static void fem_add_hit(DCRKFEM_ST* fem, int addr, int adc, int tdc);
static void fem_set_parity(DCRKFEM_ST* fem);
//
//  helper function for femDcm
//
typedef unsigned short ushort;
typedef unsigned int uint;
static void make_dcm(DCRKDCM_ST *dcm, const DCRKDCMPAR_ST *par,
		     const DCRKFEM_ST *fem);
static uint *dcm_add_data(const DCRKDCMPAR_ST *par, uint *pdata, int ich,
			  ushort tac, ushort pre, ushort post);
//
//  helper function for dcmRaw
//
static void add_dcm(DCRKDCM_ST *dcm, int threshold, vector<DCRKRAW_ST> &vraw);

//
// helper function for prdfRaw()
//
static void fill_raw_from_prdf(Event *event, CrkDAO *dao, dCrkRawWrapper *raw);

//
//  helper struct (functor) for RawHit
//
struct raw2hit {
  raw2hit(DCRKCAL_ST*, float);
  DCRKHIT_ST operator()(DCRKRAW_ST &);
  DCRKCAL_ST *cal;        // pointer to calibration data
  float min_p;            // minimum photo-electron (parameter)
};

/*
 * Structures used for optical/geometrical calculation
 */
struct Ray {  /* for ray (light ray or straight track ) */
  float x[3];
  float u[3];
  void operator=(const CrkProj &p)
  {
    for (int i = 0; i < 3; i++)
      { 
	x[i] = p.x[i]; u[i] = p.u[i];
      }
  }
};

struct Rotm;  /* Rotation matrix. Not implemented */

struct Sphere {    /* Sphere */
  float r;          /* radius */
  float phi_min;    /* min value of phi angle */
  float phi_max;    /* max value of phi angle */
  float theta_min;  /* min value of theta angle */
  float theta_max;  /* max value of theta angle */
  float center[3];  /* center position */
  Rotm *rotm;     /* rotation matrix (for future expansion) */
};

struct Tubs {    /* Tubs... a phi section of tube or cylinder */
  float r;          /* radius */
  float dz;         /* half length along axis */
  float phi_min;    /* min value of phi angle */
  float phi_max;    /* max value of phi angle */
  float center[3];  /* center position */
  Rotm *rotm;       /* rotation matrix (for future expansion) */
};

/* Parameters for analysis */
struct RingPar {
  float Rmin;
  float R0;
  float Rmax;
  float Rmax2;
};

class proj2pid {
public:
  proj2pid(DCRKGEO_ST *geom, DCRKPROJPIDPAR_ST *par, 
	   DCRKHIT_ST *hit, int nhit, vector<DCRKPID_ST> *vpid);
  void operator()(CrkProj &proj);
private:
  DCRKHIT_ST *d_hit;          /* RICH hit data */
  int      d_nhit;         /* # of data in d_hit */
  /* geometry */
  DCRKGEO_ST *d_geo;
  Sphere   d_mirror[4];    /* 4 mirrors of RICH */
  Tubs     d_sector[4];    /* 4 PMT arrays of RICH*/
  /* analysis parameters */
  RingPar  d_par;          /* Ring parameter */
  int      d_in_accept;    /* In_accept is required (1) or not (0) */
  int      d_min_npmt;     /* # of PMT hits to generate PID table */
  /* results is stored here */
  vector<DCRKPID_ST> *d_vpid;

  void set_ring_par(DCRKPROJPIDPAR_ST *par);
  void test_proj(CrkProj& proj, DCRKPID_ST *result);
  void setup_geometry(DCRKGEO_ST *geo);

  int cross_sphere(Ray *r, Sphere *s, float cross[]);
  int cross_tubs(Ray *r, Tubs *t, float cross[]);

  int reflect_by_mirror(Ray *r, Sphere *m, Ray *r_ref);
  float pmt_ray_distance(int pmt, Ray *r, float dv[]);
};

//
// comparison operators (used by "sort" algorithm)
//
static bool operator<(const DCRKRAW_ST& raw1, const DCRKRAW_ST& raw2)
    { return (raw1.pmt < raw2.pmt);}

/******************************************************************
 * CERN-like interface for some GSL routines
 ******************************************************************/
inline float 
rndm(void) 
{
  // Initialize a random number generator.
  if (rng == 0)
    {
      rng = gsl_rng_alloc(gsl_rng_mt19937);
    }
  return (float)gsl_rng_uniform_pos(rng);
}

inline float 
norran(void) 
{
  // Initialize a random number generator.
  if (rng == 0)
    {
      rng = gsl_rng_alloc(gsl_rng_mt19937);
    }
  return (float)gsl_ran_ugaussian(rng);
}

//=========================
// member functions
//========================
TCrkModule::TCrkModule()
{
}

PHBoolean
TCrkModule::rawPrdf(PHCompositeNode *top) {
  //empty for now.
  return True;
}

PHBoolean
TCrkModule::setGeo(PHCompositeNode *top) {
  dCrkGeoWrapper *geo_wrap = findNode::getClass<dCrkGeoWrapper>(top,"dCrkGeo");
  if(geo_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkGeo" << endl;
      return False;
    }
  if(geo_wrap->MaxRowCount() < 1) geo_wrap->SetMaxRowCount(1);
  geo_wrap->SetRowCount(1);

  set_default_geom(geo_wrap->TableData());
  return True;
}

PHBoolean
TCrkModule::setUcal(PHCompositeNode *top) {
  DCRKUCAL_ST default_ucal = {
    0,      /* pmt */
    80.0,   /* gain  (adc gain. adc counts/photo-electron)*/
    10.0,   /* ped   (adc pedestal)*/
    10.0,   /* clock (tdc clock...tdc counts/nsec)*/
    100.,   /* t0    (tdc offset) */
    0.0,    /* slew  (slewing parameter) */
    150.0,  /* N0p   (photo-cathode N0 value (per cm) */
    0.0,    /* P_noise    (noise hit probability) */
    10.0,   /* mean_noise (exponential slope of noise distribution)*/
    0.3,    /* sigma_pe   (one photo-electron resolution) */
    0.25    /* sigma_t    (time resolution for one photon in nsec) */
  };

  dCrkUcalWrapper *ucal_wrap = findNode::getClass<dCrkUcalWrapper>(top,"dCrkUcal");
  if(ucal_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkUcal" << endl;
      return False;
    }
  if(ucal_wrap->MaxRowCount()<N_CRK_PMT) ucal_wrap->SetMaxRowCount(N_CRK_PMT);
  DCRKUCAL_ST *ucal = ucal_wrap->TableData();
  //
  // If Ucal table is empty, fill it with the default value. Otherwise,
  // it is filled with the first element of the table.
  //
  DCRKUCAL_ST fill_value;
  if(ucal_wrap->RowCount() == 0) fill_value = default_ucal;
  else fill_value = ucal[0];

//
// Revised code to pass INSURE
//   by T. Sakaguchi on Jan. 04, 2006
//
//  fill_n(ucal,N_CRK_PMT,fill_value);
//
  for(int i = 0;i<N_CRK_PMT; i++) ucal[i] = fill_value;

  ucal_wrap->SetRowCount(N_CRK_PMT);
  return True;
}

void TCrkModule::showCal(PHCompositeNode *top) {
  dCrkCalWrapper *cal_wrap = findNode::getClass<dCrkCalWrapper>(top,"dCrkCal");
  DCRKCAL_ST *cal = cal_wrap->TableData();
  cout << " RowCount = " << cal_wrap->RowCount() <<endl;
  for(unsigned int i=0;i<cal_wrap->RowCount();i++) {
    cout << cal->pmt <<": ";
    cout << cal->adc_gain <<" ";
    cout << cal->adc_ped <<" ";
    cout << cal->tdc_clock <<" ";
    cout << cal->tdc_t0 <<" ";
    cout << cal->slew <<endl;

    cal++;
    if(i%20 == 19) {
      char ans;
      cout << "Continue (Y/N) ";
      cin >> ans;
      if(!(ans == 'Y' || ans == 'y')) break;
    }
  }
}

static PHBoolean
set_DCRKCAL_ST(PHCompositeNode *top,
	    CrkCal<PdbRichADC> &adcphe_cal, CrkCal<PdbRichADC> &adcped_cal,
            CrkCal<PdbRichPar> &t0_cal,
	    CrkCal<PdbRichPar> &tac_cal, CrkCal<PdbRichPar> &slew_cal) {

  PdbRichPar t0, tac, slew;
  PdbRichADC adcphe;
  PdbRichADC adcped;

  // get table to store calibration data
  dCrkCalWrapper *cal_wrap = findNode::getClass<dCrkCalWrapper>(top,"dCrkCal");
  if(cal_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkCal" << endl;
      return False;
    }
  if(cal_wrap->MaxRowCount()<N_CRK_PMT) cal_wrap->SetMaxRowCount(N_CRK_PMT);
  DCRKCAL_ST *cal = cal_wrap->TableData();

  for(int i=0;i<N_CRK_PMT;i++) {
    int PMT2, PMT3, PMT4;
    int PMT1ped,PMT1phe;
    float pedphe, pedwphe, PEphe, PEwphe, Npedphe, N1phe, N2phe, N3phe, chisqrphe;
    float pedped, pedwped, PEped, PEwped, Npedped, N1ped, N2ped, N3ped, chisqrped;
    float t0v, tacv, slewv;

    adcphe_cal.getval(i,adcphe);
    adcped_cal.getval(i,adcped);
    t0_cal.getval(i,t0);
    tac_cal.getval(i,tac);
    slew_cal.getval(i,slew);
    
    adcphe.getValues(PMT1phe,pedphe,pedwphe,PEphe,PEwphe,Npedphe,N1phe,N2phe,N3phe,chisqrphe);
    adcped.getValues(PMT1ped,pedped,pedwped,PEped,PEwped,Npedped,N1ped,N2ped,N3ped,chisqrped);

    t0.getValues(PMT2,t0v);
    tac.getValues(PMT3,tacv);
    slew.getValues(PMT4,slewv);

    cal->pmt = i;
    if(PEphe == 0 || PEphe < pedped) { // the channel is disabled
      cal->adc_gain = 0.0;
    } else {
      cal->adc_gain  = PEphe - pedped;
    }
    cal->adc_ped   = pedped;
    cal->tdc_clock = 1.0/tacv;
    cal->tdc_t0    = t0v;
    cal->slew      = slewv;
    cal++;
  }
  cal_wrap->SetRowCount(N_CRK_PMT);
  return True;
}

PHBoolean
TCrkModule::setCal(PHCompositeNode *top) {
  PHTimeStamp time;
  time.set(2000,5,2,0,0,0);
  setCal(top,time);
  return True;
}

PHBoolean
TCrkModule::setCal(PHCompositeNode *top, PHTimeStamp time) {
  CrkCal<PdbRichADC> adcphe_cal("adcphe");
  CrkCal<PdbRichADC> adcped_cal("adcped");
  CrkCal<PdbRichPar> t0_cal("T0");
  CrkCal<PdbRichPar> tac_cal("TAC_GAIN");
  CrkCal<PdbRichPar> slew_cal("SLEW");

  // Read calibration data from file
  adcphe_cal.fetch_DB(time);
  adcped_cal.fetch_DB(time);
  t0_cal.fetch_DB(time);
  tac_cal.fetch_DB(time);
  slew_cal.fetch_DB(time);

  // set calibration table
  return set_DCRKCAL_ST(top, adcphe_cal,adcped_cal, t0_cal,tac_cal,slew_cal);
}

PHBoolean
TCrkModule::setCal(PHCompositeNode *top, int runNumber)
{
  PHTimeStamp TimeStamp;
  //.. this part is based on PRECO real data reconstruction ..
  //  auto_ptr<RunToTime> runTime(new RunToTimePg);
  RunToTime *runTime = RunToTime::instance();
  auto_ptr<PHTimeStamp> ts(runTime->getBeginTime(runNumber)) ;

  if (ts.get() != 0)
    {
      TimeStamp = *ts ;
    }
  else
    {
      cerr << "ERROR: Can NOT get Time Stamp !!!" << endl;
      exit(1);
    }

  setCal(top, TimeStamp);
  return True;
}


PHBoolean
TCrkModule::setCal(PHCompositeNode *top,
		   const char *adcphefile, const char *adcpedfile, const char *t0file, const char *tacfile, const char *slewfile){
  //  DCRKCAL_ST default_cal = {
  //  0,      /* PMT channel ID */
  //  30.0,   /* adc_gain */
  //  10.0,   /* adc_ped */
  //  10.0,   /* tdc_clock */
  //  100.,   /* tdc_t0 */
  //  0.0     /* slew */
  //};
  CrkCal<PdbRichADC> adcphe_cal("adcphe");
  CrkCal<PdbRichADC> adcped_cal("adcped");
  CrkCal<PdbRichPar> t0_cal("T0");
  CrkCal<PdbRichPar> tac_cal("TAC_GAIN");
  CrkCal<PdbRichPar> slew_cal("SLEW");

  // Read calibration data from file
  adcphe_cal.read_file(adcphefile);
  adcped_cal.read_file(adcpedfile);
  t0_cal.read_file(t0file);
  tac_cal.read_file(tacfile);
  slew_cal.read_file(slewfile);

  // set calibration table
  return set_DCRKCAL_ST(top, adcphe_cal, adcped_cal,t0_cal,tac_cal,slew_cal);
}


PHBoolean
TCrkModule::ghitRaw(PHCompositeNode *top) {
// get the data tables required.
//     CRKGHIT_ST(in)  DSRKGHITRAWPAR_ST(in) DCRKGEO_ST(in) DCRKUCAL_ST(in)
//     DCRKRAW_ST(out) DCRKREL2S_ST(out)
//

  dCrkGhitRawParWrapper *par_wrap =
    findNode::getClass<dCrkGhitRawParWrapper>(top,"dCrkGhitRawPar");
  if(par_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkGhitRawPar" << endl;
      return False;
    }
  DCRKGHITRAWPAR_ST *par = par_wrap->TableData();

  dCrkGeoWrapper *geo_wrap = findNode::getClass<dCrkGeoWrapper>(top,"dCrkGeo");
  if(geo_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkGeo" << endl;
      return False;
    }
  DCRKGEO_ST *geo = geo_wrap->TableData();

  dCrkUcalWrapper *ucal_wrap = findNode::getClass<dCrkUcalWrapper>(top,"dCrkUcal");
  if(ucal_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkUcal" << endl;
      return False;
    }
  DCRKUCAL_ST *ucal = ucal_wrap->TableData();

  dCrkRel2sWrapper *rel_wrap = findNode::getClass<dCrkRel2sWrapper>(top,"dCrkRel2s");
  if(rel_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkRel2s" << endl;
      return False;
    }

  crkghitWrapper *ghit_wrap = findNode::getClass<crkghitWrapper>(top,"crkghit");
  if(ghit_wrap == NULL)
    {
      cout << PHWHERE << "Did not find crkghit" << endl;
      return False;
    }
  CRKGHIT_ST *ghit = ghit_wrap->TableData();
  unsigned int     nghit = ghit_wrap->RowCount();

  dCrkRawWrapper *raw_wrap = findNode::getClass<dCrkRawWrapper>(top,"dCrkRaw");
  if(raw_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find dCrkRaw" << endl;
      return False;
    }
  if(raw_wrap->MaxRowCount() < nghit) {
    cout << "Raw table too small. Allocate enough space"<<endl;
    raw_wrap->SetMaxRowCount(nghit);
  }
  DCRKRAW_ST       *raw   = raw_wrap->TableData();
//
// convert GHIT to Raw format
//
  map<int,int> MapRawParent;
  DCRKRAW_ST *raw_end = raw;
  // cout << "nghit = " << nghit <<endl; //debug  (excess output line, use iDebug check, CFM 5/29/2005)
  for(unsigned int i=0; i< nghit; i++)
    add_hit(par,geo,ucal,raw,&raw_end,ghit+i,MapRawParent);

  raw_wrap->SetRowCount(raw_end - raw);

  add_noise(par,ucal,raw,&raw_end,MapRawParent);

  sort(raw,raw_end);

  fill_ghitraw(rel_wrap,MapRawParent);

  return True;
}


PHBoolean
TCrkModule::rawFem(PHCompositeNode *top) {
  
  // This uses the default input and output nodes for backward compatibility

  return rawFem(top,"dCrkRaw","dCrkFEM");
}

PHBoolean
TCrkModule::rawFem(PHCompositeNode *top,const char *CrkRawNodeName,const char *CrkFEMNodeName) {
//
// get the required data tables
//
  dCrkRawWrapper *raw_wrap = findNode::getClass<dCrkRawWrapper>(top,CrkRawNodeName);
  if(raw_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find " << CrkRawNodeName << endl;
      return False;
    }
  DCRKRAW_ST *raw = raw_wrap->TableData();

  dCrkFEMWrapper *fem_wrap = findNode::getClass<dCrkFEMWrapper>(top,CrkFEMNodeName);
  if(fem_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find " << CrkFEMNodeName << endl;
      return False;
    }
  DCRKFEM_ST *fem = fem_wrap->TableData();

  dCrkRawFEMparWrapper *par_wrap =
    findNode::getClass<dCrkRawFEMparWrapper>(top,"dCrkRawFEMpar");
  if(par_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find dCrkRawFEMpar" << endl;
      return False;
    }
  DCRKRAWFEMPAR_ST *par = par_wrap->TableData();

//   FEM data is fixed length, stores NPMT_FEM2 (=160) channels of RICH data
//   
//   one FEM data block corresponds to 1/2 FEM (upper or lower). They are
//   read out in paralell, and thus one  FEM produces 2 FEM data blocks.
//   There are 16 FEM in RICH, so the maximum number of FEM data blocks
//   produced (MAX_FEMBLK) is 32.
//
//   No zero suppression of data in FEM
//
//   Unit of data mask is FEM (2 FEM blocks). That is, FEM data block
//   are suppressed in pairs. 
//

  const unsigned int MAX_FEMBLK = 32;
  int fem_id[MAX_FEMBLK];
  int fem_xref[MAX_FEMBLK];

  // counts the number of active FEM, and make cross reference tables
  // fem_id[] and fem_xref[]

  unsigned int i;
  unsigned int nfem = 0;
  for(i = 0; i < MAX_FEMBLK; i++) {
    int imask = 1<<i;
    if( (par->fem_mask & imask) != 0) {
      fem_id[nfem]  = i;
      fem_xref[i] = nfem;
      nfem++;
    } else {
      fem_xref[i] = -1;   // fem #i is inactive
    }
  }

  // Increase the size of FEM table if necessary
  if( fem_wrap->MaxRowCount() < nfem ) fem_wrap->SetMaxRowCount(nfem);
  fem_wrap->SetRowCount(nfem);

  // Fill up empty FEM data in the output DCRKFEM_ST structure
  // Note that i-th element of DCRKFEM_ST is for fem_id[i]-th FEM
  for(i=0; i < nfem; i++)
    make_empty_fem(fem + i, fem_id[i], par->mplex);

  // Now fill the raw hit data in FEM.
  for(i = 0; i < raw_wrap->RowCount(); i++) {
    int ifem, addr;
    DCRKRAW_ST *hit = raw + i;
    pmt2fem(hit->pmt,&ifem,&addr);
    if(fem_xref[ifem] >= 0)
      fem_add_hit(fem + fem_xref[ifem],addr,hit->adc,hit->tdc);
  }

  // Now, set parity word of each FEM data block
  for(i=0;i<nfem;i++)
    fem_set_parity(fem+i);

  return True;
}

PHBoolean
TCrkModule::femDcm(PHCompositeNode *top) {

  return femDcm(top,"dCrkFEM","dCrkDCM");
}

PHBoolean
TCrkModule::femDcm(PHCompositeNode *top,const char *dCrkFEMNodeName,const char *dCrkDCMNodeName) {
//
// get the required data tables
//
  dCrkFEMWrapper *fem_wrap = findNode::getClass<dCrkFEMWrapper>(top,dCrkFEMNodeName);
  if(fem_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find " << dCrkFEMNodeName << endl;
      return False;
    }
  DCRKFEM_ST *fem = fem_wrap->TableData();

  dCrkDCMWrapper *dcm_wrap = findNode::getClass<dCrkDCMWrapper>(top,dCrkDCMNodeName);
  if(dcm_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find " << dCrkDCMNodeName << endl;
      return False;
    }
  DCRKDCM_ST *dcm = dcm_wrap->TableData();

  dCrkDCMparWrapper *par_wrap =
    findNode::getClass<dCrkDCMparWrapper>(top,"dCrkDCMpar");
  if(par_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find dCrkDCMpar" << endl;
      return False;
    }
  DCRKDCMPAR_ST *par = par_wrap->TableData();

  unsigned int ndcm = fem_wrap->RowCount();
  if(dcm_wrap->RowCount() < ndcm ) dcm_wrap->SetMaxRowCount(ndcm);
  dcm_wrap->SetRowCount(ndcm);

  for(unsigned int i=0;i<ndcm; ++i) {
    make_dcm(dcm+i,par,fem+i);
    dcm->packetID = 6001 + i;
  }

  return True;
}

PHBoolean
TCrkModule::dcmRaw(PHCompositeNode *top) {
  dCrkDCMWrapper *dcm_wrap = findNode::getClass<dCrkDCMWrapper>(top,"dCrkDCM");
  if(dcm_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkDCM" << endl;
      return False;
    }
  DCRKDCM_ST *dcm = dcm_wrap->TableData();

  dCrkDCMparWrapper *par_wrap = 
    findNode::getClass<dCrkDCMparWrapper>(top,"dCrkDCMpar");
  if(par_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find dCrkDCMpar" << endl;      
      return False;
    }

  dCrkRawWrapper *raw_wrap = findNode::getClass<dCrkRawWrapper>(top,"dCrkRaw");
  if(raw_wrap == NULL) 
    {
      cout << PHWHERE << "Did not find dCrkRaw" << endl;
      return False;
    }
  DCRKRAW_ST *raw = raw_wrap->TableData();

  // convert DCM to "raw" format, and put it in a vector<raw>
  vector<DCRKRAW_ST> vraw;
  for(unsigned int i=0; i<dcm_wrap->RowCount(); ++i) {
    //    add_dcm(dcm+i,par->threshold,vraw);
    add_dcm(dcm+i,25,vraw);
  }
  // sort the result in "pmt" ID order
  sort(vraw.begin(),vraw.end());


  if(raw_wrap->MaxRowCount() < vraw.size())
    raw_wrap->SetMaxRowCount(vraw.size());
  raw_wrap->SetRowCount(vraw.size());

  copy(vraw.begin(),vraw.end(),raw);

  return True;
}

PHBoolean
TCrkModule::rawHit(PHCompositeNode *top) {

  dCrkRawWrapper *raw_wrap = findNode::getClass<dCrkRawWrapper>(top,"dCrkRaw");
  if(raw_wrap == NULL) 
    {
      cout << PHWHERE << " Did not find dCrkRaw" << endl;
      return False;
    }
  DCRKRAW_ST *raw = raw_wrap->TableData();
  unsigned int nraw = raw_wrap->RowCount();

  dCrkHitWrapper *hit_wrap = findNode::getClass<dCrkHitWrapper>(top,"dCrkHit");
  if(hit_wrap == NULL) 
    {
      cout << PHWHERE << " Did not find dCrkHit" << endl;      
      return False;
    }
  DCRKHIT_ST *hit = hit_wrap->TableData();

  dCrkCalWrapper *cal_wrap = findNode::getClass<dCrkCalWrapper>(top,"dCrkCal");
  if(cal_wrap == NULL) 
    {
      cout << PHWHERE << " Did not find dCrkCal" << endl;
      return False;
    }
  DCRKCAL_ST *cal = cal_wrap->TableData();
  
  dCrkRawHitParWrapper *par_wrap =
    findNode::getClass<dCrkRawHitParWrapper>(top,"dCrkRawHitPar");
  DCRKRAWHITPAR_ST *par = par_wrap->TableData();

  if(hit_wrap->MaxRowCount() < nraw) hit_wrap->SetMaxRowCount(nraw);
  hit_wrap->SetRowCount(nraw);

  transform(raw,raw+nraw,hit,raw2hit(cal,par->min_pe));

  return True;
}

PHBoolean TCrkModule::pid(PHCompositeNode *top, TCrkProjector *pj){
  //  cout << "TCrkModule::pid()"<<endl;

  TCrkProjector *projector;
  if(pj == NULL) {
    cout << "Default projector is used" << endl;
    //    projector = new TCrkDchProjector();
    projector = new TCrkCglProjector();
  } else {
    projector = pj;
  }

  dCrkHitWrapper *hit_wrap = findNode::getClass<dCrkHitWrapper>(top,"dCrkHit");
  if(hit_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkHit" << endl;
      return False;
    }
  DCRKHIT_ST *hit = hit_wrap->TableData();
  int     nhit = hit_wrap->RowCount();

  dCrkGeoWrapper *geo_wrap = findNode::getClass<dCrkGeoWrapper>(top,"dCrkGeo");
  if(geo_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkgeo" << endl;
      return False;
    }
  DCRKGEO_ST *geo = geo_wrap->TableData();

  dCrkProjPidParWrapper *par_wrap =
    findNode::getClass<dCrkProjPidParWrapper>(top,"dCrkProjPidPar");
  if(geo_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkprojPidPar" << endl;
      return False;
    }
  DCRKPROJPIDPAR_ST *par = par_wrap->TableData();

  dCrkPidWrapper *pid_wrap = findNode::getClass<dCrkPidWrapper>(top,"dCrkPid");
  if(pid_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkPid" << endl;
      return False;
    }
  DCRKPID_ST *pid = pid_wrap->TableData();

  int nproj;
  CrkProj *proj = projector->projection(top, &nproj);

  vector<DCRKPID_ST> vpid;
  for_each(proj,proj+nproj,proj2pid(geo,par,hit,nhit,&vpid));
  unsigned int npid = vpid.size();
  delete[] proj;

  //  cout << "vpid.size()="<<npid<<endl;
  if(pid_wrap->MaxRowCount() < npid) pid_wrap->SetMaxRowCount(npid);
  copy(vpid.begin(),vpid.end(),pid);
  pid_wrap->SetRowCount(npid);

  //  cout << "Now delete projector" << endl;
  if( pj == NULL) delete projector;

  return True;
}


PHBoolean TCrkModule::prdfRaw(PHCompositeNode *top, CrkDAO *dao) {
  PHNodeIterator itop(top);

  // find PRDF node
  EventNode_t *prdf =
    static_cast<EventNode_t*>(itop.findFirst("PHDataNode","PRDF"));
  if(!prdf)
    {
      cout << PHWHERE << "Did not find PRDF node" << endl;
      return False;
    }


  // find CrkRaw table node
  dCrkRawWrapper *raw_wrap = findNode::getClass<dCrkRawWrapper>(top,"dCrkRaw");
  if(raw_wrap == NULL)
    {
      cout << PHWHERE << "Did not find dCrkRaw" << endl;
      return False;
    }

  fill_raw_from_prdf(prdf->getData(), dao, raw_wrap);
  
  return True;
}

//=======================================================
//
// static helper functions.
// Since those functions are only used in this class, they are
// declared as static, completely hidden from the global scope.
//
//=======================================================

/****************************************************************************
 * Defalut values of CrkGeom parameters.
 ****************************************************************************/
static DCRKGEO_ST default_geo = {
  90.,      /* phi_cntr */
  78.75,    /* phi_open */
  100.,     /* dphi_carm */
  92.722,   /* dphi_cshe */
  -33.0365, /* pmt_phi_min */
  55.9365,  /* pmt_phi_max */
  1.11216,  /* pmt_dphi */
  2.5,      /* r_pmt_ent */
  {         /* dx_pmt[32] */
    2.424, 2.424, 2.424, 2.424, 2.424, 2.424, 2.424, 2.424, 2.424, 2.424,
    2.424, 2.424, 2.424, 2.424, 2.424, 2.424,
    -2.424,-2.424,-2.424,-2.424,-2.424,-2.424,-2.424,-2.424,-2.424,-2.424,
    -2.424,-2.424,-2.424,-2.424,-2.424,-2.424
  },
  {        /* r_pmt[32] */
    263.49, 263.49, 263.49, 263.49, 263.49,
    263.49, 263.49, 263.49, 263.49, 263.49,
    263.49, 263.49, 263.49, 263.49, 265.428, 269.301,
    263.49, 263.49, 263.49, 263.49, 263.49,
    263.49, 263.49, 263.49, 263.49, 263.49,
    263.49, 263.49, 263.49, 263.49, 267.363, 271.239
  },
  {        /* z_pmt[32] */
    141.227, 150.056, 158.885, 167.714, 176.543,
    185.372, 194.201, 203.03,  211.859, 220.688,
    229.532, 237.948, 246.055, 253.983, 260.134, 264.587,
    145.53,  154.359, 163.188, 172.017, 180.846,
    189.675, 198.504, 207.333, 216.162, 225.217,
    233.848, 242.047, 250.061, 257.907, 262.362, 266.815
  },
  {        /* theta_pmt[32] */
    35.487,35.487,35.487,35.487,35.487,
    35.487,35.487,35.487,35.487,35.487,
    37.847,40.347,43.012,45.802,48.992,48.992,
    35.487,35.487,35.487,35.487,35.487,
    35.487,35.487,35.487,35.487,
    37.847,40.347,43.012,45.802,48.992,48.992,48.992
  },
  403.0,   /* mir_rin */
  0.01,    /* mir_thck */
  96.41,   /* mir_theta1 */
  122.24,  /* mir_theta2 */
  98.99,   /* mir_thetacut */
  -44.1,   /* mir_phi1 */
  44.1,    /* mir_phi2 */
  215.,    /* mir_dz */
  258.77,  /* wi1_rin */
  0.013,   /* wi1_thck */
  115.7,   /* wi1_zend */
  406.939, /* wi2_rin */
  0.013,   /* wi2_thck */
  185.6    /* wi2_zend */
};

void crk_print_pmt(int pmt) {
  int sector, iz, iphi;
  assert(pmt2cell(pmt, sector, iz, iphi));
  cout <<"pmt="<<pmt << ":("<<sector<<","<<iz<<","<<iphi<<")";
}

static bool  pmt2cell(int pmt, int& sector, int& iz, int& iphi) {
/*
 * convert PMT_Id to (sector,iz,iphi). Note that all of those indexes
 * starts from 0 (i.e. C-fashion), not from 1.
 *
 * PMT = NCELL_SECT*sector + NCPMT_2*iphi + iz;
 *
 */
  if( pmt >= 0 && pmt < N_CRK_PMT) {
    sector =  pmt/NCELL_SECT;
    iphi   = (pmt - sector*NCELL_SECT)/NCPMT_2;
    iz     =  pmt - sector*NCELL_SECT - iphi*NCPMT_2;
    return true;
  } else {
    cout << "TCrkModule::pmt2cell: strange cell ID = " << pmt <<endl;
    sector = -1;
    iphi   = -1;
    iz     = -1;
    return false;
  }
}

/* conversion between FEM address and PMT id
 * 
 * In order to make LVL1 trigger sum, PMT address <--> FEM address
 * mapping is quite complicated.
 * 
 * One FEM handles 320 ch. This is divided 2 upper/lower part.
 * Each part produces one FEM data block (160ch), corresponding to 10 rows.
 * Each part made of 5 card. One card read 2 rows, but those 2 rows are
 * NOT contiguous. See the mapping below.
 *
 * card 0 read row # 0 (ch0-15) and 5 (ch16-31).
 * card 1 read row # 1 (ch0-15) and 6 (ch16-31).
 * card 2 read row # 2 (ch0-15) and 7 (ch16-31).
 * card 3 read row # 3 (ch0-15) and 8 (ch16-31).
 * card 4 read row # 4 (ch0-15) and 9 (ch16-31).
 *
 */
static void pmt2fem(int pmt, int *pfem, int *paddr) {
  int fem  = pmt/NPMT_FEM2;
  int addr_prime = pmt%NPMT_FEM2;
  int irow = addr_prime/NCPMT_2;
  int iz   = addr_prime%NCPMT_2;
  int card, subaddr;
  int addr;

  if(irow < NCARD_FEM) {
    card = irow;
    subaddr = iz;
  } else {
    card = irow - NCARD_FEM;
    subaddr = iz + NCPMT_2;
  }
  addr = NPMT_AMUCARD2*card + subaddr;

  *paddr = addr;
  *pfem  = fem;
}

static int fem2pmt(int fem, int addr) {
  int card    = addr/NPMT_AMUCARD2;
  int subaddr = addr%NPMT_AMUCARD2;
  int irow,iz;

  if(subaddr < NCPMT_2) {
    iz = subaddr;
    irow = card;
  } else {
    iz = subaddr - NCPMT_2;
    irow = card + NCARD_FEM;
  }
  return NPMT_FEM2*fem + NCPMT_2*irow + iz;
}

static void set_default_geom(DCRKGEO_ST *geo) {
//
//  DCRKGEO_ST *default_geo_p = &default_geo;
//  copy(default_geo_p, default_geo_p + 1, geo);
//
// Revised code to pass INSURE
//   by T. Sakaguchi on Jan. 04, 2006
//
  *geo = default_geo;
}
/*
 * Naming/addressing convension:
 * all index start from 0.
 * 
 * 1 sector : NCPMT_2 by NCPHI array of PMTs
 *
 * There are 4 sectors in PHENIX
 *   sector 0   (South, West) or (z<0,x>0)
 *   sector 1   (North, West) or (z>0,x>0)
 *   sector 2   (South, East) or (z<0,x<0) 
 *   sector 3   (North, East) or (z>0,x<0)
 *
 * iphi   0 to NCPHI-1. 0 is bottom. (lower y)
 * 
 * iz     0 to NCPMT_2-1. 0 is near collsion point (small abs(z))
 */
static void pmt_axis(DCRKGEO_ST *geo, int pmt, float uax[]) {
  int sector,iz,iphi;
  float pmt_phi;
  float uax_T;

  assert(pmt2cell(pmt, sector, iz, iphi));

  if(((sector == 1 || sector == 2) && (iphi%2 == 1)) ||
     ((sector == 0 || sector == 3) && (iphi%2 == 0)))
    uax[2] = cos(geo->theta_pmt[iz]*degree);
  else
    uax[2] = cos(geo->theta_pmt[iz + NCPMT_2]*degree);
  
  uax_T = sqrt(1.0 - uax[2]*uax[2]);
  if( sector%2 == 0) uax[2] = -uax[2];   /* Z < 0 part */
  pmt_phi = (geo->pmt_phi_min + (iphi+0.5)*(geo->pmt_dphi))*degree;
  uax[0] = -cos(pmt_phi)*uax_T;
  uax[1] = -sin(pmt_phi)*uax_T;
  if( sector >= 2) uax[0] = -uax[0]; /* X < 0 */
}

static void pmt_position(DCRKGEO_ST *geo, int pmt, float x[]) {
  int sector,iz,iphi;
  float pmt_phi;

  assert(pmt2cell(pmt, sector, iz, iphi));
  pmt_phi = (geo->pmt_phi_min + (iphi+0.5)*(geo->pmt_dphi))*degree;
  if(((sector == 1 || sector == 2) && (iphi%2 == 1)) ||
     ((sector == 0 || sector == 3) && (iphi%2 == 0))) {
    x[0] = geo->r_pmt[iz]*cos(pmt_phi);
    x[1] = geo->r_pmt[iz]*sin(pmt_phi);
    x[2] = geo->z_pmt[iz];
  } else {
    x[0] = geo->r_pmt[iz + NCPMT_2]*cos(pmt_phi);
    x[1] = geo->r_pmt[iz + NCPMT_2]*sin(pmt_phi);
    x[2] = geo->z_pmt[iz + NCPMT_2];
  }
  if( sector >= 2)   x[0] = -x[0];   /* X < 0 */
  if( sector%2 == 0) x[2] = -x[2];   /* Z < 0 */
}

/******************************************
 * helper functions for GhitRaw
 ******************************************/
static float calc_sinT(DCRKGEO_ST *geo, CRKGHIT_ST *h) {
  float uaxis[3];
  float pL = sqrt(h->px*h->px + h->py*h->py + h->pz*h->pz);
  float cosT;

  pmt_axis(geo,h->pmt,uaxis);
  cosT = (h->px*uaxis[0] + h->py*uaxis[1] + h->pz*uaxis[2])/pL;
  return(sqrt(1.0 - cosT*cosT));
}

struct raw_select {
  int d_pmt;
  raw_select(int pmt):d_pmt(pmt){}
  bool operator ()(const DCRKRAW_ST &raw) const {
    return d_pmt == raw.pmt;
  }
};

static void add_hit(DCRKGHITRAWPAR_ST *par,
		      DCRKGEO_ST  *geo,
		      DCRKUCAL_ST *ucal,
		      DCRKRAW_ST  *raw,
		      DCRKRAW_ST  **raw_end_p,
		      CRKGHIT_ST  *h,
		      map<int,int> &MapRawParent) {
  float sinT;     /* sin(Theta) relative to PMT axis */
  DCRKUCAL_ST *u_cal = ucal + h->pmt;

  assert(0 <= h->pmt && h->pmt < 5120);
  if(h->pid == PP_CRK_PHOT) {     /* Cerenkov photon hit */
    float Ntst = par->N0_pisa*rndm();
    if( Ntst < u_cal->N0p)
      sinT = calc_sinT(geo,h);
    else
      sinT = 1.0; /* rejected */
  } else          /* Charged track hit */
    sinT = 0.0;   /* Accepted by 100%  */

  if(sinT < par->sinTmax) {  /* Winston cone angular acceptance cut */
    DCRKRAW_ST *raw_found = find_if(raw, *raw_end_p, raw_select(h->pmt));
    if(raw_found == *raw_end_p) {
      raw_found->adc = 0;
      raw_found->tdc = par->max_tdc;
      raw_found->pmt = h->pmt;
      ++(*raw_end_p);
    }

    float adc = (u_cal->gain)*(1.0 + (u_cal->sigma_pe)*norran());
    float tdc =
      (u_cal->clock)*(h->tof + (u_cal->sigma_t)*norran()) + u_cal->t0;
    if(raw_found->adc == 0) raw_found->adc = (short)(adc + u_cal->ped);
    else raw_found->adc += (short) adc;
    if(raw_found->adc > par->max_adc ) raw_found->adc = par->max_adc;

    if(tdc < raw_found->tdc) raw_found->tdc = (short) tdc;
    if(raw_found->tdc < par->min_tdc ) raw_found->tdc = par->min_tdc;

    // add one entry in Raw<-->Parent cross reference table
    MapRawParent[h->pmt] = h->parent;
  }
}

static void add_noise(DCRKGHITRAWPAR_ST *par, DCRKUCAL_ST *ucal,
		       DCRKRAW_ST *raw, DCRKRAW_ST **raw_end_p,
		       map<int,int> &MapRawParent) {
/*
 * Add noise to PMT channels. Since the noise hit probabiliy should be
 * small, the straighforward algorithm used here is not efficient. I
 * will look for improvement later.
 */
  int pmt;
  for(pmt=0;pmt<N_CRK_PMT;pmt++) {
    DCRKUCAL_ST *u_cal = ucal + pmt;
    if(rndm() < u_cal->P_noise) {
      DCRKRAW_ST *raw_found = find_if(raw,*raw_end_p,raw_select(pmt));
      if(raw_found == *raw_end_p) {
	raw_found->adc = 0;
	raw_found->tdc = par->max_tdc;
	raw_found->pmt = pmt;
	++(*raw_end_p);
      }

      float adc = (- u_cal->mean_noise)*log(rndm());
      short tdc = par->min_tdc + (short)((par->max_tdc - par->min_tdc)*rndm());
      
      if(raw_found->adc == 0) raw_found->adc = (short) (adc + u_cal->ped);
      else raw_found->adc += (short)adc;
      if(tdc < raw_found->tdc) raw_found->tdc = tdc;

      map<int,int>::iterator ix = MapRawParent.find(pmt);
      if( ix == MapRawParent.end()) { // THere was no hit at pmt
	// This means that the only cause of this hit is noise and there
	// is no parent electron for this hit. I will use negative value
	// (-1) to indicate that this is a noise hit
	MapRawParent[pmt] = -1;
      }
    }
  }
}


static void fill_ghitraw(dCrkRel2sWrapper *rel_wrap, map<int,int> &m) {
  // Fill the contents of PMT_id <--> Parent tracknumber table m into
  // STAF table (ghitraw_h, ghitraw). This code assumes that cross reference
  // table m has already been created by the caller.

  // check size of the table. reallocate the table if needed.
  unsigned int nsize = m.size();
  if(rel_wrap->MaxRowCount() < nsize) rel_wrap->SetMaxRowCount(nsize);

  // fill the output relational table
  map<int,int>::iterator ix;
  DCRKREL2S_ST *ghitraw = rel_wrap->TableData();
  int i;
  for(i=0, ix=m.begin(); ix != m.end(); ++ix) {
    ghitraw[i].id1 = (short) (*ix).first;
    ghitraw[i].id2 = (short) (*ix).second;
    ++i;
  }
  rel_wrap->SetRowCount(nsize);
}

//********************************
// helper functions for RawFEM
//********************************
#define TAC_CELL   10
#define PRE_CELL   11
#define POST_CELL  10  /* always same as TAC cell */
#define PED_VALUE  100

static void make_empty_fem(DCRKFEM_ST *fem, int fem_id, int mplex) {
  fem->DAV1   = 0xFFFF;
  fem->detid  = 0x0600; 
  fem->evno   = 0;   /* 0 for now. We need a way to synchronize this */
  fem->module = (fem_id<<2) + mplex;
  fem->flag   = 0;
  fem->clock  = 0;    /* 0 for now. We need a way to synchronize this */

  fem->tac_cell  = TAC_CELL;
  fem->pre_cell  = PRE_CELL;
  fem->post_cell = POST_CELL;
  
  int i;
  for(i=0;i<NPMT_FEM2;i++) {
    fem->data[3*i+FEM_PRE_OFFSET]   = PED_VALUE;  // pre
    fem->data[3*i+FEM_POST_OFFSET]  = PED_VALUE;  // post
    fem->data[3*i+FEM_TAC_OFFSET]   = 0;  // tac
  }

  for(i=0;i<8;i++)
    fem->usr[i] = 0;
  fem->parity   = 0;
  fem->CAV2     = 0;
}

static void fem_add_hit(DCRKFEM_ST* fem, int addr, int adc, int tdc) {
  fem->data[3*addr + FEM_POST_OFFSET] += adc;
  fem->data[3*addr + FEM_TAC_OFFSET]  += tdc;
}

static void fem_set_parity(DCRKFEM_ST* fem) {
  // NOT IMPLIMENTED YET
}

//*************************************
// helper functions for DCM
//*************************************
#define ZERO_SUPPRESS_MODE 0
#define PASS_THROUGH_MODE  1

static void make_dcm(DCRKDCM_ST *dcm, const DCRKDCMPAR_ST *par,
		     const DCRKFEM_ST *fem) 
{
  static uint tag_usr[8] = {
    0x802e0000,
    0x804e0000,
    0x806e0000,
    0x808e0000,
    0x80ae0000,
    0x80ce0000,
    0x80ee0000,
    0x810e0000
  };

  dcm->flag   = (fem->flag   | 0x800F0000);
  dcm->module = (fem->module | 0x80060000);
  dcm->evno   = (fem->evno   | 0x80060000);
  dcm->clock  = (fem->clock  | 0x80060000);
  dcm->detid  = (fem->detid  | 0x80060000);

  dcm->tac_cell  = (fem->tac_cell  | 0x00020000);
  dcm->pre_cell  = (fem->pre_cell  | 0x00040000);
  dcm->post_cell = (fem->post_cell | 0x00010000);

  int i;
  uint *base  = (uint *)dcm->data;
  uint *pdata = base;
  for (i=0;i<NPMT_FEM2;i++) {
    unsigned short tac  = fem->data[3*i + FEM_TAC_OFFSET];
    unsigned short pre  = fem->data[3*i + FEM_PRE_OFFSET];
    unsigned short post = fem->data[3*i + FEM_POST_OFFSET];
    pdata = dcm_add_data(par, pdata, i, tac, pre, post);
  }

  for(i=0;i<8;i++) {
    *pdata = tag_usr[i] | fem->usr[i];
    pdata++;
  }
  *pdata = 0x802F0000 | fem->parity;
  pdata++;
  *pdata = 0x804F0000 | fem->CAV2;

  dcm->nWord = pdata - base + 9;
  if(par->mode == PASS_THROUGH_MODE ) {
    dcm->scheme = IDRICH_DCM0;
  } else { // must be ZERO_SUPPRESS_MODE
    dcm->scheme = IDRICH_DCM1;
  }
}


static uint* dcm_add_data(const DCRKDCMPAR_ST *par, uint *pdata, int ich,
			    ushort tac, ushort pre, ushort post)
{
  uint *retval;

  int flag_add_data = 0;

  if( par->mode == PASS_THROUGH_MODE ) flag_add_data = 1;

  if( par->mode == ZERO_SUPPRESS_MODE && post - pre > par->threshold )
    flag_add_data = 1;
 
  if( flag_add_data == 1 ) {
    int card    = ich/NPMT_AMUCARD2 + 1;
    int subaddr = ich%NPMT_AMUCARD2 + 1;
    uint upper_bits = ((card << 28) | (subaddr << 21));
     
    pdata[DCM_TAC_OFFSET]  = (upper_bits | 0x00090000 | tac);
    pdata[DCM_PRE_OFFSET]  = (upper_bits | 0x000a0000 | pre);
    pdata[DCM_POST_OFFSET] = (upper_bits | 0x000c0000 | post);
    retval = pdata + 3;
  } else
    retval = pdata;
  return retval;
}

//*********************************
//  helper for DCMRaw
//*********************************

static void add_dcm(DCRKDCM_ST *dcm, int threshold, vector<DCRKRAW_ST> &vraw) {
  //  int fem_id = ((dcm->module & 0xFFFF)>>2);  //fem hardware ID doesn't work
  int fem_id;
  unsigned int *pdata = (unsigned int *)dcm->data;
  int fem_id_tbl[40] = {
     0, 1, 2, 3, 4, 5, 6, 7,0,0,
     8, 9,10,11,12,13,14,15,0,0,
    16,17,18,19,20,21,22,23,0,0,
    24,25,26,27,28,29,30,31,0,0,
  };

  int pkt = dcm->packetID - 6001;
  fem_id = fem_id_tbl[pkt];

  while( (*pdata & 0x80000000) == 0) {
    // Disable checking the tag word --- the real FEE data will fail this
    // check...
    //    assert((pdata[DCM_POST_OFFSET] & 0x1f0000) == 0x00c0000);
    //    assert((pdata[DCM_PRE_OFFSET]  & 0x1f0000) == 0x00a0000);
    //    assert((pdata[DCM_TAC_OFFSET]  & 0x1f0000) == 0x0090000);
    
    int card    = ((pdata[DCM_POST_OFFSET] & 0x70000000)>>28);
    int subaddr = ((pdata[DCM_POST_OFFSET] & 0x0fe00000)>>21);
    int addr    = (card-1)*NPMT_AMUCARD2 + subaddr - 1;
    int pmt     = fem2pmt(fem_id,addr);
    
    int post = (pdata[DCM_POST_OFFSET] & 0x00003ff);
    int pre  = (pdata[DCM_PRE_OFFSET]  & 0x00003ff);
    int tac  = (pdata[DCM_TAC_OFFSET]  & 0x00003ff);
    int adc  = post - pre;
    int tdc  = tac;
    
    if( adc > threshold) {
      DCRKRAW_ST hit;
      hit.pmt=pmt;
      hit.adc=adc;
      hit.tdc=tdc;
      vraw.push_back(hit);
    }
    pdata += 3;
  }
}

//
// helper struct (functor) for RawHit
//
raw2hit::raw2hit(DCRKCAL_ST *calib, float min_pe) {
    cal    = calib;  // store pointer to the calibration data
    min_p  = min_pe; // store parameter (min. photo-electron)
}

DCRKHIT_ST raw2hit::operator()(DCRKRAW_ST &rawdata) {
  //
  // For each hit, convert the raw data to calibrated hit
  //
  DCRKHIT_ST hit;
  hit.pmt = rawdata.pmt;
  DCRKCAL_ST *c = cal + rawdata.pmt;
  if( c->adc_gain > 0) {
    hit.npe = (rawdata.adc - c->adc_ped)/c->adc_gain;
    if(hit.npe < min_p) hit.npe = min_p;
  } else { // it is a dead channel
    hit.npe = 100.; //this is an impossible value. In RICH analysis,
                    // such a high value of npe will be ignored.
                    // See CrkPID.cc how a large pulse heigh is handled.
  }
  hit.time = (rawdata.tdc - c->tdc_t0)/c->tdc_clock;
  return hit;
}

//
// helper for pid()
//

proj2pid::proj2pid(DCRKGEO_ST *geo, DCRKPROJPIDPAR_ST *par,
		   DCRKHIT_ST *hit, int nhit, vector<DCRKPID_ST> *vpid) {
  d_geo = geo;
  setup_geometry(geo);
  set_ring_par(par);
  d_hit  = hit;
  d_nhit = nhit;
  d_min_npmt  = par->min_npmt;
  d_in_accept = par->in_accept;
  d_vpid = vpid;
}

/* Parameter set for known radiators */
#define GAS_C2H6 1
#define GAS_CH4  2
#define GAS_CO2  3
static const RingPar c2h6_par = {4.8, 7.8, 14.0, 10.5 };
static const RingPar ch4_par  = {3.0, 5.9, 11.0, 8.4};
static const RingPar co2_par  = {3.0, 5.9, 11.0, 8.4};

void proj2pid::set_ring_par(DCRKPROJPIDPAR_ST *par) {
  const RingPar *p;
  if(par->gas == GAS_C2H6) {
    cout << "ETHANE radiator: ";
    p = &c2h6_par;
  } else if(par->gas == GAS_CH4) {
    cout << "METHANE radiator: ";
    p = &ch4_par;
  } else if(par->gas == GAS_CO2) {
    cout << "CO2 radiator: ";
    p = &co2_par;
  } else {
    printf("UNKNOWN radiator: ");
    p = 0;
  }
  if(p) {
    d_par = *p;
  } else {
    d_par.Rmax  = par->Rmax;
    d_par.Rmin  = par->Rmin;
    d_par.Rmax2 = par->Rmax2;
    d_par.R0    = par->R0;
  }
  cout << d_par.Rmax<<","<<d_par.Rmin<<","<<d_par.R0<<","<<d_par.Rmax2<< endl;
}

void proj2pid::setup_geometry(DCRKGEO_ST *geo) {
  int i;
  float phi_center;
  float pmt_z_center;

  phi_center = geo->phi_cntr - geo->phi_open;
  pmt_z_center = 0.5*(geo->z_pmt[0] + geo->z_pmt[31]);

  for(i=0;i<4;i++) {
    d_mirror[i].r = geo->mir_rin;
    d_mirror[i].center[0] = 0.0;
    d_mirror[i].center[1] = 0.0;

    d_sector[i].r         = geo->r_pmt[0]; /* use pmt[0] as typical value */
    d_sector[i].dz        = 0.5*(geo->z_pmt[31] - geo->z_pmt[0]);
    d_sector[i].center[0] = 0.0;
    d_sector[i].center[1] = 0.0;

    if(i < 2) { /* X > 0 */
      d_mirror[i].phi_min = (phi_center + geo->mir_phi1)*degree;
      d_mirror[i].phi_max = (phi_center + geo->mir_phi2)*degree;
      d_sector[i].phi_min = geo->pmt_phi_min*degree;
      d_sector[i].phi_max = geo->pmt_phi_max*degree;
    } else { /* X < 0 */
      d_mirror[i].phi_min = (180. - phi_center - geo->mir_phi2)*degree;
      d_mirror[i].phi_max = (180. - phi_center - geo->mir_phi1)*degree;
      d_sector[i].phi_min = (180. - geo->pmt_phi_max)*degree;
      d_sector[i].phi_max = (180. - geo->pmt_phi_min)*degree;
    }

    if( i%2 == 0) { /* Z < 0 */
      d_mirror[i].center[2] = (- geo->mir_dz);
      d_mirror[i].theta_min = (180. - geo->mir_theta2)*degree;
      d_mirror[i].theta_max = (180. - geo->mir_thetacut)*degree;
      d_sector[i].center[2] = (-pmt_z_center);
    } else { /* Z > 0 */
      d_mirror[i].center[2] = geo->mir_dz;
      d_mirror[i].theta_min = geo->mir_thetacut*degree;
      d_mirror[i].theta_max = geo->mir_theta2*degree;
      d_sector[i].center[2] = pmt_z_center;
    }
  }
}

void proj2pid::operator()(CrkProj& proj) {
  DCRKPID_ST pid_result;
  test_proj(proj,&pid_result);
  if(pid_result.npmt >= d_min_npmt &&
     (d_in_accept == 0 || pid_result.faccept == 1)) {
    d_vpid->push_back(pid_result);
  }
}

void proj2pid::test_proj(CrkProj &proj, DCRKPID_ST *result) {
  static const DCRKPID_ST zero_pid = {0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0,
				   {0.,0.,0.},0.,0.};
  Ray r;
  Ray r_ref;
  int isect;

  /* setup incident ray r, and set result ZERO */
  r = proj;
  r = proj;
  *result = zero_pid;  /* initialized to ZERO */
  result->proj_id = proj.track_id;
  
  /* determine the sector number */
  if( proj.u[0] > 0) {
    if( proj.u[2] < 0) isect = 0; else isect = 1;
  } else {
    if( proj.u[2] < 0) isect = 2; else isect = 3;
  }

  if(reflect_by_mirror( &r, d_mirror + isect, &r_ref))
    if(cross_tubs(&r_ref, d_sector + isect, result->xproj)) {
      /*
       * The ray is in RICH acceptance.
       * look for RICH hits that can be assoicated to the ray
       */
      float time_sum = 0;
      float dt_sum = 0.0;
      float dv[3] = {0.,0.,0.};
      float dvh[4];
      int   nselect = 0;
      float L1 = 0;
      int i;

      /* calcualte distance from x to ref.x */
      for(i=0;i<3;i++) {
	float dl = r_ref.x[i] - proj.x[i];
	L1 += dl*dl;
      }
      L1 = sqrt(L1);
/*
 * In the code below, there is two level of HIT pmts. If the distance
 * between the ray and a PMT is less than Rmax, the PMT is used in the
 * calculation of chi2. If the PMT is in more restricted area (Rmin<dr<Rmax2),
 * it is included in npe and timing calculation. The reason for this two
 * step method is rather arbitrary, and probably there is a better way. By
 * calculation chi2 in larger area, this method implicitly requires isolation
 * of PMT ring, which helps to remove pions but also remove good electrons
 * in very high multiplicity.
 */
      for(i=0;i<d_nhit;i++) {
	DCRKHIT_ST *h = d_hit + i;
	float dr = pmt_ray_distance(h->pmt,&r_ref,dvh);
	float cor_time = h->time - (L1 + dvh[3])/c_light;
        float cort0 = 8.62;

	if( dr < d_par.Rmax && h->time < 22) {
	  nselect++;
	  result->chi2 += (dr - d_par.R0)*(dr - d_par.R0)*(h->npe);
	  if(7.8 < cor_time && cor_time < 9.6)
	    result->chi2b += (dr-d_par.R0)*(dr-d_par.R0)*(h->npe);
	  if( d_par.Rmin < dr && dr < d_par.Rmax2 && 7.8 < cor_time &&
	      cor_time < 9.6) {
	    result->npmt++;
	    result->npe += h->npe;
	    time_sum += (h->time)*(h->npe);
	    dv[0] += dvh[0]*h->npe;
	    dv[1] += dvh[1]*h->npe;
	    dv[2] += dvh[2]*h->npe;
            dt_sum += (cor_time - cort0)*h->npe;
 	  }
	}
      }
      result->faccept = 1;
      if( result->npmt > 0) {
	float L2sq = 0;

	result->timing = time_sum/result->npe;
	result->chi2 /= result->npe;
        result->chi2b /= result->npe;
        result->dt = dt_sum/result->npe;
	result->rdisp = sqrt(dv[0]*dv[0]+dv[1]*dv[1]+dv[2]*dv[2])/result->npe;

	for(i=0;i<3;i++) {
	  float dl = result->xproj[i] - r_ref.x[i];
	  L2sq += dl*dl;
	}
	result->Lpath = L1 + sqrt(L2sq);
      }
    }
}

int proj2pid::cross_sphere(Ray *r, Sphere *s, float cross[]) {
/* 
 * calculate cross point of ray r and sphere s.
 * Output:
 *     cross[]:  cross point of r and s in foward direction of r.
 * Return value:
 *     return 1 if r crosses s, otherwise return 0.
 * LIMITATION:
 *   The code requires that the starting point of the ray (r->x) SHOULD be
 *   inside of the sphere.
 * Algorithm:
 *  The equation of the ray is
 *      xp = t*u + x               (1)
 *  The equation of the sphere is
 *     (xp - center)**2 = r**2     (2)
 *  By substituting (1) in (2), one get
 *     t**2 + 2*t*(u,v) + (v**2 - r**2) = 0 (3)
 *  where
 *     v = x - center
 *  Eq. (3) can be solved easily.
 */
  float t;    /* parameter for the equation for the ray */
  float v[3]; /* r->x - s->center */
  float uv;   /* scalar product of r->u and v */
  float D;    /* determinant of the quadratic equation (3). See above. */
  int i;      /* loop variable */

  for(i=0;i<3;i++)
    v[i] = r->x[i] - s->center[i];
  uv = r->u[0]*v[0] + r->u[1]*v[1] + r->u[2]*v[2];
  if( uv <= 0) return 0;   /* the ray must hit sphere from inside */
  
  D = uv*uv - (v[0]*v[0] + v[1]*v[1] + v[2]*v[2] - s->r*s->r);
  if( D <= 0) return 0;   /* The ray does not cross the sphere */
/*
 * This code requires both of uv and t to be positive. Therefore, the correct
 * solution is -uv+sqrt(D).
 */
  t = -uv + sqrt(D);
  if( t <= 0) return 0;
/*
 * Here come means that we have a satisfactory solution for t
 */
  for(i=0;i<3;i++)
    cross[i] = r->x[i] + t*r->u[i];
/*
 * get phi and theta angle of the cross point
 */
  {
    float xt = cross[0] - s->center[0];
    float yt = cross[1] - s->center[1];
    float phi = atan2(yt, xt);
    float phi2 = 6.2831853 + phi;
    if(( s->phi_min < phi  && phi  < s->phi_max) ||
       ( s->phi_min < phi2 && phi2 < s->phi_max)) {
      float dz = cross[2] - s->center[2];
      float rt = sqrt(xt*xt + yt*yt);
      float theta = atan2(rt,dz);
      if( s->theta_min < theta && theta < s->theta_max) return 1;
    }
  }
  return 0;
}

int proj2pid::cross_tubs(Ray *r, Tubs *tub, float cross[]) {
/* calculate crossing point of a ray and a "TUBS"(tube section).
 *
 * Output:
 *  cross[]   The first crossing point of r and t in forward direction of r
 * Return value:
 *  1 if there is a crossing point 0 otherwise
 * Limitation:
 *  the starting point of r (r->x) should be outside of the tube t
 *
 * Algorithm:
 *  The equation of the ray is 
 *    xp = x + t*u              (1)
 *  The equation of the cylinder (or tube) is
 *    x[0]**2 + x[1]**2 = r**2  (2)
 *  After some algebra, (1)+(2) is reduced to
 *    A*t**2 + 2*B*t + C = 0
 *  where
 *    A = u[0]**2 + u[1]**2
 *    B = u[0]*x[0] + u[1]*x[1]
 *    C = x[0]**2 + x[1]**2 - r**2
 */
  float B;
  float x[2];
  float *u = r->u;

  x[0] = r->x[0] - tub->center[0];
  x[1] = r->x[1] - tub->center[1];
  B = u[0]*x[0] + u[1]*x[1];
  if( B < 0) {
    float A = u[0]*u[0] + u[1]*u[1];
    float C = x[0]*x[0] + x[1]*x[1] - (tub->r)*(tub->r);
    float D = B*B - A*C;    /* determinant of quad. eqation */
    if( D > 0) {
/* We are interested in the first crossing point in the forward direction
 * of the ray. Therefore, we take the smaller positive solution for t.
 */
      float t = (-B - sqrt(D))/A;
      if( t > 0) {
	int i;
	for(i=0;i<3;i++)
	  cross[i] = r->x[i] + t*u[i];
/*
 * Now check if the crossing point is in acceptance.
 */
	{
	  float z = fabs(cross[2] - tub->center[2]);
	  if( z < tub->dz) {
	    float phi = atan2(cross[1]-tub->center[1],cross[0]-tub->center[0]);
	    float phi2 = 6.2831853 + phi;
	    if(( tub->phi_min < phi  && phi  < tub->phi_max) ||
	       ( tub->phi_min < phi2 && phi2 < tub->phi_max)
	       ) return 1;
	  }
	}
      }
    }
  }
  return 0;
}

int proj2pid::reflect_by_mirror(Ray *r, Sphere *m, Ray *r_ref) {
  float *u = r->u;
  float *xref = r_ref->x;
  float *uref = r_ref->u;
  int   f_accept;

/* determine the cross point of the ray and the mirror */
  f_accept = cross_sphere(r,m,xref);

  if(f_accept) {  /* In mirror acceptance */
    float n[3]; /* radial vector of sphere at xref */
    float n_u;  /* scalar product n*u */
    float tmp;
    int i;
/*
 * get unit vector n from xref to  m->center. n is the
 * normal vector of the sphere at xref
 */
    for(i=0;i<3;i++) n[i] = m->center[i] - xref[i];
    tmp = 1.0/sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
    for(i=0;i<3;i++) n[i] *= tmp;
/*
 * get the direction vector of the reflected ray
 */
    n_u = n[0]*u[0] + n[1]*u[1] + n[2]*u[2];
    for(i=0;i<3;i++)
      uref[i] = u[i] - 2.0*n_u*n[i];
  }
  return f_accept;
}

float proj2pid::pmt_ray_distance(int pmt, Ray *r, float dv[]) {
/*
 * calculate distance between the ray r and pmt. It also compute the
 * normal vector from the ray to pmt.
 *
 * Output:
 *   dv[0...2]     normal vector from the ray to PMT
 *   dv[3]         distance from r->x to the closest approach to PMT      
 * Return value:
 *   distance from PMT and the ray. (=sqrt(dv**2))
 * Algorithm:
 *   xpmt can be written as
 *      xpmt = x + t*u + dist*n
 *   where n is a unit vector orthogonal to u. Thus, by taking scalar product
 *   with u vector, one get
 *     (xpmt,u) = (x,u) + t*(u,u) + dist*(n,u)
 *   Since (n,u)=0 and (u,u)=1,
 *     t = (xpmt - x,u).
 * Limitation:
 *   r->u must be a unit vector.
 */
  float xpmt[3];        /* position of PMT */
  float t;              /* parameters of the ray equation */
  float *x=r->x;
  float *u=r->u;
  int i;

  pmt_position(d_geo, pmt, xpmt);
  t = (xpmt[0]-x[0])*u[0] + (xpmt[1]-x[1])*u[1] + (xpmt[2]-x[2])*u[2];
  for(i=0;i<3;i++)
    dv[i] = xpmt[i] - x[i] - t*u[i];
  dv[3] = t;
  return sqrt(dv[0]*dv[0]+dv[1]*dv[1]+dv[2]*dv[2]);
}

#if 0
// ======= helper for PRDF <--> Raw =====
static void check_prdf_data(int *pdata) {
  // This function is for debugging purpose only
  // This function check if the data pointed to *pdata is
  // consistent with (PRE,POST,TAC) triplet of PMT data from RICH FEM/DCM
  //
  // POST...0x00c0000 || (16-bit data)
  // PRE ...0x00a0000 || (16-bit data)
  // TAC ...0x0090000 || (16-bit data)
  int pre_card = (pdata[0] & 0x70000000)>>28; 
  int pre_ch   = (pdata[0] & 0x0fe00000)>>21;
  int pre_tag  = (pdata[0] &   0x1f0000)>>16;  // should be 12 (0x0c)
  int pst_card = (pdata[1] & 0x70000000)>>28; 
  int pst_ch   = (pdata[1] & 0x0fe00000)>>21;
  int pst_tag  = (pdata[1] &   0x1f0000)>>16;  // should be 10 (0x0a)
  int tac_card = (pdata[2] & 0x70000000)>>28; 
  int tac_ch   = (pdata[2] & 0x0fe00000)>>21;
  int tac_tag  = (pdata[2] &   0x1f0000)>>16;  // should be  9 (0x09)
  cout << "PRE:"<<pre_card<<","<<pre_ch<<","<<pre_tag<<endl;
  cout << "PST:"<<pst_card<<","<<pst_ch<<","<<pst_tag<<endl;
  cout << "TAC:"<<tac_card<<","<<tac_ch<<","<<tac_tag<<endl;
}
#endif

static void
fill_raw_from_prdf(Event *event, CrkDAO *dao, dCrkRawWrapper *raw_wrap) {
  //
  // copy RICH data from PRDF to DCRKRAW_ST table.
  //
  // There are several data format this code need to handle.
  // (1) VRDC data format used in the MC PRDF in <year 2001
  // (2) RUN-1 (Year2000) real data format (406)
  // (3) RUN-2 (Year2001) real data format (non-0 suppressed) (1006)
  // (4) RUN-2 (Year2001) real data format (zero suppressed)  (1106)
  //
  // Packet ID: 6001 - 6008, 6011-6018, 6021-6028, 6031-6038 are used.
  //
  static const int max_packets = 40;
  static const int BUFSIZE     = 498;
  static const int packet_base = 6001;
  vector<DCRKRAW_ST> vraw;
  Packet *p;

//  This is regular setting (by T.S. on 5/21/2002)
//  int threshold = 25;

//  This is for pp DST v01 because calibration is not tuned well at this moment (by T.S. on 5/21/2002)
  int threshold = 20;

  //  cout << "fill_raw_from_prdf() is called"<<endl;
  //  cout << "version 6/21/2001"<<endl;
  for(int k=0;k<max_packets;k++) 
    {
      int packet_id = packet_base + k;
      int words;
      if( (p = event->getPacket(packet_id)) != 0) 
	{
	  //      cout << "packet "<<packet_id<<" is found"<<endl;
	  int buffer[BUFSIZE];  // buffer for the data
	  //      cout << "# of words in packet "<<packet_id<<" is "<<words<<endl;
	  //
	  // VRDC (old) MC data. Keep this for just for backward compatibility
	  if( dao->VRDC_format()) 
	    {
	      p->fillIntArray(buffer,  // address of buffer
			      BUFSIZE, // buffer size
			      &words,  // # of words read
			      "DATA"); // omit header info
	      //	cout << "VERY OLD MC data (VRDC format)" << endl;
	      // Packet length:    504 (6 header for packet + 498 words)
	      // User data length: 498 (8 header-like words + 160*3 + 10 trailer)
	      // The order of the data: TAC, PRE, POST (repeated for 160ch)
	      //
	      int *pdata = buffer + 8;     // PMT data start at buffer+8
	      for(int ch=0; ch<160; ch++) 
		{
		  int tac  = (*pdata) & 0x03ff; pdata++;
		  int pre  = (*pdata) & 0x03ff; pdata++;
		  int post = (*pdata) & 0x03ff; pdata++;
		  int adc  = post - pre;
		  if( adc > threshold) 
		    {
		      DCRKRAW_ST raw;
		      raw.pmt = dao->get_PMTid(packet_id,ch);
		      raw.adc = adc;
		      raw.tdc = tac;
		      vraw.push_back(raw);
		    }
		}
	    }   // end of VRDC

 
	  else 
	    {  //Real data
	      // we go packet by packet.

	      for(int ch=0; ch<160; ch++) 
		{
		  //if(k==0) check_prdf_data(pdata);//check if alignment is right
		  int pre  = p->iValue(ch, RICH_PRESAMPLE);
		  int post = p->iValue(ch, RICH_POSTSAMPLE);
		  int tac  = p->iValue(ch, RICH_TIME);
		  int adc  = post - pre;

		  if( adc > threshold) 
		    {
		      DCRKRAW_ST raw;
		      raw.pmt = dao->get_PMTid(packet_id,ch);
		      raw.adc = adc;
		      raw.tdc = tac;
		      vraw.push_back(raw);
		    }
		}
	    }  // end if(real data)

	  delete p; // we are done with the packet. This should be deleted.
	}   // end if (p.... != 0)
    
    }

  // Now vraw contains all PMT data above threshold. Copy it into "dCrkRaw"
  // which is pointed to by raw_wrap.

#ifdef DEBUG
  cout << "RICH hits found:" << vraw.size() << endl;
#endif

  // Sort the result in "pmt" ID order.
 
  sort(vraw.begin(), vraw.end());

  if(raw_wrap->MaxRowCount() < vraw.size())
    {
      raw_wrap->SetMaxRowCount(vraw.size());
    }
  
  raw_wrap->SetRowCount(vraw.size());
  
  copy(vraw.begin(),vraw.end(),raw_wrap->TableData());

}



//=== Temporary ====
void TCrkModule::temp(void) {/* Do nothing */}

