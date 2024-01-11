#ifndef __EMC_GEARAWDATA_SIMMAKER_H__
#define __EMC_GEARAWDATA_SIMMAKER_H__





#include <vector>

#include <Rtypes.h>
#include <TRandom.h>

#include <SubsysReco.h>
#include <emcGeaEdep.h>



class PHCompositeNode;
class emc_geometry_t;
class emc_gea_params_t;
class emcGeaParams;
class EmcGeaRawData;

inline void norran_(float * x){ *x = gRandom->Gaus(); }

class EmcGeaRawDataSimMaker: public SubsysReco {

public:
  //
  // pbsc_timing: steering timing simulation
  //
  // 0 -> Use full pulse reconstruction + LED (default)
  // 1 -> Use time of first GEANT hit w/o corrections
  // 2 -> Derivative Zero Crossing
  //
  // was:  typedef enum { TIMING_PULSE_SHAPE_RECONSTRUCTION = 0, TIMING_SIMPLE = 1 } sim_timing_t;
  //
  typedef enum { PBSC_TIMING_LED = 0, PBSC_TIMING_SIMPLE = 1, PBSC_TIMING_DZC = 2 } pbsc_timing_t;



  //
  // pbgl_timing: steering PbGl response simulation
  //
  // 0 -> Cumulate GEANT hits (default)
  // 1 -> Cumulate GEANT hits but mix in photoelectron,
  //      noise effects 
  // 2 -> Cumulate GEANT hits but modify with empirical
  //      simple parametrization 
  //      This needs total deposited energy without
  //      attenuation
  // 3 -> Cherenkov photons were generated, do NOT attenuate!
  //
  //was: typedef enum { PBGL_RESP_ADD = 0, PBGL_RESP_NOISE = 1, PBGL_RESP_PARM = 2, PBGL_RESP_NOATTEN = 3 } pbgl_resp_t;
  //
  typedef enum { PBGL_TIMING_SIMPLE = 0, PBGL_TIMING_SMEARING = 1, 
		 PBGL_TIMING_PARAMETRIC = 2, PBGL_TIMING_CHERENKOV = 3 } pbgl_timing_t;


public:
  EmcGeaRawDataSimMaker(pbsc_timing_t pbsc = PBSC_TIMING_LED, pbgl_timing_t pbgl = PBGL_TIMING_CHERENKOV);
  ~EmcGeaRawDataSimMaker();


public:
  int Init(PHCompositeNode * root);
  int InitRun(PHCompositeNode * root);
  int process_event(PHCompositeNode * root);
  int Reset(PHCompositeNode * root);
  int ResetEvent(PHCompositeNode * root){ return Reset(root); }
  int End(PHCompositeNode * root){ return Reset(root); }


protected:
  inline xdep_t * deposit_energy(xdep_t const * const prevhit, xdep_t const * const hit, xdep_t const * const nexthit);

  inline void distribute_light(xdep_t * newdeposit, emc_gea_params_t * geapar, emc_geometry_t * geom);

  inline void calculate_response(std::vector< xdep_t * > & deposits, emcGeaParams * geaparms);
  inline void calculate_response_pbsc(std::vector< xdep_t * > & deposits, float & tof, float & edep, emc_gea_params_t * p);
  inline void calculate_response_pbgl(std::vector< xdep_t * > & deposits, float & tof, float & edep, emc_gea_params_t * p);




public: /* data loaded from PH nodes */
  void SetPbScTiming(pbsc_timing_t parm){ pbsc_timing = parm; }
  pbsc_timing_t GetPbScTiming() const { return pbsc_timing; }

  void SetPbGlTiming(pbgl_timing_t parm){ pbgl_timing = parm; }
  pbgl_timing_t GetPbGlTiming() const { return pbgl_timing; }


  pbsc_timing_t pbsc_timing;
  pbgl_timing_t pbgl_timing;



protected: /* for unused position uniformity correction */
  const static int l_correct_unif = false; 
  static float emc_pos_unif[24][24];
  static float cellsize;





protected: // burnt in constants for edep collection
#ifdef _COMPATIBLE_VERSION
  const static float e_cutoff   = 0.00001;
#else
  const static float e_cutoff   = 0.0;
#endif
  const static float e_rescale  = 1.08;
  const static float dele_small = 0.0005;
  const static float dele_max   = 0.002;




protected: /* burnt in parameters for timing */
  // common for timing
  const static float p_gatemax = 127.0;        /* 17 ns + 110 ns for clock */

  // pbsc timing
  const static float timethresh = 0.05;
  const static float timebin    = 0.05;        /*	 50 ps bins	*/
  //  const static short maxtimebin = 300;
  const static short maxtimebin = 1200;
  const static float timeoffset = 17.0;
  const static float pbsc_noise = 0.003;

  // pbgl timing
  const static float pbgl_resolution    = 0.06;
  const static float pbgl_noise         = 0.01;
  const static float pbgl_tofresolution = 0.25;
  const static float pbgl_tofoffset     = 2.5;
  const static float pbgl_rescale       = 1.18;
  const static float pbgl_ctrk_rescale  = 1.0;



  
  









protected: /* other */
  // limits due to STAFF tables
  const static int  p_maxtowerhit = 30;
  




  ClassDef(EmcGeaRawDataSimMaker, 0)
};





#endif /* ! __EMC_GEARAWDATA_SIMMAKER_H__ */

