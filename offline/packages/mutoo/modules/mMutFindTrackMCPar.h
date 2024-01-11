#ifndef __MMUTFINDTRACKPARMC_HH__
#define __MMUTFINDTRACKPARMC_HH__

#include<PHObject.h>
#include<MUTOO.h>
#include<TMutParBase.h>

//!  Runtime parameter object for mMutFindTrackMC analysis module
/*! 
*/
class mMutFindTrackMCPar : public TMutParBase
{
  
 public: 
  
  /*! Default constructor */
  mMutFindTrackMCPar() :
    _init_mode(MINIMAL),
    _vtx_mode(VTX_PERFECT),
    _use_ms_covar(0)
    {;}
  
  /*! Destructor */
  ~mMutFindTrackMCPar(){;}
  
  enum TrkMode {MINIMAL,PERFECT};

  enum VtxMode {VTX_NONE,VTX_MINIMAL,VTX_MAXIMAL,VTX_PERFECT};

  /*! Mode for initialization of TMutTrk from TMutMCTrack */
  TrkMode get_init_mode() const { return _init_mode;}

  /*! Mode for initialization of TMutTrk from TMutMCTrack */
  void set_init_mode(TrkMode mode) {_init_mode = mode;}

  /*! Mode for initialization of TMutVtx from TMutMCTrack */
  VtxMode get_vtx_mode() const { return _vtx_mode;}

  /*! Mode for initialization of TMutVtx from TMutMCTrack */
  void set_vtx_mode(VtxMode mode) {_vtx_mode = mode;}

  /*! Use multiple scattering covariance matrix */
  unsigned short get_use_ms_covar() const { return _use_ms_covar;}

  /*! Use multiple scattering covariance matrix */
  void set_use_ms_covar(unsigned short use_ms_covar) {_use_ms_covar = use_ms_covar;}

 private:  
  
  TrkMode _init_mode;
  VtxMode _vtx_mode;
  unsigned short _use_ms_covar;

};

#endif /* __MMUTFINDTRACKMCPAR_HH__ */





