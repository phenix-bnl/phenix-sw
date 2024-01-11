#ifndef __mFvtxFindTrackMCPar_HH__
#define __mFvtxFindTrackMCPar_HH__

#include<PHObject.h>
#include<FVTXOO.h>
#include<TFvtxParBase.h>

//!  Runtime parameter object for mFvtxFindTrackMC analysis module
/*! 
*/
class mFvtxFindTrackMCPar : public TFvtxParBase
{
  
 public: 
  
  /*! Default constructor */
  mFvtxFindTrackMCPar() :
    _init_mode(MINIMAL),
    _use_vtx_hits( true ),
    _hit_min_r(3.5)
    {}
  
  /*! Destructor */
  ~mFvtxFindTrackMCPar()
  {}
  
  /*! 
  track mode enumeration 
  determins which information is retrieved from MC
  to build the tracks
  */ 
  enum TrkMode {
    MINIMAL,
    PERFECT
  };

  /*! Mode for initialization of TFvtxTrk from TFvtxMCTrack */
  const TrkMode& get_init_mode() const 
  { return _init_mode;}

  /*! Mode for initialization of TFvtxTrk from TFvtxMCTrack */
  void set_init_mode( const TrkMode& mode) 
  {_init_mode = mode;}

  /*! use barrel vertex hits */
  const bool& get_use_vtx_hits( void ) const
  { return _use_vtx_hits; }
  
  /*! use barrel vertex hits */
  void set_use_vtx_hits( const bool& value ) 
  { _use_vtx_hits = value; }
 
  /*! Minimum r that a MC hit must have to be associated to a track*/
  const float& get_hit_min_r( void ) const
  { return _hit_min_r; }
  
  /*! use barrel vertex hits */
  void set_hit_min_r( const float& value ) 
  { _hit_min_r = value; }

  //! dump all parameters
  void print(std::ostream& out = std::cout) const {
    FVTXOO::PRINT(out, "mFvtxFindTrackMCPar");
    out << "_init_mode = " << _init_mode << std::endl;
    out << "_use_vtx_hits = " << _use_vtx_hits << std::endl;
    out << "_hit_min_r = " << _hit_min_r << std::endl;
  }

  private:  
  
  TrkMode _init_mode;
  
  //! add vtx MC hits to track or not
  bool _use_vtx_hits;
  float _hit_min_r;

  ClassDef(mFvtxFindTrackMCPar,1);
};

#endif /* __mFvtxFindTrackMCPar_HH__ */





