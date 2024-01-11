#ifndef __mFvtxFindTrackPar_HH__
#define __mFvtxFindTrackPar_HH__

#include<PHObject.h>
#include<FVTXOO.h>
#include<TFvtxParBase.h>

#include <TFvtxGlobalParCntrl.h>

//!  Runtime parameter object for mFvtxFindTrack analysis module
/*! 
 */
class mFvtxFindTrackPar : public TFvtxParBase
{

public:

  /*! Default constructor */
  mFvtxFindTrackPar() :
      _mode(NO_MUTR), _phi_window_width(0.0350), _theta_window_width(4), // Change to be in units of strip widths
      _max_n_hit(5000000), // just make it big
      _max_n_tracks(50000), _rClusCut(0.003), _alphaClusCut(0.0003),
//		_allowTwoHitTracks(false),
//    _allowReverseTracks(false),
//    _filterTwoHitTracks(false),
      _allowTwoHitSVXTracks(false), _twoHitMaxDCAz(0.25),
			_rCutFactor(2.0)
//    _useHICuts(false)
  {
    _allowReverseTracks = false;
//    if (TFvtxGlobalParCntrl::get_bool_par("is_pp"))
//      {
//        _allowTwoHitTracks = true;
//        _filterTwoHitTracks = true;
//        _useHICuts = false;
//      }
//    else
//      {
//        _allowTwoHitTracks = false;
//        _filterTwoHitTracks = false;
//        _useHICuts = true;
//      }
  }

  /*! Destructor */
  ~mFvtxFindTrackPar()
  {
  }

  /*! 
   track mode enumeration
   determins which information is retrieved from MC
   to build the tracks
   */
  enum Mode
  {
    USE_MUTR, NO_MUTR
  };

  /*! Track finding mode: seed with MuTr or seed with last plane silicon hits */
  Mode
  get_mode() const
  {
    return _mode;
  }

  /*! Track finding mode: seed with MuTr or seed with last plane silicon hits */
  void
  set_mode(Mode mode)
  {
    _mode = mode;
  }

  /*! Width of phi search window (in radians) for track finding */
  double
  get_phi_window_width() const
  {
    return _phi_window_width;
  }

  /*! Width of phi search window (in radians) for track finding */
  void
  set_phi_window_width(double phi_window_width)
  {
    _phi_window_width = phi_window_width;
  }

  /*! Width of theta search window (in radians) for track finding */
  double
  get_theta_window_width() const
  {
    return _theta_window_width;
  }

  /*! Width of theta search window (in radians) for track finding */
  void
  set_theta_window_width(double theta_window_width)
  {
    _theta_window_width = theta_window_width;
  }

  /*! return clone tracks limit for mFvtxFindTrack::clone_trk()*/
  unsigned short
  get_max_n_tracks() const
  {
    return _max_n_tracks;
  }

  /*! set clone tracks limit for mFvtxFindTrack::clone_trk()*/
  void
  set_max_n_tracks(unsigned short max)
  {
    _max_n_tracks = max;
  }

  /*! max FVTX clusters to be processed per FVTX + VTX detector for one 48 azimuthal slice*/
  unsigned int
  get_max_n_hit() const
  {
    return _max_n_hit;
  }

  /*! max FVTX clusters to be processed per FVTX + VTX detector for one 48 azimuthal slice*/
  void
  set_max_n_hit(unsigned int max)
  {
    _max_n_hit = max;
  }

  /*! set Hough pattern matching cut on pair r intercept*/
  void
  set_rClusCut(double val)
  {
    _rClusCut = val;
  }

  double
  get_rClusCut() const
  {
    return _rClusCut;
  }

  /* set Hough pattern matching cut on pair sin(alpha)*/
  void
  set_alphaClusCut(double val)
  {
    _alphaClusCut = val;
  }

  double
  get_alphaClusCut() const
  {
    return _alphaClusCut;
  }

	/* set Hough pattern matching cuts, additional factor for pairs with VTX/FVTX ST4 hit*/
  void
  set_rCutFactor(double val)
  {
    _rCutFactor = val;
  }

  double
  get_rCutFactor() const
  {
    return _rCutFactor;
  }

  /* set save two hit tracks after primary PR */
  void
  set_allowTwoHitTracks(bool val)
  {
    obsolete_warning("set_allowTwoHitTracks()",
        "TFvtxGlobalParCntrl::set_bool_flag(\"is_pp\")");
    TFvtxGlobalParCntrl::set_bool_par("is_pp", val);
  }

  bool
  get_allowTwoHitSVXTracks() const
  {
    return _allowTwoHitSVXTracks;
  }

  /* set ALL hit combos from the inner two layers */
  void
  set_allowTwoHitSVXTracks(bool val)
  {
    _allowTwoHitSVXTracks = val;
  }

  bool
  get_allowTwoHitTracks() const
  {
    return TFvtxGlobalParCntrl::get_bool_par("is_pp");
  }

  /* set look for tracks that point away from the IP */
  void
  set_allowReverseTracks(bool val)
  {
    _allowReverseTracks = val;
  }

  bool
  get_allowReverseTracks() const
  {
    return _allowReverseTracks;
  }

  /* set filtering of two hit tracks for duplicates */
  void
  set_filterTwoHitTracks(bool val)
  {
    obsolete_warning("set_filterTwoHitTracks()",
        "TFvtxGlobalParCntrl::set_bool_flag(\"is_pp\")");
    TFvtxGlobalParCntrl::set_bool_par("is_pp", val);
  }

  bool
  get_filterTwoHitTracks() const
  {
    return TFvtxGlobalParCntrl::get_bool_par("is_pp");
  }

//  /* set use of geometrical restrictions to speed up HI reco */
  void
  set_useHICuts(bool val)
  {
    obsolete_warning("set_useHICuts()",
        "TFvtxGlobalParCntrl::set_bool_flag(\"is_pp\")");
    TFvtxGlobalParCntrl::set_bool_par("is_pp", !val);
  }

  bool
  get_useHICuts() const
//  {
//    return !TFvtxGlobalParCntrl::get_bool_par("is_pp");
//  }
  {
    return not TFvtxGlobalParCntrl::get_bool_par("is_pp");
  }

  /* set maximum DCA z value for two hit tracking */
  void
  set_twoHitMaxDCAz(double val)
  {
    _twoHitMaxDCAz = val;
  }

  double
  get_twoHitMaxDCAz() const
  {
    return _twoHitMaxDCAz;
  }

  //! dump all parameters
  void
  print(std::ostream& out = std::cout) const
  {
    FVTXOO::PRINT(out, "mFvtxFindTrackPar");
    out << "_mode = " << _mode << std::endl;
    out << "_phi_window_width = " << _phi_window_width << std::endl;
    out << "_theta_window_width = " << _theta_window_width << std::endl;
    out << "_rClusCut = " << _rClusCut << std::endl;
    out << "_alphaClusCut = " << _alphaClusCut << std::endl;
    out << "_max_n_hit = " << _max_n_hit << std::endl;
    out << "_max_n_tracks = " << _max_n_tracks << std::endl;
    out << "_allowReverseTracks = " << get_allowReverseTracks() << std::endl;
    out << "_allowTwoHitTracks = " << get_allowTwoHitTracks() << std::endl;
    out << "_allowTwoHitSVXTracks = " << get_allowTwoHitSVXTracks() << std::endl;
    out << "_filterTwoHitTracks = " << get_filterTwoHitTracks() << std::endl;
    out << "_twoHitMaxDCAz = " << get_twoHitMaxDCAz() << std::endl;
    out << "_useHICuts = " << get_useHICuts() << std::endl;
    out << "_rCutFactor = " << get_rCutFactor() << std::endl;
  }

private:

  Mode _mode;
  double _phi_window_width;
  double _theta_window_width;
  unsigned int _max_n_hit;
  unsigned short _max_n_tracks;
  double _rClusCut;
  double _alphaClusCut;
//	bool _allowTwoHitTracks;
  bool _allowTwoHitSVXTracks;
  bool _allowReverseTracks;
//  bool _filterTwoHitTracks;
  double _twoHitMaxDCAz;
//  bool _useHICuts;
	double _rCutFactor;

  static void
  obsolete_warning(const std::string & function,
      const std::string & suggestion);

ClassDef(mFvtxFindTrackPar,2)
  ;
};

#endif /* __mFvtxFindTrackPar_HH__ */

