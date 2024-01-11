#ifndef __TMUTTRKRES_H__
#define __TMUTTRKRES_H__

#include<TDataType.h>
#include<TMutFitPar.hh>
#include<MUTOO.h>

/*! @ingroup classes */
//!  The Muon tracker Track Residual object 

/*! 
  <b>The MUTR Track Residual Object</b><br>
  TMutTrkRes presents an interface to data associated with an active detector
  element that has been used to constrain a track.  An array of these objects
  can be stored in the TMutTrk object and then accessed at the DST level for
  detector resolution studies.  Potentially one of these object  exists
  for every track hit association, hence given speed considerations the 
  ability to fill and store TMutTrkRes objects in TMutTrks is controllable 
  via run-time parameter in the track reconstruction module mMutTrackFit.
*/

typedef unsigned short UShort_t;
typedef unsigned long ULong_t;

class TMutTrkRes : public TObject
{
public:

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */
  TMutTrkRes(){;}  

  /*! Virtual destructor */
  virtual ~TMutTrkRes(){;}   

#ifndef __CINT__
  // CINT doesn't understand locators so we hide the non-default 
  // constructor
  //
  TMutTrkRes(const MUTOO::cathode_locator& location,
	     const TMutFitPar& fit_par,
	     double cos_theta_ac,
	     double w_trk,    
	     double w_meas,    
	     double r_trk,
	     double cos_theta_wz,
	     double cos_theta_r,
	     double q_peak,
	     double q_tot,
	     double w_fit_error,
	     UShort_t clus_width,
	     UShort_t peak_strip,
	     ULong_t status) :
    _arm(location.get<0>()),
    _station(location.get<1>()),
    _octant(location.get<2>()),
    _half_octant(location.get<3>()),
    _gap(location.get<4>()),
    _cathode(location.get<5>()),
    _fit_par(fit_par),
    _r_trk(r_trk),
    _w_trk(w_trk),
    _w_meas(w_meas),
    _cos_theta_ac(cos_theta_ac),
    _cos_theta_r(cos_theta_r),
    _cos_theta_wz(cos_theta_wz),
    _q_peak(q_peak),
    _q_tot(q_tot),
    _w_fit_error(w_fit_error),
    _clus_width(clus_width),
    _z_begin(0),
    _z_end(0),
    _peak_strip(peak_strip),
    _status(status)
  {;}
#endif
  //@}

  //! @name Functional Interface
  //@{  
  /*! Read only pointer to TMutFitPar */
  const TMutFitPar* get_fit_par() const {return &_fit_par;}
  /*! w coordinate of track (relative to center of peak strip) */
  double get_w_trk() const { return _w_trk;}
  /*! w coordinate of cluster centroid (relative to center of peak strip) */
  double get_w_meas() const { return _w_meas;}
  /*! Coordinate of track relative to center of strip, along strip dir */
  double get_r_trk() const { return _r_trk;}  
  /*! Angle between anode and cathdoe wire (radians) */
  double get_cos_theta_ac() const { return _cos_theta_ac; }
  /*! Angle the projection of the track onto the zw plane makes with the w-axis */
  double get_cos_theta_wz() const { return _cos_theta_wz;}
  /*! Angle the projection of the track onto the zw plane makes with the w-axis */
  double get_cos_theta_r() const { return _cos_theta_r;}
  /*! Fit peak charge */
  double get_q_peak() const { return _q_peak;}
  /*! Fit total charge */
  double get_q_tot() const { return _q_tot;}
  /*! Upstream endpoint of track */
  double get_z_begin() const { return _z_begin;}
  /*! Downstream endpoint of track */  
  double get_z_end() const { return _z_end;}
  /*! Fit error propagated through measurment model */
  double get_w_fit_error() const { return _w_fit_error;}
  /*! Number of strips in underlying cluster */
  UShort_t get_clus_width() const { return _clus_width;}
  /*! Peak strip in in underlying cluster */
  UShort_t get_peak_strip() const { return _peak_strip;}
  /*! Get status */
  ULong_t get_status() const { return _status;}
  
  /*! Read only reference to TMutFitPar */
  void set_fit_par(const TMutFitPar& fit_par) { _fit_par = fit_par; }
  /*! w coordinate of track (relative to center of peak strip) */
  void set_w_trk(double w_trk) { _w_trk = w_trk; }
  /*! w coordinate of cluster centroid (relative to center of peak strip) */
  void set_w_meas(double w_meas) { _w_meas = w_meas; }
  /*! Coordinate of track relative to center of strip, along strip dir */
  void set_r_trk(double r_trk) { _r_trk = r_trk;} 
  /*! Angle between anode and cathdoe wire (radians) */
  void set_cos_theta_ac(double cos_theta_ac) { _cos_theta_ac = cos_theta_ac; }
  /*! Angle the projection of the track onto the zw plane makes with the w-axis */
  void set_cos_theta_wz(double cos_theta_wz) { _cos_theta_wz = cos_theta_wz; }
  /*! Angle between the track and the strip */
  void set_cos_theta_r(double cos_theta_r) { _cos_theta_r = cos_theta_r; }
  /*! Fit peak charge */
  void set_q_peak(double q_peak) { _q_peak = q_peak;}
  /*! Fit total charge */
  void set_q_tot(double q_tot) { _q_tot = q_tot;}
  /*! Set w fit error */
  void set_w_fit_error(double w_fit_error) { _w_fit_error = w_fit_error;}
  /*! Fit error propagated through measurment model */
  void set_z_begin(double z_begin) { _z_begin = z_begin; }
  /*! Downstream endpoint of track */
  void set_z_end(double z_end) { _z_end = z_end; }
  /*! Number of strips in underlying cluster */
  void set_clus_width(UShort_t clus_width) { _clus_width = clus_width;}
  /*! Number peak strip of underlying cluster */
  void set_peak_strip(UShort_t peak_strip) { _peak_strip = peak_strip;}
  /*! Number peak strip of underlying cluster */
  void set_status(ULong_t status) { _status = status;}

  //@}

  //! @name Locators
  //@{  
  /*! Arm [0,1] */
  UShort_t  get_arm() const {return _arm;}
  /*! Station [0,2] */
  UShort_t  get_station() const {return _station;}
  /*! Octant [0,7] */
  UShort_t  get_octant() const {return _octant;}
  /*! Half octant [0,1] */
  UShort_t  get_half_octant() const {return _half_octant;}
  /*! Gap [0,2] */
  UShort_t  get_gap() const {return _gap;}
  /*! Index [0,1023] */
  UShort_t  get_cathode() const {return _cathode;}
  /*! Arm [0,1] */
  void set_arm(UShort_t arm){ _arm = arm; }
  /*! Station [0,2] */
  void set_station(UShort_t station){ _station = station; }               	
  /*! Octant [0,7] */
  void set_octant(UShort_t octant){ _octant = octant; }               
  /*! Half octant [0,1] */
  void set_half_octant(UShort_t half_octant){ _half_octant = half_octant; }               
  /*! Gap [0,2] */
  void set_gap(UShort_t gap){ _gap = gap; }               
  /*! Cathode [0,1] */
  void set_cathode(UShort_t cathode){ _cathode = cathode; }               
  //@}

  //! @name Dumpers
  //@{  
  void print(std::ostream& os = std::cout) const {
    MUTOO::PRINT(os,GetName());
    os << " arm: " << _arm
       << "  station: " << _station
       << "  octant: " << _octant
       << "  half octant: " << _half_octant
       << "  gap: " << _gap
       << "  cathode: " << _cathode << std::endl;
    
    // dump fit parameters
    //  
    os << " fit parameters = {";
    os << "x:" << _fit_par.get_x() << ", ";
    os << "y:" << _fit_par.get_y() << ", ";
    os << "dxdz: " << _fit_par.get_dxdz() << ", ";
    os << "dydz: " << _fit_par.get_dydz() << "}" << "   ";    
    os << " z reference: " << _fit_par.get_z() << std::endl;  
    
    os << " w_trk: " << _w_trk << std::endl;
    os << " w_meas: " << _w_meas << std::endl;
    os << " residual: " << _w_trk-_w_meas << std::endl;
    os << " r_trk: " << _r_trk << std::endl;
    os << " cos theta anode/cathode: " << _cos_theta_ac << std::endl;
    os << " cos theta wz: " << _cos_theta_wz << std::endl;
    os << " cos theta r: " << _cos_theta_r << std::endl;
    os << " w_fit_error: " << _w_fit_error << std::endl;
    os << " cluster width: " << _clus_width << std::endl;
    MUTOO::PRINT(os,"**");
  }
  //@}
private:
  
  UShort_t _arm;
  UShort_t _station;
  UShort_t _octant;
  UShort_t _half_octant;
  UShort_t _gap;
  UShort_t _cathode;

  TMutFitPar _fit_par;
  double _r_trk;
  double _w_trk;
  double _w_meas;
  double _cos_theta_ac;
  double _cos_theta_r;
  double _cos_theta_wz;
  double _q_peak;
  double _q_tot;
  double _w_fit_error;
  UShort_t _clus_width;
  double _z_begin;
  double _z_end;
  UShort_t _peak_strip;
  ULong_t _status;
  ClassDef(TMutTrkRes,1)
};

#endif
