#ifndef _TMUTSTUB_H_
#define _TMUTSTUB_H_

// $Id: TMutStub.hh,v 1.10 2014/01/26 17:55:14 bbannier Exp $

/*!
   \file TMutStub.hh
   \brief Class for Muon Tracker stubs
   \author S. Kelly
   \version $Revision: 1.10 $
   \date $Date: 2014/01/26 17:55:14 $
*/

#include<PHKey.hh>
#include<MUTOO.h>
#include<PHVector.h>
#include<vector>
#include<TMutFitPar.hh>
#include<TMutTrkRes.hh>


/*! @ingroup interface */
//!  The Muon tracker Stub object 
/*! 
  <b>The MUTR Stub Object </b><br>
  
  The Muon track stub object presents an interface to a TMutFitPar object 
  which includes the xyz position where the tracklet pass though each cathode
  plane and the dx/dz, dy/dz ( orientation ) of the tracklet.
  
  The hits contributing to a tracklet are localized to a single station.
  <p>
*/

class TMutStub : public PHKey {
  
public:
  
  //! Name of the list of coord points contained in the object 
  typedef std::vector<TMutTrkRes> residual_list;
  
  //! Name of a const iterator to point_list 
  typedef std::vector<TMutTrkRes>::const_iterator const_residual_iterator;
  
  //! @name Constructors/Destructors
  //@{  
  //! Default constructor 
  TMutStub()
  {}  
  
  //! Default destructor 
  virtual ~TMutStub()
  {}
  
  //! Constructor with key 
  TMutStub(const Key& key) : PHKey(key)
  {}
  
  //@}  
  
  //! @name Fit Parameters Interface
  //@{  
  //! Read only reference to TMutFitPar 
  virtual const TMutFitPar* get_fit_par() const 
  {return 0;}
  
  //! TMutFitPar (track pars in geometric units) 
  virtual void set_fit_par(const TMutFitPar* fit_par) 
  {}

  //! TMutFitPar (track pars in geometric units) 
  virtual void set_fit_par(const TMutFitPar& fit_par) 
  {}

  //! Upstream z 
  virtual void set_z_begin(double z_begin) 
  {}

  //! Downstream z 
  virtual void set_z_end(double z_begin) 
  {}

  //! Return a normalized vector in the direction of the closest gap0 anode wire 
  virtual PHVector get_anode_direction() const 
  { return PHVector(); }

  //@}
  
  //! @name Residual List Interface
  //@{  
  
  //! Push a TMutTrkRes object onto w residual list 
  virtual void push_w_residual(const TMutTrkRes& residual)
  {}
    
  //! Clear w residual list 
  virtual void clear_w_residual_list()
  {}
  
  //! Number of w residuals 
  virtual size_t get_n_w_residual() const 
  { return 0;}

  //! Read only access to w residual list 
  virtual const residual_list* get_w_residual_list() const 
  {return 0;} 

  //! Push r residual onto r residual list 
  virtual void push_r_residual(double residual)
  {}
  
  //! Clear r residual list 
  virtual void clear_r_residual_list()
  {}

  //! Number of r residuals 
  virtual size_t get_n_r_residual() const 
  {return 0;}

  //! Read only access to r residual list 
  virtual const std::vector<double>* get_r_residual_list() const 
  { return 0;} 

  //! Number of associated TMutCoord 
  virtual UShort_t get_n_coord() const 
  {return 0;}
  
  //! Number of associated TMutGapCoord 
  virtual UShort_t get_n_gap_coord() const 
  {return 0;}
  
  //! @name Merit Statistics Interface
  //@{  
  
  //! Chi square statistic of w fit (sum of square of w pulls) 
  virtual double get_w_chi_square() const 
  { return 0; }
  
  //! Chi square statistic of r fit (sum of square of r pulls) 
  virtual double get_r_chi_square() const 
  { return 0; }
  
  //! Total chi square (w_chi_square + r_chi_square) 
  virtual double get_chi_square() const 
  { return 0; }
  
  //! Chi square statistic of w fit (sum of square of w pulls)   
  virtual void set_w_chi_square(double w_chi_square) 
  {}
  
  //! Chi square statistic of r fit (sum of square of r pulls) 
  virtual void set_r_chi_square(double r_chi_square) 
  {}
  
  //! Chi square pdf (w_chi_square/(ncoord-4) 
  virtual double get_chi_pdf() const 
  { return 0; }
  
  //@}

  //! @name Windows Interface
  //@{  
  
  //! Phi window (radians) 
  virtual double get_phi_min() const 
  { return 0; }
  
  //! Phi window (radians) 
  virtual double get_phi_max() const 
  { return 0; }
  
  //! Phi window (radians) 
  virtual void set_phi_min(double phi_min)
  {}
  
  //! Phi window (radians) 
  virtual void set_phi_max(double phi_max)
  {}
  
  //! Theta window (radians) 
  virtual double get_theta_min() const 
  { return 0; }
  
  //! Theta window (radians) 
  virtual double get_theta_max() const 
  { return 0; }
  
  //! Theta window (radians) 
  virtual void set_theta_min(double theta_min)
  {}
  
  //! Theta window (radians) 
  virtual void set_theta_max(double theta_max)
  {}

  #ifndef __CINT__
  
  //! Returns current stub theta 
  virtual double get_theta() const 
  {return 0;}
  
  //! Returns true if this stub's theta in given window 
  virtual bool check_theta_window(const std::pair<float,float>& theta_window)
  {return false;}
  
  //! Returns current stub theta 
  virtual double get_phi() const 
  {return 0;}
  
  //! Returns true if this stub's phi in given window 
  virtual bool check_phi_window(const std::pair<float,float>& phi_window) const 
  {return false;}
  
  //! Returns true if this stub has an associated TMutGapCoord 
  virtual bool has_point() const 
  { return 0; }
  
  //! Project stub tangent onto w-z plane and calculate dwdz 
  virtual double get_dwdz() const 
  { return 0; }
#endif
  //@}
  
  //! @name Locators
  //@{    
  
  #ifndef __CINT__
  //! return class ID, mapped from class name
  virtual PHClassId::id_type get_class_id( void ) const
  { return (_class_id) ? _class_id : (_class_id = PHClassId::get( GetName() ) ); }
  #endif

  
  //! Arm [0,1] 
  virtual void set_arm(UShort_t arm) 
  {}
  
  //! Station [0,2] 
  virtual void set_station(UShort_t station) 
  {}
  
  //! Octant [0,7] 
  virtual void set_octant(UShort_t octant) 
  {}
  
  //! Half-octant [0,1] 
  virtual void set_half_octant(UShort_t hoct) 
  {}
  
  //! Index [0,1024] 
  virtual void set_index(UShort_t index) 
  {}
  
  //! Arm [0,1] 
  virtual UShort_t get_arm() const 
  {return 0;}
  
  //! Station [0,2] 
  virtual UShort_t get_station() const 
  {return 0;}
  
  //! Octant [0,7] 
  virtual UShort_t get_octant() const 
  {return 0;}
  
  //! Half-octant [0,1] 
  virtual UShort_t get_half_octant() const 
  {return 0;}
  
  //! Index [0,1024] 
  virtual UShort_t get_index() const 
  {return 0;}
  
  //@}
  
  //! @name Dumpers
  //@{  
  /*! 
    Print the TMutStub to given ostream.  If max is true print will
    dump the list of residuals also.
  */
  virtual void print(std::ostream& os, bool max) const {}
  virtual void print(std::ostream& os = std::cout) const { print(os, false); }
  //@}
  

  private:
  
#ifndef __CINT__
  
  //! static class ID 
  static PHClassId::id_type _class_id;

#endif
  
  ClassDef(TMutStub,1)
};

#endif
