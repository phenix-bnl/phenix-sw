// $Id: mFvtxKalFitPar.h,v 1.12 2014/03/07 16:19:54 jinhuang Exp $
#ifndef __MFVTXKALFITPAR_H__
#define __MFVTXKALFITPAR_H__

/*!
   \file    mFvtxKalFitPar.h
   \brief   Runtime parameter object for mFvtxKalFit analysis module
   \author  Melynda Brooks
   \version $Revision: 1.12 $
   \date    $Date: 2014/03/07 16:19:54 $
*/

#include<FVTXOO.h>
#include<PHObject.h>
#include<PHException.h>
#include<TFvtxParBase.h>
#include<set>

/*! 
  \class mFvtxKalFitPar
  \brief Runtime parameter object for mFvtxKalFit analysis module
*/

class mFvtxKalFitPar : public TFvtxParBase
{	
 public: 

	//! shortcut for pdetector set
	typedef std::set< int > detector_set;

  /*! default constructor */
  mFvtxKalFitPar():
    _momentum_resolution( 0.5 ),
    _angular_resolution( 0.2 ),
    _position_resolution( 100 ),
    _z_reference( 0 ),
    _chi_cut( 0.01 ),
    _min_n_coord( 3 ),
    _max_iterations( 100 ),
//    _use_vtx( false ),
    _use_mutr( false ),
    _use_muid( false ),
    _update_cov_matrix( true ),
    _extrapolate_to_mutr( true ),
    _do_evaluation( false ),
    _evaluation_file( "mFvtxKalFit.root" ),
    _vtx_z_smear(0.0400),
    _vtx_phi_smear(0.0050),
    _vtx_phi_error(0.0014), // intrinsic detector resolution (50 micron pixel)
    _vtx_r_error(0.0075),   // based on Dec 2013 alignment capabilities
    _vtx_z_error(0.0123)    // intrinsic detector resolution (425 micron pixel)
  { 
    
		
  }
	
  //! destructor
  ~mFvtxKalFitPar(){ }	
  
  //! PHOOL inteface requirement
  void identify(std::ostream& os = std::cout) const 
  { os << "mFvtxKalFitPar";}

  //! gets starting point momentum resolution
  Double_t get_momentum_resolution( void ) const 
  { return _momentum_resolution; }

  //! sets starting point momentum resolution
  void set_momentum_resolution( Double_t value ) 
  { _momentum_resolution = value; }

  //! gets starting point angular resolution
  Double_t get_angular_resolution( void ) const 
  { return _angular_resolution; }	
  
  //! sets starting point angular resolution
  void set_angular_resolution( Double_t value ) 
  { _angular_resolution = value; }	

  //! gets starting point position resolution
  Double_t get_position_resolution( void ) const 
  { return _position_resolution; }
  
  //! sets starting point position resolution
  void set_position_resolution( Double_t value ) 
  { _position_resolution = value; }

  //! gets reference z for extrapolation upstream of the absorber
  Double_t get_z_reference( void ) const 
  { return _z_reference; }
  
  //! sets reference z for extrapolation upstream of the absorber
  void set_z_reference( Double_t value ) 
  { _z_reference = value; }

  //! gets min number of coordinates / track to be fitted
  unsigned short get_min_n_coord( void ) const 
  { return _min_n_coord; } 
  
  //! sets min number of coordinates / track to be fitted
  void set_min_n_coord( unsigned short value ) 
  { _min_n_coord = value; }
  
  //! sets max number of iterations during fit
  void set_max_iterations( unsigned short value ) 
  { _max_iterations = value; }
 
  //! gets max number of iterations during fit
  unsigned short get_max_iterations( void ) const 
  { return _max_iterations; } 

  //! gets cut on relative chi_square difference to stop iterations
  Double_t get_chi_cut( void ) const 
  { return _chi_cut; } 
 
  //! sets cut on relative chi_square difference to stop iterations
  void set_chi_cut( Double_t value ) 
  { _chi_cut = value; }

//  //! gets use_vtx flag
//  bool get_use_vtx() const
//  { return _use_vtx; }
//
//  //! sets use_vtx flag
//  void set_use_vtx( bool value )
//  { _use_vtx = value; }
 
  //! gets use_mutr flag
  bool get_use_mutr() const 
  { return _use_mutr; }
 
  //! sets use_mutr flag
  void set_use_mutr( bool value ) 
  { _use_mutr = value; }
 
  //! gets use_muid flag
  bool get_use_muid() const 
  { return _use_muid; }
 
  //! sets use_muid flag
  void set_use_muid( bool value ) 
  { _use_muid = value; }
 
  //! gets update_cov_matrix (from starting point, between iterations)
  bool get_update_cov_matrix() const 
  { return _update_cov_matrix; }
  
  //! sets update_cov_matrix (from starting point, between iterations)
  void set_update_cov_matrix( bool value ) 
  { _update_cov_matrix = value; }
  
  //! extrapolation to mutr
  bool get_extrapolate_to_mutr( void ) const
  { return _extrapolate_to_mutr; }
  
  //! extrapolation to mutr
  void set_extrapolate_to_mutr( bool value )
  { _extrapolate_to_mutr = value; }

  //! gets histogram flag
  bool get_do_evaluation( void ) const 
  { return _do_evaluation; }
  
  //! sets histogram flag
  void set_do_evaluation( bool value ) 
  { _do_evaluation = value; }

  //! gets histogram file
  std::string get_evaluation_file( void ) const 
  { return _evaluation_file; }
  
  //! sets histogram file
  void set_evaluation_file( const std::string& value )
  { _evaluation_file = value; }
 
  //! semaring to add to z position of vtx hit (cm)
  Double_t get_vtx_z_smear( void ) const 
  { return _vtx_z_smear; } 
 
  //! sets smearing of vtx z position
  void set_vtx_z_smear( Double_t value ) 
  { _vtx_z_smear = value; }

  //! semaring to add to phi position of vtx hit (cm)
  Double_t get_vtx_phi_smear( void ) const 
  { return _vtx_phi_smear; } 
 
  //! sets smearing of vtx phi position
  void set_vtx_phi_smear( Double_t value ) 
  { _vtx_phi_smear = value; }

  //! phi error associated with a VTX cluster phi position (cm):
  Double_t get_vtx_phi_error( void ) const
  { return _vtx_phi_error; }

  //! sets phi error associated with a VTX cluster phi position (cm):
  void set_vtx_phi_error( Double_t value )
  { _vtx_phi_error = value; }

  //! r error associated with a VTX cluster r position (alignment error, cm):
  Double_t get_vtx_r_error( void ) const
  { return _vtx_r_error; }

  //! sets r error associated with a VTX cluster r position (alignment error, cm):
  void set_vtx_r_error( Double_t value )
  { _vtx_r_error = value; }

  //! z error associated with a VTX cluster z position
  Double_t get_vtx_z_error( void ) const
  { return _vtx_z_error; }

  //! sets z error associated with a VTX cluster z position:
  void set_vtx_z_error( Double_t value )
  { _vtx_z_error = value; }

  //! dump all parameters
  void print(std::ostream& out = std::cout) const {
    FVTXOO::PRINT(out, "mFvtxKalFitPar");
    out << "_verbosity = " << _verbosity << ".\n";
    out << "_momentum_resolution = " << _momentum_resolution << "GeV.\n";
    out << "_angular_resolution = " << _angular_resolution << "rad.\n";
    out << "_position_resolution = " << _position_resolution << "cm.\n";
    out << "_z_reference = " << _z_reference << "cm.\n";
    out << "_update_cov_matrix = " << _update_cov_matrix << ".\n";
    out << "_min_n_coord = " << _min_n_coord << ".\n";
    out << "_max_iterations = " << _max_iterations << ".\n";
    out << "_chi_cut = " << _chi_cut << ".\n";

    out << "_extrapolate_to_mutr = " << _extrapolate_to_mutr << "\n";
    out << "_do_evaluation = " << _do_evaluation << ".\n";
    out << "_evaluation_file = " << _evaluation_file << ".\n";
//    out << "_use_vtx = " << _use_vtx << ".\n";
    out << "_vtx_z_smear = " << _vtx_z_smear << ".\n";
    out << "_vtx_phi_smear = " << _vtx_phi_smear << ".\n";
    out << "_vtx_phi_error = " << _vtx_phi_error << ".\n";
    out << "_vtx_r_error = " << _vtx_r_error << ".\n";
    out << "_vtx_z_error = " << _vtx_z_error << ".\n";

    FVTXOO::PRINT(out, "**");
  }

 private:
	
  //! relative momentum resolution for starting covariance matrix [NO UNIT]. default is 0.5.
  Double_t _momentum_resolution;  
 
  //! absolute angular resolution for starting covariance matrix	[tangent(rad)]. default is 0.2.
  Double_t _angular_resolution;   
	
  //! position resolution for starting covariance matrix
  Double_t _position_resolution;  
	
  //! position along the beam for kalman filter end-extrapolation. default is 0
  Double_t _z_reference;          
	
  //! cut on relative chi2 increment to stop fit iterations [NO UNIT] default is 0.01.
  Double_t _chi_cut;              

  //! minimal number of TFvtxCoord in track before filter [NO UNIT]. default is 6.
  unsigned short _min_n_coord;    
	
  //! max number of filter/smooth iterations [NO UNIT]. default is 50.
  unsigned short _max_iterations; 
	
//  //! if true, adds the vtx cluster parameters to the fit
//  bool _use_vtx;
	
  //! if true, adds the mutr cluster parameters to the fit
  bool _use_mutr;						
	
  //! if true, adds the muid cluster parameters to the fit
  bool _use_muid;						
  /*!  
    if true, starting covariance matrix is updated between KF iteration using 
    last hit smoothed parameters from previous iteration
  */
  bool _update_cov_matrix; 

  //! if true, track parameters at ____ are swimmed to mutr ____ through the field
  bool _extrapolate_to_mutr;

  //! if true evaluation histograms are booked and filled
  bool _do_evaluation;
  
  //! filname where to write evaluation histograms
  std::string _evaluation_file;

  //! smearing to add to z position of vtx hit (cm)
  Double_t _vtx_z_smear;              

  //! smearing to add to phi position of vtx hit (cm)
  Double_t _vtx_phi_smear;              

  //! phi error associated with a vtx hit (cm)
  Double_t _vtx_phi_error;

  //! r error associated with a vtx hit (alignment error, cm)
  Double_t _vtx_r_error;

  //! z error associated with a vtx hit
  Double_t _vtx_z_error;

  ClassDef(mFvtxKalFitPar, 1);
};

#endif







