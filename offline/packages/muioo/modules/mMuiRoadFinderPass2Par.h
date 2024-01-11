// $Id: mMuiRoadFinderPass2Par.h,v 1.1 2006/04/22 01:59:14 hpereira Exp $
#ifndef __MMUIROADFINDERPASS2PAR_HH__
#define __MMUIROADFINDERPASS2PAR_HH__

#include<PHObject.h>
#include<TMuiParBase.h>
#include<boost/array.hpp>

/*! 
Runtime parameter object for mMuiRoadFinderPass2 analysis module
*/
class mMuiRoadFinderPass2Par : public TMuiParBase
{
  
 public: 

  /*! default constructor */
  mMuiRoadFinderPass2Par():
    _residual_cut( -1 ),
    _pull_cut( -1 ),
    _chi_cut( -1 ),
    _do_smoothing( false ),
    _do_evaluation( false ),
    _evaluation_file( "mMuiRoadFinderPass2.root" )
    {
      set_verbosity(MUIOO::NONE); 

      // min momentum required to reach certain MUID gap/plane (at theta = 0)
      // from a technical note from Vince in 2000:
      // /phenix/WWW/publish/vince/misc/eloss-doc.ps
      //
      // South 
      _min_momentum[0][0] = 1.49;
      _min_momentum[0][1] = 1.63;
      _min_momentum[0][2] = 1.76;
      _min_momentum[0][3] = 2.04;
      _min_momentum[0][4] = 2.31;

      // North
      _min_momentum[1][0] = 1.63;
      _min_momentum[1][1] = 1.76;
      _min_momentum[1][2] = 1.90;
      _min_momentum[1][3] = 2.18;
      _min_momentum[1][4] = 2.45;
    }
  
  /*! destructor */
  ~mMuiRoadFinderPass2Par(){;}
  
  /*! PHOOL inteface requirement */
  void identify( std::ostream& os = std::cout) const 
  { os << "mMuiRoadFinderPass2Par";}

  /*! \brief 
    sets cut on filtered residual to determine whether to keep mui_cluster or not. 
    negative value means cut disabled
  */
  void set_residual_cut( Double_t value ) 
  { _residual_cut = value; }

  /*! \brief 
    sets cut on filtered pull (residual/cov_error) to determine whether to keep mui_cluster or not. 
    negative value means cut disabled
  */
  void set_pull_cut( Double_t value ) 
  { _pull_cut = value; }
  
  /*! \brief 
    sets cut on measurement chi2 contribution to determine whether to keep mui_cluster or not. 
    negative value means cut disabled
  */
  void set_chi_cut( Double_t value ) 
  { _chi_cut = value; }
  
  //! sets min momentum required to reach a certain MUID gap/plane
  void set_min_momentum( UShort_t arm, UShort_t plane, Double_t value ) 
  { _min_momentum[arm][plane] = value; } 

  //! sets smoothing switch
  void set_do_smoothing( bool value ) 
  { _do_smoothing = value; }

  //! sets histogram switch
  void set_do_evaluation( bool value ) 
  { _do_evaluation = value; }
  
  //! sets histogram file
  void set_evaluation_file( const std::string& value )
  { _evaluation_file = value; }

  /*! \brief 
    gets cut on filtered residual to determine whether to keep mui_cluster or not. 
    negative value means cut disabled
  */
  Double_t get_residual_cut( void ) const 
  { return _residual_cut; } 

  /*! \brief 
    gets cut on filtered pull (residual/cov_error) to determine whether to keep mui_cluster or not. 
    negative value means cut disabled
  */
  Double_t get_pull_cut( void ) const 
  { return _pull_cut; } 

  /*! \brief 
    gets cut on measurement chi2 contribution to determine whether to keep mui_cluster or not. 
    negative value means cut disabled
  */
  Double_t get_chi_cut( void ) const 
  { return _chi_cut; } 

  //! gets min momentum required to reach a certain MUID gap/plane
  Double_t get_min_momentum( UShort_t arm, UShort_t plane ) const 
  { return _min_momentum[arm][plane]; } 

  //! gets smoothing switch
  bool get_do_smoothing( void ) const 
  { return _do_smoothing; }

  //! gets histogram switch
  bool get_do_evaluation( void ) const 
  { return _do_evaluation; }

  //! gets histogram file
  std::string get_evaluation_file( void ) const 
  { return _evaluation_file; }

  //! dump all parameters	
  void print( std::ostream& os = std::cout ) {
    MUIOO::PRINT( os, "mMuiRoadFinderPass2Par" );
    os << "_verbosity = " << _verbosity << ".\n";
    os << "_residual_cut = " << _residual_cut << ".\n";
    os << "_pull_cut = " << _pull_cut << ".\n";
    os << "_chi_cut = " << _chi_cut << ".\n";
    for (int iarm = 0; iarm<MUIOO::MAX_ARM; iarm++) { 
      os << "_min_momentum Arm " << iarm << " : ";
      for (int iplane = 0; iplane<MUIOO::MAX_PLANE; iplane++)  
	    os << _min_momentum[iarm][iplane] << " ";
      os << ".\n";
    }
    os << "_do_evaluation = " << _do_evaluation << ".\n";
    os << "_evaluation_file = " << _evaluation_file << ".\n";
    MUIOO::PRINT( os, "**" );
  }
	
 private:
  
  Double_t _residual_cut; //!< keep mui_cluster if within cut on residual  [CM]. Negative means disabled
  Double_t _pull_cut;     //!< keep mui_cluster if within cut on pull [NO UNIT]. Negative means disabled
  Double_t _chi_cut;      //!< keep mui_cluster if within cut on chi2 [NO UNIT]. Negative means disabled
  
  //! minimum momentum required for a muon to reach a certain MUID gap/plane, at theta=0 [GeV/c] 
  Double_t _min_momentum[MUIOO::MAX_ARM][MUIOO::MAX_PLANE]; 
  bool _do_smoothing;     //!< if true, kalman filter smoothing is done.

  //! if true evaluation histograms are booked and filled
  bool _do_evaluation;
  
  //! filname where to write evaluation histograms
  std::string _evaluation_file;
  
};

#endif /* __MMUIROADFINDERPASS2PAR_HH__ */







