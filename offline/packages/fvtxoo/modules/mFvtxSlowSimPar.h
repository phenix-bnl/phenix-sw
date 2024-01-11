// $Id: mFvtxSlowSimPar.h,v 1.9 2014/01/26 16:47:16 bbannier Exp $
#ifndef __mFvtxSlowSimPar_h__
#define __mFvtxSlowSimPar_h__

/*!
  \file mFvtxFastSimPar.h
  \brief Runtime parameter object for mFvtxFastSim analysis module
  \author H. Pereira Da Costa
  \version $Revision: 1.9 $
  \date $Date: 2014/01/26 16:47:16 $
*/

#include <PHObject.h>
#include <PHException.h>
//#include <boost/array.hpp>
#include <iostream>

#include "TFvtxParBase.h"
#include "FVTXOO.h"

/*! @ingroup modules */
//! Runtime parameter object for mFvtxFastSim analysis module
class mFvtxSlowSimPar : public TFvtxParBase
{
  
 public: 
  
  /*! default constructor */
  mFvtxSlowSimPar():
    _do_clusters( true ),
    _use_volume_id( true ),
    _check_consistency( true ),
    _do_evaluation( false ),
    _evaluation_file( "mFvtxSlowSim.root" )
  {}
  
  /*! destructor */
  ~mFvtxSlowSimPar()
  {}
  
  //! PHOOL inteface requirement
  void identify(std::ostream& os = std::cout) const 
  { os << "mFvtxSlowSimPar";}
  
  //! number of parameters
  enum { n_acceptance_parameters = FVTXOO::MAX_ARM*FVTXOO::MAX_STATION };

  //! \brief return unique index for arm/station
  static int get_acceptance_index( int arm, int station )
  { return station + FVTXOO::MAX_STATION*arm; }

  //! clustering 
  void set_do_clusters( const bool& value )
  { _do_clusters = value; }
  
  //! clustering
  const bool& get_do_clusters( void ) const
  { return _do_clusters; }

  //! use volume index to get detector matching pisa hit
  /*!
    use volume index to get detector matching pisa hit.
    If set to false, the pisa hit position is used instead
  */
  void set_use_volume_id( const bool& value )
  { _use_volume_id = value; }
  
  //! use volume index to get detector matching pisa hit
  /*!
    use volume index to get detector matching pisa hit.
    If set to false, the pisa hit position is used instead
  */
  const bool& get_use_volume_id( void ) const
  { return _use_volume_id; }
  
  //! consistency check
  void set_check_consistency( const bool& value )
  { _check_consistency = value; }
  
  //! consistency check
  const bool& get_check_consistency( void ) const
  { return _check_consistency; }
  
  //! evaluation flag
  void set_do_evaluation( const bool& value )
  { _do_evaluation = value;}
  
  //! evaluation flag
  const bool& get_do_evaluation( void ) const
  { return _do_evaluation; }
  
  //! evaluation file
  void set_evaluation_file( const std::string& value )
  { _evaluation_file = value; }
  
  //! evaluation file
  const std::string& get_evaluation_file( void ) const
  { return _evaluation_file; }
  
  //! printing (insert values of all parameters here
  void print(std::ostream& out = std::cout) const {
    FVTXOO::PRINT(out, "mFvtxSlowSimPar");
    out << "_verbosity: " << get_verbosity() << std::endl;
    out << "_do_clusters: " << _do_clusters << std::endl;
    out << "_use_volume_id: " << _use_volume_id << std::endl;
    out << "_check_consistency: " << _check_consistency << std::endl;
    out << "_do_evaluation: " << _do_evaluation << std::endl;
    out << "_evaluation_file: " << _evaluation_file << std::endl;
    FVTXOO::PRINT(out, "**");
  }

 private:
  //! clustering
  bool _do_clusters;
  
  //! use volume id to get detector from pisa hit
  bool _use_volume_id;
  
  //! perform consistency check
  bool _check_consistency;	 
  
  //! evaluation tag
  bool _do_evaluation;
  
  //! evaluation output file
  std::string _evaluation_file;
     
  
  ClassDef(mFvtxSlowSimPar, 1);
};

#endif
