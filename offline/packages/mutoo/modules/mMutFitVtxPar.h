#ifndef __mMutFitVtxPAR_HH__
#define __mMutFitVtxPAR_HH__

// $Id: mMutFitVtxPar.h,v 1.9 2011/12/24 04:48:30 slash Exp $

#include <TMutParBase.h>
#include <TMutParameterDB.h>

/*! \brief
  Runtime parameter object for mMutFitVtx analysis module
*/
class mMutFitVtxPar : public TMutParBase
{
  public:

  /*! default constructor */
  mMutFitVtxPar():
    _max_iterations( 10 ),
    _z_reference( 0 ),
    _chi_cut( 0.005 ),
    _single_trk_fit( false )
  {

    // load values from parameter file
    TMutParameterDB::get().get<unsigned short>( "mMutFitVtx_verbosity", _verbosity );
    TMutParameterDB::get().get<unsigned short>( "mMutFitVtx_max_iterations", _max_iterations );
    TMutParameterDB::get().get<Double_t>( "mMutFitVtx_z_reference", _z_reference );
    TMutParameterDB::get().get<Double_t>( "mMutFitVtx_chi_cut", _chi_cut );
    TMutParameterDB::get().get<bool>( "mMutFitVtx_single_trk_fit", _single_trk_fit );

  }

  //! destructor
  ~mMutFitVtxPar()
  { }

  //! sets maximum number of iterations
  void set_max_iterations( unsigned short value )
  { _max_iterations  = value; }

  //! sets default vertex z position when no external vertex is found
  void set_z_reference( Double_t value )
  { _z_reference  = value; }

  //! sets cut on relative chi_square increment between passes
  void set_chi_cut( Double_t value )
  { _chi_cut  = value; }

  //! sets single track vertex fit switch
  void set_single_trk_fit( bool value )
  { _single_trk_fit = value; }

  //! retrieves maximum number of iterations
  unsigned short get_max_iterations( void ) const
  { return _max_iterations; }

  //! retrieves default vertex z position when no external vertex is found
  Double_t get_z_reference( void ) const
  { return _z_reference; }

  //! retrieves cut on relative chi_square increment between passes
  Double_t get_chi_cut( void ) const
  { return _chi_cut; }

  //! retrieves single track vertex fit switch
  bool get_single_trk_fit( void ) const
  { return _single_trk_fit; }

  //! dumps parameters to screen
  void print( std::ostream& out = std::cout )
  {
    MUTOO::PRINT( out, "mMutFitVtxPar" );
    out << "_max_iterations = " << _max_iterations << ".\n";
    out << "_z_reference = " << _z_reference << " cm.\n";
    out << "_chi_cut = " << _chi_cut << ".\n";
    out << "_single_trk_fit = " << _single_trk_fit << ".\n";
    MUTOO::PRINT( out, "**" );
  }

  private:

  //! max number of filter/smooth iterations [NO UNIT]
  unsigned short _max_iterations;

  //! position along the beam for kalman filter end-extrapolation. default is 0.
  Double_t _z_reference;

  //! cut on relative chi2 increment to stop fit iterations [NO UNIT] default is 0.01
  Double_t _chi_cut;

  //! if true (1) performs single track vertex fit together with double trk vertex fit
  bool _single_trk_fit;

};

#endif
