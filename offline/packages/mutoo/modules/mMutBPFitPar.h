#ifndef __MMUTBPFITPAR_HH__
#define __MMUTBPFITPAR_HH__

#include<MUTOO.h>
#include<PHObject.h>
#include<TMutParBase.h>

/*! 
Runtime parameter object for mMutBPFit analysis module
*/
class mMutBPFitPar : public TMutParBase
{
  
 public: 

  /*! default constructor */
  mMutBPFitPar() :
    _use_vertex123(false),
    _use_vertex23(false),
    _use_p_dep_sigma(false),
    _use_muid_23fit(false),
    _use_muid_123fit(false),
    _ref_plane(DOWNSTREAM)
    {;}
  
  /*! destructor */
  ~mMutBPFitPar()
  {;}
  
  /*! PHOOL inteface requirement */
  void identify(std::ostream& os = std::cout) const 
  { os << "mMutBPFitPar";}
  
  /*! Init Mode */
  enum RefPlane 
  { 
    UPSTREAM, 
    DOWNSTREAM 
  };
  
  /*! Reference Plane */
  RefPlane get_ref_plane() const 
  { return _ref_plane;}
  
  /*! Reference Plane */
  void set_ref_plane(RefPlane ref_plane) 
  { _ref_plane = ref_plane;}

  //! use vertex in station 1, 2 and 3 fit  
  bool get_use_vertex123() const
  { return _use_vertex123; }

  //! use vertex in station 1, 2 and 3 fit
  void set_use_vertex123(bool value)
  { _use_vertex123 = value; }
  
  //! use vertex in station 2 and 3 fit
  bool get_use_vertex23() const
  { return _use_vertex23; }

  //! use vertex in station 2 and 3 fit
  void set_use_vertex23(bool value )
  { _use_vertex23 = value; }
  
  //! use muid in station 2 and 3 fit
  bool get_use_muid_23fit() const
  { return _use_muid_23fit; }

  //! use muid in station 2 and 3 fit
  void set_use_muid_23fit(bool value )
  { _use_muid_23fit = value; }
  
  //! use muid in station 1, 2 and 3 fit
  bool get_use_muid_123fit() const
  { return _use_muid_123fit; }

  //! use muid in station 1, 2 and 3 fit
  void set_use_muid_123fit(bool value )
  { _use_muid_123fit = value; }
  
  //! use momentum dependent weighting of the vertex in fit
  bool get_use_p_dep_sigma() const
  { return _use_p_dep_sigma; }

  //! use momentum dependent weighting of the vertex in fit
  void set_use_p_dep_sigma(bool value)
  { _use_p_dep_sigma = value; }

  //! write parameters to stream
  void print( std::ostream& out = std::cout ) const
  {
    MUTOO::PRINT( out, "mMutBPFitPar" );
    out << "_verbosity = " << _verbosity << std::endl;
    
    out << "_use_vertex23 = " << _use_vertex23 << std::endl;
    out << "_use_vertex123 = " << _use_vertex123 << std::endl;
    out << "_use_p_dep_sigma = " << _use_p_dep_sigma << std::endl;
    
    out << "_use_muid_23fit = " << _use_muid_123fit << std::endl;
    out << "_use_muid_123fit = " << _use_muid_123fit << std::endl;
    
    out << "_ref_plane = " << _ref_plane << std::endl;
    MUTOO::PRINT( out, "**" );
  }

  
 private:  

  //! use vertex in station 1, 2 and 3 fit
  bool _use_vertex123;

  //! use vertex in station 2 and 3 fit
  bool _use_vertex23;
  
  //! use momentum dependent vertex weight in fit
  bool _use_p_dep_sigma;
  
  //! use muid in station 2 and 3 bend plane fit
  bool _use_muid_23fit;

  //! use muid in station 1, 2 and 3 bend plane fit
  bool _use_muid_123fit;
  
  //! reference plane at which the BP fit parameters are calculated
  RefPlane _ref_plane;

};

#endif /* __MMUTBPFITPAR_HH__ */







