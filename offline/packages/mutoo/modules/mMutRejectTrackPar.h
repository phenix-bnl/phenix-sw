#ifndef __MMUTREJECTTRACKPAR_HH__
#define __MMUTREJECTTRACKPAR_HH__

#include <iostream>
#include<PHObject.h>
#include<TMutParBase.h>

/*! 
Runtime parameter object for mMutRejectTrack analysis module
*/
class mMutRejectTrackPar : public TMutParBase
{
  
 public: 

  /*! default constructor */
  mMutRejectTrackPar() : 
    _remove_rejected(false),
    _reject_outliers(false),
    _max_shared_coords(0),
    _max_chi_inc(100),
    _ghost_qual_subset_fac(1.0),
    _ghost_chi_min(10.0),
    _ghost_quality_cut( 5 ),
    _dp_cut( -1 )
    {;}
  
  /*! destructor */
  ~mMutRejectTrackPar(){;}

  /*! remove ghost tracks */
  void set_remove_rejected( const bool& remove_rejected)
  {_remove_rejected = remove_rejected;}

  /*! remove ghost tracks */
  const bool& get_remove_rejected() const 
  {return _remove_rejected;}

  /*! Outlier coordinates rejection */
  void set_reject_outliers( const bool& reject_outliers)
  {_reject_outliers = reject_outliers;}
  
  /*! Outlier coordinates rejection */
  const bool& get_reject_outliers() const 
  { return _reject_outliers; }
  
  //! maximum number of shared coordinates between two tracks
  void set_max_shared_coords( const unsigned short& value )
    { _max_shared_coords = value; }

  //! maximum number of shared coordinates between two tracks
  const unsigned short& get_max_shared_coords( void ) const
    { return _max_shared_coords; }

  /*! Maximum chi square increment cut applied by outlier rejection  */
  void set_max_chi_inc( const double& max_chi_inc) 
  { _max_chi_inc = max_chi_inc;}

  /*! Maximum chi square increment cut applied by outlier rejection  */
  const double& get_max_chi_inc() const 
  { return _max_chi_inc;}

  /*! If chi-square ratio > this, keep track with most hits */
  void set_ghost_qual_subset_fac( const double& ghost_qual_subset_fac) 
  { _ghost_qual_subset_fac = ghost_qual_subset_fac;}

  /*! If chi-square ratio > this, keep track with most hits */
  const double& get_ghost_qual_subset_fac() const 
  { return _ghost_qual_subset_fac;}

  /*! If chi-squares less than this, keep track with most hits */
  void set_ghost_chi_min( const double& ghost_chi_min) 
  { _ghost_chi_min = ghost_chi_min;}

  /*! If chi-squares less than this, keep track with most hits */
  const double& get_ghost_chi_min() const 
  { return _ghost_chi_min;}
  
  /*! \brief 
    quality cut for selecting a track with lower chisquare and fewer hits
    the larger the cut the more the hits are favorized
  */
  void set_ghost_quality_cut( const double& value )
  { _ghost_quality_cut = value; }
  
  /*! \brief 
    quality cut for selecting a track with lower chisquare and fewer hits
    the larger the cut the more the hits are favorized
  */
  const double& get_ghost_quality_cut( void ) const
  { return _ghost_quality_cut; }

  //! relative momentum difference
  void set_dp_cut( const double& value )
  { _dp_cut = value; }
  
  //! relative momentum difference
  const double& get_dp_cut( void ) const
  { return _dp_cut; }
  
  //! print module parameters
  void print( std::ostream& out = std::cout )
  {
    MUTOO::PRINT( out, "mMutRejectTrackPar" );
    out << "_remove_rejected = " << _remove_rejected << std::endl;
    out << "_reject_outliers = " << _reject_outliers << std::endl;
    out << "_max_shared_coords = " << _max_shared_coords << std::endl;
    out << "_max_chi_inc = " << _max_chi_inc << std::endl;
    out << "_ghost_qual_subset_fac = " << _ghost_qual_subset_fac << std::endl;
    out << "_ghost_chi_min = " << _ghost_chi_min << std::endl;
    out << "_ghost_quality_cut = " << _ghost_quality_cut << std::endl;
    out << "_dp_cut = " << _dp_cut << std::endl;
    MUTOO::PRINT( out, "**" );
  }

 private:  

  //! set to true to delete ghost tracks from the map
  bool _remove_rejected;

  //! set to true to remove bad coordinates from tracks
  bool _reject_outliers;

  //! maximum number of shared coordinates between two tracks
  unsigned short _max_shared_coords;

  //! chisquare cut do decide if a coordinate is good or not
  double _max_chi_inc;
  double _ghost_qual_subset_fac;
  double _ghost_chi_min;
  
  /*! \brief 
    quality cut for selecting a track with lower chisquare and fewer hits
    the larger the cut the more the hits are favorized
  */
  double _ghost_quality_cut;
  
  /*!  \brief
    cut on relative momentu difference to tell if two tracks are likely to
    be ghost one from the other
  */
  double _dp_cut;

};

#endif /* __MMUTREJECTTRACKPAR_HH__ */







