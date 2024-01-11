
#ifndef __TMUTCLUSTERFIT_H__
#define __TMUTCLUSTERFIT_H__

/*
  \file TMutClusterFit.h
  \author: H. Pereira
  \version $Revision: 1.20 $
  \date $Date: 2011/12/24 04:48:20 $
  \brief Encapsulates the cluster fit algorithm 
  to be used by different modules 
  including mMutFitClus
*/

#include <vector>

#include <MUTOO.h>
#include <PHGslMatrix.h>
#include <PHGslRng.h>
#include <TMutHitMap.h>
#include <TMutMCHitMap.h>
#include <TMutClusMap.h>

/*!
  \brief Encapsulates the cluster fit algorithm do be used by different modules 
  including mMutFitClus
*/
class TMutClusterFit
{
  public: 
  
  // fit one cluster
  static bool fit( TMutClusMap::pointer );

  //! set gsl_fiter verbosity
  static void set_verbosity( MUTOO::Verbosity value )
  { _verbosity = value; }
  
  //! Enumeration for the sample fitting scheme
  enum FitType {
    
    //! use strip with max charge
    CENTER_PEAK,
     
    //! use ratio of max/total charge
    PEAK_RATIO,
        
    //! use parametrized lookup table of strip charges vs position
    MATHIESON_LOOKUP,
        
    //! full mathieson fit using gsl
    GSL_MATHIESON,
    
    //! use MC information when available and GSL mathieson fit otherwise
    /*! works for MC embedded into real data */
    MC_GSL_MATHIESON,
        
    //! use MC information to get number of tracks to be fitted into a cluster
    /*! works for MC or MC embedded into MC */
    MC_TRK_GSL_MATHIESON,

    //! use MC information
    /*! warning: works only on pure MC or MC embedded into MC. */
    MC
  };
  
  //! return fit type name
  static std::string get_fit_type_name( const FitType& type )
  {
    switch( type )
    {
      case CENTER_PEAK: return "CENTER_PEAK";
      case PEAK_RATIO: return "PEAK_RATIO";  
      case MATHIESON_LOOKUP: return "MATHIESON_LOOKUP";  
      case GSL_MATHIESON: return "GSL_MATHIESON";  
      case MC_GSL_MATHIESON: return "MC_GSL_MATHIESON";  
      case MC_TRK_GSL_MATHIESON: return "MC_TRK_GSL_MATHIESON";  
      case MC: return "MC";
      default: return "UNKNOWN";
    }
  }

  //! tells us whether to use fit error or pull distribution error for cluster peak position
  static void set_use_fit_error(const bool b)
  {_use_fit_error = b;}
  
  static void set_use_zerosup_strips(const bool b)
  {_use_zerosup_strips = b;}
  
  static void set_use_fitfunc_zerosup(const bool b)
  {_use_fitfunc_zerosup = b;}
  
  //! use gsl fit error in place of hard coded pulls when the former is larger than the later
  static void set_use_hybridmax_error(const bool b)
  {_use_hybridmax_error = b;}
  
  //! force fit of three wide clusters to one track maximum
  static void set_force_three_wide_clusters(const bool b)
  { _force_three_wide_clusters = b;}
  
  //! defines fit type for single track clusters (width < 3)
  static void set_single_track_fit_type( const FitType& fit_type );
  
  //! defines fit type for multi track clusters (width >= 3 )
  static void set_multi_track_fit_type( const FitType& fit_type );
  
  //! scale for paak_ratio fit
  static void set_peak_ratio_scale( const double& value )
  { _peak_ratio_scale = value; }
  
  //! max chisquare for good fit
  static void set_chi_max_good_fit( const double& value )
  { _chi_max_good_fit = value; }
  
  //! max number of fit tracks in a cluster
  static void set_max_fit( const int& value )
  { _max_fit = value; }
  
  //! enable multitrack fit
  static void set_multi_track_fit( const bool& value )
  { _multi_track_fit = value; }
  
  //! set mc smearing for non stereo planes
  static void set_mc_smear_perp( const double& value )
  { _mc_smear_perp = value; }
  
  //! set mc smearing for stereo planes
  static void set_mc_smear_ster( const double& value )
  { _mc_smear_ster = value; }

  //! print parameters method
  static void print_parameters( std::ostream& out = std::cout ) 
  {
    MUTOO::PRINT( out, "TMutClusterFit::print_parameters" );
    out << "TMutClusterFit::flag multi_track_fit     : " << _multi_track_fit << std::endl;
    out << "TMutClusterFit::flag use_mc              : " << _use_mc << std::endl;
    out << "TMutClusterFit::flag use_mc_ntracks      : " << _use_mc_ntracks << std::endl;
    out << "TMutClusterFit::flag use_fit_error       : " << _use_fit_error << std::endl;
    out << "TMutClusterFit::flag use_zerosup_strips  : " << _use_zerosup_strips << std::endl;
    out << "TMutClusterFit::flag use_fitfunc_zersup  : " << _use_fitfunc_zerosup << std::endl;
    out << "TMutClusterFit::flag use_hybridmax_error : " << _use_hybridmax_error << std::endl;
    out << "TMutClusterFit::flag force_three_wide_clusters : " << _force_three_wide_clusters << std::endl;
    MUTOO::PRINT( out, "**" );

  }

  //! print gsl mathieson fit function static parameters
  static void print_tanhs( std::ostream& out = std::cout );

  //! hit list
  typedef std::vector<TMutHitMap::pointer> sample_list;
  
  //! gsl Mathieson cluster fitting - number of tracks to fit set by call
  /*! the method is made public because it can be called directly via mMutMatchCoord */
  static void gsl_mathieson_fit( sample_list&, TMutClusMap::pointer, unsigned int n_trk_forced = 0);

  private:

  //! Handle single hit clusters
  static void single_hit_cluster(sample_list&, TMutClusMap::pointer);

  //! Cluster fitting function
  static void peak_ratio_fit(sample_list&, TMutClusMap::pointer);

  //! Cluster fitting function
  static void center_peak_fit(sample_list&, TMutClusMap::pointer);

  //! New framework Mathieson lookup cluster fitting
  static void mathieson_lookup(sample_list&, TMutClusMap::pointer);

 	//! gsl Mathieson cluster fitting
 	static void gsl_mathieson_fit(sample_list& samples , TMutClusMap::pointer clus_ptr )
  { gsl_mathieson_fit( samples, clus_ptr, 0); }


  //! MC cluster fitting
  /*!
    retrieves the list of MC hits associated to the cluster.
    creates one centroid for each different track contributing.
    Note this works only when running either on pure MC simulations
    or on MC embedded into MC (e.g. Hijing)
  */
  static void mc_fit(sample_list&, TMutClusMap::pointer);
  
  
  //! compare q values of input hits 
  struct greater_q_ftor
  {
    bool operator()(TMutHitMap::const_pointer ptr1, TMutHitMap::const_pointer ptr2) const 
    { return ptr1->get()->get_q() > ptr2->get()->get_q(); }
  };

  //! compare q values of input hits 
  struct less_q_ftor
  {
    bool operator()(TMutHitMap::const_pointer ptr1, TMutHitMap::const_pointer ptr2) const 
    { return ptr1->get()->get_q() < ptr2->get()->get_q(); }
  };

  //! compare strip values of input hits
  struct less_strip_ftor
  {
    bool operator() (TMutHitMap::const_pointer ptr1, TMutHitMap::const_pointer ptr2) const 
    { return ptr1->get()->get_strip() < ptr2->get()->get_strip(); }
  };

  //! perform gsl non llinear minimization using Mathieson fonction 
  static int do_gsl_mathieson_fit(
      
    //! strip charges
    const std::vector<double>& q_values,
      
    //! strip errors
    const std::vector<double>& q_errors,
      
    //! strip zero-suppress limits
    const std::vector<double>& q_mins,
      
    //! cluster width
    const int& cluster_width,
    
    //! strip index
    const int& stripbegin,
    
    //! strip width
    const double& strip_width,
    
    //! number of tracks
    const int& ntracks,
    
    //! output parameters
    std::vector<double> &par_out,
    
    //! output covariance matrix
    PHGslMatrix &cov_out,
    
    //! output chisquare
    double & chisq,
    
    //! arm index
    const int &arm,
    
    //! station index
    const int	&station,
    
    //! cathode index
    const int	&cathplane,
    
    //! octant index
    const int   &octant,
    
    //! capacitive coupling
    const double &ac_in,
    
    //! capacitive coupling
    const double &cap_coupling,
    
    //! total charge for each track in cluster
    std::vector<double> &qtot_out 
  );
  
  //! stores peak strip, position and charge
  /*!
    stores peak strip position and charge associated
    to a given cluster. This is the initial guess
  */
  class peak_strip_data
  {
    public:
    
    //! constructor
    peak_strip_data( const unsigned short& strip, const double &peak_charge, const double& charge, const double& position ):
        _strip( strip ),
        _peak_charge( charge ),
        _charge( charge ),
        _position( position )
    {}	
      
                  
    //! strip id
    unsigned short _strip;

    //! peak charge
    double _peak_charge;
    
    //! total charge guess
    double _charge;
      
    //! position guess
    double _position;
    
    //! sort peak_strip from highest to lowest charge
    struct greater_peak_charge_ftor
    {
      bool operator()(const peak_strip_data& p1, const peak_strip_data& p2 )
      { return p1._peak_charge > p2._peak_charge; }
    };

    //! sort peak_strip from lowest to highert strip
    struct less_strip_ftor
    {
      bool operator()(const peak_strip_data& p1, const peak_strip_data& p2 )
      { return p1._strip < p2._strip; }
    };
    
  };

  //! find peak clusters from the list of strips
  static std::vector< peak_strip_data > find_peak_strips( const sample_list& samples );

  //! handle MC data for "perfect" cluster fit
  class mc_data 
  {
    
    public:
    
    //! constructor
    mc_data( TMutMCHitMap::const_pointer mc_hit_ptr, const double& w );

    //! add a hit
    void add_hit( TMutHitMap::const_pointer hit_ptr );
            
    //! associated TMutMCHit
    TMutMCHitMap::value_type _mc_hit;

    //! strip list
    TMutMCHit::strip_list _mc_strips;
    
    //! list of hits associated to the data
    std::vector< TMutHitMap::value_type > _hits;
            
    //! MC track id
    unsigned int _mc_track_id;
    
    //! position perpendicular to the strips
    double _w;
    
    //! charge 
    double _q;
    
    //! retrieve mc_data based on track id
    class same_mc_track_ftor
    {
      
      public:
          
      //! constructor
      same_mc_track_ftor( const unsigned int& mc_track_id ):
        _mc_track_id( mc_track_id )
      {}
      
      //! prediction
      bool operator() ( const mc_data& data ) const
      { return data._mc_track_id == _mc_track_id; }
      
      private:
      
      //! track id
      unsigned int _mc_track_id;
      
    };
        
  };

  //! find MC data associated to a given cluster
  static std::vector< mc_data > find_mc_data( TMutClusMap::const_pointer clus_ptr );
  
      
  //! stores parameters for the gsl mathieson fit
  struct gsl_data {
        
    /*! \brief
      number of tracks being fit to cluster.
      Number of fit parameters
      is 2* this (xpos and Qanode for each track) 
    */
    size_t ntracks; 
    
    //! First strip in cluster
    int stripbegin;			
    
    //! Width of cluster 
    int cluster_width;		
    
    //! charge on each strip
    std::vector<double> q_values; 
    
    //! sigma of charge on each strip
    std::vector<double> q_errors;
        
    //! minimum of charge on each strip above zero-suppression
    std::vector<double> q_mins;
        
    //! strip width; 
    double strip_width;	 
    
    //! capacitive coupling
    double AC;		
    
    //! arm index				
    int arm;		
    
    //! station index
    int station;
    
    //! cathode index
    int cathplane;
    
    //! octant index
    int octant;

    //! capacitive coupling
    double cap_coupling;
    
    //! total charge per centroid from fit
    std::vector<double> qtot_out; 
    
  };

  //! calculates the mathieson function for a set of parameters and charges
  static int _expb_f (const gsl_vector * x, void *params, gsl_vector * f);

  //! calculates the mathieson derivatives for a set of parameters and charges
  static int _expb_df (const gsl_vector * x, void *params, gsl_matrix * J);

  //! calculate both the mathieson and the derivative for a set of parameters and charges
  static int _expb_fdf ( const gsl_vector * x, void *params, gsl_vector * f, gsl_matrix * J )
  {
    _expb_f (x, params, f);
    _expb_df (x, params, J);
    return GSL_SUCCESS;
  }
  
  //! initialize gsl mathieson fit static parameters
  static bool initialize_tanhs( void );
  
  /*! 
    the pointer to member function 
    syntax, the read is that FitFunctionPtr is a pointer to a 
    member function that takes a const ref to a vector of TMutHit
    pointers and the cluster pointer as arguements
  */	
  typedef void (*FitFunctionPtr) (sample_list&, TMutClusMap::pointer);
  
  //! static fit function pointer to single track cluster fit
  static FitFunctionPtr _single_trk_fit_funtion;
  
  //! static fit function pointer to single track cluster fit
  static FitFunctionPtr _multi_trk_fit_funtion;
  
  //! peak ratio scale
  static double _peak_ratio_scale;
  
  //! max chisquare for good fit
  static double _chi_max_good_fit;
  
  //! max number of tracks fit through one cluster
  static int _max_fit;
  
  //! true to enable multitrack fit
  static bool _multi_track_fit;
  
  //! if true, use MC info to replace cluster fit output
  static bool _use_mc;
  
  //! if true, use MC info to get number of tracks / clusters
  /*! works on MC embedded into MC */
  static bool _use_mc_ntracks;

  static bool _use_fit_error;
  static bool _use_zerosup_strips;
  static bool _use_fitfunc_zerosup;
  
  //! use errors from gsl fit if it is larger than the hard-coded pulls
  static bool _use_hybridmax_error;
  
  //! force three wide clusters to be fitted with only one track
  static bool _force_three_wide_clusters;
  
  //! mc smearing for non stereo planes
  static double _mc_smear_perp;
  
  //! mc smearing for stereo planes
  static double _mc_smear_ster;
  
  //! max number of cathodes for a given station
  enum { NumberOfCathodes = MUTOO::NumberOfGaps*MUTOO::NumberOfCathodePlanes };

  //! gsl mathieson fit function static parameters
  static double _tanhs[4][MUTOO::NumberOfArms][MUTOO::NumberOfStations][NumberOfCathodes][MUTOO::NumberOfOctants]; 
  
  //! random generator seed
  static PHGslRng _rng;
  
  //! verbosity level
  static MUTOO::Verbosity _verbosity;
   
};

#endif
