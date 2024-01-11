#ifndef __MMUICLUSTERFINDERPAR_HH__
#define __MMUICLUSTERFINDERPAR_HH__

#include<PHObject.h>
#include<MUIOO.h>
#include<TMuiParBase.h>

//!  Runtime parameter object for mMuiClusterFinder analysis module
class mMuiClusterFinderPar : public TMuiParBase
{
  
 public: 
  
  //! default constructor
  mMuiClusterFinderPar() : 
    _max_clus_width(2),
    _max_nclusters_plane(300),
    _clustering_mode(ISOLATED)
    {}
  
  //! destructor */
  ~mMuiClusterFinderPar(){;}
    
  //! clustering mode
  enum ClMode 
  {
    
    //! no clustering
    NONE, 
    
    /*! \brief 
      accept clusters if not two big. 
      Too large clusters are split into single hit clusters
    */
    ISOLATED, 
      
    //! build clusters of any size from adjacent hits
    NORMAL
  };
  
  //! Clustering mode */
  ClMode get_clustering_mode() const 
  { return _clustering_mode; }
  
  //! Maximum cluster width
  UShort_t get_max_cluster_width() const 
  {return _max_clus_width;} 
  

  //! Maximum number of clusters in one plane, if more than that all clusters are cleared. CLS 12-20-12
  UShort_t get_max_nclusters_plane() const
  {return _max_nclusters_plane;}

  //! Clustering mode 
  void set_clustering_mode(ClMode mode)
  {_clustering_mode = mode;}
  
  //! Maximum cluster width
  void set_max_cluster_width(UShort_t width) 
  {_max_clus_width = width;}
  
  //! Maximum number of clusters in one plane, if more than that all clusters are cleared. CLS 12-20-12
  void set_max_nclusters_plane(UShort_t max)
  {_max_nclusters_plane = max;}

  //! print module
  void print( std::ostream& out = std::cout )
  {
    
    MUIOO::PRINT( out, "mMuiClusterFinderPar::print" );
    out << "_max_clus_width: " << _max_clus_width << std::endl;
    out << "_max_nclusters_plane: " << _max_nclusters_plane << std::endl;
    out << "_clustering_mode: " << _clustering_mode << std::endl;
    MUIOO::PRINT( out, "**" );
    
  }
  
 private:  
  
  //! maximum cluster width
  UShort_t _max_clus_width;
  
  //! maximum number of clusters
  UShort_t _max_nclusters_plane;

  //! clustering mode
  ClMode _clustering_mode;

};

#endif /* __MMUICLUSTERFINDERPAR_HH__ */





