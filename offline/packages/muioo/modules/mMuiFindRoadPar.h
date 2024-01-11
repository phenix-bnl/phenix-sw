#ifndef __MMUIFINDROADPAR_HH__
#define __MMUIFINDROADPAR_HH__

#include<PHObject.h>
#include<MUIOO.h>
#include<TMuiParBase.h>

//!  Runtime parameter object for mMuiFindRoad analysis module
/*! 
*/
class mMuiFindRoadPar : public TMuiParBase
{
  
 public: 

  /*! Enumeration to select road finding algorithm (no reverse or reverse 
    search also */
  enum Mode 
  { 
    NO_REVERSE, 
    REVERSE 
  };
  
  /*! Default constructor */
  mMuiFindRoadPar(): 
    _use_window(false),
    _phi_window(0.2),      // radians 
    _theta_window(0.2),      // radians
    _coord_dca_cut(4.), // tubes
    _coord_proximity_cut(4.), // tubes
    _min_point(2),
    _min_cluster(4),
    _mode(NO_REVERSE)
    {    
    }
  
  /*! Destructor */
  ~mMuiFindRoadPar(){;}

  /*! Set use of window */
  void set_use_window(bool b) 
  { _use_window = b; }
  
  /*! Get algorithm */
  bool get_use_window() const 
  {return _use_window;}

  /*! Opening angle cut for adding gap coords to roads */
  double get_phi_window() const 
  { return _phi_window; }
  
  /*! Opening angle cut for adding gap coords to roads */
  void set_phi_window(double phi_window)  
  { _phi_window=phi_window; }

  /*! Opening angle cut for adding gap coords to roads */
  double get_theta_window() const 
  { return _theta_window; }
  
  /*! Opening angle cut for adding gap coords to roads */
  void set_theta_window(double theta_window)  
  { _theta_window=theta_window; }

  /*! Track coord dca cut used during association */
  double get_coord_dca_cut() const 
  { return _coord_dca_cut; }
  
  /*! Track coord dca cut used during association */
  void set_coord_dca_cut(double coord_dca_cut)  
  { _coord_dca_cut=coord_dca_cut; }

  /*! Track coord proximity cut used during association */
  double get_coord_proximity_cut() const 
  { return _coord_proximity_cut; }
  
  /*! Track coord proximity cut used during association */
  void set_coord_proximity_cut(double coord_proximity_cut)  
  { _coord_proximity_cut=coord_proximity_cut; }

  /*! Minimum number of points per road */
  UShort_t get_min_point() const 
  { return _min_point; }
  
  /*! Minimum number of points per road */
  void set_min_point(UShort_t min_point)  
  { _min_point = min_point; }

  /*! Minimum number of TMuiCluster per road */
  UShort_t get_min_cluster() const 
  { return _min_cluster; }
  
  /*! Minimum number of TMuiCluster per road */
  void set_min_cluster(UShort_t min_cluster)  
  { _min_cluster = min_cluster; }

  /*! Set algorithm */
  void set_mode(Mode mode) 
  { _mode = mode; }
  
  /*! Get algorithm */
  Mode get_mode() const 
  {return _mode;}

  void print(std::ostream& os = std::cout) const 
  {
    os << "** mMuiFindRoadPar - value dump **" << std::endl;
    if (_use_window) os << " use window" << std::endl;
    else os << " not use window" << std::endl;
    os << " phi_window " << _phi_window << std::endl         
       << " theta_window " << _theta_window << std::endl       
       << " coord_dca_cut " << _coord_dca_cut << std::endl      
       << " coord_proximity_cut " << _coord_proximity_cut << std::endl
       << " min_point " << _min_point << std::endl       
       << " min_cluster " << _min_cluster << std::endl;
    if (_mode == NO_REVERSE) os << " not reverse algorithm" << std::endl;
    else os << " reverse algorithm " << std::endl;
    os << " ** " << std::endl;
  }

 private:  

  bool _use_window;  
  double _phi_window;  
  double _theta_window;  
  double _coord_dca_cut;
  double _coord_proximity_cut;
  UShort_t _min_point;
  UShort_t _min_cluster;      
  Mode _mode;

};

#endif /* __MMUIFINDROADPAR_HH__ */
