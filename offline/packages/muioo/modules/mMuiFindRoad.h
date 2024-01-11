#ifndef __MMUIFINDROAD_HH__
#define __MMUIFINDROAD_HH__

#include<PHTimeServer.h>
#include<PHPoint.h>
#include<TMutFitPar.hh>
#include<vector>
#include<TMuiRoadMapO.h>
#include<TMuiClusterMapO.h>
#include<mMuiFindRoadPar.h>
#include<TMuiRoadFinder.h>
#include<boost/array.hpp>

/**
 * Associate clusters into roads 
 * - a la mutoo stub finding
 */

/*! \ingroup modules */

//! Associate clusters into roads
/*!
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMuiFindRoadPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMuiClusterMapO*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
<td> TMuiRoadMapO*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
</table>
*/

class mMuiFindRoad
{
 public: 
  
  mMuiFindRoad(); 
  virtual ~mMuiFindRoad(){}
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  
  
  // private methods
  //
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  void find_roads();

  void associate_clusters();
  
  void associate_cluster(TMuiRoadMapO::pointer road_ptr,
			 TMuiClusterMapO::pointer in_cluster_ptr,
			 const PHPoint& road_point,
			 double in_distance);

  void fit_roads();

  void set_road_parameters();

  void apply_road_cuts();
  
  void eliminate_duplicates();
  
  bool cluster_list_equal(const TMuiRoadMapO::pointer road1,
			  const TMuiRoadMapO::pointer road2);
  
  // binary predicate for sorting container of shared pointers to 
  // keyed objects
  //
  struct less_ftor
  {
    bool operator()(const TMuiClusterMapO::pointer& val1, const TMuiClusterMapO::pointer& val2){
      return val1->get()->get_key().get_obj_key() < val2->get()->get_key().get_obj_key();
    }
  };
  
  struct equal_ftor
  {
    bool operator()(const TMuiClusterMapO::pointer& val1, const TMuiClusterMapO::pointer& val2){
      return val1->get()->get_key().get_obj_key() == val2->get()->get_key().get_obj_key();
    }
  };
  
  // Interface pointers
  //
  const mMuiFindRoadPar* _mod_par;           // parameter table
  TMuiClusterMapO* _cluster_map;       	     // IOC
  TMuiRoadMapO* _road_map;		     // IOC
  
  // Local road list
  //
  TMuiRoadFinder::road_list _roads;
  
  // Timer
  //
  PHTimeServer::timer _timer;
};

#endif /* __MMUIFINDROAD_HH__ */



