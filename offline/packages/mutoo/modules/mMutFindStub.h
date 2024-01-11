#ifndef __MMUTFINDSTUB_HH__
#define __MMUTFINDSTUB_HH__

#include<PHTimeServer.h>
#include<PHPoint.h>
#include<TMutFitPar.hh>
#include<vector>
#include<TMutGapCoordMap.h>
#include<TMutStubMap.h>
#include<TMutCoordMap.h>
#include<mMutFindStubPar.h>
#include<TMutStubFinder.h>
#include<boost/array.hpp>

class PHCompositeNode;

/*! \ingroup modules */

//!Finds groups of contiguous hit strips and associates them
//! with a TMutClus object.
/*!
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutFindStubPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutGapCoordMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
<td> TMutCoordMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
<td> TMutTrkMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
</table>
*/

class mMutFindStub
{
 public: 
  
	//! constructor
  mMutFindStub();
	
	//! destructor 
  virtual ~mMutFindStub(){}
	
	//! event method 
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  
  
  //! get local pointer to needed nodes.
  void set_interface_ptrs(PHCompositeNode* top_node);
  
	//! find all stubs
  void find_stubs();

	//! associate coordinates to a stub
  void associate_coords();
  
	//! associate a coordinate to a stup
  void associate_coord(TMutStubMap::pointer stub_ptr,
		       TMutCoordMap::pointer in_coord_ptr,
		       const PHPoint& stub_point,
		       double in_distance);

	//! associate gap coordinates to stubs 
  void associate_gap_coords();

	//! check if stubs pass cuts
  void apply_stub_cuts();

	//! remove duplicated (same associated coordinates) stubs
  void eliminate_duplicates();

	//! returns true if two stubs have same associated coords
  bool coord_list_equal(const TMutStubMap::pointer stub1, const TMutStubMap::pointer stub2);
  
  // Interface pointers  
  //! parameter table
  const mMutFindStubPar* _mod_par;
  
  //! map of Coordinates
  TMutCoordMap* _coord_map;
  
  //! map of stubs
  TMutStubMap* _stub_map;

  //! Local stub list
  TMutStubFinder::stub_list _stubs;
  
  //! Timer
  PHTimeServer::timer _timer;
  
};

#endif /* __MMUTFINDSTUB_HH__ */



