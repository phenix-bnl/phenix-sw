#ifndef __MMUTFINDSTUBPAR_HH__
#define __MMUTFINDSTUBPAR_HH__

#include<PHObject.h>
#include<MUTOO.h>
#include<TMutParBase.h>

//!  Runtime parameter object for mMutFindStub analysis module
/*! 
*/
class mMutFindStubPar : public TMutParBase
{
  
 public: 
  
  /*! Default constructor */
  mMutFindStubPar(): 
    _theta_window(0.7),      // radians (40 degrees)
    _coord_proximity_cut(3), // cm
    _min_coord_st12(4),
    _min_coord_st3(3),
    _min_gap_coord_st12(2),
    _min_gap_coord_st3(1),
    _mode(NO_REQUIRE_GC),
    _occupancy_cut(0)
    {;}
  
  /*! Destructor */
  ~mMutFindStubPar(){;}

  /*! Enumeration to select stub finding algorithm */
  enum Mode { REQUIRE_GC, NO_REQUIRE_GC };

  /*! Opening angle cut for adding gap coords to stubs */
  double get_theta_window() const { return _theta_window; }
  
  /*! Opening angle cut for adding gap coords to stubs */
  void set_theta_window(double theta_window)  { _theta_window=theta_window; }

  /*! Minimum number of TMutCoord station 1 and station 2 stubs */
  unsigned short get_min_coord_st12() const { return _min_coord_st12; }
  
  /*! Minimum number of TMutCoord station 1 and station 2 stubs */
  void set_min_coord_st12(unsigned short min_coord_st12)  { _min_coord_st12 = min_coord_st12; }

  /*! Minimum number of TMutCoord station 3 stubs */
  unsigned short get_min_coord_st3() const { return _min_coord_st3; }
  
  /*! Minimum number of TMutCoord station 3 stubs */
  void set_min_coord_st3(unsigned short min_coord_st3)  { _min_coord_st3 = min_coord_st3; }

  /*! Minimum number of TMutGapCoord station 1 and station 2 stubs */
  unsigned short get_min_gap_coord_st12() const { return _min_gap_coord_st12; }
  
  /*! Minimum number of TMutGapCoord station 1 and station 2 stubs */
  void set_min_gap_coord_st12(unsigned short min_gap_coord_st12)  { _min_gap_coord_st12 = min_gap_coord_st12; }

  /*! Minimum number of TMutGapCoord station 3 stubs */
  unsigned short get_min_gap_coord_st3() const { return _min_gap_coord_st3; }
  
  /*! Minimum number of TMutGapCoord station 3 stubs */
  void set_min_gap_coord_st3(unsigned short min_gap_coord_st3)  { _min_gap_coord_st3 = min_gap_coord_st3; }

  /*! Track coord proximity cut used during association */
  double get_coord_proximity_cut() const { return _coord_proximity_cut; }
  
  /*! Track coord proximity cut used during association */
  void set_coord_proximity_cut(double coord_proximity_cut)  { _coord_proximity_cut=coord_proximity_cut; }

  /*! Set algorithm */
  void set_mode(Mode mode) { _mode = mode; }
  
  /*! Get algorithm */
  Mode get_mode() const {return _mode;}

  /*! Set state - occupancy cut */
  void set_occupancy_cut(unsigned short occupancy_cut) { _occupancy_cut = occupancy_cut; }
  
  /*! Get state - occupancy cut  */
  unsigned short get_occupancy_cut() const {return _occupancy_cut;}

 private:  
  
  double _theta_window;  
  double _coord_proximity_cut;
  unsigned short _min_coord_st12;
  unsigned short _min_coord_st3;
  unsigned short _min_gap_coord_st12;
  unsigned short _min_gap_coord_st3;
  Mode _mode;
  unsigned short _occupancy_cut;
};

#endif /* __MMUTFINDSTUBPAR_HH__ */





