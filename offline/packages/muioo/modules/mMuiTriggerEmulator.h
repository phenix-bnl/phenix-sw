#ifndef __MMUITRIGGEREMULATOR_HH__
#define __MMUITRIGGEREMULATOR_HH__

#include<PHTimer.h>
#include<TMuiMCHitMapO.h>
#include<TMuiHitMapO.h>
#include<mMuiTriggerEmulatorPar.h>
#include<boost/array.hpp>
#include<list>
#include<bitset>

#include<gsl/gsl_rng.h>

/**
 * MUID/MUIOO Trigger Emulator
 */

/*! \ingroup modules */

/*! 
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMuiTriggerEmulatorPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMuiHitMapO*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
</table>
*/

class mMuiTriggerEmulator
{
 public: 
  
  mMuiTriggerEmulator(); 

  virtual ~mMuiTriggerEmulator(){}

  virtual PHBoolean event(PHCompositeNode*);

  bool is_deep_deep() { return _deep_deep; }
  bool is_deep_shallow() { return _deep_shallow; }
  
 private:  
  
  // private methods
  //
  void set_interface_ptrs(PHCompositeNode* top_node);

  void clear_state_variables();

  void fill_quadrant_list();

  void set_plane_bitset();
  
  void set_trigger_status();

  // Interface pointers
  //
  const mMuiTriggerEmulatorPar* _mod_par;           
  TMuiHitMapO* _hit_map;

  // Four lists of hits, one for each quadrant
  //
  typedef std::list<TMuiHitMapO::value_type> hit_list;
  boost::array<hit_list,4> _quad_hit_list;

  // Four bitsets one for each quadrant
  //  
  boost::array<std::bitset<10>,4> _quad_bitset;

  // Trigger bitmasks
  //
  std::bitset<10> _deep_mask;
  std::bitset<10> _shallow_mask;
  
  // Trigger booleans
  //
  bool _deep_deep;
  bool _deep_shallow;

  // Timer
  //
  PHTimer _timer;
};

#endif /* __MMUITRIGGEREMULATOR_HH__ */






















