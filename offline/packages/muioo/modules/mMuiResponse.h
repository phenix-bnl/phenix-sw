#ifndef __MMUIRESPONSE_HH__
#define __MMUIRESPONSE_HH__

#include<PHGslRng.h>
#include<PHTimeServer.h>
#include<TMuiMCHitMapO.h>
#include<TMuiHitMapO.h>
#include<mMuiResponsePar.h>

class PHCompositeNode;
class TMutMCTrkMap;

/*! \ingroup modules */
//! Reads TMuiMCHit and TMutMCTrk and generates TMuiHit objects.
/*! 
  Reads TMuiMCHit and TMutMCTrk and generates TMutHit objects with
  appropriate masking for dead/low efficiency twopacks. 
  Currently outputs fills the TMuiHitMapO (new framework).

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMuiResponsePar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> const TMuiMCHitMapO*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> const TMutMCTrkMap*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMuiHitMapO*</td>
<td> Parameter Table </td>
<td> mutable </td>
</tr>
</table>
*/

class mMuiResponse
{
 public: 
  
  mMuiResponse(); 
  virtual ~mMuiResponse(){}
  virtual PHBoolean event(PHCompositeNode*);
  
 private:  
  
  // private methods
  void set_interface_ptrs(PHCompositeNode* top_node);

  bool is_alive(TMuiMCHitMapO::const_pointer mc_hit_ptr); 

  void response_loop();

  void fill_hit(TMuiMCHitMapO::pointer mc_hit_ptr);

  bool check_efficiency(UShort_t arm, 
			UShort_t plane, 
			UShort_t panel, 
			UShort_t orientation, 
			UShort_t twopack,
			float fraction) const;
  
  //! Interface pointers
  const mMuiResponsePar* _mod_par;
  const TMutMCTrkMap* _mc_trk_map;  
  TMuiMCHitMapO* _mc_hit_map;  
  TMuiHitMapO* _hit_map;
  
  //! Random number generator.
  PHGslRng _rng;

  //! Timer
  PHTimeServer::timer _timer;
};

#endif /* __MMUIRESPONSE_HH__ */
