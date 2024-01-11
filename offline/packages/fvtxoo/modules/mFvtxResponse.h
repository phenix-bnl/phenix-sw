#ifndef __MFVTXRESPONSE_HH__
#define __MFVTXRESPONSE_HH__

#include<PHTimeServer.h>
#include<TFvtxMCHitMap.h>
#include<TFvtxHitMap.h>
#include<mFvtxResponsePar.h>
#include <TTree.h>
#include<PHGslRng.h>

class TMutMCTrkMap;
class PHCompositeNode;

/*! \ingroup modules */
//! Reads TFvtxMCHit and TFvtxMCTrk and generates TFvtxHit objects.
/*! 
  Reads TFvtxMCHit and TFvtxMCTrk and generates TFvtxHit objects with
  appropriate masking for dead/low efficiency strips.  Adds a gaussian
  noise on active strip to simulate the effect of noisy electronics.
  Currently outputs fills the TFvtxHitMap (new framework).

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mFvtxResponsePar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> const TFvtxMCHitMap*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> const TFvtxMCTrkMap*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TFvtxHitMap*</td>
<td> Parameter Table </td>
<td> mutable </td>
</tr>
</table>
*/

class mFvtxResponse
{
 public: 
  
  //! response
  mFvtxResponse(); 
  
  //! destructor
  virtual ~mFvtxResponse(){} 
  
  //! event summary
  virtual PHBoolean event(PHCompositeNode*);

  //! print summary of acceptance rejected hits                                                 
  void print_summary( std::ostream& out = std::cout );
   
 private:  
  
  // private methods

  //! initialize local pointers
  void set_interface_ptrs(PHCompositeNode* top_node);
  
  //! book evaluation tree                                                                                                 
  void book_evaluationRes_tree( void );

  //! fill evaluation tree                                                                                                 
  void fill_evaluationRes_tree( void );

  //! run the response
  void response_loop();
  
  //! add noise to the charge on the strhips
  void add_noise_to_charge();
  
  //! add noise hits to the event
  void add_noise_hits();
  
  //! apply on-chip zero-suppression 
  void zero_suppress_loop();
  
  //! Convert strip charge to ADC value
  void do_adc_conversion();
  
  //! distributes MC hit on strips
  void create_hit(TFvtxMCHitMap::const_pointer mc_hit_ptr);
           
  // Interface pointers
  //! pointer to module parameters
  const mFvtxResponsePar* _mod_par;  
  
  //! pointer to MC hit map         
  const TFvtxMCHitMap* _mc_hit_map;

  //! pointer to hit map  
  TFvtxHitMap* _hit_map;
  
  //! Random number generator
  PHGslRng _rng;

  //! Timer
  PHTimeServer::timer _timer;
  //! pisa evaluation tree                                                                                                 
  TTree* _FvtxHit_tree;

  //! event counter                                                                                                        
  int _evt;

  int _arm;

  int _cage;

  int _station;

  int _sector;

  int _column;

  double _q;

};

#endif /* __MFVTXRESPONSE_HH__ */






