#ifndef __MMUTRESPONSE_HH__
#define __MMUTRESPONSE_HH__

// $Id: mMutResponse.h,v 1.17 2011/12/24 04:48:30 slash Exp $

#include<PHGslRng.h>
#include<PHTimeServer.h>
#include<TMutMCHitMap.h>
#include<TMutHitMap.h>
#include<mMutResponsePar.h>

class PHCompositeNode;

/*! \ingroup modules */
//! Reads TMutMCHit and generates TMutHit objects.
/*! 
	Reads TMutMCHit and generates TMutHit objects with
	appropriate masking for dead/low efficiency strips.	Adds a gaussian
	noise on active cathodes to simulate the effect of noisy electronics.
	Currently outputs fills the TMutHitMap (new framework) and/or 
	dMutCalibCathode (old framework) depending on runtime flags.

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutResponsePar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> const TMutMCHitMap*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutHitMap*</td>
<td> Parameter Table </td>
<td> mutable </td>
</tr>
</table>
*/

class mMutResponse
{
 public: 
	
	//! response
	mMutResponse(); 
	
	//! destructor
	virtual ~mMutResponse()
  {}
	
	//! event summary
	virtual PHBoolean event(PHCompositeNode*);
		
 private:	
	
	// private methods

	//! initialize local pointers
	void set_interface_ptrs(PHCompositeNode* top_node);
	
	//! run the response
	void response_loop();
	
	//! distributes MC hit on strips
	void update_cathode(TMutMCHitMap::const_pointer hit_ptr, bool skip_cath[MUTOO::NumberOfPlanes]);
	
	//! assiciates MC hit to its hit counterparts on each cathode
	void associate_mchit(TMutHitMap::iterator hit_iter, TMutMCHitMap::const_pointer mc_hit_ptr);
						
	//! check given MCHit HV to tell if hit must be generated or not
	bool check_hv_status(TMutMCHitMap::const_pointer mc_hit_ptr);
	
	//! returns true if strip status is OK
	bool check_strip_status(unsigned short arm,				 
				unsigned short station,		 
				unsigned short octant,			
				unsigned short half_octant, 
				unsigned short gap,				 
				unsigned short cathode,		 
				unsigned short strip);	
	
	//! get the adc samples from a given hit
	void calculate_adc_samples(TMutHitMap::const_pointer hit_ptr);
	
	//! generate electronic noise (do nothing)
	void get_electronics_noise();
	
	// Interface pointers
	//! pointer to module parameters
	const mMutResponsePar* _mod_par;	
	
	//! pointer to MC hit map				 
	const TMutMCHitMap* _mc_hit_map;
	
	//! pointer to hit map	
	TMutHitMap* _hit_map;
	
	//! Random number generator
	PHGslRng _rng;

	//! Timer
	PHTimeServer::timer _timer;
};

#endif /* __MMUTRESPONSE_HH__ */
