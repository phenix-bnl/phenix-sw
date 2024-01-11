// $Id: mMutSlowSim.h,v 1.28 2011/12/24 04:48:31 slash Exp $
#ifndef __MMUTSLOWSIMMODULE_HH__
#define __MMUTSLOWSIMMODULE_HH__

//////////////////////////////////////////////////////////////////
/*!
	\file mMutSlowSim.h
	\brief Generate TMutMCHit and associated TMutMCTrk objects from PISA hits.
	\author S. Kelly
	\version $Revision: 1.28 $
	\date $Date: 2011/12/24 04:48:31 $
*/
//////////////////////////////////////////////////////////////////

// PHENIX includes
#include <PHPoint.h>
#include <mumhits.h>
#include <table_header.h>

// Mutoo includes
#include <mMutSlowSimPar.h>
#include <PHGslRng.h>
#include <PHTimeServer.h>
#include <TMutMCHitMap.h>

// SL/BOOST/GSL includes
#include<vector>
#include<boost/array.hpp>

// Forward declarations
class MutWire;
class MutStrip;
class PHCompositeNode;
class fkinWrapper;
class primaryWrapper;
class TMutMCTrkMap;
class TMCPrimaryMap;

/*! \ingroup modules */
//! Generate TMutMCHit and associated TMutMCTrk objects from PISA hits.
/*!
	TBD
<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutSlowSimPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
</tr>
<tr>
<td> TMutMCHitMap*</td>
<td> Monte-Carlo Hit Container </td>
<td> mutable </td>
</tr>
<tr>
<td> TMutMCTrkMap*</td>
<td> Monte-Carlo Trk Container </td>
<td> mutable </td>
</tr>

</table>
*/

class mMutSlowSim
{
 public:

	//! constructor
	mMutSlowSim();

	//! destructor
	virtual ~mMutSlowSim()
  {}

	//! event method
	virtual PHBoolean event(PHCompositeNode*);

	//! print summary of acceptance rejected hits
	void print_summary( std::ostream& out = std::cout );

 private:

	//! shortcut for strip number and associated charge
	typedef std::pair<int,double> strip_charge_pair;

	//! vector of (strip number,charge) pairs
	typedef std::vector<strip_charge_pair> strip_charge_vector;

	//! short name for fkin wrapper.
	//typedef PHIODataNode<fkinWrapper> fkinNode_t;

	//! write primary vertex information
	void write_primaries( void );

	//! get local pointers to needed nodes/maps
	void set_interface_ptrs(PHCompositeNode* top_node);

	//! generate MCHits/Tracks from pisa
	void simulator_loop( void );

	//! create MC tracks
	void finish_tracks();

	//! convert PISA hit into MCHits; associate strips.
	void digitize(const MUMHITS_ST&);

	//! associate MC tracks and hits
	void associate_mctrk(TMutMCHitMap::pointer);

	//! associate MC tracks and hits
	void associate_mctrk_and_primary();

	//! create a new MC track
	void fill_new_mctrk(TMutMCHitMap::pointer, int trackID = 0);

	/*! \brief
		Enumeration to convert PISA plane specification in
		mumhits table into arm, station, octant, plane.
	*/
	enum MutToPISA {PLANE=1,STATION=2,OCTANT=3,ARM=4};

	/*! \brief
		convert PISA plane specification in
		mumhits table into arm, station, octant, plane.
	*/
	static int decode_pisa_location(int pisa_location, MutToPISA mut_component)
		{

			// Get things out of FORTRAN-land and never look back
			int mod = static_cast<int>(std::pow(10.0,mut_component));
			double denom = std::pow(10.0,mut_component-1);
			int val = static_cast<int>((pisa_location%mod)/denom) - 1;

			// Fix PISA/MUT inconsistency South=1 (PISA) South=0 (MUT)
			if(mut_component==ARM) {
			if(val == 0) return 1;
			else if(val==1) return 0;
			}
			return val;
		}

	/*!
		extrapolate pisa hit to the nearest gap mid point z
		should this be done here or in mutgeom ?
	*/
	PHPoint extrapolate_pisa_hit( const MUMHITS_ST& ) const;

	//! return true if pisa_hit is in muon tracker active volume
	bool in_active_volume(const MUMHITS_ST&) const;

	//! return error on lorentz correction
	double get_lorentz_error(const MUMHITS_ST&) const;

	//! return error on w coordinate for smearing
	double get_w_error() const;

	//! return error on wire position
	double get_anode_error(const MUMHITS_ST&, MutWire*) const;

	//! return error for stereo cathodes
	double get_stereo_correction(double cos_stereo, double dx_wire, unsigned short station) const;

	//! return energy deposit for a MIP
	double get_energy_deposit( int arm, int station, int octant, int gap ) const;

	//! return primary track id for a MCHit by tracing all it's ancestor
	//
	int get_primary_trk_id(int parent_trk_id);

	//! parameter table
	const mMutSlowSimPar* _mod_par;

	//! mc hits container
	TMutMCHitMap* _mc_hit_map;

	//! mc trks container
	TMutMCTrkMap* _mc_trk_map;

	//! primary particle container
	TMCPrimaryMap* _mc_primary_map;

	//! header struct mum hits STAF table
	TABLE_HEAD_ST _mumhits_h;

	//! pointer to mumhits array
	MUMHITS_ST*	 _mumhits;

	//! pointer to the fkin table
	fkinWrapper*	_fkinNode;

	//! pointer to primary table
	primaryWrapper* _primary;

	//! number of MC hits/arm/station
	boost::array< unsigned int, mMutSlowSimPar::n_acceptance_parameters > _total_mc_hits;

	//! number of accepted MC hits/arm/station
	boost::array< unsigned int, mMutSlowSimPar::n_acceptance_parameters > _accepted_mc_hits;

	//! Random number generator
	PHGslRng _rng;

	//! module timer
	PHTimeServer::timer _timer;
};

#endif /* __MMUTSLOWSIM_HH__ */







