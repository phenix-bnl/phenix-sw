#ifndef __MMUTCALIB_HH__
#define __MMUTCALIB_HH__

// $Id: mMutCalibrate.h,v 1.10 2006/11/27 15:42:11 hpereira Exp $
/*
	\file mMutCalibrate.h
	\author	S.Kelly, H.Pereira
	\version $Revision: 1.10 $
	\date $Date: 2006/11/27 15:42:11 $
*/

// MUTOO
#include<MUTOO_FEM.h>
#include<TMutHitMap.h>
#include<mMutCalibratePar.h>
#include<PHTimeServer.h>

// BOOST/STL
#include<boost/array.hpp>

class MutCalibStrip;
class mMutResponsePar;

/*! \ingroup modules */

//!Applies pedistal and gain calibrations to raw ADC values.
//!Invokes currently selected pulse fitting scheme to derive
//!charge (q) and t0 values associated with active cathode
//!strips

/*!

The runtime parameter table mMutCalibratePar specifies the pulse
fitting scheme via the enumeration mMutCalibratePar::Mode.  Currently
available pulse fitting modes are DATA and SIMULATIONS.  In addition
the runtime parameter table is used to specify the relative timing
of samples and the rise and fall time of the pulse which are
subequently used as non-fit parameters in the pulse fitting.

<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutCalibratePar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutHitMap*</td>
<td> IOC</td>
<td> mutable </td>
</tr>
<tr>
<td> MutCalibStrip* </td>
<td> External Interface </td>
<td> mutable </td>
</tr>
</table>
</ul>
*/

class mMutCalibrate
{

	public:

	//! constructor
	mMutCalibrate();

	//! destructor
	virtual ~mMutCalibrate()
	{}

	//! event method
	virtual PHBoolean event(PHCompositeNode*);

	private:

	//! calibrated sample data
	class Sample {

		public:

		//! constructor
		Sample() : t(0), q(0) {}

		//! time
		Float_t t;

		//! charge
		Float_t q;

	};

	//! array of calibrated sample data
	class Samples: public boost::array<Sample,MUTOO_FEM::NSAMPLES>
	{

		public:

		//! constructor
		Samples( void ):
			_dead( false ),
			_rejected( false ),
			_saturated( false )
		{ assign( Sample() ); }

		//! channel is dead
		bool _dead;

		//! hit is to be removed from the map
		bool _rejected;

		//! channel is saturated
		bool _saturated;

	};

	//! get local pointers to needed nodes
	void set_interface_ptrs(PHCompositeNode* top_node);

	//! define sample fit/average method
	void set_fit_type(mMutCalibratePar::FitType);

	//! fill samples (charge unit) for given hit
	Samples get_calibrated_samples(const TMutHitMap::pointer hit_ptr);

	//! compute hit charge from calibrated samples
	void fit_calibrated_samples(const Samples& samples, TMutHitMap::pointer hit_ptr );

	//!@name attributes
	//@{
	//! module parameters
	mMutCalibratePar* _mod_par;

	//! response module parameter
	/*!
	it is needed to retrieve any additional smearing on the charge that is not 
	accounted for in the error. This has no effect when processing real data
	*/
	mMutResponsePar* _res_mod_par;
	
	//! mutr hit map
	TMutHitMap* _hit_map;

	//! calibration singleton (from MutGeom)
	MutCalibStrip* _calib;

	//! Module timer
	PHTimeServer::timer _timer;

	//!@name calibrated sample fit
	//@{
	
	// We define a generic interface to sample fitting
	// functions via declaring a pointer to member
	// f(const Samples&, TMutHitMap::pointer)

	//! pointer to sample fit member function syntax
	/*!
	the read is that FitFunctionPtr is a pointer to a
	member function that has the same arguments as f(..) above.
	*/
	typedef void (mMutCalibrate::* FitFunctionPtr) (const Samples&, TMutHitMap::pointer);

	// If you want to implement a new sample fitting scheme then
	// simply define a new member function that has an argument
	// list that is identical to f(...) and set the below pointer
	// equal to the address of the member function in the
	// set_fit_type method;

	//! pointer to sample fit member function
	FitFunctionPtr _fit_func;

	// Currently available sample fitting routines
	//! use sample average to get the hit charge from samples
	void average_fit(const Samples&, TMutHitMap::pointer);

	//! use exponential fit function to get hit charge from samples
	void exponential_fit(const Samples&, TMutHitMap::pointer);
	//@}
};

#endif /* __MMUTCALIB_HH__ */

