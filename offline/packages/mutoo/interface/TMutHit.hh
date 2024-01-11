#ifndef _TMUTHIT_H_
#define _TMUTHIT_H_

// $Id: TMutHit.hh,v 1.11 2011/12/29 20:19:30 slash Exp $

/*!
   \file TMutHit.hh
   \brief Class for Muon Tracker hits
   \author S. Kelly
   \version $Revision: 1.11 $
   \date $Date: 2011/12/29 20:19:30 $
*/

/*! @ingroup interface */
//! PHENIX Muon tracker hit object */
/*!
	TMutHit provides the interface to uncalibrated and calibrated MUTR hit
	data.
*/

#include<PHKey.hh>
#include<MUTOO.h>

class MutStrip;

typedef unsigned short UShort_t;

class TMutHit : public PHKey
{

	public:

	//! @name Constructors/Destructors
	//@{

	//! Default constructor
	TMutHit( void )
	{}

	//! Construct with key and location
	TMutHit(const Key& key) : PHKey(key)
	{}

	//! Virtual destructor
	virtual ~TMutHit( void )
	{}

	//@}

	//! @name Functional Interface
	//@{

	//! Charge derived form adc samples and pedistal gain calibration information
	virtual Float_t	 get_q( void ) const
	{return 0;}

	//! pulse t0 crossing
	virtual Float_t	 get_t( void ) const
	{return 0;}

	//! Q error
	virtual Float_t	 get_error_q( void ) const
	{return 0;}

	//! Adc sample [0,3]
	virtual UShort_t	get_adc(const UShort_t index) const
	{return 0;}

	//! Amu cell number [0,3] associated with like indexed adc value
	virtual UShort_t get_amu(const UShort_t index) const
	{return 0;}

	//! Get MutStrip object form hit pointer
	MutStrip* get_strip_geom( void ) const;

	//! Charge derived form adc samples and pedistal gain calibration information
	virtual void set_q(float q)
	{}

	//! Pulse t0 crossing
	virtual void set_t(float t)
	{}

	//! Q error
	virtual void set_error_q(float error_q)
	{}

	//! Adc sample [0,3]
	virtual void set_adc(UShort_t index, UShort_t adc)
	{}

	//! Amu cell number [0,3]
	virtual void set_amu(UShort_t index, UShort_t amu)
	{}

	//@}

	//! @name Locators
	//@{

  #ifndef __CINT__
  //! return class ID, mapped from class name
  virtual PHClassId::id_type get_class_id( void ) const
  { return (_class_id) ? _class_id : (_class_id = PHClassId::get( GetName() ) ); }
  #endif

	//! Arm [0,1]
	virtual UShort_t	get_arm( void ) const
	{return 0;}

	//! Station [0,2]
	virtual UShort_t	get_station( void ) const
	{return 0;}

	//! Octant [0,7]
	virtual UShort_t	get_octant( void ) const
	{return 0;}

	//! Half octant [0,1]
	virtual UShort_t	get_half_octant( void ) const
	{return 0;}

	//! Gap [0,2]
	virtual UShort_t	get_gap( void ) const
	{return 0;}

	//! Cathode [0,1]
	virtual UShort_t	get_cathode( void ) const
	{return 0;}

	//! Strip [0,124]
	virtual UShort_t	get_strip( void ) const
	{return 0;}

	#ifndef __CINT__
	//! Returns a boost tuple of (arm, station, octant, half_octant, gap, cathode)
	virtual MUTOO::cathode_locator get_location( void ) const
	{ return boost::make_tuple( get_arm(), get_station(), get_octant(), get_half_octant(), get_gap(), get_cathode() ); }
	#endif

	//! Arm [0,1]
	virtual void set_arm(UShort_t arm)
	{}

	//! Station [0,2]
	virtual void set_station(UShort_t station)
	{}

	//! Octant [0,7]
	virtual void set_octant(UShort_t octant)
	{}

	//! Half octant [0,1]
	virtual void set_half_octant(UShort_t half_octant)
	{}

	//! Gap [0,2]
	virtual void set_gap(UShort_t gap)
	{}

	//! Cathode [0,1]
	virtual void set_cathode(UShort_t cathode)
	{}

	//! Strip [0,124]
	virtual void set_strip(UShort_t strip)
	{}

  enum Status
  {
    //! strip is at the edge of the cathode
    EDGE,

    //! hit is saturated
    SATURATED,

    //! hit is attenuated
    ATTENUATED,

    //! strip is dead
    DEAD,

    //! strip is bad (meaning that the error is too large)
    BAD

  };

  //! returns true if strip is on the edge of the detector
  virtual bool get_is_edge( void ) const
  { return get_status() & (1<<EDGE); }

  //! set strip is edge
  virtual void set_is_edge( void )
  { set_status( get_status() | (1<<EDGE) ); }

  //! returns true if strip charge is saturated (from sample loop)
  virtual bool get_is_saturated( void ) const
  { return get_status() & (1<<SATURATED); }

  //! set strip is saturated
  virtual void set_is_saturated( void )
  { set_status( get_status() | (1<<SATURATED) ); }

  //! returns true if hit is attenuated
  virtual bool get_is_attenuated( void ) const
  { return get_status() & (1<<ATTENUATED); }

  //! set strip is attenuated
  virtual void set_is_attenuated( void )
  { set_status( get_status() | (1<<ATTENUATED) ); }

  //! true if hit is dead
  virtual bool get_is_dead( void ) const
  { return get_status() & (1<<DEAD); }

  //! true if hit is dead
  virtual void set_is_dead( void )
  { set_status( get_status() | (1<<DEAD) ); }

  //! true if hit is bad
  virtual bool get_is_bad( void ) const
  { return get_status() & (1<<BAD); }

  //! true if hit is bad
  virtual void set_is_bad( void )
  { set_status( get_status() | (1<<BAD) ); }

  //! Get the status word
  virtual ULong_t get_status( void ) const
  { return 0; }

  //@}

  //! @name Dumpers
  //@{
  //! Print data members to ostream os, defaults to std::cout
  virtual void print(std::ostream& os = std::cout) const
  {}
  //@}

  protected:

  //! Get the status word
  virtual void set_status( ULong_t )
  {}

  //! status word
  virtual void clear_status( void )
  {}

  private:

#ifndef __CINT__

  //! static class ID
  static PHClassId::id_type _class_id;

#endif

	ClassDef(TMutHit,1)
};

#endif
