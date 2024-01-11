#ifndef __TMUTCOORD_H__
#define __TMUTCOORD_H__

// $Id: TMutCoord.hh,v 1.18 2011/12/29 20:19:29 slash Exp $

/*!
   \file TMutCoord.hh
   \brief Class for Muon Tracker 1D Cathode Coordinates
   \author S. Kelly
   \version $Revision: 1.18 $
   \date $Date: 2011/12/29 20:19:29 $
*/

#include<PHKey.hh>
#include<PHLine.h>
#include<MUTOO.h>
#include<map>

/*! @ingroup interface */

//!  The Muon tracker coordinate object 
/*!  The Muon tracker coordinate object */

class TMutCoord : public PHKey
{
  
 public:

  //! Enumeration for coordinate status bits 
  enum Status {PEAK_BOUND, LOW_CHARGE, HIGH_CHARGE, STEREO, USEDTWICE};
  
  //! @name Constructors/Destructors
  //@{    
  //! Default Constructor 
  TMutCoord()
  {}

  //!Construct with arm, station, octant, half_octant, gap 
  TMutCoord(const Key& key) : PHKey(key) 
  {}

  //! Default Destructor 
  virtual ~TMutCoord()
  {}
  
  //@}

  //! @name Functional Interface
  //@{
  //! Return coordinate 
  virtual PHLine get_coord() const
  {return PHLine();}
  
  //! Return end point 
  virtual PHPoint get_coord_end() const 
  { return PHPoint();}
  
  //! Return starting point 
  virtual PHPoint get_coord_begin() const 
  { return PHPoint();}
  
  //! Return mid-point 
  virtual PHPoint get_coord_midpoint() const 
  { return PHPoint();}
  
  //! Return mean z coord 
  virtual double get_mean_z() const 
  { return 0;}
  
  //! Charge associated with peak strip 
  virtual Float_t get_q_peak() const 
  {return 0;}
  
  //! Total charge associated with centroid fit
  virtual Float_t get_q_tot() const 
  {return 0;}
  
  //! Peak strip 
  virtual UShort_t get_peak_strip() const 
  {return 0;}
  
  //! Offset from peak strip (cm) 
  virtual Float_t get_w() const
  {return 0;}
  
  //! Offset from the 0th strip (cm) 
  virtual double get_w_absolute() const;
  
  //! Cosine of angle between anode wire and strip 
  virtual Float_t get_cos_theta_wire() const 
  {return 0;}
  
  //! Coordinate error  
  virtual Float_t get_error() const 
  {return 0;}
  
  //! Error on fitted total charge 
  virtual double get_q_error()const 
  {return 0;}

  //! Coordinate 
  virtual void set_coord(const PHLine& coord)
  {}
  
  //! Charge associated with peak strip 
  virtual void set_q_peak(Float_t q_peak) 
  {}
  
  //! Total charge associated with this centroid 
  virtual void set_q_tot(Float_t q_tot) 
  {}
  
  //! Peak strip 
  virtual void set_peak_strip(UShort_t peak_strip) 
  {}
  
  //! Offset from peak strip (cm) 
  virtual void set_w(Float_t w) 
  {}
  
  //! Cosine of angle between anode wire and strip 
  virtual void set_cos_theta_wire(Float_t cos_theta) 
  {}
  
  //! Coordinate error 
  virtual void set_error(double error)
  {}
  
  //! Error on fitted total charge 
  virtual void set_q_error(double q_error)
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
  virtual UShort_t  get_arm() const 
  {return 0;}
  
  //! Station [0,2] 
  virtual UShort_t  get_station() const 
  {return 0;}
  
  //! Octant [0,7] 
  virtual UShort_t  get_octant() const 
  {return 0;}
  
  //! Half octant [0,1] 
  virtual UShort_t  get_half_octant() const 
  {return 0;}
  
  //! Gap [0,2] 
  virtual UShort_t  get_gap() const 
  {return 0;}
  
  //! Index [0,1023] 
  virtual UShort_t  get_cathode() const 
  {return 0;}
  
  //! Index [0,1023] 
  virtual UShort_t  get_index() const 
  {return 0;}  
  
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
  
  //! Index [0,1023] 
  virtual void set_index(UShort_t index) 
  {}  
	
	#ifndef __CINT__  
  //! Returns a boost tuple of (arm, station, octant, half_octant, gap, cathode) 
  virtual MUTOO::cathode_locator get_location() const 
  {
    return boost::make_tuple( get_arm(), get_station(), get_octant(), get_half_octant(), get_gap(), get_cathode() );
  }
	#endif  
  
	//@}
  //! @name TMutCoord Status
  //@{  
  //! Set the peak_bound bit 
  virtual void set_peak_bound() 
  {}
  
  //! Get the peak_bound bit 
  virtual bool get_peak_bound() const 
  { return 0; }

  //! Set the low_charge bit 
  virtual void set_low_charge() 
  {}
  
  //! Get the low_charge bit 
  virtual bool get_low_charge() const 
  { return 0; }

  //! Set the high_charge bit 
  virtual void set_high_charge() 
  {}
  
  //! Get the high_charge bit 
  virtual bool get_high_charge() const 
  { return 0; }

  //! Set the stereo bit 
  virtual void set_stereo() {}
  
  //! Get the stereo bit 
  virtual bool get_stereo() const 
  { return 0; }

  //! Set the usedtwice bit 
  virtual void set_usedtwice() {}
  
  //! Get the usedtwice bit 
  virtual bool get_usedtwice() const 
  { return 0; }

  //! Set the status word 
  virtual void set_status(ULong_t status) 
  {}
  
  //! Get the status word 
  virtual ULong_t get_status() const 
  { return 0;}
  
  //! Clear the status word 
  virtual void clear_status() 
  {}
  //@}

  //! @name Dumpers
  //@{    
  //! Print object to stream, default stream is std::cout 
  virtual void print(std::ostream& os = std::cout) const
  {} 
  //@}

  //! @name Interface to non persistent data members
  //
#ifndef __CINT__
  //! Store chi square increment associated with track 
  virtual void push_chi_sqr_inc(ULong_t track_key, double chi_square)
  {}
  
  //! Retrieve chi square increment associated with track 
  virtual double get_chi_sqr_inc(ULong_t track_key)
  {return 0;}
  
  //@}
#endif

  private:
  
#ifndef __CINT__
  
  //! static class ID 
  static PHClassId::id_type _class_id;

#endif
  
  ClassDef(TMutCoord,1)
};

#endif










