#ifndef __TMUTGAPCOORD_H__
#define __TMUTGAPCOORD_H__

// $Id: TMutGapCoord.hh,v 1.9 2011/12/29 20:19:30 slash Exp $

/*!
   \file TMutGapCoord.hh
   \brief Class for Muon Tracker 2D Gap Coordinates
   \author S. Kelly
   \version $Revision: 1.9 $
   \date $Date: 2011/12/29 20:19:30 $
*/

#include<PHKey.hh>
#include<PHPoint.h>
#include<MUTOO.h>
#include<PHVector.h>

/*! @ingroup interface */

//!  The Muon tracker gap coord object 
/*!  The Muon tracker gap coord object */

class TMutGapCoord : public PHKey
{
  
 public:

  //! @name Constructors/Destructors
  //@{    
  //! Default Constructor 
  TMutGapCoord()
  {}

  //! Construct with arm, station, octant, half_octant, gap 
  TMutGapCoord(const Key& key) : PHKey(key)
  {}

  //! Virtual destructor 
  virtual ~TMutGapCoord()
  {}
  
  //@}

  //! @name Functional Interface
  //@{    
  //! Gap coordinate 
  virtual void set_coord(const PHPoint& coord)
  {}  

  //! Return PHPoint by value 
  virtual PHPoint get_coord() const 
  { return PHPoint(); }

  //! Anode wire number 
  virtual UShort_t get_anode() const 
  { return 0; }  

  //! Anode wire number 
  virtual void set_anode(UShort_t anode) 
  {}

  //! Distance to closest anode wire 
  virtual double get_anode_dca() const 
  { return 0; }

  //! Distance to closest anode wire 
  virtual void set_anode_dca(double anode_dca) 
  {}  

  //! Unit vector in direction of associated anode wire 
  virtual PHVector get_anode_direction() const 
  { return PHVector(); }

  //! Charge correction for plane 0-plane 1 matching (q2=(1+corr)*q1)
  virtual double get_charge_corr() const 
  { return 0; }

  //! Charge correction for plane 0-plane 1 matching (q2=(1+corr)*q1)
  virtual void set_charge_corr(double charge_corr) 
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
  virtual UShort_t  get_station() const {return 0;}
  
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
  
  //! Index [0,1023] 
  virtual void set_index(UShort_t index) 
  {} 
  
#ifndef __CINT__  
  //! Returns a boost tuple of (arm, station, octant, half_octant, gap) 
  virtual MUTOO::gap_locator get_location() const 	
	{ return boost::make_tuple( get_arm(), get_station(), get_octant(), get_half_octant(), get_gap() ); }

#endif  
  
  //@}
  
  //! @name Dumpers
  //@{  
  //! Print object to stream, default stream is std::cout 
  virtual void print(std::ostream& os = std::cout) const
  {} 
  //@}

  private:
  
#ifndef __CINT__
  
  //! static class ID 
  static PHClassId::id_type _class_id;

#endif
  
  ClassDef(TMutGapCoord,1)
};


#endif /* __TMUTGAPCOORD_H__*/










