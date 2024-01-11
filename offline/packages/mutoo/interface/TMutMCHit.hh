#ifndef __TMUTMCHIT_H__
#define __TMUTMCHIT_H__

// $Id: TMutMCHit.hh,v 1.13 2011/12/29 20:19:30 slash Exp $

/*!
   \file TMutMCHit.hh
   \brief The Muon tracker Monte Carlo hit object 
   \author S. Kelly
   \version $Revision: 1.13 $
   \date $Date: 2011/12/29 20:19:30 $
*/

#include<TDataType.h>
#include<PHKey.hh>
#include<TMutMCStrip.hh>
#include<PHException.h>
#include<PHPoint.h>
#include<PHVector.h>
#include<TMutFitPar.hh>

#include<MUTOO.h>

/*! @ingroup interface */
//!	The Muon tracker Monte Carlo hit object 
class TMutMCHit : public PHKey 
{
  
  public:

  //! Name for the list of TMutMCStrip contained in this object 
  typedef std::vector<TMutMCStrip> strip_list;

  //! Name for the TMutMCStrip list iterator	
  typedef strip_list::const_iterator strip_iterator;

  //! @name Constructors/Destructor
  //@{		
  //! Default 
  TMutMCHit()
  {}

  //! Construct with key 
  TMutMCHit(const Key& key) : PHKey(key)
  {}

  //! Default 
  virtual ~TMutMCHit() 
  {}
  //!@}

  //! @name TMutMCStrip Interface
  //@{		
  //! construct and add a new strip with specified parameters 
  virtual void add_strip(UShort_t cathode, UShort_t strip, Float_t q)
  {}
  
  //! clear strip list 
  virtual void clear_strip_list()
  {}	
           
  //! number of strips associated with this MChit 
  virtual size_t get_n_strip() const 
  { return 0;}
  
  //! read only access to strip list 
  virtual const strip_list* get_strip_list() const 
  {
    // Print a warning about leaked resource, in case it is called.
    //
    MUTOO::TRACE(DESCRIPTION("Resource leak from base"));
    return new strip_list();
  }
  //!@}

  //! @name Functional Interface
  //@{		
  //!	User word to tag which file 
  virtual ULong_t get_file_key() const 
  {return 0;}
  
  //!	Get the PISA track number associated with this track 
  virtual Long_t get_track_id() const 
  {return 0;}
  
  //! Get time-of-flight associated with this hit 
  virtual Float_t get_tof() const 
  {return 0;}
  
  //! Get energy loss associated with this hit 
  virtual Float_t get_eloss() const 
  {return 0;}
  
  //! Get x-position of track for this hit 
  virtual Float_t get_x() const 
  {return 0;}
  
  //! Get y-position of track for this hit 
  virtual Float_t get_y() const 
  {return 0;}
  
  //! Get z-position of track for this hit 
  virtual Float_t get_z() const 
  {return 0;}
  
  //! Get PHPoint(x,y,z) of this hit 
  virtual PHPoint get_coord() const 
  { return PHPoint( get_x(), get_y(), get_z() ); }
  
  //! Get x-momentum of track at this hit position 
  virtual Float_t get_px() const 
  {return 0;}
  
  //! Get y-momentum of track at this hit position 
  virtual Float_t get_py() const 
  {return 0;}
  
  //! Get z-momentum of track at this hit position 
  virtual Float_t get_pz() const 
  {return 0;}

  //! get momentum
  virtual PHVector get_momentum() const
  { return PHVector( get_px(), get_py(), get_pz() ); }
  
  //! Return the track tangent in TMutFitPar format 
  virtual TMutFitPar get_fit_par() const 
  { return TMutFitPar(); }
  
  //! W coordinate without resolution & stereo 
  virtual Float_t get_w_true(UShort_t index) const 
  { return 0;}
  
  //! W coordinate with resolution & stereo 
  virtual Float_t get_w_digit(UShort_t index) const
  { return 0;}
      
  //! User word to tag which file 
  virtual void set_file_key(ULong_t file_key) 
  { }
  
  //! PISA track number associated with this track 
  virtual void set_track_id(Long_t track_id) 
  { }
  
  //! Set time-of-flight associated with this hit 
  virtual void set_tof(Float_t tof) 
  { }
  
  //! Get energy loss associated with this hit 
  virtual void set_eloss(Float_t eloss) 
  { }
  
  //! Set x-position of track for this hit 
  virtual void set_x(Float_t x) 
  { }
  
  //! Set y-position of track for this hit 
  virtual void set_y(Float_t y) 
  { }
  
  //! Set z-position of track for this hit 
  virtual void set_z(Float_t z) 
  { }
  
  //! Set x-momentum of track at this hit position 
  virtual void set_px(Float_t px) 
  { }
  
  //! Set y-momentum of track at this hit position 
  virtual void set_py(Float_t py) 
  { }
  
  //! Set z-momentum of track at this hit position 
  virtual void set_pz(Float_t pz) 
  { }
  
  //! W coordinate without resolution & stereo	
  virtual void set_w_true(UShort_t index, Float_t w_true) 
  { }
  
  //! W coordinate with resolution & stereo	
  virtual void set_w_digit(UShort_t index, Float_t w_digit) 
  { }
  
  //!@}
  
  //! @name Locators
  //@{		
 
  #ifndef __CINT__
  //! return class ID, mapped from class name
  virtual PHClassId::id_type get_class_id( void ) const
  { return (_class_id) ? _class_id : (_class_id = PHClassId::get( GetName() ) ); }
  #endif
  
  //! Get arm number for this hit 
  virtual UShort_t get_arm() const 
  {return 0;}
  
  //! Get station number for this hit 
  virtual UShort_t get_station() const 
  {return 0;}
  
  //! Get octant number for this hit 
  virtual UShort_t get_octant() const 
  {return 0;}
  
  //! Get half-octant number for this hit 
  virtual UShort_t get_half_octant() const 
  {return 0;}
  
  //! Get gap number for with this hit 
  virtual UShort_t get_gap() const 
  {return 0;}
  
  #ifndef __CINT__	
  //! Return gap location for this hit 
  virtual MUTOO::gap_locator get_location( void ) const
  { return boost::make_tuple( get_arm(), get_station(), get_octant(), get_half_octant(), get_gap() ); }
  #endif	
  
  //! Get anode wire number for with this hit 
  virtual UShort_t get_wire() const 
  {return 0;}
  
  //! Get hit index associated with this hit 
  virtual UShort_t get_index() const 
  {return 0;}
  
  //! Set arm number for this hit 
  virtual void set_arm(UShort_t arm) 
  { }
  
  //! Set station number for this hit 
  virtual void set_station(UShort_t station) 
  { }
  
  //! Set octant number for this hit 
  virtual void set_octant(UShort_t octant) 
  { }
  
  //! Set half-octant number for this hit 
  virtual void set_half_octant(UShort_t half_octant) 
  { }
  
  //! Set gap number for with this hit 
  virtual void set_gap(UShort_t gap) 
  { }
  
  //! Set anode wire number for with this hit 
  virtual void set_wire(UShort_t wire) 
  { }
  
  //! Set hit index associated with this hit 
  virtual void set_index(UShort_t index) 
  { }
  
  //!@}
  
  //! @name Dumpers
  //@{		
  //! dumper 
  virtual void print(std::ostream& os = std::cout) const 
  {}
  //!@}

  private:
  
#ifndef __CINT__
  
  //! static class ID 
  static PHClassId::id_type _class_id;

#endif  
  
  ClassDef(TMutMCHit,1)
};


#endif /* __TMutMCHit_H__*/



