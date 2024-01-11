// $Id: TMutMCTrk.hh,v 1.22 2015/04/01 16:04:22 snowball Exp $

/*!
  \file    TMutMCTrk.hh
  \brief   The muon tracker Monte Carlo track object
  \author  Sean Kelly
  \version $Revision: 1.22 $
  \date    $Date: 2015/04/01 16:04:22 $
*/

#ifndef __TMUTMCTRK_H__
#define __TMUTMCTRK_H__

#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<PHVector.h>
#include<MUTOO.h>

/*! @ingroup interface */
//!  The Muon tracker Monte Carlo track object 
/*!  The Muon tracker Monte Carlo track object */

class TMutMCTrk : public PHKey
{
  
 public:

  //! @name Constructors/Destructors
  //@{      
  /*! Default constructor */
  TMutMCTrk(){;}

  /*! Default constructor  */
  TMutMCTrk(const Key& key) : PHKey(key){;}

  /*! Destructor */
  virtual ~TMutMCTrk(){;}

  //@}

  //! @name Functional interface
  //@{    
  
  /*!  User word to tag key */  
  virtual ULong_t get_file_key() const 
  {return 0;}
  
  /*!  Get the PISA track number associated with this track */  
  virtual Long_t get_track_id() const 
  {return 0;}
  
  /*!  Get the particle ID associated with this track */
  virtual UShort_t get_pid() const 
  {return 0;}

  /*!  Get the particle ID associated with this track */
  virtual Int_t get_pidG4() const 
  {return get_pid();}
  
  /*!  Get the particle charge associated with this track */
  virtual Float_t get_charge() const 
  {return 0;}
  
  /*!  Get parent id information of this track */
  virtual Int_t get_parent_id() const 
  { return 0;}
  
  /*!  Get parent track id information of this track */
  virtual Int_t get_parent_track_id() const 
  {return 0;}
  
  /*!  Get grandparent track id information of this track */
  virtual Int_t get_grandparent_track_id() const 
  {return 0;}
  
  /*!  Set the particle ID associated with this track */
  virtual void set_pid(UShort_t pid) 
  {}

  /*!  Set the particle ID associated with this track */
  virtual void set_pid(Int_t pid) 
  {}
  
  /*!  Set user word */
  virtual void set_file_key(ULong_t file_key) 
  {}
  
  /*!  Set the PISA track number associated with this track */
  virtual void set_track_id(Long_t track_id) 
  {}
  
  /*!  Set the PISA track charge associated with this track */
  virtual void set_charge(Float_t charge) 
  {}
  
  /*! Set the parent id information with this track */
  virtual void set_parent_id(Int_t parent_id) 
  {}
  
  /*! set the track it of parent particle of this mc track */
  virtual void set_parent_track_id(Int_t parent_track_id) 
  {}
  
  /*! set the track it of grandparent particle of this mc track */
  virtual void set_grandparent_track_id(Int_t grandparent_track_id) 
  {}
  
  //@}

  //! utility method to store parameters based on PISA ancestry information
  /*! 
  although this method is virtual, there should be no need to re-implement it in subclasses, 
  because it never accesses the class members directly. As long as it stay so, one can modify
  the algorithm without breaking the backward compatibility
  */
  virtual void from_pisa( int track_id, MUTOO::Verbosity verbosity = MUTOO::NONE );
  
  //! @name Track Parameters at Primary Vertex
  //@{    
  /*!  Get the x-position at the origin of this track */
  virtual Float_t get_x_orig() const 
  {return 0;}
  
  /*!  Get the y-position at the origin of this track */
  virtual Float_t get_y_orig() const 
  {return 0;}
  
  /*!  Get the z-position at the origin of this track */
  virtual Float_t get_z_orig() const 
  {return 0;}
  
  /*!  Get the x-momentum at the origin of this track */
  virtual Float_t get_px_orig() const 
  {return 0;}
  
  /*!  Get the y-momentum at the origin of this track */
  virtual Float_t get_py_orig() const 
  {return 0;}
  
  /*!  Get the z-momentum at the origin of this track */
  virtual Float_t get_pz_orig() const 
  {return 0;}
  
  /*!  Get the total momentum at the origin of this track */
  virtual Float_t get_ptot_orig() const 
  {return 0;}
  
  /*!  Set the x-position at the origin of this track */
  virtual void set_x_orig(Float_t x_orig) 
  { }
  
  /*!  Set the y-position at the origin of this track */
  virtual void set_y_orig(Float_t y_orig) 
  { }
  
  /*!  Set the z-position at the origin of this track */
  virtual void set_z_orig(Float_t z_orig) 
  { }
  
  /*!  Set the x-momentum at the origin of this track */
  virtual void set_px_orig(Float_t px_orig) 
  { }
  
  /*!  Set the y-momentum at the origin of this track */
  virtual void set_py_orig(Float_t py_orig) 
  { }
  
  /*!  Set the z-momentum at the origin of this track */
  virtual void set_pz_orig(Float_t pz_orig) 
  { }

  //! calculate rapidity at vertex, assuming muo
  virtual double get_rapidity( void ) const;

  //@}

  //! @name Track Parameters at Upstream Gap in MUTR
  //@{    
  /*!  Get the x-position at the most upstream gap in MUTR  */
  virtual Float_t get_x_us_gap() const 
  {return 0;}
  
  /*!  Get the y-position at the most upstream gap in MUTR */
  virtual Float_t get_y_us_gap() const 
  {return 0;}
  
  /*!  Get the z-position at the most upstream gap in MUTR */
  virtual Float_t get_z_us_gap() const 
  {return 0;}
  
  /*!  Get the x-momentum at the most upstream gap in MUTR */
  virtual Float_t get_px_us_gap() const 
  {return 0;}
  
  /*!  Get the y-momentum at the most upstream gap in MUTR */
  virtual Float_t get_py_us_gap() const 
  {return 0;}
  
  /*!  Get the z-momentum at the most upstream gap in MUTR */
  virtual Float_t get_pz_us_gap() const 
  {return 0;}
  
  /*!  Get the total momentum at the most upstream gap in MUTR */
  virtual Float_t get_ptot_us_gap() const 
  {return 0;}
  
  /*!  Set the x-position at the most upstream gap in MUTR */
  virtual void set_x_us_gap(Float_t x_us_gap) 
  { }
  
  /*!  Set the y-position at the most upstream gap in MUTR */
  virtual void set_y_us_gap(Float_t y_us_gap) 
  { }
  
  /*!  Set the z-position at the most upstream gap in MUTR */
  virtual void set_z_us_gap(Float_t z_us_gap) 
  { }
  
  /*!  Set the x-momentum at the most upstream gap in MUTR */
  virtual void set_px_us_gap(Float_t px_us_gap) 
  { }
  
  /*!  Set the y-momentum at the most upstream gap in MUTR */
  virtual void set_py_us_gap(Float_t py_us_gap)
  { }
  
  /*!  Set the z-momentum at the most upstream gap in MUTR */
  virtual void set_pz_us_gap(Float_t pz_us_gap) 
  { }
  
  /*!  Returns the normalized tangent to the track in upstream gap */
  virtual PHVector get_us_tangent() const 
  { return PHVector(); }
  
  //@}

  //! @name Track Parameters at Downstream Gap in MUTR
  //@{    
  /*!  Get the x-position at the most downstream gap in MUTR  */
  virtual Float_t get_x_ds_gap() const 
  {return 0;}
  
  /*!  Get the y-position at the most downstream gap in MUTR */
  virtual Float_t get_y_ds_gap() const 
  {return 0;}
  
  /*!  Get the z-position at the most downstream gap in MUTR */
  virtual Float_t get_z_ds_gap() const 
  {return 0;}
  
  /*!  Get the x-momentum at the most downstream gap in MUTR */
  virtual Float_t get_px_ds_gap() const 
  {return 0;}
  
  /*!  Get the y-momentum at the most downstream gap in MUTR */
  virtual Float_t get_py_ds_gap() const 
  {return 0;}
  
  /*!  Get the z-momentum at the most downstream gap in MUTR */
  virtual Float_t get_pz_ds_gap() const 
  {return 0;}
  
  /*!  Get the total momentum at the most downstream gap in MUTR */
  virtual Float_t get_ptot_ds_gap() const 
  {return 0;}
  
  /*!  Set the x-position at the most downstream gap in MUTR */
  virtual void set_x_ds_gap(Float_t x_ds_gap) 
  { }
  
  /*!  Set the y-position at the most downstream gap in MUTR */
  virtual void set_y_ds_gap(Float_t y_ds_gap) 
  { }
  
  /*!  Set the z-position at the most downstream gap in MUTR */
  virtual void set_z_ds_gap(Float_t z_ds_gap) 
  { }
  
  /*!  Set the x-momentum at the most downstream gap in MUTR */
  virtual void set_px_ds_gap(Float_t px_ds_gap) 
  { }
  
  /*!  Set the y-momentum at the most downstream gap in MUTR */
  virtual void set_py_ds_gap(Float_t py_ds_gap) 
  { }
  
  /*!  Set the z-momentum at the most downstream gap in MUTR */
  virtual void set_pz_ds_gap(Float_t pz_ds_gap) 
  { }
  
  //@}
  
  /*! returns true if mc track crosses mutr station at same octant index */
  virtual bool same_octant( void ) const;

  //! @name Track Parameters at Downstream Gap in MUTR
  //@{    
  /*! Returns true if has hits in given station */
  virtual bool has_hits(UShort_t station) const;
  
  /*! Returns true if has hits each station */
  virtual bool is_ingeo() const;
  
  //@}
  //! @name Locators
  //@{    
  /*!  Get the arm number associated with this track */
  virtual UShort_t get_arm() const 
  {return 0;}
  
  /*!  Get the track index for this track */
  virtual UShort_t get_index() const 
  {return 0;}
  
  /*!  Set the arm number associated with this track */
  virtual void set_arm(UShort_t arm) 
  { }
  
  /*!  Set the track index for this track */
  virtual void set_index(UShort_t index) 
  { }
  
  //@}
  
  //! @name Dumpers
  //@{    
  /*! Print data */
  virtual void print(std::ostream& os = std::cout) const 
  {;}

  /*! TMutMCTrk version */
  virtual Int_t get_version() const 
  {return 0;}
  //@}
  
  ClassDef(TMutMCTrk,1)
};
		
#endif /* __TMutMCTrk_H__*/












