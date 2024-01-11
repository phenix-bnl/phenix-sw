// Interface Object Class : TMCPrimary
// Author: S.Kelly
// Date: 9/07/03
// Description: Class for Monte-Carlo Primary 

#ifndef __TMCPRIMARY_H__
#define __TMCPRIMARY_H__

#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<PHVector.h>
#include<MUTOO.h>
#include<cmath>

/*! @ingroup interface */
//! Monte-Carlo Primary Object
/*! Monte-Carlo Primary Object */

class TMCPrimary : public PHKey
{
  
 public:

  //! @name Constructors/Destructors
  //@{      
  /*! Default constructor */
  TMCPrimary(){;}

  /*! Default constructor  */
  TMCPrimary(const Key& key) : PHKey(key){;}

  /*! Destructor */
  virtual ~TMCPrimary(){;}

  //@}

  //! @name Functional interface
  //@{    
  /*!  User word to tag key */  
  virtual ULong_t get_file_key() const {return 0;}
  /*!  Get the PISA track number associated with this track */  
  virtual Long_t get_pisa_process_id() const {return 0;}
  /*!  Get the particle ID associated with this track */
  virtual UShort_t get_pid() const {return 0;}
  /*!  Get the particle ID associated with this track */
  virtual Int_t get_pidG4() const {return get_pid();}
  /*!  Get the track ID associated with this track */
  virtual Int_t get_trk_id() const {return 0;}

  /*!  Set user word */
  virtual void set_file_key(ULong_t file_key) {}
  /*!  Set the particle ID associated with this track */
  virtual void set_pisa_process_id(Long_t pisa_process_id) {}
  /*!  Set the particle ID associated with this track */
  virtual void set_pid(UShort_t pid) {}
  /*!  Set the particle ID associated with this track */
  virtual void set_pid(Int_t pid) {}
  /*!  Set the track ID associated with this track */
  virtual void set_trk_id(Int_t trk_id) {}
  //@}

  //! @Event infomation, impact parameter and Ncoll.
  //@{
  /*! impact parameter */
  virtual Float_t get_imp() const { return 0;}
  virtual void set_imp(Float_t imp) {}
  /*! N binary collisions */
  virtual Int_t get_Ncoll() const { return 0;}
  virtual void set_Ncoll(Int_t Ncoll) {}
  //@}

  //! @name Track Parameters at Primary Vertex
  //@{    
  /*!  Get the x-position at the origin of this track */
  virtual Float_t get_x_orig() const {return 0;}
  /*!  Get the y-position at the origin of this track */
  virtual Float_t get_y_orig() const {return 0;}
  /*!  Get the z-position at the origin of this track */
  virtual Float_t get_z_orig() const {return 0;}
  /*!  Get the x-momentum at the origin of this track */
  virtual Float_t get_px_orig() const {return 0;}
  /*!  Get the y-momentum at the origin of this track */
  virtual Float_t get_py_orig() const {return 0;}
  /*!  Get the z-momentum at the origin of this track */
  virtual Float_t get_pz_orig() const {return 0;}
  /*!  Get the total momentum at the origin of this track */
  virtual Float_t get_ptot_orig() const {return 0;}
  /*!  Get the energy at the origin of this track */
  virtual Float_t get_energy_orig() const {return 0;}
  /*!  Get the rapidity at the origin of this track */
  virtual Float_t get_rapidity_orig() const {return 0;}
  /*!  Get the pt at the origin of this track */
  virtual Float_t get_pt_orig() const {return 0;}

  /*!  Set the x-position at the origin of this track */
  virtual void set_x_orig(Float_t x_orig) { }
  /*!  Set the y-position at the origin of this track */
  virtual void set_y_orig(Float_t y_orig) { }
  /*!  Set the z-position at the origin of this track */
  virtual void set_z_orig(Float_t z_orig) { }
  /*!  Set the x-momentum at the origin of this track */
  virtual void set_px_orig(Float_t px_orig) { }
  /*!  Set the y-momentum at the origin of this track */
  virtual void set_py_orig(Float_t py_orig) { }
  /*!  Set the z-momentum at the origin of this track */
  virtual void set_pz_orig(Float_t pz_orig) { }
  /*!  Set the energy at the origin of this track */
  virtual void set_energy_orig(Float_t energy_orig) { }
  //@}

  //@}
  //! @name Locators
  //@{    
  /*!  Get the track index for this track */
  virtual UShort_t get_index() const {return 0;}
  /*!  Set the track index for this track */
  virtual void set_index(UShort_t index) { }
  //@}
  
  //! @name Dumpers
  //@{    
  /*! Print data */
  virtual void print(std::ostream& os = std::cout) const {;}
  /*! Get version */
  virtual Int_t get_version() const {return 0;}
  //@}
  
  ClassDef(TMCPrimary,1)
};
#endif /* __TMCPRIMARY_H__*/












