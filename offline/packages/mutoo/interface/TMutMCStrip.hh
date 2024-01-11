// Class : TMutMCStrip
// Author: S.Kelly/C.Zhang
// Date: 02/11/18
// Description: Hit Strips fired by TMutMCHit.

#ifndef __TMUTMCSTRIP_H__
#define __TMUTMCSTRIP_H__

#include<TDataType.h>
#include<TObject.h>
#include<iostream>

/*! @ingroup classes */
//!  The Muon tracker Monte Carlo strip object 
/*!
Each TMutMCHit object can deposit
charge on a number of strips in the surrounding cathodes.  The
TMutMCStrip object represent such a charge deposit.  TMutMCHit
internally maintains a list of TMutMCStrips.
*/

class TMutMCStrip : public TObject
{
  public:

  //! @name Constructors/Destructors
  //@{    

  /*! Construct with cathode (0,1), strip (stripnum), q, w_true and w_digit */
  TMutMCStrip(
    UShort_t cathode = 0,
    UShort_t strip = 0,
    Float_t q = 0):
    _cathode(cathode), 
    _strip(strip), 
    _q(q)
  {}

  /*! Destructor */
  virtual ~TMutMCStrip()
  {}
  //@}


  //! @name Functional Interface
  //@{    
  /*! Set Charge associated with this strip */
  virtual void set_q(Float_t q) 
  { _q = q;}
  
  /*! Get Charge associated with this centroid fit */
  virtual Float_t get_q() const 
  {return _q;}
  
  //!@}

  //! @name Locators
  //@{    
  /*! Get Cathode */
  virtual UShort_t get_cathode() const 
  {return _cathode;}
  
  /*! Get Strip */
  virtual UShort_t get_strip() const 
  {return _strip;}
  
  /*! Set Cathode */
  virtual void set_cathode(UShort_t cathode) 
  { _cathode = cathode;}
  
  /*! Set Strip */
  virtual void set_strip(UShort_t strip) 
  { _strip = strip;}
  
  //!@}

  //! @name Dumpers
  //@{    
  /*! Print data*/
  virtual void print(std::ostream& os = std::cout) const 
  {
    os << " cathode: " << _cathode 
       << " strip: " << _strip 
       << " q: " << _q << std::endl;
  }
  //@}
  
private:
  
  UShort_t _cathode;
  UShort_t _strip;
  Float_t _q;
  ClassDef(TMutMCStrip,1)

};


#endif /* __TMutMCStrip_H__*/








