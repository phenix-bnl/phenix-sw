// Class : TMuiMCTwoPack
// Author: J.Nagle
// Date: 06/24/2003
// Description: Hit TwoPacks fired by TMuiMCHitO

#ifndef __TMUIMCTWOPACK_H__
#define __TMUIMCTWOPACK_H__

#include<TDataType.h>
#include<TObject.h>
#include<iostream>
#include<string>

/*! @ingroup classes */
//!  The Muon tracker Monte Carlo strip object 
/*!
Each TMuiMCHitO object can strike multiple twopacks in and between
different orientations for a given panel.
*/

class TMuiMCTwoPack : public TObject
{
public:
  /*! Status enumeration */
  enum Status {NOMINAL, MASKED};
  
  //! @name Constructors/Destructors
  //@{    
  /*! Default constructor */
  TMuiMCTwoPack() {;}

  /*! Construct with orientation, panel, and twopack_index */
  TMuiMCTwoPack(UShort_t orient,
		UShort_t panel,
		UShort_t twopack_index) :
    _orient(orient), 
    _panel(panel), 
    _twopack_index(twopack_index),
    _status(NOMINAL){;}

  /*! Destructor */
  virtual ~TMuiMCTwoPack(){;}
  //@}


  //! @name Functional Interface
  //@{    
  //!@}

  //! @name Locators
  //@{    
  /*! Get Orientation */
  // virtual UShort_t get_orient() const {return _orient;}
  UShort_t get_orient() const {return _orient;}
  /*! Get Panel */
  //virtual UShort_t get_panel() const {return _panel;}
  UShort_t get_panel() const {return _panel;}
  /*! Get TwoPack Index */
  UShort_t get_twopack_index() const {return _twopack_index;}
  /*! Get Status */
  UShort_t get_status() const {return _status;}
  /*! Set Orientation */
  void set_orient(UShort_t orient) { _orient = orient;}
  /*! Set Panel */
  void set_panel(UShort_t panel) { _panel = panel;}
  /*! Set TwoPack Index */
  void set_twopack_index(UShort_t twopack_index) { _twopack_index = twopack_index;}
  /*! Set Status */
  void set_status(UShort_t status) { _status = status;}
  //!@}

  //! @name Dumpers
  //@{    
  /*! Print data*/
  virtual void print(std::ostream& os = std::cout) const 
  {
    std::string status;
    if(_status == NOMINAL) status += "NOMINAL";
    if(_status == MASKED) status += "MASKED";
    os << " orientation: " << _orient 
       << " panel: " << _panel 
       << " twopack_index: " << _twopack_index
       << " status: " << status << std::endl;
  }
  //@}
  
private:
  
  UShort_t _orient;
  UShort_t _panel;
  UShort_t _twopack_index;
  UShort_t _status;
  ClassDef(TMuiMCTwoPack,1)

};


#endif /* __TMuiMCTwoPack_H__*/

