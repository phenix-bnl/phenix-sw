// Interface Object Class : TMuiPseudoBLT
// Author: chun zhang
// Data: 12/03/03
// Description: Class for muon identifier road

#ifndef _TMUIPseudoBLTO_H_
#define _TMUIPseudoBLTO_H_

// CINT compatible headers//
//
#include<TDataType.h>
#include<PHKey.hh>
#include<PHException.h>
#include<MUIOO.h>


/*! @ingroup interface */
//!  The MUID Pseudo-BLT Interface Object 

/*! 
  <b>The MUID pseudo-BLT Interface Object </b><br>

  An interface class for offline BLT emulator output.

  <p>
*/

class TMuiPseudoBLTO : public PHKey {

public:

  //! @name Constructors/Destructors
  //@{  
  
  /*! Default constructor */
  TMuiPseudoBLTO()
  {}  
  
  /*! Default destructor */  
  virtual ~TMuiPseudoBLTO()
  {}
  
  /*! Default constructor */  
  TMuiPseudoBLTO(const Key& key) :
    PHKey(key)
    {}
  
  /*! Construct with key and location */
  TMuiPseudoBLTO(const Key&,
     UShort_t arm,
     UShort_t index)
    {}
  
  //@}  
  
  //! @ Setters 
  //@{  
  
  /*! set deep-deep pseudo_BLT trigger  */  
  virtual void fire_2D() 
  {}

  /*! set deep-shallow pseudo_BLT trigger  */  
  virtual void fire_1D1S() 
  {}

  /*! set one deep pseudo_BLT trigger, 1D trigger is decided in a single quadrant.  */  
  virtual void fire_1D(const UShort_t iquad) 
  {}

  /*! set one shallow pseudo_BLT trigger,, 1S trigger is decided in a single quadrant.  */  
  virtual void fire_1S(const UShort_t iquad) 
  {}

  //@}
  //! @ Setters 
  //@{  
  
  /*! set deep-deep pseudo_BLT trigger  */  
  virtual void fire_reco_2D() 
  {}

  /*! set deep-shallow pseudo_BLT trigger  */  
  virtual void fire_reco_1D1S() 
  {}

  /*! set one deep pseudo_BLT trigger, 1D trigger is decided in a single quadrant.  */  
  virtual void fire_reco_1D(const UShort_t iquad) 
  {}

  /*! set one shallow pseudo_BLT trigger,, 1S trigger is decided in a single quadrant.  */  
  virtual void fire_reco_1S(const UShort_t iquad) 
  {}

  //@}

  //! @ Getters
  //@{

  /*! look up if 2D Pseudo-BLT fires */
  virtual bool is_2D_fired() const 
  { return false;}

  /*! look up if 1D1S Pseudo-BLT fires */
  virtual bool is_1D1S_fired() const 
  { return false;}

  /*! look up if 1D Pseudo-BLT fires, 1D trigger is decided in a single quadrant. */
  virtual bool is_1D_fired(const UShort_t& iquad) const 
  { return false;}

  /*! look up if 1S Pseudo-BLT fires, 1S trigger is also decided in a single quadrant */
  virtual bool is_1S_fired(const UShort_t& iquad) const 
  { return false;}

  //@}

  //! @ sanity check for 2D and 1D1S triggers based on the information we have from 1D and 1S
  //@{
  /*! check if 2D trigger output is consistent with 1D triggers */
  virtual bool check_2D_trigger() const 
  { return false;}

  /*! check if 1D1S trigger output is consistent with 1D triggers */
  virtual bool check_1D1S_trigger() const 
  { return false;}

  //@}

  /*! look up if 2D fires according to muid reco hits */
  virtual bool is_reco_2D_fired() const 
  { return false;}

  /*! look up if 1D1S fires according to muid reco hits */
  virtual bool is_reco_1D1S_fired() const 
  { return false;}

  /*! look up if 1D fires according to muid reco hits, 1D trigger is decided in a single quadrant. */
  virtual bool is_reco_1D_fired(const UShort_t iquad) const 
  { return false;}

  /*! look up if 1S fires according to muid reco hits, 1S trigger is also decided in a single quadrant */
  virtual bool is_reco_1S_fired(const UShort_t iquad) const 
  { return false;}

  //@}

  //! @ sanity check for 2D and 1D1S triggers based on reco hits inforamtion
  //@{
  /*! check if 2D trigger output is consistent with trigger decisions from reco hits*/
  virtual bool check_2D_with_reco() const 
  { return false;}

  /*! check if 1D1S trigger output is consistent with trigger decisions from reco hits*/
  virtual bool check_1D1S_with_reco() const 
  { return false;}

  //@}

  //! @name Locators
  //@{
  /*! Arm [0,1] */
  virtual void set_arm( UShort_t arm) 
  {}

  /*! Index */
  virtual void set_index( UShort_t index) 
  {}

  /*! Arm [0,1] */
  virtual UShort_t get_arm() const 
  {return 0;}

  /*! Index */
  virtual UShort_t get_index() const 
  {return 0;}

  //@}

  //! @name Dumpers
  //@{  

  virtual void print(std::ostream& os = std::cout) const {}

  //@}

  ClassDef(TMuiPseudoBLTO,1)

};


class TMuiPseudoBLTO_v1 : public TMuiPseudoBLTO {

public:

  TMuiPseudoBLTO_v1();  

  virtual ~TMuiPseudoBLTO_v1(){}

  TMuiPseudoBLTO_v1(const Key&,
        UShort_t arm,
        UShort_t index);  


  TMuiPseudoBLTO_v1(const TMuiPseudoBLTO*);  

  TMuiPseudoBLTO_v1(const TMuiPseudoBLTO&);  

  void fire_2D() 
  { _is_2D = true;}

  void fire_1D1S() 
  { _is_1D1S = true;}

  void fire_1D( const UShort_t iquad) 
  {
    switch(iquad) 
    {
      case 0:
        _is_1D_quad0 = true;
      break;

      case 1:
        _is_1D_quad1 = true;
      break;
      
      case 2:
        _is_1D_quad2 = true;
      break;
      
      case 3:
        _is_1D_quad3 = true;
      break;
      
      default: break;
    }
  }

  void fire_1S( const UShort_t iquad) 
  {
    switch(iquad) 
    {
      case 0:
        _is_1S_quad0 = true;
      break;
      
      case 1:
        _is_1S_quad1 = true;
      break;
      
      case 2:
        _is_1S_quad2 = true;
      break;
      
      case 3:
        _is_1S_quad3 = true;
      break;
      
      default: break;
      }
  }
  
  bool check_2D_trigger() const;
  bool check_1D1S_trigger() const;

  bool is_2D_fired() const 
  { return _is_2D;}
  
  bool is_1D1S_fired() const 
  { return _is_1D1S;}

  bool is_1D_fired(const UShort_t& iquad) const 
  {
    switch(iquad)
    {
      case 0: return _is_1D_quad0;
      case 1: return _is_1D_quad1;
      case 2: return _is_1D_quad2;
      case 3: return _is_1D_quad3;
      default: return false;
    }
  }

  bool is_1S_fired(const UShort_t& iquad) const 
  {
    switch(iquad)
    {
      case 0: return _is_1S_quad0;
      case 1: return _is_1S_quad1;
      case 2: return _is_1S_quad2;
      case 3: return _is_1S_quad3;
      default: return false;
    }
  }

  void fire_reco_2D() 
  { _is_reco_2D = true;}

  void fire_reco_1D1S() 
  { _is_reco_1D1S = true;}

  void fire_reco_1D( const UShort_t iquad) 
  {
    switch(iquad) 
    {
      case 0:
        _is_reco_1D_quad0 = true;
      break;
      
      case 1:
        _is_reco_1D_quad1 = true;
      break;
      
      case 2:
        _is_reco_1D_quad2 = true;
      break;
      
      case 3:
        _is_reco_1D_quad3 = true;
      break;
      
      default: break;
    }
  }

  void fire_reco_1S( const UShort_t iquad) 
  {
    switch(iquad) 
    {
      case 0:
        _is_reco_1S_quad0 = true;
      break;
      
      case 1:
        _is_reco_1S_quad1 = true;
      break;
      
      case 2:
        _is_reco_1S_quad2 = true;
      break;
      
      case 3:
        _is_reco_1S_quad3 = true;
      break;
      
      default: break;
    }
  }

  bool is_reco_2D_fired() const 
  { return _is_reco_2D;}

  bool is_reco_1D1S_fired() const 
  { return _is_reco_1D1S;}

  bool is_reco_1D_fired(const UShort_t iquad) const 
  {
    switch(iquad)
    {
      case 0: return _is_reco_1D_quad0;
      case 1: return _is_reco_1D_quad1;
      case 2: return _is_reco_1D_quad2;
      case 3: return _is_reco_1D_quad3;
      default: return false;
    }
  }

  bool is_reco_1S_fired(const UShort_t iquad) const 
  {
    switch(iquad)
    {
      case 0: return _is_reco_1S_quad0;
      case 1: return _is_reco_1S_quad1;
      case 2: return _is_reco_1S_quad2;
      case 3: return _is_reco_1S_quad3;
      default: return false;
    }
  }
  
  bool check_2D_with_reco() const 
  { return (_is_reco_2D == _is_2D); }
  
  bool check_1D1S_with_reco() const 
  { return (_is_reco_1D1S == _is_1D1S); }

  void set_arm( UShort_t arm) 
  { _arm=arm; }

  void set_index( UShort_t index) 
  { _index=index; }

  UShort_t get_arm() const 
  {return _arm;}

  UShort_t get_index() const 
  {return _index;}

  void print(std::ostream& os = std::cout) const 
  {
    MUIOO::PRINT(os,GetName());
    os << " arm:         " << _arm << std::endl;
    os << " index:       " << _index << std::endl;
    os << " is_2D:       " << _is_2D << std::endl;
    os << " is_1D1S:     " << _is_1D1S << std::endl;
    os << " is_1D_quad0: " << _is_1D_quad0 << std::endl;
    os << " is_1D_quad1: " << _is_1D_quad1 << std::endl;
    os << " is_1D_quad2: " << _is_1D_quad2 << std::endl;
    os << " is_1D_quad3: " << _is_1D_quad3 << std::endl;
    os << " is_1S_quad0: " << _is_1S_quad0 << std::endl;
    os << " is_1S_quad1: " << _is_1S_quad1 << std::endl;
    os << " is_1S_quad2: " << _is_1S_quad2 << std::endl;
    os << " is_1S_quad3: " << _is_1S_quad3 << std::endl;
    os << " trigger decisions from reconstructed hits. " << std::endl;
    os << " is_reco_2D:       " << _is_reco_2D << std::endl;
    os << " is_reco_1D1S:     " << _is_reco_1D1S << std::endl;
    os << " is_reco_1D_quad0: " << _is_reco_1D_quad0 << std::endl;
    os << " is_reco_1D_quad1: " << _is_reco_1D_quad1 << std::endl;
    os << " is_reco_1D_quad2: " << _is_reco_1D_quad2 << std::endl;
    os << " is_reco_1D_quad3: " << _is_reco_1D_quad3 << std::endl;
    os << " is_reco_1S_quad0: " << _is_reco_1S_quad0 << std::endl;
    os << " is_reco_1S_quad1: " << _is_reco_1S_quad1 << std::endl;
    os << " is_reco_1S_quad2: " << _is_reco_1S_quad2 << std::endl;
    os << " is_reco_1S_quad3: " << _is_reco_1S_quad3 << std::endl;
    MUIOO::PRINT(os,"**");
  }
  
private:
  
  UShort_t _arm;
  UShort_t _index;
  
  bool _is_2D;
  bool _is_1D1S;
  bool _is_1D_quad0;
  bool _is_1D_quad1;
  bool _is_1D_quad2;
  bool _is_1D_quad3;
  bool _is_1S_quad0;
  bool _is_1S_quad1;
  bool _is_1S_quad2;
  bool _is_1S_quad3;

  bool _is_reco_2D;
  bool _is_reco_1D1S;
  bool _is_reco_1D_quad0;
  bool _is_reco_1D_quad1;
  bool _is_reco_1D_quad2;
  bool _is_reco_1D_quad3;
  bool _is_reco_1S_quad0;
  bool _is_reco_1S_quad1;
  bool _is_reco_1S_quad2;
  bool _is_reco_1S_quad3;
 
  ClassDef(TMuiPseudoBLTO_v1,1)
};

#endif /* _TMUIPSEUDOBLTO_H_ */
        








