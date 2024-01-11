#ifndef __PHIOARRAY_V1_H
#define __PHIOARRAY_V1_H

#include "MUTOO.h"
#include "PHIOArray.hh"

#include "phool.h"
#include "PHObject.h"

#include "TClonesArray.h"

#include <iostream>

class PHIOArray_v1 : public PHIOArray
{
  
public:
  
  /*! Default constructor */
  PHIOArray_v1() : 
    _array_ptr(0),
    _map_key(0)    
  {}

  /*! Construct with a pointer to a TClonesArray.  The ownership of the
   TClonesArray is <b>not</b> transfered to this class */
  
  PHIOArray_v1(TClonesArray* array_ptr, PHKey::map_key_type map_key) : 
    _array_ptr(array_ptr),
    _map_key(map_key)
    {}
    
  /*! Destructor -- null op.  Internal array has external ownership */
  virtual ~PHIOArray_v1();
  
  /*! PHOOL interface requirement */
  virtual void identify(std::ostream& os = std::cout) const  
  { os << "PHIOArray_v1"; }
  
  /*! PHOOL interface requirement */
  virtual int  isValid() const 
  {return 1;}  
  
  /*! PHOOL interface requirement. */
  virtual void Reset();
  
  /*! This class is for IO only.  We provide read only
    handle to internal data */
  virtual TClonesArray* get_array() 
  { return _array_ptr; }

  /*! Set the array array pointer */
  virtual void set_array(TClonesArray* array_ptr) 
  { _array_ptr = array_ptr;}
  
  /*! Set the array array pointer */
  virtual void set_map_key(ULong_t map_key) 
  { _map_key = map_key;}
  
  /*! Set the array array pointer */
  virtual ULong_t get_map_key() const 
  { return _map_key;}
  
  virtual void print(std::ostream& os = std::cout) const;
  
  private:
  // *NOTE* || below instructs rootcint not split array  
  //! pointer to the underlying TClones array
  TClonesArray* _array_ptr ;  //||
    
  //! pointer to the parrent map key
  PHKey::map_key_type _map_key;
  
  ClassDef(PHIOArray_v1,1)    
};

#endif /* __PHIOArray_V1_H */

