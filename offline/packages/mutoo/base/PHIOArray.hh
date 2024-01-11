#ifndef __PHIOARRAY_H
#define __PHIOARRAY_H

#include "MUTOO.h"
#include "PHKey.hh"

#include "phool.h"
#include "PHObject.h"

#include <iostream>

class TClonesArray;

/*! \ingroup classes */
//! Bridge class for IOC/PHOOL persistence
/*! Bridge class for input/output of interface objects */
class PHIOArray : public PHObject
{
public:

  /*! PHOOL interface requirement */
  virtual int  isValid() const 
  {return 1;}  
  
  /*! PHOOL interface requirement */
  virtual void Reset() 
  {}
  
  /*! PHOOL interface requirement */
  virtual void identify(std::ostream& os = std::cout) const 
  { os << "PHIOArray"; } 
  
  /*! Get a read only pointer to TClonesArray of interface objects. */
  virtual TClonesArray* get_array() = 0;
  
  /*! Set the array pointer */
  virtual void set_array(TClonesArray* array_ptr) = 0;
  
  /*! Set the map key */
  virtual void set_map_key(ULong_t) = 0;
  
  /*! Set the map key */
  virtual ULong_t get_map_key() const = 0;
  
  /*! Dummy print */
  virtual void print(std::ostream& os = std::cout) const 
  { MUTOO::TRACE("PHIOArray::print"); }
  
  ClassDef(PHIOArray,1)    
};

#endif /* __PHIOARRAY_H */
