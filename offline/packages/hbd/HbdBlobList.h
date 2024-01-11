#ifndef __HBDBLOBLIST_HH_
#define __HBDBLOBLIST_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"
#include "HbdBlob.h"

// Container for the HbdBlobs.
// This contains a list of all HbdBlobs in an event.
// 
// Created by Jeffery Mitchell on 11/13/03.
//



class HbdBlobList : public PHObject
{

 public:
  virtual ~HbdBlobList() {}

  virtual void set_nBlobs (const unsigned int NTRACK) 
    {
      std::cout << "HbdBlobList::Error get_nBlobs not overridden" << std::endl;
      return;
    }
  virtual unsigned int  get_nBlobs () const 
    {
      std::cout << "HbdBlobList::Error get_nBlobs not overridden" << std::endl;
      return 0;
    }

  // Set data members
  virtual int  set_TClonesArraySize(const unsigned int nclus) {return 0;}
  virtual void AddBlob       (const unsigned int iblob) {return;}
  virtual void RemoveBlob    (const unsigned int iblob) {return;}
  virtual HbdBlob* AddBlob(const unsigned int iblob, 
			   const HbdBlob &blob) {return NULL;}

  // Get data members
  virtual HbdBlob* get_blob(const unsigned int iblob) const 
    {
      std::cout << "Single blob return not implemented for your version of blob list" << std::endl;
      return 0;
    }

  virtual HbdBlobList* clone() const
    {
      std::cout << "Clone method not implemented for your version of HbdBlobList" << std::endl;
      return 0;
    }


  // Standard functions of all inheritors of PHObject classes...
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual HbdBlobList object" << std::endl;
    return;
  }

  ClassDef(HbdBlobList,1)

};
#endif /* __HBDBLOBLIST_HH_ */
