#ifndef __HBDBLOBLISTV2_H
#define __HBDBLOBLISTV2_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "HbdBlobList.h"
#include "HbdBlobv2.h"

class HbdBlobListv2 : public HbdBlobList
{

 public:

  HbdBlobListv2();
  HbdBlobListv2(const HbdBlobListv2&);
  HbdBlobListv2& operator=(const HbdBlobListv2&);
  virtual ~HbdBlobListv2();

  HbdBlobListv2* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Implementations of the set/get methods...
  void set_nBlobs (const unsigned int nblob) 
    {nBlobs = nblob; return;}
  unsigned int  get_nBlobs () const {return nBlobs;}

  // Routines to manipulate the blob array...
  int set_TClonesArraySize(const unsigned int nblob);
  void AddBlob          (const unsigned int iblob);
  void RemoveBlob       (const unsigned int iblob);
  HbdBlobv2* AddBlob (const unsigned int iblob, 
			    const HbdBlob& blob);
  HbdBlobv2* get_blob(const unsigned int iblob) const;

 protected:

  TClonesArray *GetBlob() const {return Blob;}
  unsigned int nBlobs;
  TClonesArray *Blob;

private:
  void copyto(HbdBlobListv2& dest) const;

  ClassDef(HbdBlobListv2,1)

};

#endif /* __HBDBLOBLISTV2_H */






