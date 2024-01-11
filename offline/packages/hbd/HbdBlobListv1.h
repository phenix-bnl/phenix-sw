#ifndef __HBDBLOBLISTV1_H
#define __HBDBLOBLISTV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "HbdBlobList.h"
#include "HbdBlobv1.h"

class HbdBlobListv1 : public HbdBlobList
{

 public:

  HbdBlobListv1();
  HbdBlobListv1(const HbdBlobListv1&);
  HbdBlobListv1& operator=(const HbdBlobListv1&);
  virtual ~HbdBlobListv1();

  HbdBlobListv1* clone() const;

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
  HbdBlobv1* AddBlob (const unsigned int iblob, 
			    const HbdBlob& blob);
  HbdBlobv1* get_blob(const unsigned int iblob) const;

 protected:

  TClonesArray *GetBlob() const {return Blob;}
  unsigned int nBlobs;
  TClonesArray *Blob;

private:
  void copyto(HbdBlobListv1& dest) const;

  ClassDef(HbdBlobListv1,1)

};

#endif /* __HBDBLOBLISTV1_H */






