////////////////////////////////////////////////////////////////////////
//		
//   Class that defines Level-2 PC1-PC3 track <-> EMC association object
//     for writing L2EMCAssociator primitives to one of the DSTs.
//		
//   Author: Cole
//   Date:   2-Feb-2003
//
//   $Log: L2PC1PC3TrackEMCAssocMicroV1.h,v $
//   Revision 1.7  2007/06/08 00:48:50  phnxlvl2
//   Changed all includes from quote form to bracket form to avoid include path problems when compiling
//
//   Revision 1.6  2005/12/08 14:31:21  pinkenbu
//   no std:: for assert - I think it's a stupid preproc macro, why do people use it anyway if they have no intentions whatsoever to compile the code with the flag which removes the assert check?
//
//   Revision 1.5  2005/12/07 16:55:34  dave
//   add needed cassert header
//
//   Revision 1.4  2003/10/10 01:25:53  dave
//   added std:: to compile under gcc 3.2
//
//   Revision 1.3  2003/03/24 01:08:53  frawley
//   Fixed segfault problem when writing dst file by changing _associations construction to use L2SinglePC1PC3TrackEMCAssocMicroV1 instead of L2PC1PC3TrackEMCAssocMicroV1
//
//   Revision 1.2  2003/03/04 13:50:28  cole
//   Fix class name in TClonesArray construction
//
//   Revision 1.1  2003/02/25 21:13:07  cole
//   Adding L2PC1PC3EMCAssoc classes for storing PC1PC3 track associations with EMCAl clusters
//   
//		
////////////////////////////////////////////////////////////////////////

#ifndef __L2PC1PC3TrackEMCAssocMicroV1_H__
#define __L2PC1PC3TrackEMCAssocMicroV1_H__

#include <phool.h>
#include <iostream>
#include <cassert>
#include <TClonesArray.h>
#include <L2PC1PC3TrackEMCAssocDST.h>
#include <L2SinglePC1PC3TrackEMCAssocMicroV1.h>

class L2PC1PC3TrackEMCAssocMicroV1 : public L2PC1PC3TrackEMCAssocDST 
{
 public:

  L2PC1PC3TrackEMCAssocMicroV1() : _numAssoc(0), 
    _associations("L2SinglePC1PC3TrackEMCAssocMicroV1", _initialSize)
  {}

  ~L2PC1PC3TrackEMCAssocMicroV1() {}

  void    identify(std::ostream& os = std::cout) const;
  void    Reset();
  int     isValid() const {return _numAssoc > 0;}
  
  void  storeLvl2Primitives(void* voidPrimitives_ptr);
  
  unsigned int get_numAssoc() const {return _numAssoc;}

  int get_trackIndex(unsigned int i) const { return getAssocPtr(i)->get_trackIndex(); }
  int get_clusterIndex(unsigned int i) const { return getAssocPtr(i)->get_clusterIndex();  }

  float get_energy(unsigned int i) const { return getAssocPtr(i)->get_energy(); }
  float get_deltarphi(unsigned int i) const { return getAssocPtr(i)->get_deltarphi();}
  float get_deltaz(unsigned int i) const { return getAssocPtr(i)->get_deltaz(); }
  
private:

  enum {_initialSize = 50};

  L2SinglePC1PC3TrackEMCAssocMicroV1* getAssocPtr(unsigned int i) const
  {
    L2SinglePC1PC3TrackEMCAssocMicroV1* ptr = 
      static_cast<L2SinglePC1PC3TrackEMCAssocMicroV1*>(_associations.UncheckedAt(i));
    assert(ptr != 0);
    return ptr;
  }

  unsigned int _numAssoc;
  TClonesArray _associations;
  
  ClassDef(L2PC1PC3TrackEMCAssocMicroV1, 1)
};
#endif	// __L2PC1PC3TrackEMCAssocMicroV1_H__
    




