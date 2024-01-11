////////////////////////////////////////////////////////////////////////
//		
//   Class that defines Level-2 PC1-PC3 track <-> EMC association object
//     for writing L2EMCAssociator primitives to one of the DSTs.
//		
//   Author: Cole
//   Date:   2-Feb-2003
//
//   $Log: L2PC1PC3TrackEMCAssocDST.h,v $
//   Revision 1.3  2007/06/08 00:48:49  phnxlvl2
//   Changed all includes from quote form to bracket form to avoid include path problems when compiling
//
//   Revision 1.2  2003/10/10 01:25:53  dave
//   added std:: to compile under gcc 3.2
//
//   Revision 1.1  2003/02/25 21:13:06  cole
//   Adding L2PC1PC3EMCAssoc classes for storing PC1PC3 track associations with EMCAl clusters
//   
//		
////////////////////////////////////////////////////////////////////////

#ifndef __L2PC1PC3TrackEMCAssocDST_H__
#define __L2PC1PC3TrackEMCAssocDST_H__

#include <iostream>
#include <PHObject.h>

class L2PC1PC3TrackEMCAssocDST : public PHObject 
{
 public:
  
  L2PC1PC3TrackEMCAssocDST(){}
  virtual ~L2PC1PC3TrackEMCAssocDST() {}
  virtual void    identify(std::ostream& os = std::cout) const = 0;
  virtual void    Reset() = 0;
  virtual int     isValid() const = 0;
  
  virtual void storeLvl2Primitives(void* voidPrimitives_ptr) = 0;
  virtual unsigned int  get_numAssoc() const = 0;

  virtual int   get_trackIndex(unsigned int i) const = 0;
  virtual int   get_clusterIndex(unsigned int i) const = 0;
  virtual float  get_energy(unsigned int i) const = 0;
  virtual float  get_deltarphi(unsigned int i) const = 0;
  virtual float  get_deltaz(unsigned int i) const = 0;
  
  ClassDef(L2PC1PC3TrackEMCAssocDST, 1)
};
#endif	// __L2PC1PC3TrackEMCAssocDST_H__
    
