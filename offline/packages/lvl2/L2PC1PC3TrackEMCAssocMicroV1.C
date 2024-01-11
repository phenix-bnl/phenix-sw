////////////////////////////////////////////////////////////////////////
//		
//   Implementation of the L2PC1PC3EMCAssocMicroV1 class
//		
//   Author: Cole
//   Date:   2-Feb-2003
//
//   $Log: L2PC1PC3TrackEMCAssocMicroV1.C,v $
//   Revision 1.7  2007/06/08 00:48:49  phnxlvl2
//   Changed all includes from quote form to bracket form to avoid include path problems when compiling
//
//   Revision 1.6  2006/02/17 19:47:56  dave
//   removed #includes that the ignominy includechecker identified as unneeded
//
//   Revision 1.5  2004/12/07 14:35:34  pinkenbu
//   fix ripples from removing using namespace from lvl2
//
//   Revision 1.4  2003/03/22 23:14:26  frawley
//   Fixed a problem with _numAssoc book keeping
//
//   Revision 1.3  2003/02/26 15:27:24  cole
//   Add classImpl()
//
//   Revision 1.2  2003/02/25 22:20:46  cole
//   Fix void* -> const conversion problem
//
//   Revision 1.1  2003/02/25 21:13:07  cole
//   Adding L2PC1PC3EMCAssoc classes for storing PC1PC3 track associations with EMCAl clusters
//   
//		
////////////////////////////////////////////////////////////////////////

#include <L2PC1PC3TrackEMCAssocMicroV1.h>
#include <L2EMCAssociator.h>

//INCLUDECHECKER: Removed this line: #include <iostream>

ClassImp(L2PC1PC3TrackEMCAssocMicroV1)

using namespace std;

L2SinglePC1PC3TrackEMCAssocMicroV1 constructAssoc(const L2EMCAssociatorV1::AssociatedClusterPara& in)
{
  return L2SinglePC1PC3TrackEMCAssocMicroV1(in.track_index, in.cluster_index, in.energy, in.deltarphi, in.deltaz);

}

void L2PC1PC3TrackEMCAssocMicroV1::identify(ostream& os) const
{
  os << "identify yourself: L2PC1PC3TrackEMCAssocMicroV1 Object" << endl;
  os << "Number of track-tile associations : " << _numAssoc << endl;
}

void L2PC1PC3TrackEMCAssocMicroV1::Reset()
{
  _numAssoc=0;
  _associations.Clear();
}

void L2PC1PC3TrackEMCAssocMicroV1::storeLvl2Primitives(void* voidPrimitives_ptr)
{
  // Clear the existing associations table
  Reset();

  L2EMCAssociatorV1* primitives_ptr = static_cast<L2EMCAssociatorV1*>(voidPrimitives_ptr);

  unsigned int numPrimAssoc = primitives_ptr->EMCClusterTrack.size();
  if (numPrimAssoc  > _initialSize)
  {
    _associations.Expand(numPrimAssoc);
  }
  
  //  We use the AssociatedClusterPara structure defined in the primitives
  //
  for (unsigned int iassoc = 0; iassoc < numPrimAssoc; iassoc++)
  {
    //  Create the new entry using the new placement sytax and the copy constructor defined
    //    by AssociatedClusterPara
    //
    new (_associations[iassoc]) L2SinglePC1PC3TrackEMCAssocMicroV1(constructAssoc(primitives_ptr->EMCClusterTrack[iassoc]));
    
    _numAssoc++;
    
  }
}



