#ifndef __MPCGEACLUSTERCONTAINERV1_H__
#define __MPCGEACLUSTERCONTAINERV1_H__

#include <mpcGeaClusterContainer.h>
#include <mpcGeaClusterContentV1.h>
 
#include <phool.h>

#include <TClonesArray.h>

#include <cassert>
#include <iosfwd>

/** (ABC) Container of MPC - geant cluster association.    

@ingroup interface

 */

//class TClonesArray;

class mpcGeaClusterContainerV1 : public mpcGeaClusterContainer
{
public:

  mpcGeaClusterContainerV1();

  virtual ~mpcGeaClusterContainerV1();

/*
  mpcGeaClusterContainer& operator=(const mpcGeaClusterContainer &rhs);
  mpcGeaClusterContainer& operator+(const mpcGeaClusterContainer &rhs);
  mpcGeaClusterContainer& operator+=(const mpcGeaClusterContainer &rhs);
*/

  // defined in base class
  //int findPrimary(const int feech);

  mpcGeaClusterContent* addCluster(const mpcGeaClusterContent &c);

  mpcGeaClusterContent* getCluster(unsigned int iclus) const
  {
    assert( iclus <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpcGeaClusterContentV1*>(GetArray()->At(iclus));
  }

  // the actual size of the object.
  unsigned int size(void) const { return GetArray()->GetLast()+1; }

  int isValid() const
  {
    // if size>0, return 1, otherwise 0
    return((size()>0) ? 1 : 0);
  }

  void Reset();

  void identify(std::ostream& os=std::cout) const;

  void print(std::ostream& os=std::cout) const;

protected:
  UShort_t n_mpcgeaclusters;

  TClonesArray *mpcgclus;

  TClonesArray *GetArray() const {return mpcgclus;}

  mpcGeaClusterContainer *get(const unsigned int iclus) const {
    assert ( iclus <= (unsigned int)GetArray()->GetLast() );
    return static_cast<mpcGeaClusterContainerV1*>(GetArray()->At(iclus));
  }

  /** This one is protected on purpose. 
      See e.g. Scott Meyers' More Effective C++ Item 33.*/
  //mpcGeaClusterContainer& operator=(const mpcGeaClusterContainer&);

private:

  ClassDef(mpcGeaClusterContainerV1,1) // Array of mpcGeaClusterContainerV1 (ABC)
    
};

#endif	// __MPCGEACLUSTERCONTAINERV1_H__

