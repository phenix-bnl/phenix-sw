// ============================
// FILE: SvxRawhitClusterList.h
// ============================
#ifndef __SVXRAWHITCLUSTERLIST_HH_
#define __SVXRAWHITCLUSTERLIST_HH_

#include <phool.h>
#include <PHObject.h>

#include <iostream>

class SvxRawhitCluster;

/**
 * @brief  The abstract class for a list (container) of rawhit-cluster relations.
 *
 * @date  Created  by V. L. Rykov on 15-Feb-2004
 * @date  Modified by V. L. Rykov on 11-May-2004:
 *        Sorting and index search methods added.
 */
class SvxRawhitClusterList : public PHObject
{

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxRawhitClusterList         () {
    //std::cout << "SvxRawhitClusterList object created" << std::endl;
  }
  
  virtual ~SvxRawhitClusterList() {
    //std::cout << "SvxRawhitClusterList object destroyed" << std::endl;
  }

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset()         { PHOOL_VIRTUAL_WARN("void Reset()"       )           ;}
  virtual int  isValid() const { PHOOL_VIRTUAL_WARN("int isValid() const"); return 0 ;}
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxRawhitClusterList object"
       << std::endl;
  }

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""

  /// Add a new entry to itself and return a pointer to it
  virtual SvxRawhitCluster* addRawhitCluster (const          int ihit = -1) {
    PHOOL_VIRTUAL_WARN("SvxRawhitCluster* addRawhitCluster(const int=-1)" );
    return NULL;
  }
  /// See SvxGhitRawhitList::removeGhitRawhit()
  virtual void            removeRawhitCluster(const unsigned int ihit     ) {
    PHOOL_VIRTUAL_WARN("void removeRawhitCluster(const unsigned int)"     );
  }
  /// Get the number of entries stored
  virtual int get_nRawhitClusters () const {
    PHOOL_VIRTUAL_WARN("int get_nRawhitClusters() const");
    return -9999;
  }
  /// Get the pointer of the "ihit"-th entry
  virtual SvxRawhitCluster* get_RawhitCluster (const unsigned int ihit) const {
    PHOOL_VIRTUAL_WARN("SvxRawhitCluster* get_RawhitCluster(const unsigned int) const");
    return NULL;
  }
  /// Return true if this list has been sorted by rawhit ID.
  virtual bool             check_rawhitSorted ()                        const {
    PHOOL_VIRTUAL_WARN("bool check_rawhitSorted() const" );
    return false;
  }
  /// Return true if this list has been sorted by cluster ID.
  virtual bool            check_clusterSorted ()                        const {
    PHOOL_VIRTUAL_WARN("bool check_clusterSorted() const");
    return false;
  }

  // Routines to manipulate the cluster array...
  // """""""""""""""""""""""""""""""""""""""""""

  /// Remove all list entries which are "0"
  virtual int Compress()
    { PHOOL_VIRTUAL_WARN("int Compress()"                              ); return -9999 ;}
  virtual int set_TClonesArraySize(const unsigned int nhit)
    { PHOOL_VIRTUAL_WARN("int set_TClonesArraySize(const unsigned int)"); return -9999 ;}

  // Sorting and index search
  // """"""""""""""""""""""""
  virtual void sortRawhits () { PHOOL_VIRTUAL_WARN("void sortRawhits()" ) ;}
  virtual void sortClusters() { PHOOL_VIRTUAL_WARN("void sortClusters()") ;}
  virtual void unSort      () { PHOOL_VIRTUAL_WARN("void unSort()"      ) ;}

  /// See SvxGhitRawhitList::indexOfGhit()
  virtual bool indexOfRawhit(const int id, int& idx_lb, int& idx_ub, 
                             int ifrom=0, int iupto=-1) const {
     PHOOL_VIRTUAL_WARN("int indexOfRawhit() const");
    return false;
  }
  /// See SvxGhitRawhitList::indexOfGhit()
  virtual bool indexOfCluster(const int id, int& idx_lb, int& idx_ub, 
                             int ifrom=0, int iupto=-1) const {
     PHOOL_VIRTUAL_WARN("int indexOfCluster() const");
    return false;
  }  
  
  // Methods
  // """""""
  virtual void print () const { PHOOL_VIRTUAL_WARN("void print()" ) ;}

  ClassDef(SvxRawhitClusterList, 1);
};
#endif /* __SVXRAWHITCLUSTERLIST_HH_ */
