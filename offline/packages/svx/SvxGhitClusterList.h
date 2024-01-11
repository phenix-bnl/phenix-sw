// ==========================
// FILE: SvxGhitClusterList.h
// ==========================
#ifndef __SVXGHITCLUSTERLIST_HH_
#define __SVXGHITCLUSTERLIST_HH_

#include <phool.h>
#include <PHObject.h>

#include <iostream>

class SvxGhitCluster;

/**
 * @brief  The abstract class for a list (container) of Ghit-cluster relations.
 *
 * @date  Created  by V. L. Rykov on 15-Feb-2004
 * @date  Modified by V. L. Rykov on 11-May-2004:
 *        Sorting and index search methods added.
 */
class SvxGhitClusterList : public PHObject
{

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxGhitClusterList         () {
    //std::cout << "SvxGhitClusterList object created" << std::endl;
  }
  
  virtual ~SvxGhitClusterList() {
    //std::cout << "SvxGhitClusterList object destroyed" << std::endl;
  }

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset()         { PHOOL_VIRTUAL_WARN("void Reset()"       )           ;}
  virtual int  isValid() const { PHOOL_VIRTUAL_WARN("int isValid() const"); return 0 ;}
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxGhitClusterList object"
       << std::endl;
  }

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""

  /// Add a new entry to itself and return a pointer to it
  virtual SvxGhitCluster* addGhitCluster   (const          int ihit = -1) {
    PHOOL_VIRTUAL_WARN("SvxGhitCluster* addGhitCluster(const int=-1)" );
    return NULL;
  }
  /// See SvxGhitRawhitList::removeGhitRawhit()
  virtual void            removeGhitCluster(const unsigned int ihit     ) {
    PHOOL_VIRTUAL_WARN("void removeGhitCluster(const unsigned int)" );
  }
  /// Get the number of entries stored
  virtual int get_nGhitClusters () const {
    PHOOL_VIRTUAL_WARN("int get_nGhitClusters() const");
    return -9999;
  }
  /// Get the pointer of the "ihit"-th entry
  virtual SvxGhitCluster* get_GhitCluster (const unsigned int ihit) const {
    PHOOL_VIRTUAL_WARN("SvxGhitCluster* get_GhitCluster(const unsigned int) const");
    return NULL;
  }
  /// Return true if this list has been sorted by ghit ID.
  virtual bool           check_ghitSorted ()                        const {
    PHOOL_VIRTUAL_WARN("bool check_ghitSorted() const"   );
    return false;
  }
  /// Return true if this list has been sorted by cluster ID.
  virtual bool        check_clusterSorted ()                        const {
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
  virtual void sortGhits   () { PHOOL_VIRTUAL_WARN("void sortGhits()"   ) ;}
  virtual void sortClusters() { PHOOL_VIRTUAL_WARN("void sortClusters()") ;}
  virtual void unSort      () { PHOOL_VIRTUAL_WARN("void unSort()"      ) ;}

  /// See SvxGhitRawhitList::indexOfGhit()
  virtual bool indexOfGhit(const int id, int& idx_lb, int& idx_ub, 
                           int ifrom=0, int iupto=-1) const {
     PHOOL_VIRTUAL_WARN("int indexOfGhit() const");
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

  ClassDef(SvxGhitClusterList, 1);
};
#endif /* __SVXGHITCLUSTERLIST_HH_ */
