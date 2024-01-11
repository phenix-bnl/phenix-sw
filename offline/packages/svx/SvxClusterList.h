// ======================
// FILE: SvxClusterList.h
// ======================
#ifndef __SVXCLUSTERLIST_HH_
#define __SVXCLUSTERLIST_HH_

#include <iostream>
#include <phool.h>
#include <PHObject.h>
#include <SvxSensor.h>

class SvxCluster;

/**
 * @brief  The abstract class for a list (container) of clusters.
 *
 * @date  Template used: SvxGhitList.h by Jeffery Mitchell as of 11/20/2003
 * @date  Created  by V. L. Rykov on 09-Mar-2004
 * @date  Modified by V. L. Rykov on 11-May-2004:
 *        Sorting and index search methods added.
 */
class SvxClusterList : public PHObject
{

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxClusterList         () {
    //std::cout << "SvxClusterList object created" << std::endl;
  }
  
  virtual ~SvxClusterList() {
    //std::cout << "SvxClusterList object destroyed" << std::endl;
  }

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset()         { PHOOL_VIRTUAL_WARN("void Reset()"       )           ;}
  virtual int  isValid() const { PHOOL_VIRTUAL_WARN("int isValid() const"); return 0 ;}
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxClusterList object" << std::endl;
  }

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""

  /// Add a new hit to itself and return a pointer to the hit
  virtual SvxCluster* addCluster    (const          int ihit = -1) {
    PHOOL_VIRTUAL_WARN("SvxCluster* addCluster(const int=-1)" );
    return NULL;
  }
  /// See SvxGhitList::removeGhit()
  virtual void        removeCluster (const unsigned int ihit     ) {
    PHOOL_VIRTUAL_WARN("void removeCluster(const unsigned int)" );
  }
  /// Get the number of hits stored
  virtual int  get_nClusters      ()                        const  {
    PHOOL_VIRTUAL_WARN("int get_nClusters() const");
    return -9999;
  }
  /// Get the pointer of the "ihit"-th hit
  virtual SvxCluster* get_Cluster (const unsigned int ihit) const  {
    PHOOL_VIRTUAL_WARN("SvxCluster* get_Cluster(const unsigned int) const");
    return NULL;
  }
  /// Return true if this list has been sorted by hit ID
  virtual bool check_hitIDsorted  ()                    const {
    PHOOL_VIRTUAL_WARN("bool check_hitIDsorted() const"             );
    return false;
  }
  /// Return true if this list has been sorted by sensor ID
  virtual bool check_sensorIDsorted ()                  const {
    PHOOL_VIRTUAL_WARN("bool check_sensorIDsorted() const"          );
    return false;
  }

  // Routines to manipulate the cluster array...
  // """""""""""""""""""""""""""""""""""""""""""

  /// Remove all hit-list entries which are "0"
  virtual int Compress()
    { PHOOL_VIRTUAL_WARN("int Compress()"                              ); return -9999 ;}
  virtual int set_TClonesArraySize(const unsigned int nhit)
    { PHOOL_VIRTUAL_WARN("int set_TClonesArraySize(const unsigned int)"); return -9999 ;}

  // Sorting and index search
  // """"""""""""""""""""""""
  virtual void sort_hitID    () { PHOOL_VIRTUAL_WARN("void sort_hitID()"   ) ;}
  virtual void sort_sensorID () { PHOOL_VIRTUAL_WARN("void sort_sensorID()") ;}
  virtual void unSort        () { PHOOL_VIRTUAL_WARN("void unSort()"       ) ;}

  /// See SvxGhitList::indexOfGhit()
  virtual int  indexOfCluster (const int        hitid    ,
			       const int        ifrom = 0,
			       const int        iupto =-1) const {
    PHOOL_VIRTUAL_WARN("int indexOfCluster(const int,"
	    "const unsigned int=0,const unsigned int=-1) const");
    return -1;
  }  
  /// See SvxGhitList::indexOfGhit()
  virtual int  indexOfCluster (const SvxCluster*   hit      ,
			       const int        ifrom = 0,
			       const int        iupto =-1) const {
    PHOOL_VIRTUAL_WARN("int indexOfCluster(const SvxCluster*,"
	    "const unsigned int=0,const unsigned int=-1) const");
    return -1;
  }
  /// See SvxGhitList::indexOfGhit()
  virtual bool indexOfCluster(const SvxSensor* sensor, 
                             int& idx_lb, int& idx_ub, 
                             int ifrom = 0, int iupto =-1) const {
    PHOOL_VIRTUAL_WARN("bool indexOfCluster(...) const");
    return false;
  }

  // Methods
  // """""""
  virtual void print () const { PHOOL_VIRTUAL_WARN("void print()" ) ;}

  ClassDef(SvxClusterList, 1);
};
#endif /* __SVXCLUSTERLIST_HH_ */
