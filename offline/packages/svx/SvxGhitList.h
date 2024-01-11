// ===================
// FILE: SvxGhitList.h
// ===================
#ifndef __SVXGHITLIST_HH_
#define __SVXGHITLIST_HH_

#include <iostream>
#include <phool.h>
#include <PHObject.h>
#include <SvxSensor.h>

class SvxGhit;

/**
 * @brief  The abstract class for a list (container) of GEANT hits.
 *
 * @date  Template used: SvxGhitList.h by Jeffery Mitchell as of 11/20/2003
 * @date  Created  by V. L. Rykov on 09-Mar-2004
 * @date  Modified by V. L. Rykov on 10-May-2004:
 *        Sorting and index search methods added.
 */
class SvxGhitList : public PHObject
{

 public:
  // Constructor(s) & destructor
  // """"""""""""""""""""""""""
  SvxGhitList         () {
    //std::cout << "SvxGhitList object created" << std::endl;
  }
  
  virtual ~SvxGhitList() {
    //std::cout << "SvxGhitList object destroyed" << std::endl;
  }

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset()         { PHOOL_VIRTUAL_WARN("void Reset()"       )           ;}
  virtual int  isValid() const { PHOOL_VIRTUAL_WARN("int isValid() const"); return 0 ;}
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxGhitList object" << std::endl;
  }

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""

  /// Add a new hit to itself and return a pointer to the hit
  virtual SvxGhit* addGhit    (const          int ihit = -1)  {
    PHOOL_VIRTUAL_WARN("SvxGhit* addGhit(const int=-1)" );
    return NULL;
  }
  /**
   * @brief  Remove the "ihit"-th hit.
   *   This only sets the pointer of the removed hit to "0", and thus
   *   one must call Compress() to shrink the array size
   *  (see ROOT TClonesArray::RemoveAt())
   */
  virtual void     removeGhit (const unsigned int ihit     )  {
    PHOOL_VIRTUAL_WARN("void removeGhit(const unsigned int)" );
  }
  /// Get the number of hits stored
  virtual int get_nGhits      ()                        const {
    PHOOL_VIRTUAL_WARN("int get_nGhits() const");
    return -9999;
  }
  /// Get the pointer of the "ihit"-th hit
  virtual SvxGhit* get_Ghit (const unsigned int ihit)   const {
    PHOOL_VIRTUAL_WARN("SvxGhit* get_Ghit(const unsigned int) const");
    return NULL;
  }
  /// Return true if this list has been sorted by hit ID.
  virtual bool check_hitIDsorted  ()                    const {
    PHOOL_VIRTUAL_WARN("bool check_hitIDsorted() const"             );
    return false;
  }
  /// Return true if this list has been sorted by sensor ID.
  virtual bool check_sensorIDsorted ()                  const {
    PHOOL_VIRTUAL_WARN("bool check_sensorIDsorted() const"          );
    return false;
  }

  // Routines to manipulate the ghit array...
  // """"""""""""""""""""""""""""""""""""""""

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

  /**
   * @brief  Find a ghit which has the specified hit ID.
   *
   * @param[in] hitid  a hit with this "hitid" is searched for.
   * @param[in] ifrom  limit the search range (this index is included).
   * @param[in] iupto  limit the search range (this index is included).
   * @return  A hit ID if a hit is found.  Otherwise -1.
   */
  virtual int  indexOfGhit (const int        hitid    ,
			    const int        ifrom = 0,
			    const int        iupto =-1) const {
    PHOOL_VIRTUAL_WARN("int indexOfGhit(const int,"
	    "const unsigned int=0,const unsigned int=-1) const");
    return -1;
  }
  /**
   * @brief  [NOT RECOMMEND TO USE] Find a ghit which has the specified pointer address.
   *
   * @param[in] hit    a hit with the pointer address of this "hit" is searched for.
   * @param[in] ifrom  limit the search range.
   * @param[in] iupto  limit the search range.
   * @return  A hit ID if a hit is found.  Otherwise -1.
   */  
  virtual int  indexOfGhit (const SvxGhit*   hit      ,
			    const int        ifrom = 0,
			    const int        iupto =-1) const {
    PHOOL_VIRTUAL_WARN("int indexOfGhit(const SvxGhit*,"
	    "const unsigned int=0,const unsigned int=-1) const");
    return -1;
  }
  /**
   * @brief  Find a series of hits in the specified sensor.
   *
   * The internal hit list has to be sorted before this function is called.
   * If no hit is found, idx_lb and idx_ub are set to -1 and -2, respectively,
   * and thus it is safe to use the loop "for (int idx = idx_lb; idx <= idx_ub; idx++)" without checking the return value.
   *
   * @param[in]  sensor    hits in this "sensor" is searched for.
   * @param[out] idx_lb    the lower bound of array index above which hits are in the "sensor".
   * @param[out] idx_ub    the upper bound of array index below which, including "idx_ub" itself, hits are in the "sensor".
   * @param[in]  ifrom     limit the search range.
   * @param[in]  iupto     limit the search range.
   * @return     true if at least one hit is found.
   */
  virtual bool indexOfGhit(const SvxSensor* sensor, 
                           int& idx_lb, int& idx_ub, 
                           int ifrom = 0, int iupto =-1) const {
    PHOOL_VIRTUAL_WARN("bool indexOfGhit(...) const");
    return false;
  }

  // Methods
  // """""""
  virtual void print () const { PHOOL_VIRTUAL_WARN("void print()" ) ;}

  ClassDef(SvxGhitList, 1);
};
#endif /* __SVXGHITLIST_HH_ */
