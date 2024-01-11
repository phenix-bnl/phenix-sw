// =========================
// FILE: SvxGhitRawhitList.h
// =========================
#ifndef __SVXGHITRAWHITLIST_HH_
#define __SVXGHITRAWHITLIST_HH_

#include <phool.h>
#include <PHObject.h>
#include <iostream>

class SvxGhitRawhit;

/**
 * @brief  The abstract class for a list (container) of Ghit-rawhit relations.
 *
 * @date  Created  by V. L. Rykov on 12-Feb-2004
 * @date  Modified by V. L. Rykov on 11-May-2004:
 *        Sorting and index search methods added.
 */
class SvxGhitRawhitList : public PHObject
{

 public:
  // Constructor(s) & destructor
  // """""""""""""""""""""""""""
  SvxGhitRawhitList         () {
    //std::cout << "SvxGhitRawhitList object created" << std::endl;
  }
  
  virtual ~SvxGhitRawhitList() {
    //std::cout << "SvxGhitRawhitList object destroyed" << std::endl;
  }

  // The "standard PHObject response" functions...
  // """""""""""""""""""""""""""""""""""""""""""""
  virtual void Reset()         { PHOOL_VIRTUAL_WARN("void Reset()"       )           ;}
  virtual int  isValid() const { PHOOL_VIRTUAL_WARN("int isValid() const"); return 0 ;}
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxGhitRawhitList object"
       << std::endl;
  }

  // Add/remove/set/get methods...
  // """""""""""""""""""""""""""""

  /// Add a new entry to itself and return a pointer to it
  virtual SvxGhitRawhit* addGhitRawhit   (const          int ihit = -1) {
    PHOOL_VIRTUAL_WARN("SvxGhitRawhit* addGhitRawhit(const int=-1)" );
    return NULL;
  }
  /**
   * @brief  Remove the "ihit"-th entry.
   *   This only sets the pointer of the removed entry to "0", and thus
   *   one must call Compress() to shrink the array size
   *  (see ROOT TClonesArray::RemoveAt())
   */
  virtual void           removeGhitRawhit(const unsigned int ihit     ) {
    PHOOL_VIRTUAL_WARN("void removeGhitRawhit(const unsigned int)"  );
  }
  /// Get the number of entries stored
  virtual int get_nGhitRawhits () const {
    PHOOL_VIRTUAL_WARN("int get_nGhitRawhits() const");
    return -9999;
  }
  /// Get the pointer of the "ihit"-th entry
  virtual SvxGhitRawhit* get_GhitRawhit (const unsigned int ihit) const {
    PHOOL_VIRTUAL_WARN("SvxGhitRawhit* get_GhitRawhit(const unsigned int) const");
    return NULL;
  }
  /// Return true if this list has been sorted by ghit ID.
  virtual bool        check_ghitSorted  ()                        const {
    PHOOL_VIRTUAL_WARN("bool check_ghitSorted() const"  );
    return false;
  }
  /// Return true if this list has been sorted by rawhit ID.
  virtual bool       check_rawhitSorted ()                        const {
    PHOOL_VIRTUAL_WARN("bool check_rawhitSorted() const");
    return false;
  }

  // Routines to manipulate the ghitrawhit array...
  // """"""""""""""""""""""""""""""""""""""""""""""

  /// Remove all list entries which are "0"
  virtual int Compress()
    { PHOOL_VIRTUAL_WARN("int Compress()"                              ); return -9999 ;}
  virtual int set_TClonesArraySize(const unsigned int nhit)
    { PHOOL_VIRTUAL_WARN("int set_TClonesArraySize(const unsigned int)"); return -9999 ;}

  // Sorting and index search
  // """"""""""""""""""""""""
  virtual void sortGhits   () { PHOOL_VIRTUAL_WARN("void sortGhits()"  ) ;}
  virtual void sortRawhits () { PHOOL_VIRTUAL_WARN("void sortRawhits()") ;}
  virtual void unSort      () { PHOOL_VIRTUAL_WARN("void unSort()"     ) ;}

  /**
   * @brief  Find a series of entries which have the specified ghit ID
   *
   * The internal list has to be sorted by ghit ID before this function is called.
   * If no hit is found, idx_lb and idx_ub are set to -1 and -2, respectively,
   * and thus it is safe to use the loop "for (int idx = idx_lb; idx <= idx_ub; idx++)" without checking the return value.
   *
   * @param[in]  id        this hit ID is searched for.
   * @param[out] idx_lb    the lower bound of array index above which hits have the specified ID.
   * @param[out] idx_ub    the upper bound of array index below which, including "idx_ub" itself, hits have the specified ID.
   * @param[in]  ifrom     limit the search range.
   * @param[in]  iupto     limit the search range.
   * @return     true if at least one entry is found.
   */
  virtual bool indexOfGhit(const int id, int& idx_lb, int& idx_ub, 
                           int ifrom=0, int iupto=-1) const {
     PHOOL_VIRTUAL_WARN("int indexOfGhit() const");
    return false;
  }
  /// Same as indexOfGhit() but by rawhit ID
  virtual bool indexOfRawhit(const int id, int& idx_lb, int& idx_ub, 
                             int ifrom=0, int iupto=-1) const {
     PHOOL_VIRTUAL_WARN("int indexOfRawhit() const");
    return false;
  }
  
  // Methods
  // """""""
  virtual void print () const { PHOOL_VIRTUAL_WARN("void print()" ) ;}

  ClassDef(SvxGhitRawhitList, 1);
};
#endif /* __SVXGHITRAWHITLIST_HH_ */
