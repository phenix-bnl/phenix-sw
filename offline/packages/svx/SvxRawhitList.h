// =====================
// FILE: SvxRawhitList.h
// =====================
#ifndef __SVXRAWHITLIST_HH_
#define __SVXRAWHITLIST_HH_

#include <phool.h>
#include <PHObject.h>
#include <SvxSensor.h>

#include <iostream>

class SvxRawhit;

/**
 * @brief  The abstract class for a list (container) of rawhits.
 *
 * @date  Template used: SvxGhitList.h by Jeffery Mitchell as of 11/20/2003
 * @date  Created  by V. L. Rykov on 09-Mar-2004
 * @date  Modified by V. L. Rykov on 13-May-2004:
 *        Sorting and index search methods added.
 */
class SvxRawhitList : public PHObject
{

public:
    // Constructor(s) & destructor
    // """"""""""""""""""""""""""
    SvxRawhitList         () {
        //std::cout << "SvxRawhitList object created" << std::endl;
    }

    virtual ~SvxRawhitList() {
        //std::cout << "SvxRawhitList object destroyed" << std::endl;
    }

    // The "standard PHObject response" functions...
    // """""""""""""""""""""""""""""""""""""""""""""
    virtual void Reset()         { PHOOL_VIRTUAL_WARN("void Reset()"       )           ;}
    virtual int  isValid() const { PHOOL_VIRTUAL_WARN("int isValid() const"); return 0 ;}
    virtual void identify(std::ostream &os = std::cout) const {
        os << "Identify yourself: virtual SvxRawhitList object" << std::endl;
    }

    // Copy class
//  virtual SvxRawhitList* clone() const {
//    PHOOL_VIRTUAL_WARN("SvxRawhitList* clone()" );
//    return NULL;
//  }

    // Add/remove/set/get methods...
    // """""""""""""""""""""""""""""

    /// Add a new hit to itself and return a pointer to the hit
    virtual SvxRawhit* addRawhit    (const          int ihit = -1) {
        PHOOL_VIRTUAL_WARN("SvxRawhit* addRawhit(const int=-1)" );
        return NULL;
    }
    /// See SvxGhitList::removeGhit()
    virtual void       removeRawhit (const unsigned int ihit     ) {
        PHOOL_VIRTUAL_WARN("void removeRawhit(const unsigned int)" );
    }
    /// Get the number of hits stored
    virtual int get_nRawhits      ()                        const  {
        PHOOL_VIRTUAL_WARN("int get_nRawhits() const");
        return -9999;
    }
    /// Get the pointer of the "ihit"-th hit
    virtual SvxRawhit* get_Rawhit (const unsigned int ihit) const  {
        PHOOL_VIRTUAL_WARN("SvxRawhit* get_Rawhit(const unsigned int) const");
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
    /// Return true if this list has been sorted by sensor ID
    virtual bool check_pixelROCIDsorted ()                  const {
        PHOOL_VIRTUAL_WARN("bool check_pixelROCIDsorted() const"          );
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
    virtual void sort_pixelROCID () { PHOOL_VIRTUAL_WARN("void sort_pixelROCID()") ;}
    virtual void indexRangeOfRawhitsPixelROCPixelROC () { PHOOL_VIRTUAL_WARN("void indexRangeOfRawhitsPixelROCPixelROC()") ;}
    virtual void unSort        () { PHOOL_VIRTUAL_WARN("void unSort()"       ) ;}
    /// added by akimoto (12/29/2010)
    /// these functions are to set hitIDsorted or sensorIDsorted separately
    virtual void set_hitIDsorted (bool sorted)    { PHOOL_VIRTUAL_WARN( "void set_hitIDsorted()")    ;}
    virtual void set_sensorIDsorted (bool sorted) { PHOOL_VIRTUAL_WARN( "void set_sensorIDsorted()") ;}
    virtual void set_pixelROCIDsorted (bool sorted) { PHOOL_VIRTUAL_WARN( "void set_pixelROCIDsorted()") ;}

    /// See SvxGhitList::indexOfGhit()
    virtual int  indexOfRawhit (const int        hitid    ,
                                const int        ifrom = 0,
                                const int        iupto = -1) const {
        PHOOL_VIRTUAL_WARN("int indexOfRawhit(const int,"
                           "const unsigned int=0,const unsigned int=-1) const");
        return -1;
    }
    /**
     * @brief  [NOT WELL MAINTAINED] Find a first occurrence of rawhits
     *         which match the specified hit under the specified comparison condition "npar".
     *
     * @param[in] rawhit  a hit compared.
     * @param[in] npar    set the condition of comparisons to the "rawhit";
     *   @li  npar=0 ... up to sensor
     *   @li  npar=1 ... up to sensorSection
     *   @li  npar=2 ... up to sensorReadout
     *   @li  npar=3 ... up to channel
     * @param[in] ifrom   limit the search range.
     * @param[in] iupto   limit the search range.
     * @return  A hit ID if a hit is found.  Otherwise -1.
     */
    virtual int  indexOfRawhit (      SvxRawhit& rawhit   ,
                                      const int        npar  = 3,
                                      const int        ifrom = 0,
                                      const int        iupto = -1) const {
        PHOOL_VIRTUAL_WARN("int indexOfRawhit(SvxRawhit&,const int=3,"
                           "const unsigned int=0,const unsigned int=-1) const");
        return -1;
    }

    // SvxRawhit having the pointer equal to hit
    /// See SvxGhitList::indexOfGhit()
    virtual int  indexOfRawhit (const SvxRawhit* hit      ,
                                const int        ifrom = 0,
                                const int        iupto = -1) const {
        PHOOL_VIRTUAL_WARN("int indexOfRawhit(const SvxRawhit*,"
                           "const unsigned int=0,const unsigned int=-1) const");
        return -1;
    }

    /// See SvxGhitList::indexOfGhit()
    virtual bool indexOfRawhit(const SvxSensor* sensor,
                               int& idx_lb, int& idx_ub,
                               int ifrom = 0, int iupto = -1) const {
        PHOOL_VIRTUAL_WARN("bool indexOfRawhit(...) const");
        return false;
    }

    /// SvxRawhit's matching input SvxRawhit (pixelROC ID)
    virtual bool indexRangeOfRawhitsPixelROC(SvxRawhit* rawhit,
            int& idx_lb, int& idx_ub,
            int ifrom = 0, int iupto = -1) const {
        PHOOL_VIRTUAL_WARN("bool indexRangeOfRawhitsPixelROC(...) const");
        return false;
    }

    // Methods
    // """""""
    virtual void print () const { PHOOL_VIRTUAL_WARN("void print()" ) ;}

    ClassDef(SvxRawhitList, 1);
};
#endif /* __SVXRAWHITLIST_HH_ */
