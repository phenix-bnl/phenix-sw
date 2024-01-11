#ifndef __SvxRawhitListv5_HH_
#define __SvxRawhitListv5_HH_

#include "SvxRawhitList.h"
#include "TClonesArray.h"
#include "SvxRawhitv5.h"

/**
 * @brief Container class for SvxRawhitv5
 *
 * Container class for SvxRawhitv5, inheriting from the base container class
 * SvxRawhitList
 *
 * @author Darren McGlinchey
 * @email darren.mcglinchey@colorado.edu
 * @date 20 Jun 2016
 */
class SvxRawhitListv5 : public SvxRawhitList
{
private:
    enum {SVXNRAWHIT = 40000};   ///< Default initial size of Rawhit list


protected:
    TClonesArray* m_hit_list;          ///< Hit container
    bool          m_hitIDsorted;       ///< rawhits are sorted, using hitID
    bool          m_sensorIDsorted;    ///< rawhits are sorted, using sensorID
    bool          m_pixelROCIDsorted;  ///< rawhits are sorted, using pixelROC
    bool          listPointersSet;     ///< Each SvxRawhitv4 knows list pointer
    int           m_hit_id_unused;     ///< Smallest hit ID not used.

    /**
     * Use STL sort functions to sort the list
     */
    void SortSTL();

    /**
     * Supply list pointer to each SvxRawhit
     */
    void restoreListPointers();


public:

    /** Default constructor **/
    SvxRawhitListv5(const unsigned int length = SVXNRAWHIT);

    /** Virtual destructor **/
    virtual ~SvxRawhitListv5();

    /** Reset method. Clear array and reset flags **/
    void Reset();

    /** Standard PHObject method for validity check **/
    int  isValid() const;

    /** Standard PHObject identification method **/
    void identify(std::ostream &os = std::cout) const;


    //! @name Insert/Set/Get
    //@{

    /**
     * Add SvxRawhit
     *
     * @param[in] ihit Index of hit to be added
     * @return Pointer to new SvxRawhit object
     */
    SvxRawhit* addRawhit    (const int ihit = -1);

    /**
     * Remove SvxRawhit
     *
     * @param[in] ihit Index of hit to be removed
     */
    void removeRawhit(const unsigned int ihit);

    /**
     * Get SvxRawhit
     *
     * @param[in] ihit Index of hit to be retrieved
     * @return Pointer to SvxRawhit object
     */
    SvxRawhit* get_Rawhit(const unsigned int ihit) const
    { return (SvxRawhit*) m_hit_list->UncheckedAt(ihit) ;}

    /**
     * Get the number of SvxRawhits in the list
     *
     * @return The number of SvxRawhit objects in the list
     */
    int get_nRawhits() const
    { return m_hit_list->GetLast() + 1 ;}

    /**
     * Set flag indicating the list is sorted by hit ID
     *
     * @param[in] sorted True indicates list is sorted by hit ID, else False
     */
    void set_hitIDsorted(bool sorted)
    { m_hitIDsorted = sorted; }

    /**
     * Set flag indicating the list is sorted by sensor ID
     *
     * @param[in] sorted True indicates list is sorted by sensor ID, else False
     */
    void set_sensorIDsorted(bool sorted)
    { m_sensorIDsorted = sorted; }

    /**
     * Set flag indicating the list is sorted by pixelROC ID
     *
     * @param[in] sorted True indicates list is sorted by pixelROC, else False
     */
    void set_pixelROCIDsorted(bool sorted)
    { m_pixelROCIDsorted = sorted; }

    /**
     * Check if the list has been sorted by hit ID
     *
     * @return True if sorted by hitID, False otherwise
     */
    bool check_hitIDsorted() const
    { return m_hitIDsorted;}

    /**
     * Check if the list has been sorted by sensor ID
     *
     * @return True if sorted by sensor ID, False otherwise
     */
    bool check_sensorIDsorted() const
    { return m_sensorIDsorted;}

    /**
     * Check if the list has been sorted by pixelROC ID
     *
     * @return True if sorted by pixelROC ID, False otherwise
     */
    bool check_pixelROCIDsorted() const
    { return m_pixelROCIDsorted;}

    //@}

    //! @name Array manipulation
    //@{

    /**
     * Compress the list
     *
     * @return Number of SvxRawhit objects in the list
     */
    int Compress();

    /**
     * Set the size of the list
     *
     * @param[in] nhit New size of the list
     * @return Size of the list
     */
    int set_TClonesArraySize(const unsigned int nhit);

    //@}

    //! @name Array sorting and searching
    //@{

    /**
     * Sort the list by hit ID
     */
    void sort_hitID();

    /**
     * Sort the list by sensor ID
     */
    void sort_sensorID();

    /**
     * Sort the list by sensor ID
     */
    void sort_pixelROCID();

    /**
     * Unsort the list
     *
     * Resets the sorted flags to false, indicating the list is no longer sorted
     */
    void unSort();


    /**
     * Find the index of the first occurrence of the SvxRawhit
     * in the list with a matching hit ID
     *
     * @param[in] hitid Hit ID of the SvxRawhit being searched for
     * @param[in] ifrom Index in the list the search begins at
     * @param[in] iupto Index in the list the search ends at
     * @return Index of the matched SvxRawhit
     */
    int indexOfRawhit (const int hitid,
                       int ifrom = 0,
                       int iupto = -1) const;


    /**
     * Find the index corresponding to the first occurrence of the SvxRawhit
     * in the list according to the input SvxRawhit.
     *
     * npar
     *
     * @param[in] rawhit Rawhit object to be compared against
     * @param[in] npar Parameters to be compared.
     *                 Sets SvxRawhit::compareChan, See SvxRawhitv5::Compare()
     * @param[in] ifrom Index in the list the search begins at
     * @param[in] iupto Index in the list the search ends at
     * @return Index of the matched SvxRawhit
     */
    int indexOfRawhit (SvxRawhit& rawhit,
                       const int npar  = 3,
                       const int ifrom = 0,
                       const int iupto = -1) const;


    /**
     * Find the index corresponding to the first occurance of the SvxRawhit
     * in the list which matches the input pointer.
     *
     * @param[in] hit Pointer to SvxRawhit being searched for
     * @param[in] ifrom Index in the list the search begins at
     * @param[in] iupto Index in the list the search ends at
     * @return Index of the matched SvxRawhit
     */
    int indexOfRawhit (const SvxRawhit* hit,
                       const int        ifrom = 0,
                       const int        iupto = -1) const;

    /**
     * Find the index range of SvxRawhits in the list which match the
     * input sensor information
     *
     * @param[in] sensor Sensor information being searched for
     * @param[out] idx_lb Lower value of the found index range
     * @param[out] idx_ub Upper value of the found index range
     * @param[in] ifrom Index in the list the search begins at
     * @param[in] iupto Index in the list the search ends at
     * @return True means search was successful, false otherwise
     */
    bool indexOfRawhit(const SvxSensor* sensor,
                       int& idx_lb, int& idx_ub,
                       int ifrom = 0,
                       int iupto = -1) const;


    /**
     * Find the index range of SvxRawhits matching a pixelROCID
     *
     * Use the input SvxRawhit to compare against the desired values
     *
     * @param[in] rawhit Rawhit holding information to be compared against
     * @param[out] idx_lb Lower value of the found index range
     * @param[out] idx_ub Upper value of the found index range
     * @param[in] ifrom Index in the list the search begins at
     * @param[in] iupto Index in the list the search ends at
     * @return True means search was successful, false otherwise
     */
    bool indexRangeOfRawhitsPixelROC(SvxRawhit* rawhit,
                                     int& idx_lb, int& idx_ub,
                                     int ifrom = 0,
                                     int iupto = -1) const;

    //@}

    /**
     * Print list information
     */
    void print() const;


    //---
    ClassDef(SvxRawhitListv5, 1)
};
#endif /* __SvxRawhitListv5_HH_ */
