#ifndef __SvxPixelRawHitList__
#define __SvxPixelRawHitList__

#include "PHObject.h"

#include <map>
#include <vector>

class SvxRawhitList;
class SvxRawhit;
class svxAddress;

/**
 * List of svx pixel hits for each svx pixel chip
 *
 * This class was designed to be used in the VTXP event re-alignment
 * effort. It stores raw svx pixel hits keeping only the chip index
 * and channel number as unique identifiers.
 *
 * @author Darren McGlinchey
 * @email darren.mcglinchey@colorado.edu
 * @date 29 Jun 2016
 */
class SvxPixelRawHitList: public PHObject
{
public:

    /** Default constructor **/
    SvxPixelRawHitList(){};

    /**
     * Constructor 
     *
     * Takes in an SvxRawhitList and propogates the hits to 
     * the internal map from the SvxRawhitList.
     */
    SvxPixelRawHitList(SvxRawhitList* hitlist);

    SvxPixelRawHitList(SvxPixelRawHitList* hitlist);

    /** Destructor **/
    virtual ~SvxPixelRawHitList(){};

    /**
     * Standard PHObject Reset 
     *
     * This clears the map member variable.
     */
    void Reset();

    /** Standard PHObject identify **/
    void identify(std::ostream& os = std::cout) const;

    /**
     * Add hit from SvxRawhit object
     */
    void add_hit(SvxRawhit *hit);

    /**
     * Add a hit in the given chip, channel
     */
    void add_hit(short chipID, short channel); 

    /**
     * Add a set of hits to the given chip
     */
    void add_chipHits(short chipID, std::vector<short> *hits);

    /**
     * Get all the hits for a given chip
     */
    std::vector<short> get_hits(short chipID) const;

    /**
     * Get the number of hits in a given chip
     */
    int get_nhits(short chipID);

    /**
     * Get the total number of hits stored in this list
     */
    int get_nhits();

    /**
     * Clear the hits for a given chip
     */
    void reset_chip(short chipID);

    /**
     * Fill an SvxRawhitList
     *
     * @return The number of hits added to SvxRawhitList
     */
    int fill_rawhitlist(SvxRawhitList *hitlist, svxAddress *svx);

    ///
    /// D. McGlinchey - These should all really be in svxAddress,
    ///                 but that class is horribly bloated ...
    ///

    /** 
     * Get the chipID
     * 
     * @param[in] lyr  Layer     (0-1)
     * @param[in] ldr  Ladder    (0-19)
     * @param[in] sen  Sensor    (0-3)
     * @param[in] roc  Pixel ROC (0-7)
     * @return    chip index     (0-479)
     */
     short get_chipID(int lyr, int ldr, int sen, int roc) const;


    /**
     * Get the layer from the chipID
     */
     int get_layer(short chipID) const;

    /**
     * Get the ladder from the chipID
     */
     int get_ladder(short chipID) const;

    /**
     * Get the sensor from the chipID
     */
     int get_sensor(short chipID) const;

    /**
     * Get the pixel ROC from the chipID
     */
     int get_pixelROC(short chipID) const;

    /**
     * for debugging - print chip content
     */
     void Print(const short chipID) const;

private:

    /**
     * Map of svx pixel hits
     * 
     * Key:   unique chip ID (0-479)
     * Value: vector of unique channel numbers (0-8191)
     */
    std::map<short, std::vector<short> > m_chiphits;


    //---
    ClassDef(SvxPixelRawHitList, 1)
};

#endif //__SvxPixelRawHitList__
