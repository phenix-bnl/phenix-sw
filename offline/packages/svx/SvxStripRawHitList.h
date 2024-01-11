#ifndef __SvxStripRawHitList__
#define __SvxStripRawHitList__

#include "PHObject.h"

#include <map>
#include <vector>

class SvxRawhitList;
class SvxRawhit;
class svxAddress;

/**
 * List of svx strip hits for each svx pixel chip
 *
 * This class was designed to save strip rawhit with compact size.
 * It stores raw svx pixel hits and ADCs keeping only the chip index
 * and channel number as unique identifiers.
 *
 * @author Takashi Hachiya
 * @email hachiya@rcf.rhic.bnl.gov
 * @date 19 May 2017
 */
class SvxStripRawHitList: public PHObject
{
public:

    /** Default constructor **/
    SvxStripRawHitList(){};

    /**
     * Constructor 
     *
     * Takes in an SvxRawhitList and propogates the hits to 
     * the internal map from the SvxRawhitList.
     */
    //--SvxStripRawHitList(SvxRawhitList* hitlist, svxAddress* address=NULL);

    //--SvxStripRawHitList(SvxStripRawHitList* hitlist, svxAddress* address=NULL);

    /** Destructor **/
    virtual ~SvxStripRawHitList(){};

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
    void add_hit(SvxRawhit *hit, svxAddress* address);

    /**
     * Add a hit in the given chip, data(channel+ADC)
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
    int fill_rawhitlist(SvxRawhitList *hitlist, svxAddress *svx, const int adcoffset);

    ///

    /** 
     * Get the chipID
     * 
     * @param[in] lyr  Layer     (2-3)
     * @param[in] ldr  Ladder    (0-24) 0-15 for layer2, 0-23 for layer3
     * @param[in] sen  Sensor    (0-5) 0-4 for layer2, 0-5 for layer3
     * @param[in] roc  strip ROC (0-11)
     * @return    unique chip ID (13bit data) = Lyr(12) + Ldr(11-7) + Sen(6-4) + Chip(3-0)
     *                                           1bit      5bit       3bit        4bit
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
     int get_stripROC(short chipID) const;

    /**
     * for debugging - print chip content
     */
     void Print(const short chipID) const;

private:
    /**
     * Map of svx pixel hits
     * 
     * Key:   unique chip ID (13bit data) = Lyr(12) + Ldr(11-7) + Sen(6-4) + Chip(3-0)
     *                                        1bit      5bit       3bit        4bit
     * Value: vector of data (unique channel numbers (0-127) + ADC(0-255)(
     *          data = channel(MSB15-8) + ADC(LSB7-0)
     */
    std::map<short, std::vector<short> > m_chiphits;


    //---
    ClassDef(SvxStripRawHitList, 1)
};

#endif //__SvxPixelRawHitList__
