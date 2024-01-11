#ifndef __SVXRAWHITV5_HH_
#define __SVXRAWHITV5_HH_

#include "SvxRawhit.h"
#include "SvxRawhitList.h"

/**
 * @brief Implementation class (ver. 5) of SvxRawhit.
 *
 * Ver. 5 of SvxRawhit extending the Compare() method to chip (pixel only).
 * This is necessary for the event re-alignment and reduces some of the
 * generality inherent in SvxRawhit ...
 *
 * @author Darren McGlinchey
 * @email darren.mcglinchey@colorado.edu
 * @date 21 Jun 2016
 */
class SvxRawhitv5 : public SvxRawhit
{

public:

    /**
     * Constructor
     *
     * @param[in] lst Pointer to SvxRawhitList
     * @param[in] rawhit SvxRawhit whose information is copied
     */
    SvxRawhitv5(SvxRawhitList* lst = NULL, SvxRawhit* rawhit = NULL);

    /**
     * Copy constructor
     *
     * @param[in] rawhit SvxRawhit whose information is copied
     */
    SvxRawhitv5(SvxRawhit* rawhit);

    /** Destructor **/
    virtual ~SvxRawhitv5()
    {/*std::cout << "SvxRawhitv5 object destroyed" << std::endl;*/}


    /** Standard PHObject Reset method **/
    void Reset();

    /** Standard PHOBject identify method **/
    void identify(std::ostream &os = std::cout) const;

    //! @name Setters
    //@{

    /** Set the hit ID **/
    void set_hitID(const int val);

    /** Set the Svx section value **/
    void set_svxSection(const int val);

    /** Set the Svx layer value **/
    void set_layer(const int val);

    /** Set the Svx ladder value **/
    void set_ladder(const int val);

    /** Set the Svx sensor value **/
    void set_sensor(const int val);

    /** Set the Svx sensor section value **/
    void set_sensorSection(const int val);

    /** Set the Svx sensor readout index **/
    void set_sensorReadout(const int val);

    /** Set the Svx channel value **/
    void set_channel(const int val);

    /** Set the pixel ROC **/
    void set_pixelROC(const int val);

    /** Set the ADC value **/
    void set_adc(const int val) { adc = val; }

    /** Set the sensor type **/
    void set_sensorType(const int val) { sensorType = (short) val; }

    /** Set the pixel module index **/
    void set_pixelModule(const int val) { pixelModule = (short) val; }

    /** Set the Hot/dead flag **/
    void set_HotDeadFlag (const int val) { HotDeadFlag = val; }

    /** Set clustering flag **/
    void set_isOkForClustering(const bool val) { isOkForClustering = val; }

    //@}
    //! @name Getters
    //@{

    int  get_sensorSection()     const { return (int) sensorSection    ;}
    int  get_sensorReadout()     const { return (int) sensorReadout    ;}
    int  get_sensorType()        const { return (int) sensorType       ;}
    int  get_channel()           const { return       channel          ;}
    int  get_adc()               const { return       adc              ;}
    int  get_pixelModule()       const { return       pixelModule      ;}
    int  get_pixelROC()          const { return       pixelROC         ;}
    int  get_HotDeadFlag()       const { return       HotDeadFlag      ;}
    bool get_isOkForClustering() const { return       isOkForClustering;}

    //@}

    /**
     * Set the pointer to the SvxRawhitList to which this SvxRawhit
     * belongs. This is then used to set sortability flags in the list
     * if SvxRawhit information is changed.
     *
     * @param[in] lst Pointer to SvxRawhitList containing this SvxRawhit
     */
    void set_ListPointer (SvxRawhitList* lst)
    { rawhitList = lst; }

    /**
     * Set the level at which SvxRawhits are compared.
     *
     * The comparison is done in Compare(). See description for detail
     * on compareChan values.
     */
    void set_compareChan (unsigned int   val) { compareChan = val ;}

    /**
     * Compare against another SvxRawhit. Needed for sorting.
     *
     * Details of the comparison are governed by two switches
     *  SvxHit::sorting_switch & compareChan
     *
     *  SvxHit::sorting_switch = -1: See SvxHit::Compare()
     *  SvxHit::sorting_switch =  1: See compareChan values
     *
     *  compareChan = 0: See SvxHit::Compare()
     *  if (compareChan <= 3)
     *     compareChan = 1: Additionally sort on sensorSection
     *     compareChan = 2: Additionally sort on sensorReadout
     *     compareChan = 3: Additionally sort on channel
     *  else
     *     compareChan = 4: Additionally sort on pixelROC
     *     compareChan = 5: Additionally sort on channel
     *
     * @param[in] rawhit SvxRawhit to be compared against
     * @return Compare value (-1, 0, 1)
     */
    Int_t Compare (const TObject* rawhit) const;

    /**
     * Clone this SvxRawhit
     */
    virtual SvxHit* Clone()
    { return new SvxRawhitv5(this); }

    /**
     * Copy the input SvxHit
     */
    virtual void Copy(SvxHit* hit);

    /**
     * Print information from this SvxRawhit
     */
    void print() const;

protected:

    // Data member definition
    short sensorSection;     ///< sensor section number
    short sensorReadout;     ///< sensor readout number: 0 pixel/x-strip; 1 u-strip
    short sensorType;        ///< Sensor type (1-9 pixel, 11-19 & 21-29 stipixel)
    int   HotDeadFlag;       ///< Flag on Rawhit if it fails Hot/Dead Map check 0=Good, -1=Dead, 1=Hot, 2=ADC:RMS Fail(strip)
    int   channel;           ///< channel number
    int   adc;               ///< adc value (must be "int", NOT "short")
    short pixelModule;       ///< pixel module (half-ladder) number (range: 0-59)
    short pixelROC;          ///< pixel readout chip number (range: 0-7)
    bool  isOkForClustering; ///< Flag if this Rawhit should be used in clustering, true=yes, false=no

    /// Pointer to the container
    SvxRawhitList* rawhitList; //! not persistent
    //SvxRawhitList* rawhitList;

    /// Comparison/sorting of sensorSection, sensorReadout & channel (varies from 0 to 3)
    static unsigned int compareChan;

    ClassDef(SvxRawhitv5, 1)
};

#endif
