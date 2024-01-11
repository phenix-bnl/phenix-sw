/** \file SvxDeadMap.h
 * Contains the class for the SVX dead/hot pixel map simulation.
 */

#ifndef __SVXDEADMAP_HH_
#define __SVXDEADMAP_HH_

#include <map>

#include <PdbCalBank.hh>

/** 
 * @brief  Represents a channel's status (dead, normal, hot, test).
 *
 * "test" is meant to tell the simulation to output an event message,
 *     but no other observable effect.
 */
enum SvxChannelStatus {
    SVX_DEAD = -1,     /**< Channel never has output. */
    SVX_NORMAL = 0,    /**<  Channel is functioning normally. */
    SVX_HOT = 1,       /**<  Channel has output whether there is an event or not. */
    SVX_BADRMS = 2,    /**<  Channel has bad RMS. */
    SVX_TEST = -999    /**< For testing purposes. */
};

/** Merely an alias for syntactic brevity. */
typedef std::map < int, SvxChannelStatus > deadMap_t;

/**
 * @brief The class for the SVX dead/hot stripixel map.
 *
 * Despite its general name, this class is now used exclusively for
 * the stripixels. Use SvxPixelHotDeadMap to interface the pixel map.
 *
 * Throughout, "striplayer" refers to a stripixel layer (0 or 1).
 *
 * The map is read from a text file or the database as a series of
 * status records, one line (or record) per channel.  A line may look
 * like this:
 *
 * striplayer# ladder# sensor# sensorSection# readout# channel# status
 * 
 * with "status" being the integer corresponding (in the enum below)
 * to a descriptive flag like SVX_DEAD or SVX_HOT.
 *
 * To save space, normally functioning channels are not explicitly
 * mentioned.  A client may query a channel's status by specifying it
 * with the same parameters as above.  If no entry is present in the
 * dead map, it is considered to be operating normally; otherwise its
 * abnormal status as read from the map is returned.
 *
 * N.B.: At this time, no range checking of the various parameters
 * (striplayer#, ladder#, etc.) is done, so the status of nonexistent
 * channels can be specified and queried.  It is the responsibility of
 * the client to check that the parameters provided are in range.
 */
class SvxDeadMap {
  public:
    /** Constructor */
    SvxDeadMap(const int verbosityLevel = 0);

    /** Destructor */
    virtual ~SvxDeadMap() {}

    /** Ubiquitous verbosity setter. Higher value increases verbosity. */
    void Verbosity(const int level);

    void set_UseDatabase(bool yesno) {UseDatabase=yesno;}

    //bool Fetch();

    /** Reads in dead/hot channels from the text file given by filename.
 	 * 
 	 * @param[in] filename  Input file (defaults to ./svxDeadMap.txt).
 	 */
    bool readFromFile(std::string filename = "svxStripDeadMap.txt");
    bool readReadoutsFromReadoutsFile(std::string filename = "BadStripReadouts_347129.txt");
    bool writeToFile(std::string filename = "svxStripDeadMap.txt");
    bool writeReadoutsToFile(std::string filename = "svxStripDeadMap.txt");

    /** Reads dead/hot channels from database rather than a file. 
	 *
	 * @param[in] T	
	 *		Timestamp to use to determine which records are valid when
	 *		retrieving the map.
	 *
	 * @return true if the map was successfully read.
	 */
    bool readFromDatabase(PHTimeStamp * T);
    bool readReadoutsFromDatabase(PHTimeStamp * T);
    bool readFromDatabase(int run);
    bool readReadoutsFromDatabase(int run);

    /** Write the deadMap, as exists at time of calling, to the database. 
	 *
	 * @param[in] Tbeg	
	 *		Timestamp of the beginning of the validity interval for the map.
	 * @param[in] Tend
	 *		Timestamp of the end of the validity interval for the map.
	 *
	 * @return true if the map was successfully written.
	 */
    bool writeToDatabase(PHTimeStamp * Tbeg, PHTimeStamp * Tend);
    bool writeReadoutsToDatabase(PHTimeStamp * Tbeg, PHTimeStamp * Tend);
    bool writeToDatabase(int run1, int run2);
    bool writeReadoutsToDatabase(int run1, int run2);

    /** Get status of channel identified by:
	 *	 (striplayer, ladder, sensor, sensorSection, readout, channel).
	 *
	 * The numbering scheme will be the same as what was used when the maps
	 * were read in, which presumably should be the same as the SvxSimReco's.
	 * At the time of this writing, that means:
	 *
	 * @param[in] striplayer
	 *	 SVX striplayer number.  Striplayer 0,1 = VTX layer 2,3.
	 * @param[in] ladder
	 *	 Ladder number within the striplayer.  Ladders are numbered in a 
	 *   counterclockwise manner per striplayer, as looking down the beam toward
	 *   the negative z direction, with ladder 0 being the bottommost of the
	 *   east-side ladders in the striplayer.
	 * @param[in] sensor
	 *   Sensor number within the ladder. Sensors are numbered consecutively 
	 *   within a ladder from +z to -z; i.e., sensor 0 in a ladder is the one 
	 *   located at the most positive z.
	 * @param[in] sensorSection
	 *    Section number within a sensor. Sensors are divided into sections, 
	 *    which are more of a logical division than what might make sense in 
	 *    terms of the physical sensor. See SvxStripPixel.C for the parameters.  
	 *    Also consecutively numbered.
	 * @param[in] readout
	 *    Readout numbers are always 0 for simple pixels, but can be 0 or 1
	 *    for strip pixels (corresponding to the "x" and "u" readouts).
	 * @param[in] channel
	 *    Channel numbers depend on simulation parameters and are computed by
	 *    a formula that depends on specifics of the sensor in question.
	 */
    SvxChannelStatus channelStatus(const int striplayer,
                                   const int ladder,
                                   const int sensor,
                                   const int sensorSection,
                                   const int readout,
                                   const int channel) const;

    int channelStatus(const int striplayer,
                      const int ladder,
                      const int sensor,
                      const int sensorSection,
                      const int readout,
                      const int channel, 
                      const int dummy) const;

    int readoutStatus(const int striplayer,
                     const int ladder,
                     const int sensor,
                     const int sensorSection,
                     const int readout
                    ) const;
                      
   /** set status for one channel */
    void set_channelStatus(const int striplayer,
                           const int ladder,
                           const int sensor,
                           const int sensorSection,
                           const int readout,
                           const int channel,
                           SvxChannelStatus status);

    /** Ubiquitous object printer. */
    void print() const;

    /** Function mapping integer to channel status enum value. 
        This function is also used in the OnCal
      */
    SvxChannelStatus toSvxChannelStatus(const int i);

    /** generate ReadoutMap from DeadMap 
        This function is used in the OnCal
      */
    int generateReadoutMapFromDeadMap();

    /** test function */
    bool compareDeadMap(SvxDeadMap *map=NULL) const;
    bool compareReadoutMap(SvxDeadMap *map=NULL) const;

  private:

    bool UseDatabase;

    // To keep things simple for the moment, define some upper bounds on 
    // the numbers of various elements (or elements per parent element).
    // Similar constants are already defined in SvxSimReco, but not
    // accessible.  If time and interest warrants, this could all be made 
    // more flexible by somehow having the SimReco provide the true values.
    const static size_t MAXLAYERS = 2;
    const static size_t MAXLADDERS = 24;
    const static size_t MAXSENSORS = 6;


    /**
 	 * The data structure for the in-memory dead/hot map.
 	 * 
 	 * As a first cut, the data structure to store the dead/hot maps will be
 	 * logically a sparse matrix (we assume most of the channels in the 
 	 * detector will not be dead or hot)!  I'm actually going with something
 	 * in between a normal multidimensional array and a sparse matrix; the
 	 * map is a 3-D array indexed by striplayer, ladder, and sensor number, but
 	 * then the elements are STL maps (associative arrays) where the keys are
 	 * effectively ints made up of bit-shifted, OR'ed-together 
 	 * sensor section, readout, and channel number values (see the "hash"
 	 * method below) and the values are channel status values.
 	 */
    deadMap_t deadMap[MAXLAYERS][MAXLADDERS][MAXSENSORS];

   // map of dead stripixel readouts (quarter of a sensor)
    //      striplayer(0-1)   ladder(0-15 or 0-23) sensor(0-4 or 0-5) section(0-1), readout (0-1)
    short int deadReadoutMap[2][24][6][2][2];

    /** Ubiquitous verbosity level.  Higher value yields more messages. */
    int verbosity;

    /** Computes a unique single integer value given a sensor section,
  	 *  sensor readout number, and channel number.  
 	 *
 	 * Although we could use
 	 * extra array dimensions for section number and readout number above, 
 	 * we expect not every sensor section will necessarily have a dead/hot
 	 * channel (a section may ultimately contain only a few hundred channels).
 	 * But it would be wasteful to store int 3-tuples in the map keys above
 	 * as well, since section number and readout number are very small
 	 * (single-digit integers), so I instead roll the three values into
 	 * one int using some bit shifting.
 	 */
    int hash(const int sensorSection,
             const int readout, 
             const int channel) const;


    /** convert Nbadsensor to deadReadoutMap 
        this used in readReadoutFromFile 

        return : nBadReadout
       */
    int convertDeadReadoutMap();
};

#endif

