/** \file SvxPixelHotDeadMapv2.h
 * Contains the class for the SVX pixel hot/dead pixel map for real data for Run11.
 * Run by run pixel map should be applied in Run11.
 * Hiroyuki Sako, 2011/06/10
 * Treat run by run calibration data for vertex pixel detector
 * The table is svxpixelhotdeadrun in calibrations and the corresponding
 * data structure is defined in offline/database/PdbSvxPixelHotDeadMap.
 * For fast reference, we use arrays instead of hash map.
 * Revision 2011/07/21
 * We now use two kinds of maps
 * 1) PdbSvxPixelHotDeadChipMap and
 * 2) PdbSvxPixelHotDeadPixelMap
 * 1) is for bad chips for every run
 * 2) is for bad pixels for each chip in a range of runs
 * In this class 1) and 2) are merged automatically to maker pixel maps for all pixels for a given run
 *
 * Note: Originally there is SvxHotMap class and corresponding PdbSvxDeadMap which include
 *       both pixel and strip layers.
 *       However the data structure of these classes are optimized for strip detectors
 *       whereas it does not fit well for the pixel detectors efficiently.
 */

#ifndef SVXPIXELHOTDEADMAPV2_HH_
#define SVXPIXELHOTDEADMAPV2_HH_

#include <string>

class PdbSvxPixelHotDeadPixelMap;
class PdbSvxPixelHotDeadChipMap;

/** 
 * @brief  Represents a pixel status (dead, normal, hot, test, unknown).
 *
 */

/**
 * @brief The class for the SVX pixel dead/hot pixel map for each run.
 *
 * The chip map is read from a text file or the database (svxpixelhotdeadchipmap) and 
 * the pixel map is read from a text file or the database (svxpixelhotdeadpixelmap) as a series of 
 * status records, one line (or record) per pixel.  A line looks like:
 *
 * module ROC column row rate status
 * 
 * with "status" being the integer corresponding
 *
 */
class SvxPixelHotDeadMapv2 {
public:

  /** Constructor */
  SvxPixelHotDeadMapv2(const int verbosityLevel = 0);

  /** Destructor */
  virtual ~SvxPixelHotDeadMapv2(){}

  /** Ubiquitous verbosity setter. Higher value increases verbosity. */
  void Verbosity(const int level);

  /** Reads in dead/hot pixels from the text file given by filename.
   * 
   * @param[in] filename  Input file (defaults to ./svxPixelHotDeadMap.txt).
   */
  bool readChipMapFromFile(const std::string &filename);
  bool readPixelMapFromFile(const std::string &filename);
  bool readPixelMapFromFile(const int module, const int ROC, const std::string &filename);
  bool writeChipMapToFile(const int runStart, const int runEnd, const std::string &filename);
  bool writePixelMapToFile(const int runStart, const int runEnd, const std::string &filename);
  bool writePixelMapToFile(const int runStart, const int runEnd, const int module, const int ROC, const std::string &filename);


  /** Reads in dead/hot pixels from the text file given by filename.
   *  The ref_file is a run-independent status list, and diff_file lists run-dependent updates.
   * 
   */
  bool readPixelMapsFromFile(int &runStart, int &runEnd, std::string ref_file, std::string diff_file);

  /**
   */
  void setUseChipMap(const bool flag=true) { m_useChipMap = flag; }
  void setUsePixelMap(const bool flag=true){ m_usePixelMap = flag; };

  /** Reads dead/hot pixels from database rather than a file. 
   *
   * @param[in] run
   *    Run Number
   *
   * @return true if the map was successfully read.
   */
  bool readFromDatabase(const int run, const bool read_reference=false);
  bool readChipMapFromDatabase(const int run);
  bool readPixelMapFromDatabase(const int run);
  bool readReferencePixelMapFromDatabase(const int run);

  /** Write the map to the database. 
   *
   * @param[in] run
   *    Run Number
   *
   * @return true if the map was successfully written.
   */
  bool writeChipMapToDatabase(const int run);
  bool writeReferencePixelMapToDatabase(const int runStart, const int runEnd);
  bool writePixelMapToDatabase(const int runStart, const int runEnd);

  /** set status for one pixel */
  void setPixelStatus(const int module,
          const int ROC,
          const int column,
          const int row,
          const int status);

  /** set raw status for one pixel */
  void setRawPixelStatus(const int module,
       const int ROC,
       const int column,
       const int row,
       const signed char status);

  /** set status for one pixel */
  void setPixelStatus(const int layer,
          const int ladder,
          const int south_north,
          const int ROC,
          const int column,
          const int row,
          const int status);
    
  /** set status for one chip */
  void setRawChipStatus(const int module,
      const int ROC,
      const signed char status);

  /** set status for one chip */
  void setChipStatus(const int module,
         const int ROC,
         const int status);


  /** set fraction of good pixels for one tile */
  void setTileGoodFrac(const int module,
           const int ROC,
           const int tile);

  /** set fraction of good pixels for all tiles 
      (requires map array to be filled first) */
  void setTileMap();


  void print() const;

  /** 
   * get the status for a given pixel. 
   * This is the combined status of the chip & pixel map
   **/
  int getStatus(const int module,
    const int ROC,
    const int column,
    const int row) const;

  /** 
   * get the status for a given pixel. 
   * This is the combined status of the chip & pixel map
   **/
  int getStatus(const int layer,
    const int ladder,
    const int south_north,
    const int ROC,
    const int column,
    const int row) const;


  /** get the status for the chip */
  int getChipStatus(const int module,
        const int ROC) const;

  /**
   * get the pixel status.
   * based only off the pixel-level deadmap
   * and ignores the chip status
   **/
  int getPixelStatus(const int module,
         const int ROC,
         const int column,
         const int row) const;

  /** check if this pixel should be used in clustering */
  bool isPixelOkForClustering(const int module,
    const int ROC,
    const int column,
    const int row) const;

  /** get the fraction of good pixels in a tile */
  float getTileGoodFrac(const int module,
      const int ROC,
      const int tile) const;
  
  
  void printBadPixels(const int module, const int roc) const;
  
  void setBlankStatus(const char blank = 0);
  char getBlankStatus() const {return blank_status;}
  
  int FirstRun() const {return firstrun;}
  int LastRun() const {return lastrun;}
  int ComparePixelMaps(const SvxPixelHotDeadMapv2 &rhs) const;
  int CompareChipMaps(const SvxPixelHotDeadMapv2 &rhs) const;
  unsigned int countBadChips() const;
  unsigned int countBadPixels() const;

  // To keep things simple for the moment, define some upper bounds on 
  // the numbers of various elements (or elements per parent element).
  // Similar constants are already defined in SvxSimReco, but not
  // accessible.  If time and interest warrants, this could all be made 
  // more flexible by somehow having the SimReco provide the true values.
  const static int NMODULE  = 60;
  const static int NROC     = 8;
  const static int NCOLUMN  = 32;
  const static int NROW     = 256;
  const static int NTILE    = 16; // 4x4 split of one ROC (8 cols x 64 rows).

private:
  void initializeMaps();
  void initializeChipMap();
  void initializePixelMap(const int module, const int ROC);
  bool rangeCheck(const int module, const int ROC) const;
  bool rangeCheck(const int module, const int ROC, const int column, const int row) const;
  bool rangeCheck(const int module, const int ROC, const int tile) const;
  void importChipMap(const PdbSvxPixelHotDeadChipMap &rhs);
  void exportChipMap(const int module, const int ROC, PdbSvxPixelHotDeadChipMap &rhs);
  void importPixelMap(const PdbSvxPixelHotDeadPixelMap &rhs);
  void exportPixelMap(const int module, const int ROC, const int column, const int row, PdbSvxPixelHotDeadPixelMap &rhs);


  /**
   * The data structure for the in-memory dead/hot map.
   * 
   * As a first cut, the data structure to store the dead/hot maps will be
   * logically a matrix of [module][chip][column][row]
   */

  char pixelmap[NMODULE][NROC][NCOLUMN][NROW];
  char chipmap[NMODULE][NROC];
  float tilemap[NMODULE][NROC][NTILE];

  /** Ubiquitous verbosity level.  Higher value yields more messages. */
  int verbosity;
  unsigned int countBadPixels(const int module, const int ROC) const;

  bool m_useChipMap;
  bool m_usePixelMap;
  
  int reference_map_bankID_offset;
  void setReferenceOffset(){reference_map_bankID_offset = 1;}
  void setDifferenceOffset(){reference_map_bankID_offset = 0;}
  
  // this is the value which is not written to the database
  // by default it is zero, but for difference maps we do
  // want to write the status zero to the database
  char blank_status;
  int firstrun;
  int lastrun;
};

#endif
