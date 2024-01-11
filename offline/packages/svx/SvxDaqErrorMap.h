#ifndef __SVXDAQERRORMAP_H__
#define __SVXDAQERRORMAP_H__


#include "SvxParameters.h"

class PHTimeStamp;

/** 
 * @brief  Represents DAQ error status for pixel and strip modules (good=0, bad=1).
 *
 */

/**
 * @brief The class for the SVX DAQ error map of pixel and strip for each run.
 *
 * The pixel and strip map is read from a text file or the database (svxpixelhotdeadchipmap)
 *
 * module status
 * 
 * with "status" is bit-wise variable corresponding the DAQ error (good=0, bad= not 0);
 *  0x01 = bad length of the data
 *  0x02 = DCM check sum 
 *  0x04 = FEM parity
 *  0x08 = DCM FEM parity
 *  0x10 = FEM clock
 *  0x20 = FEM event
 *  0x40 = GL1 clock
 *  0x80 = SubSys error
 */


class SvxDaqErrorMap {
  public:
    enum {ERRCODE = 1000};

  public:
    /** Constructor */
    SvxDaqErrorMap();

    /** Destructor */
    virtual ~SvxDaqErrorMap();

    /** Ubiquitous verbosity setter. Higher value increases verbosity. */
    virtual void Verbosity(const int level) { m_verbose=level; }


    /** Reads from the text file given by filename.
     * 
     * @param[in] filename  Input file (defaults to ./SvxDaqErrorMap.txt).
     */
    virtual bool readFromFile(const char* filename = "svxDaqErrorMap.txt");
    virtual bool writeToFile (const char* filename = "svxDaqErrorMap.txt");


    /** Reads from the database given by runnumber.
     * 
     * @param[in] filename  Input file (defaults to ./SvxDaqErrorMap.txt).
     */
    virtual bool readFromDatabase(const int runnumber);
    virtual bool writeToDatabase (const int runnumber);
    virtual bool writeToDatabase (PHTimeStamp* Tstart, PHTimeStamp *Tstop, const char *desc);



    virtual void printStatus();
    virtual void printMaskModule();


    /**
     Get Status of pixel and strip 
     */
    virtual unsigned int getPixelStatus(const int module);
    virtual unsigned int getStripStatus(const int module);
    virtual unsigned int getStatus(const int detector, const int module);

    /**
     Set Status of pixel and strip 
     */
    virtual void setPixelStatus(const int module, const unsigned int status);
    virtual void setStripStatus(const int module, const unsigned int status);
    virtual void setStatus(const int detector, const int module, const unsigned int status);
   

   // test
   virtual void test();
   virtual bool compare(SvxDaqErrorMap& map);

  private:
   void clearStatus();

   int getPixelNBad();
   int getStripNBad();
   int getNBad(const int detector);

  private:
    unsigned int m_pixelStatus[SVXNMODULEPIXEL];
    unsigned int m_stripStatus[SVXNMODULESTRIP];

    int m_verbose;
};

#endif
