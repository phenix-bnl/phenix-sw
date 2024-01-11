#ifndef __EMCSTATICDATA_H__
#define __EMCSTATICDATA_H__

#include <vector>
#include <cassert>

class EmcSector;
class PbScCalibrationData;
class PbGlCalibrationData;
class PHTimeStamp; 
  
/** Base accessor for PbSc and PbGl sector static characteristics. 

  Object of this class stores pointers to EmcSector objects for both
  EMCAL subsystems - PbSc and PbGlass.
  All specifics (light yield per unit energy, PMT gains etc ) are dealt
  with by the derived classes, obtained through getPbScData() and
  getPbGlData() methods.
  It is also the responsability of the derived class to initialize all data 
  including those declared in the base class.
	 
	 @author: E.Kistenev
	 @date:  03/00/99
*/

class EmcStaticData 
{
public:
  /** Creates one instance of EmcStaticData Class. 
      When created - it establishes pointers but don't load the data. 
      To actually load the Static data - user needs to decide which 
      Sectors he wants to use in his Application and to make a call to 
      buildEmcSector method below. */
  static EmcStaticData * buildEmcStaticData();
  ///
  static int deleteEmcStaticData();
  ///
    EmcSector * getSector(int SectorNumber) {
    assert(SectorNumber>=0 && SectorNumber<(int)Sectors.size()) ;
    return Sectors[SectorNumber];}
  /// 
    void buildEmcSector(const char * , PHTimeStamp* ts = 0);
  ///
    PbScCalibrationData * getPbScData(){return PbScData;}
  ///
    PbGlCalibrationData * getPbGlData(){return PbGlData;}

 protected:
  EmcStaticData() ;
  ~EmcStaticData() ;

 private:

  static EmcStaticData * single; //!
  static int access_count; //!
  //  the following pointers are "zeros" if legacy data collection failed
    PbScCalibrationData * PbScData; //!
    PbGlCalibrationData * PbGlData; //!
    std::vector<EmcSector*> Sectors ; //!

};
#endif
