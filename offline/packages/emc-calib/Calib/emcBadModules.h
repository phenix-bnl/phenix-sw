#ifndef __EMCBADMODULES_H__
#define __EMCBADMODULES_H__

#ifndef __PHOBJECT_H__
#include "PHObject.h"
#endif
#include <iostream>

/** (ABC) Information center about Q&A of EMCAL towers.

@ingroup interface
@ingroup calibration

   Q&A information might come from online only or from online and
   physics.

   Of prime importance here is the concept of dead and warnmaps.
   dead=something is surely wrong, 
   warn=something is strange

   Both are 20 bits words, and are a characteristic of a single tower,
   but give information of the status of the \e neighbouring towers.

   Some bits are related to energy information, while the others are
   for timing information. By using a proper bit mask, one can test
   for specific things. Typically you may want to insure that
   deadmap=warnmap=0 (everything is as perfect as possible), or make
   a less stringent test, e.g. deadmap & 0X1CE70 to test that amplitude
   is correct in a 3x3 region around that tower.

   Note that those flags are also available in the emcClusterContent class,
   where they represent the information of the central tower of the cluster.

   \code
  // For amplitude bits are:
  // ---------------------
  // |   | 18| 19| 20|   |
  // ---------------------
  // | 13| 14| 15| 16| 17|
  // ---------------------  ^ y
  // | 8 | 9 | 10| 11| 12|  |
  // ---------------------  |
  // | 3 | 4 | 5 | 6 | 7 |  |
  // ---------------------  ------> z(x)
  // |   | 0 | 1 | 2 |   |
  // ---------------------
  // as viewed from the back of the central tower (which has bit 10 set
  // to 1 if it's itself a bad module); corner towers are excluded
  //
  // For ToF bits are :
  // -------------
  // | 27| 28| 29|  ^ y
  // -------------  |
  // | 24| 25| 26|  |
  // -------------  |
  // | 21| 22| 23|  ------> z(x)
  // -------------
  // as viewed from the back of the central tower (which has bit 25 set
  // to 1 if it's itself a bad module)
  //
  \endcode 
 */

class emcBadModules : public PHObject
{

public:

  enum EInformationOrigin { 
    ///
    kNone=-1, 
    /// Online QA (typically by onlcal)
    kOnline, 
    /// Offline QA (typically by first analysis on that dataset)
    kPhysics, 
    /// Both online and offline QA
    kAll };

  /// Dtor.
  virtual ~emcBadModules();

  virtual emcBadModules* clone(void) const = 0;

  /**@name Retrieve dead/warn neighbour information.
     The 'Fast' methods do not do any check, i.e. no
     bound checking nor check whereas information is available/uptodate or not
   */
  //@{ 
  /// Get deadneighbours information for the tower referenced by towerID.
  virtual unsigned int Deadmap(int towerID) = 0;
  ///
  virtual unsigned int DeadmapFast(int towerID) const = 0;
  
  /// Get warnneighbours information for the tower referenced by towerID.
  virtual unsigned int Warnmap(int towerID) = 0;
  ///
  virtual unsigned int WarnmapFast(int towerID) const = 0;
  //@}
  
  /**@name Access to underlying separate information. 
     (Q&A from online or Q&A from physics). 
     The 'Fast' methods do not do any check, i.e. no
     bound checking nor check whereas information is available/uptodate or not.
  */
  //@{

  using TObject::Error;
  using TObject::Warning;

  ///
  virtual unsigned int Error(emcBadModules::EInformationOrigin source, 
			     int towerID) = 0;
  ///
  virtual unsigned int ErrorFast(emcBadModules::EInformationOrigin source, 
				 int towerID) const = 0;
  ///
  virtual unsigned int Warning(emcBadModules::EInformationOrigin source, 
			       int towerID) = 0;
  ///
  virtual unsigned int WarningFast(emcBadModules::EInformationOrigin source, 
				   int towerID) const = 0;

  //@}

  using TObject::Print;

  /// Print one tower info (or all if towerid=-1).
  virtual std::ostream& Print(int towerid=-1, std::ostream& out=std::cout) = 0;

  /// Number of towers.
  virtual unsigned int size(void) const = 0;

  ClassDef(emcBadModules,0) // ABC of an advanced list of bad emcal modules.
};
 
inline
std::ostream& operator << (std::ostream& out, emcBadModules& bm)
{ 
  return bm.Print(0,out); 
}

#endif
