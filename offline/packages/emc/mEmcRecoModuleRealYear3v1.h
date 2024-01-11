#ifndef __mEmcRecoModuleRealYear3v1_h__
#define __mEmcRecoModuleRealYear3v1_h__

#include <SubsysRecoStack.h>
#include "emcManageable.h"
#include <string>
#include <sstream>

class PHTimeStamp;
class PHFlag;

/** SubsysReco for real data, Run3. 

By default, all emcal sectors are treated, the various data obtained from
database come from Postgres, and the full variability of tower gains is used.
 While the default should be fine, experts may want to play with some parameters, explained hereafter.

The sectors to be treated can be changed according recoConsts flag EMCSECTORS, e.g.

\code
recoConsts* rc = recoConsts::instance();
rc->set_CharFlag("EMCSECTORS","pbsc"); // use only PbSc
rc->set_CharFlag("EMCSECTORS","pbgl"); // use only PbGl
rc->set_CharFlag("EMCSECTORS","W0E3"); // use only W0 and E3 sectors
\endcode

Calibration data source can be changed using recoConsts flag EMCDATASOURCE, e.g.
\code
recoConsts* rc = recoConsts::instance();
rc->set_CharFlag("EMCDATASOURCE","File_ASCII"); // use ASCII file as data source (WARNING: this is really an export mode of operation!)
rc->set_CharFlag("EMCDATASOURCE","DB_Objy"); // use Objy as data source (if that still works...)
\endcode

Usage of gain variation can be disconnected if recoConsts flag EMCCONSTANTGAINS is set to 1.

\sa setup() to see the list of internal modules used.

@author Laurent Aphecetche
*/

class mEmcRecoModuleRealYear3v1 : public SubsysRecoStack
{
public:
  
  mEmcRecoModuleRealYear3v1(const PHFlag&);

  virtual ~mEmcRecoModuleRealYear3v1();

  using SubsysRecoStack::process_event;

  using SubsysRecoStack::End;

  const char* getName() const { return Name(); }

   /** Defines the nodes we need, together with the list of (PH)modules
      to be called at each event (in that order) : 
      1. mEmcCalibratorModulev1
      2. mEmcTOFCorr5Module
      3. mEmcClusterizerv0
  */
  int InitRun(PHCompositeNode* topNode); 

  virtual void identify(std::ostream& os = std::cout) const;

  virtual int isValid() const { return 1; }

private:

  void createNodeTree(PHCompositeNode* topNode);
  void setup_calibrator(PHCompositeNode* topNode);
  void setup_clustering(PHCompositeNode* topNode);

private:

  PHTimeStamp* fTimeStamp;
  int fRunNumber;
  std::string fDstNodeName;
  bool fConstantGains;

  std::string fSectors;
  emcManageable::EStorage fDataSource;
  std::ostringstream fStartupMessage;

  static const float fgTowerThresholdPbSc;
  static const float fgTowerThresholdPbGl;
  static const float fgMinClusterEnergyPbSc;
  static const float fgMinClusterEnergyPbGl;
};

#endif
