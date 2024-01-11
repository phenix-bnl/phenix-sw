#ifndef __MEMCRECOMODULEREALYEAR4_H__
#define __MEMCRECOMODULEREALYEAR4_H__

#include <SubsysRecoStack.h>
#include "emcManageable.h"
#include <string>
#include <sstream>

class emcDataStorageMap;
class PHTimeStamp;
class PHFlag;

/** SubsysReco for real data, Run4 (and beyond, if nothing better's available).

By default, all emcal sectors are treated, the various data obtained from
database come from Postgres, and the full variability of tower gains is used.
 While default should be fine, experts may want to play with some parameters, explained hereafter.

The sectors to be treated can be changed according to recoConsts flag EMCSECTORS, e.g.

\code
recoConsts* rc = recoConsts::instance();
rc->set_CharFlag("EMCSECTORS","pbsc"); // use only PbSc
rc->set_CharFlag("EMCSECTORS","pbgl"); // use only PbGl
rc->set_CharFlag("EMCSECTORS","W0E3"); // use only W0 and E3 sectors
\endcode

Calibration data source can be changed globally, using recoConsts flag EMCDATASOURCE, or very selectively, using the EMCEXPERTDATASOURCES, e.g.
\code
recoConsts* rc = recoConsts::instance();
rc->set_CharFlag("EMCDATASOURCE","File_ASCII"); // use ASCII file as data source (WARNING: this is really an expert mode of operation!)
rc->set_CharFlag("EMCDATASOURCE","DB_Objy"); // use Objy as data source (if that still works...)
rc->set_CharFlag("EMCEXPERTDATASOURCES","QAs:None,Pedestals5:File_ASCII"); // disable QA collection and read pedestals from files (really really expert mode)
\endcode

Usage of gain variation can be disconnected if recoConsts flag EMCCONSTANTGAINS is set to 1 (default 0).

Clustering can be disabled (thus leaving only the calibration stage) by setting RUNEMCCLUSTERING integer flag to 0 (default is 1, i.e. run the clustering).

\sa setup() to see the list of internal modules used.

@author Laurent Aphecetche
*/

class mEmcRecoModuleRealYear4 : public SubsysRecoStack
{
public:
  
  mEmcRecoModuleRealYear4(const PHFlag&);

  virtual ~mEmcRecoModuleRealYear4();

  using SubsysRecoStack::process_event;

  using SubsysRecoStack::End;

  const char* getName() const { return Name(); }

  /** Defines the nodes we need, together with the list of (PH)modules
      to be called at each event (in that order) : 
      -# mEmcCalibratorModulev2
      -# mEmcTOFCorr6Module
      -# mEmcClusterizerv0
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
  emcDataStorageMap* fDataStorageMap;
  std::ostringstream fStartupMessage;
  static const float fgTowerThresholdPbSc;
  static const float fgTowerThresholdPbGl;
  static const float fgMinClusterEnergyPbSc;
  static const float fgMinClusterEnergyPbGl;
};

#endif
