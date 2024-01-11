// ===============
// FILE: SvxSimulator.h
// ===============
#ifndef __SVXSIMULATOR_H__
#define __SVXSIMULATOR_H__

#include <SubsysReco.h>

#include <phool.h>

#include <PHTimeStamp.h>
#include <PHTimeServer.h>

#include "SvxParameters.h"
#include "svxAddress.hh"

#include <iostream>

class SvxGhitList          ;
class SvxRawhitList        ;
class SvxGhitRawhitList    ;
class SvxSensor            ;
class SvxPisaHit;
//class svxDetectorGeo;
class PHCompositeNode      ;
class PHTimeStamp;

class TFile;

/**
 * @brief  A SubsysReco module to read PISA output and create raw hits.
 * @date Created by Sasha Lebedev in December 2009
 */
class SvxSimulator : public SubsysReco
{

 public:

  SvxSimulator(const std::string &name = "SVXSIM");
  virtual ~SvxSimulator();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  // Analysis methods
  int generate_ghits();        ///< Example. Fills ghits with dummy values
  int fillGhitList(SvxPisaHit* svx); ///< Fill GEANT hits, d_ghit,  from the PISA tree
  int fillRawhitList(svxAddress &SvxAddressObject);        ///< Fill d_rawhit & d_ghit2rawhit from d_ghit

  // remove T.Hachiya 2011.08.14
  //void set_ReadParFromFile(int a) {ReadParFromFile=a;} ///< see ReadParFromFile
  //void set_TimeStamp(PHTimeStamp T);

  void set_ChargeAsymXUWidth(float val);      ///< Just an interface.  See SvxSensor::set_sAQ()
  void set_StripixelNoise(float val);         ///< Just an interface.  See SvxSensor::set_sNOISE()
  void set_StripixelZeroSup(int val);         ///< Just an interface.  See SvxSensor::set_adcthre_zs()
  void set_StripixelAdcThreshold(int val);    ///< Just an interface.  See SvxSensor::set_adcthre_rawhit()
  void set_StripixelAdcSumThreshold(int val); ///< Just an interface.  See SvxSensor::set_adcthre_rawhit_sum()


 protected:

  /// Termination upon SVX parameter reading failure
  void pisaPar_error() {
    std::cerr << "SvxSimulator ERROR: pisaParFile read error" << std::endl;
    exit(EXIT_FAILURE);
  }

  // Data nodes
  SvxGhitList          *d_ghit;           ///< GEANT hits
  SvxRawhitList        *d_rawhit;         ///< SVX raw hits
  SvxGhitRawhitList    *d_ghit2rawhit;    ///< SVX ghit<->rawhit relater

  // SVX geometry definitions
  // """"""""""""""""""""""""

  enum {NSVXSECTIONS = 1};                         ///< Number of SVX sections (only barrel now)
  // Svx section indexes: 0 - barrel, 1 - north, 2 - south
  enum {SVXBARRELINDEX, SVXNORTHINDEX, SVXSOUTHINDEX};
  unsigned int svxSecNlayers[NSVXSECTIONS];        ///< Number of layers/section:
                                                   ///< 0-Barrel 1-North, 2-South
  // SvxSensor objects: Barrel
//  enum {SVXMAXBARLAYER  =  4};                     ///< Max. num. of bar. layers
//  enum {SVXMAXBARLADDER = 40};                     ///< Maximum ladders/layer
//  enum {SVXMAXBARSENSOR = 10};                     ///< Maximin sensors/ladder
  unsigned int nBarLadder[SVXLADDERNUMBER];         ///< ladders/layer
  unsigned int nBarSensor[SVXSENSORNUMBER];        ///< sensor/ladder
  static const unsigned int barSenType[];          ///< sensor type
  /// Pointers to barrel sensor objects
  SvxSensor *(*barSensor)[SVXLADDERNUMBER][SVXSENSORNUMBER];

  /// Utility to find the svx Nodes.
  int  CreateNodeTree(PHCompositeNode *topNode);
  void GetNodes(PHCompositeNode *topNode);

  void renumberRawhitID();

  // Internal global variables
  const char  *rootFile;        ///< Name of the input ROOT file
  TFile       *rootTFile;       ///< Pointer to the input ROOT file
  int          pisaNumEntries;  ///< Total number of ntuple entries
  int          pisaEntryIndex;  ///< Ntuple entry being read in
  const char  *pisaInFile;      ///< Name of the input PISA ROOT file
  TFile       *pisaInTFile;     ///< Pointer to the input PISA ROOT file
  const char  *pisaParFile;     ///< Name of the input svxParPISA.txt file

  // Counters
  int nSvxGhits;          ///< Number of SvxGhit objects filled
  int nSvxRawhits;        ///< Number of SvxRawhit objects filled

  // Parameter for charge sharing btw x-u.
  float m_stripixel_sAQ;                ///< see SvxSensor::sAQ
  float m_stripixel_sNOISE;             ///< see SvxStrip11v1::m_sNOISE
  int   m_stripixel_adcthre_zs;         ///< see SvxStrip11v1::m_ADCThresholdZS
  int   m_stripixel_adcthre_rawhit;     ///< see SvxStrip11v1::m_adc_min_rawhit
  int   m_stripixel_adcthre_rawhit_sum; ///< see SvxStrip11v1::m_adc_min_rawhit_sum

//  int fillhistograms;

  PHTimeServer::timer _timer;   ///< Timer
};
#endif 
