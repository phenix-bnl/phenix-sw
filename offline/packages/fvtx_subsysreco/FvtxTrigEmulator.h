#ifndef __FVTXTRIGEMULATOR_H__
#define __FVTXTRIGEMULATOR_H__

/*!
  \file    FvtxTrigEmulator.h
  \brief   SubsysReco module to emulate the FVTX trigger
  \author  D. McGlinchey
  \version $Revision: 1.4 $
  \date    $Date: 2016/05/03 21:01:29 $
*/

#include <SubsysReco.h>
#include <phool.h>
#include <PHTimeServer.h>
#include <FVTXOO.h>

#include <iostream>

class PHCompositeNode;
class TFvtxHitMap;
class TFvtxTrigEmulInfo;
class TString;


/*!
*/
class FvtxTrigEmulator : public SubsysReco
{

public:

  /*! Constructor with name */
  FvtxTrigEmulator(const std::string &name = "FVTXTRIGEMULATOR");

  /*! Virtual destructor */
  virtual ~FvtxTrigEmulator();

  /*! Initialization function */
  int Init(PHCompositeNode *topNode);

  /*! Initialization function called at the start of a run */
  int InitRun(PHCompositeNode *topNode);

  /*! Process event */
  int process_event(PHCompositeNode *topNode);

  /*! End */
  int End(PHCompositeNode *topNode);

  /*! Print the current trigger configuration parameters */
  void PrintTrigConfig();

  /*! Read the trigger configuration from the files set above */
  int ReadTrigConfigFromFile();

  //! @name Setters
  //@{

  /*! Set Adc threshold for hits */
  void set_AdcThreshold(int thre, int arm = 0);

  /*! Set number of hits required in each station */
  void set_StationThreshold(int thre, int arm = 0);

  /*! Set number of stations required in given sector */
  void set_SectorThreshold(int thre, int arm = 0);

  /*! Set FEM logic for paired sectors [OR, AND] */
  void set_FemLogic(TString log, int arm = 0);

  /*! Set number of required FEM's in a cage */
  void set_CageThreshold(int thre, int arm = 0);

  /*! Set logic for cages in fiven arm [OR, AND] */
  void set_ArmLogic(TString log, int arm = 0);

  /*! Set coincidence mode for both arms [OR, AND] */
  void set_CoincidenceMode(TString log);

  /*! Set mode [Multitrack, Multihit] */
  void set_Mode(TString log, int arm = 0);

  /*! Set flag to read the trigger configuration parameters from file */
  void set_ReadTrigConfigFromFile(bool rff) { m_readConfigFromFile = rff; };

  /*! Set the trigger configuration file name for the given arm */
  void set_TrigConfigFileName(const std::string &name, int arm = 0);


  //@}
  //! @name Getters
  //@{

  int get_AdcThreshold(int arm = 0);
  int get_StationThreshold(int arm = 0);
  int get_SectorThreshold(int arm = 0);
  int get_CageThreshold(int arm = 0);
  TString get_FemLogic(int arm = 0);
  TString get_ArmLogic(int arm = 0);
  TString get_Mode(int arm = 0);
  TString get_CoincidenceMode();

  //@}

protected:

  //-- Functions

  /*! Get the data nodes from the node tree */
  int GetNodes(PHCompositeNode *topNode);

  /*! Add new data nodes to the node tree */
  int CreateNodeTree(PHCompositeNode *topNode);

  /*! Fill the Fvtx hit array */
  int FillHitArray();

  /*! Emulate the trigger decision for a given even */
  int EmulateTriggerDecision();

  /*! Create the trigger info object and put it on the node tree */
  int SaveTriggerDecision();

  /*! Check if the arm index is valid */
  bool armValid(int arm);

  /*! Print the current hit array */
  void PrintHits();

  /*! Print the current trigger decisions */
  void PrintTrigDecision();

  //-- Data nodes
  TFvtxHitMap *d_hitmap;
  TFvtxTrigEmulInfo *d_triginfo;

  //-- Variables


  /*! Number of hits in each [arm][cage][station][sector] */
  int m_hitarray[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE][FVTXOO::MAX_SECTOR][FVTXOO::MAX_STATION];


  //configuration parameters
  int m_cfgAdcThre[FVTXOO::MAX_ARM];           /// adc treshold
  int m_cfgStationNhitThre[FVTXOO::MAX_ARM];   /// num of hits per station
  int m_cfgSectorStationThre[FVTXOO::MAX_ARM]; /// num of active stations per sector
  int m_cfgCageThre[FVTXOO::MAX_ARM];          /// num of active FEM's per cage
  int m_cfgFemLogic[FVTXOO::MAX_ARM];          /// logic for fem trigger (0:OR, 1:AND)
  int m_cfgArmLogic[FVTXOO::MAX_ARM];          /// logic for arm trigger (0:OR, 1:AND)
  int m_cfgMode[FVTXOO::MAX_ARM];              /// hit logic (0:Multitrack, 1:Multihit)
  int m_cfgCoincMode;

  /// variables for reading in trigger configuration from file
  bool m_readConfigFromFile;
  std::string m_trigConfigFileName[FVTXOO::MAX_ARM];


  /// trigger decisions for each arm
  bool m_trigStation[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE][FVTXOO::MAX_SECTOR][FVTXOO::MAX_STATION];
  bool m_trigSector[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE][FVTXOO::MAX_SECTOR];
  bool m_trigFEM[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE][FVTXOO::MAX_SECTOR/2];
  bool m_trigCage[FVTXOO::MAX_ARM][FVTXOO::MAX_CAGE];
  bool m_trigArm[FVTXOO::MAX_ARM];
  bool m_trig;


  PHTimeServer::timer _timer; ///< Timer
};
#endif


