// ===============
// FILE: SvxParManager.h
// ===============
#ifndef SVXPARMANAGER_H__
#define SVXPARMANAGER_H__

#include <SubsysReco.h>
#include <phool.h>
#include <PHTimeStamp.h>
#include <iostream>

class PHCompositeNode;
class svxAddress;
class svxDetectorGeo;
class SvxPixelHotDeadMap;
class SvxPixelHotDeadMapv2;
class SvxDeadMap; // strips only (class should be renamed)
class SvxBeamCenterPar;
class SvxStripThreshold;
class SvxDaqErrorMap;

/**
 * @brief  A SubsysReco module to coordinate svxAddress, svxDetectorGeo, svxPixelHotDead
 * @date  Created by Takashi Hachiya in Aug 2011
 *
 * @date Added strips in September 2011. Sasha Lebedev
 * @date Added Offsets of the coordinate system in Dec 2011. Takashi Hachiya
 * @date Added BeamCenter in Dec 2011. Takashi Hachiya
 *
 */
class SvxParManager : public SubsysReco
{

 public:

  SvxParManager(const std::string &name = "SVXPARRECO");
  virtual ~SvxParManager();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void set_ReadAddrParFromDB(int a); // call address->set_usedatabase
  void set_ReadGeoParFromFile(int a) {m_readGeofile=a;} ///< see m_readGeoFile
  void set_TimeStamp(PHTimeStamp T) {m_timeStamp = T;}
  void set_UseProductionGeo(bool flag) { m_useProductionGeoFlag = flag; }


  // Read pixel map files in new ref/diff format
  void set_UseRefDiffPixelMap(bool b) {m_useRefDiffPixelMap=b;}        ///< disable pixel hot/dead map
  void set_ReadRefDiffPixelMapFromFile(int a) {m_readRefDiffPixelMapFiles=a;}
  void set_RefDiffPixelMapFiles(const std::string &chanref,
				const std::string &chandif,
				const std::string &chipmap);

  // Original functions (to be deprecated) for handling pixel hot/dead map
  void set_UsePixelMap(bool b) {m_usePixelMap=b;}        ///< disable pixel hot/dead map
  void set_ReadPixelMapFromFile(int a) {m_readPixelMapfile=a;} ///< see m_readGeoFile
  void set_PixelHotDeadChipFileName(const std::string &name)  {m_PixelHotDeadChipFileName = name;}
  void set_PixelHotDeadPixelFileName(const std::string &name) {m_PixelHotDeadPixelFileName = name;}

  // For now, the strip maps follow the original design (there is no reference).
  void set_ReadStripHotDeadFromFile(bool yesno) {m_readStripHotDeadFromFile=yesno;}
  void set_StripHotDeadFileName(const std::string &name) {m_StripHotDeadFileName = name;}
  void set_StripHotDeadHybridsFileName(const std::string &name) {m_StripHotDeadHybridsFileName = name;}
  void set_StripHotDeadReadoutsFileName(const std::string &name) {m_StripHotDeadReadoutsFileName = name;}

  void set_OffsetVtxToCnt(float x, float y, float z);
  void set_OffsetEastToWest(float x, float y, float z);
  void set_OffsetFromDB(bool yesno) { m_offsetFromDB=yesno; } // default is yes
  void set_BeamCenter(float x, float y); // m_beamcenterFromDB is changed to false
  void set_BeamCenterFromDB(bool yesno) { m_beamcenterFromDB=yesno; } // default is yes
  void set_GeometryFileName(const std::string &name) {m_GeometryFileName = name;}
  void Load_ThresholdFile(const std::string &thresholdfile) { m_stripThresholdFileName = thresholdfile;}
  void set_UseStripThresholdDatbase(const bool yn) { m_useStripThresholdDatabase=yn;}
  void set_UseOldStripThresholdFile(const bool yn) { m_useOldStripThresholdFile=yn;}
  void set_useDaqErrorMapDatabase(bool flag)      { m_useDaqErrorMapDatabase = flag;}
  void set_DaqErrorMapFile(const std::string &file) { m_daqErrorMapFileName=file; } 

  void set_UseOldHotDeadMap(bool b)  {m_oldhotdead=b;}

  svxAddress*         getSvxAddress()      const { return m_address; } 
  svxDetectorGeo*     getSvxDetectorGeo()  const { return m_geometry; } 
  SvxPixelHotDeadMap* getPixelHotDeadMap() const { return m_pixelhotdead; } 
  SvxDeadMap*         getStripHotDeadMap() const { return m_striphotdead; } 
  SvxBeamCenterPar*   getBeamCenterPar()   const { return m_beamcenter; } 
  SvxStripThreshold*  getStripThreshold()  const { return m_stripthreshold; } 
  SvxDaqErrorMap*     getDaqErrorMap()     const { return m_daqerr; } 

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  void setGeoBankIdForOldProduction(int runnumber);
  void setOffsetBankIdForOldProduction(int runnumber);
  
 private:
  svxAddress         *m_address;         // for SVX raw hits
  svxDetectorGeo     *m_geometry;        // SVX DetectorGeo Object
  SvxPixelHotDeadMap *m_pixelhotdead;    // Pixel HotDead Map
  SvxPixelHotDeadMapv2 *m_pixelhotdead2; // Pixel HotDead Map v2
  SvxDeadMap         *m_striphotdead;    // Strip HotDead Map
  SvxBeamCenterPar   *m_beamcenter;      // BeamCenter
  SvxStripThreshold  *m_stripthreshold;  // StripThreshold
  SvxDaqErrorMap     *m_daqerr;          // DaqError Map

  int                m_readGeofile;
  int                m_readRefDiffPixelMapFiles; // Use ref/diff format (AMA 9/25/2013)
  bool               m_useRefDiffPixelMap; // Use ref/diff format (DCM 9/26/2013)
  int                m_readPixelMapfile;  // Use original full format (To be deprecated)
  bool               m_usePixelMap; // Use original full format (To be deprecated)
  bool               m_readStripHotDeadFromFile;
  PHTimeStamp        m_timeStamp;

  // Text files: reference and run-based diff, for pixel chips and pixel channels
  std::string        m_PixChipFile;
  std::string        m_RefPixChannelFile;
  std::string        m_DifPixChannelFile;

  // File names for original full maps (to be deprecated)
  std::string        m_PixelHotDeadChipFileName;
  std::string        m_PixelHotDeadPixelFileName;
  std::string        m_StripHotDeadFileName;
  std::string        m_StripHotDeadHybridsFileName;
  std::string        m_StripHotDeadReadoutsFileName;

  std::string        m_GeometryFileName;
  bool               m_beamcenterFromDB;
  bool               m_offsetFromDB;
  bool               m_useProductionGeoFlag;

  std::string        m_stripThresholdFileName;
  bool               m_useStripThresholdDatabase;
  bool               m_useOldStripThresholdFile;

  bool               m_useDaqErrorMapDatabase;
  std::string        m_daqErrorMapFileName;

  bool               m_oldhotdead;
};
#endif
