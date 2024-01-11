#ifndef __EMCINDEXER_H__
#define __EMCINDEXER_H__

#include <vector>
#include <string>

/** General EMCAL Indexer class.
    Primary source of conversion to/from various types of emcal indices,
    within various "frames" (whole emcal, one sector, one supermodule).

    If you were to pick up only one index, please use TowerID (ranging from 0
    to 24767), as it is the less ambiguous and the most compact (only one integer).

    \note All detector specific functions are defined in the PbGl and 
    PbSc Indexer child classes. 
    @ingroup emcnumbering
*/ 

class EmcIndexer
{
public:

  EmcIndexer() {}
  virtual ~EmcIndexer() {}

  /**@name Methods specific to sub-system (PbSc/PbGl).
           To be overriden in subclasses. 
  */
  //@{

  /// i(SectorTower) -> SM#, iSM144, iSM144T, x(SM144T), y(SM144T).
  virtual void iST_SMInd(const int , int &, int &, int &, int &) = 0;

  /// Converts i(SectorTower) -> x(SectorTower), y(SectorTower).
  virtual void iSTxyST(const int , int &, int &) = 0;

  /// SM, i(SM Tower) -> i(SectorTower).
  virtual int SMiSMTiST(int , int ) = 0;

  /// SM# -> xSM, ySM.
  virtual void SMxySM(int, int &, int &) = 0; 

  /// xSM,ySM -> SM# (reverse operation of SMxySM).
  virtual int xySMiSM(int x, int y) = 0;  

  /// SM#, x(SM Tower), y(SM Tower) -> i(SectorTower).
  virtual int SMxySMTiST(int , int , int ) = 0; 

  /// x(SM Tower), y(SM Tower)  -> i(SM Tower).
  virtual int xySMTiSMT(int , int ) = 0;  

  //@}

  /**@name (Static) Methods common for PbSc and PbGl. 
     As such, they do need to know the Sector Number */
  //@{

  /// Checks validity of the Sector, x, y combination.
  static bool IsValid(int iS, int x, int y);

  /** Converts PHENIX (on-line) Sector number and Sector Tower Number 
      into PHENIX TowerId. */
  static int iSiSTiPX(int iS, int iST);

  /// PHENIX tower(TowerId) -> SectorNumber, SectorTower.
  static void iPXiSiST(int TowerId, int & iS, int & iST);

  /// PHENIX tower(TowerId) -> SectorNumber, SuperModule, Supermodule tower
  static void iPXiSiSMiSMT(int TowerId, int & iS, int & iSM, int & iSMT);

  /// Converts i(SectorTower) -> x(SectorTower), y(SectorTower).
  static void iSTxyST(int iS, int iST, int & x, int & y);

  /// Sector, x(SectorTower), y(SectorTower) -> i(SectorTower).
  static int xySTiST(int iS, int x,int y);

  /// Gets TowerId for the Tower x/y in Sector iS(online).
  static int getTowerId(int iS, int x, int y);

  /** Decodes TowerId into Sector Nmber iS (online) and x/y 
      inside the Sector. */
  static void decodeTowerId(int TowerId, int & iS, int & x, int & y);

  /// Get (arm,sector_in_arm,yrow,zrow) from a TowerID (offline used).
  /// returns true for valid, false for invalid towerID.
  static bool TowerLocation(int towerID, int& arm, int& sector_in_arm,
			    int& yrow, int& zrow);

  /// Get TowerID from (arm,sector_in_arm,yrow,zrow (offline used).
  //  static int TowerID(int arm, int sector_in_arm, int yrow, int zrow);

  /// Decode software key
  static void decodeSoftwareKey(int software, int& arm, int& sector_in_arm,
				int& yrow, int& zrow);

  /// Get TowerID from (arm,sector_in_arm,yrow,zrow (offline used).
  static int TowerID(int arm, int sector_in_arm, int yrow, int zrow);
  static int TowerID(int softwareKey);

  /// Get (offline) softwareKey from TowerID.
  //  static long getSoftwareKey(int TowerId) 
  static int SoftwareKey(int TowerId);
  static int SoftwareKey(int arm, int sector_in_arm, int yrow, int zrow);

  /// Tells if an absolute tower id is a PbSc reference.
  static bool isPbScReference(int TowerId);

  /// Tells if an absolute tower id is a PbGl reference.
  static bool isPbGlReference(int TowerId);

  /// Tells if an absolute tower id is a reference tower.
  static bool isReference(int TowerId) {
    return (TowerId>24767);
  }

  /// Tells if a fem holds reference signals.
  static bool isReferenceFEM(int femAbsolutePosition) {
    return (femAbsolutePosition>=EmcIndexer::FirstReferenceFEMAbsolutePosition());
  }
    
  /// Tells if an absolute tower id is PbSc.
  static bool isPbSc(int TowerId) {
    return ( (TowerId>=0 && TowerId<108*144) || isPbScReference(TowerId) );
  }

  /// Tells if an absolute FEM id is PbSc
  static bool isPbScFEM(int absFEM) {
    return ( (absFEM>=0 && absFEM<108) || (absFEM>=172 && absFEM<=174));
  }

  /// Tells if an absolute tower id is PbGl.
  static bool isPbGl(int TowerId) {
    return ( (TowerId>=15552 && TowerId<24768) || isPbGlReference(TowerId) );
  }

  /// Tells if an absolute FEM id is PbGl
  static bool isPbGlFEM(int absFEM) {
    return (absFEM>=108 && absFEM<172) || (absFEM>=176 && absFEM<=179);
  }

  /// SectorNumber, SectorTower -> SM144 NUmber (Sector scope), SM144 Tower.
  static void iSiSTiSMiSMT(const int iS, const int iST, int& iSM , int& iSMT);

  ///  Sector, SM144 Number(Sector Scope) -> PHENIX SM144 (0-171).
  static int iSiSM144_PXSM144(int iS, int iSM144);

  /** i(SM144 Tower) -> FEM Channel 
      Converts Tower Number (scope of SM144) into FEM Channel number. 
      Assumes ASIC24=true and Channel Numbers from 0 to 143) */
  static int iSMTiCH(const int iSMT) { return iSMTiCH(iSMT,true); }

  // SectorNumber,SMNumber,SMTower -> PHENIX Tower.
  static int iSiSMiSMTiPX(int iS, int iSM, int iSMT);
  
  /** i(SM Tower), ASIC Readout Flag -> FEM Channel. 
      Same as above but includes FIX for 144/192 channels switching. 
      It returns the channel number either in the 0-143 scope if ASIC24 
      is TRUE or in the 0-191 scope if ASIC24 is false. */
  static int iSMTiCH(const int iSMT, const bool fl);

  ///  i(SM144 Tower) -> x(SM144 Tower), y(SM144 Tower)        
  static void iSM144TxySM144T(const int iSMT, int& xsmt, int& ysmt);


  // SectorNumber,SMNumber,SMTower -> Sector Tower.
  static int iSiSMiSMTiST(int iS, int iSM, int SMT);

  ///  PHENIX SM144 (0-171) -> Sector, SM NUmber(Sector SM).
  static void PXSM144_iSiSM144(int PXSM144, int& iS, int & iSM144);

  /** From TowerId (PHENIX scope) to FEM number (PHENIX scope)
      and FEM channel number.*/
  static void PXPXSM144CH(int PX, int& PXSM144, int& CH);

  /// SM144 (EMC Scope), FEM Channel -> PHENIX EMC Tower.
  static int PXSM144iCH_iPX(const int SM144, const int CH);

  /**  FEM Channel -> i(SM144 Tower).
       Converts FEM Channel into Tower Number (scope of SM144). 
       Assumes ASIC24=true and Channel Numbers from 0 to 143). */
  static int iCHiSMT(int ch);

  /**  FEM Channel -> i(SM144 Tower).
       Converts FEM Channel into Tower Number (scope of SM144). */
  static int iCHiSMT(const int , bool ASIC24); 

  /// Max. number of FEMs in EMCAL.
  static int MaxNumberOfFEMs(void) { return 182; }

  /** The absolute id of the first reference FEM.
      i.e. if (absPosition>=FirstReferenceFEMAbsolutePosition) then absPosition
      is a reference FEM.
   */
  static int FirstReferenceFEMAbsolutePosition(void) { return 172; }

  ///  Sector identifier -> Sector Number 
  static int EmcSectorNumber(const char* sectorName);
  
  ///  Sector number -> Sector Identifier 
  static const char* EmcSectorId(int sectorNumber);

   /// Arm,Sector (offline convention) -> sectorNumber (online)
  static int sectorOfflineToOnline(int arm, int offline_sector);

  /// sectorNumber(online) -> Arm,Sector (offline convention);
  static void sectorOnlineToOffline(int sectorOnline, int& arm,
                                    int& offline_sector);

  //@}

  /**@name (Static) methods again, but probably for experts only. */
  //@{

  /** "Absolute" Channel number to TowerNumber (PHENIX scope).
      On request from Sergei Belikov: Conversion from Channel number 
      defined as "absolute" or equal to 192 X SM144 number (incrementing over
      all eight sectors 0-171) + FEMChannel (0-191) to TowerNumber 
      (PHENIX scope) -1 is returned for formally disconnected FEM channels 
      (12-16 and 28-31 on every ASIC board) */
  static int absFEMCHiPX(const int absFEMCH);

  /** TowerNumber (PHENIX scope) to "Absolute" Channel number.
      On request from Sergei Belikov: Conversion from TowerNumber 
      (PHENIX scope) into Channel number defined as "absolute" or 
      equal to 192 X SM144 number 
      (incrementing over all eight sectors 0-171) + FEMChannel (0-191) */
  static int iPXabsFEMCH(const int iPX);

  /** Conversion from absolute FEM Channel number (0-191) into ASIC number
      and ASIC Channel number (0-31). */
  static void iCH192ASICi32(const int iCH192, int& ASIC, int& iCH32);

  /** i(SM144 Tower), ASIC24 -> Q-base, Cell, 
      ASIC, Preamp, Input, FEM Channel. */
  static void iSM144TtoHWIndexes(int iSMT, bool ASIC24, int & QB, int &Cell, 
				 int &ASIC, int &Preamp, int &Input, 
				 int &FEMChannel);
  
  /** Converts FEM Channel number or EMC Tower number into all other 
      possible indices */
  static void findItemIdentity(const int ItemId, const bool ASIC24, 
			       const bool EMCalMapStyle, int &iS, int& iSM, 
			       int& iSMT, int &iFEM, int &iCH192, int &ASIC, 
			       int &ASICCh);

  //@}

private:

  static std::vector<std::string> fEmcSectorIdent;

};

#endif




