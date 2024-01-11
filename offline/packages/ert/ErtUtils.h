/*!
  \author Jamie Nagle
  \author Cesar Luiz da Silva
*/

#ifndef ErtUtils_h
#define ErtUtils_h
 
#include <TMath.h>

  //-----------------------------------------------------------
  //! ERT Utilities - commonly used functions and accessors

class ErtUtils {
  
  enum { W, E };
  enum { West, East };

 public:
  
  //====================================================================================================
  // Below are a set of functions related to the ERT Trigger Lookup Table
  //====================================================================================================

  //! is_included_in_ERT_LUT
  /*! Function returns true or false if combination of EMC SM and RICH TriggerTile combination
      are in the ERT Look-Up Table (LUT) used in runnumber for a particle with momentum mom.
      TrigTile can be obtained by using get_EMC_TrigTile_FromsmID and get_RICH_TrigTile_FromsmID.
  */
  static bool is_included_in_ERT_LUT(int runnumber, int arm, int EMC_TrigTile, int RICH_TrigTile, float mom = 0.0);

  //! get_ERT_RICH_LUT
  /*! Function returns a list of RICH TriggerTiles associated with EMC SuperModules
      given an electron with momentum mom.
      Returned aLUT[0] contains the number of associations.
      Returned aLUT[1...] is the association list.
  */
  static void get_ERT_RICH_LUT(int runnumber, int arm, int EMC_TrigTile, int *aLUT, float mom = 0.0);

  //! get_lut_bankid
  /*! Function returns the bankID of the ERT_Electron LUT stored in database.*/
  static int get_lut_bankid(int runnumber);

  //====================================================================================================
  // Below are a set of functions related to converting from Detector Index Values to ERT Index Values
  //====================================================================================================

  //! get_EMC_TrigTile_FromsmID
  /*! Function returns the ERTmap index for the EMC in the trigger when passing in the arm, sect, SM
   */
  static int get_EMC_TrigTile_FromsmID(int arm, int sect, int sm);

  //! get_RICH_TrigTile_FromsmID
  /*! Function returns the ERTmap index for the RICH in the trigger when passing in the arm, sect, SM
   */
  static int get_RICH_TrigTile_FromsmID(int arm, int sect, int sm);

  static int  get_RICH_TrigTile_FromPMT(int pmt);

  static int  get_EMC_TrigTile_FromModule(int moduleid);
  static void get_EMC_Module_FromTrigTile(int emctt,int *moduleids);

  static void get_EMC_smID_FromTrigTile(int &arm, int &sector,int &smID, int emctt);

  static void get_RICH_smID_FromTrigTile(int &arm, int &sector, int &smID, int crktt);

  static void get_EMC_ArmSectSide_FromTrigTile(int &arm,int &sect,int &side,int emctt);
  static void get_RICH_ArmSectSide_FromTrigTile(int &arm,int &sect,int &side,int crktt);

  static void get_EMC_Module_Neighbors(int moduleID,int *neighbour);
  static int  get_RICH_TrigTile_Neighbors(int crktt,int *crkneighbours);


  //====================================================================================================
  // Below are a set of functions related to converting between different Detector Index Values
  //====================================================================================================

  //! get_EMC_Towerkey_FromIndex
  /*! Function passes in EMC arm, sector, supermodule and returns EMC towerkey index
   */
  static int get_EMC_Towerkey_FromIndex(int arm, int sector, int iy, int iz);

  //! get_EMC_smID_FromTowerkey
  /*! Function passes in EMC towerkey index and returns EMC arm, sector, supermodule 
   */
  static bool get_EMC_smID_FromTowerkey(int towerkey, int &arm, int &sector, int &smID);

  static int  get_EMC_Module_FromTowerkey(int towerkey);
  //! get_EMC_smID_FromTowerID
  /*! Function passes in EMC towerID provided from EmcIndexer and returns EMC arm, sector, supermodule 
   */
  static int get_EMC_Module_FromTowerID(int towerid);

  //! get_RICH_smID_FromPMT
  /*! Function passes in RICH PMT number and returns RICH arm, sector, and supermodule
   */
  static void get_RICH_smID_FromPMT(int pmt, int &armRICH, int &sectRICH,int &smRICH);

  //! get_RICH_pmt_FromPhiZ
  /*! Returns PMT RICH index from cros_phi and cross_z CNT variables
   */
  static int get_RICH_pmt_fromcrossing(int emc_arm, float cross_phi, float cross_z);

  //====================================================================================================
  // Below are a set of functions related to various ERT masks and status variables
  //====================================================================================================

 private:

};

#endif
