/************************************************************/
/*     Class: PHG3toG4PostDetConstruction                   */
/*                                                          */
/*     Desc: Constructs detector sensitve volumes           */
/*                                                          */
/*                                                          */
/*   Author: Matt Snowball (snowball.at.rcf.rhic.bnl.gov)   */
/*                                                          */
/************************************************************/

#ifndef PHG3toG4PostDetConstruction_h
#define PHG3toG4PostDetConstruction_h 1

#include <g4root/TG4RootDetectorConstruction.h>

#include <Geant4/G4Types.hh>                            // for G4double, G4bool

#include <string>
#include <vector>

class G4UserLimits;
class PHG3toG4BbcSD;
class PHG3toG4MuonArmSD;
class PHG3toG4MuonIDSD;
class PHG3toG4MuonRPCSD;
class PHG3toG4SvxSD;

class PHG3toG4PostDetConstruction : public TVirtualUserPostDetConstruction
{
private:
   
   PHG3toG4PostDetConstruction();
   static PHG3toG4PostDetConstruction *fgInstance; // Self pointer
   
   bool startsWith(std::string input,std::string stringToCheck);

   void setSvxSDList();
   void setMuonArmSDList();
   void setMuonIDSDList();
   void setMuonRPCSDList();
   void setBbcSDList();
   void setMagPDList();

   std::vector<std::string> _sensitiveVolumeListSvx;
   std::vector<std::string> _sensitiveVolumeListMuonArm;
   std::vector<std::string> _sensitiveVolumeListMuonID;
   std::vector<std::string> _sensitiveVolumeListMuonRPC;
   std::vector<std::string> _sensitiveVolumeListBbc;
   std::vector<std::string> _sensitiveVolumeListMag;

   G4bool _userLimit_MASTER;
   G4bool _userLimit_SVX;
   G4bool _userLimit_MUI;
   G4bool _userLimit_MUT;
   G4bool _userLimit_MUPC;
   G4bool _userLimit_BBC;
   G4bool _userLimit_MAG;

   G4double _maxStep_MASTER, _maxTof_MASTER, _minKinEnergy_MASTER, _minEnergyDep_MASTER;
   G4double _maxStep_SVX, _maxTof_SVX, _minKinEnergy_SVX, _minEnergyDep_SVX;
   G4double _maxStep_MUI, _maxTof_MUI, _minKinEnergy_MUI, _minEnergyDep_MUI;
   G4double _maxStep_MUT, _maxTof_MUT, _minKinEnergy_MUT, _minEnergyDep_MUT;
   G4double _maxStep_MUPC, _maxTof_MUPC, _minKinEnergy_MUPC, _minEnergyDep_MUPC;
   G4double _maxStep_BBC, _maxTof_BBC, _minKinEnergy_BBC, _minEnergyDep_BBC;
   G4double _maxStep_MAG, _maxTof_MAG, _minKinEnergy_MAG, _minEnergyDep_MAG;
   
   G4UserLimits *masterUserLimits;
   PHG3toG4BbcSD *bbcSD;
   G4UserLimits *bbcUserLimits;
   PHG3toG4SvxSD *svxSD;
   G4UserLimits *svxUserLimits;
   PHG3toG4MuonArmSD *mutSD;
   G4UserLimits *mutUserLimits;
   PHG3toG4MuonIDSD *muiSD;
   G4UserLimits *muiUserLimits;
   PHG3toG4MuonRPCSD *mupcSD;
   G4UserLimits *mupcUserLimits;
   //   PHG3toG4MuonMAGPD *magPD;
   G4UserLimits *magUserLimits;

public:
   virtual ~PHG3toG4PostDetConstruction();

   static PHG3toG4PostDetConstruction *GetInstance();

   virtual void          Initialize(TG4RootDetectorConstruction *dc);

   void SetMaxTof(std::string detector, G4double v);
   void SetMaxStep(std::string detector, G4double v);
   void SetMinKinEnergy(std::string detector, G4double e);
   void SetMinEnergyDep(std::string detector, G4double e);
  
};
#endif
   
   

