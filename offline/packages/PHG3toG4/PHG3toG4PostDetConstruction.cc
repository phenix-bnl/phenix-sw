#include "PHG3toG4PostDetConstruction.h"

#include "PHG3toG4BbcSD.h"              // for PHG3toG4BbcSD
#include "PHG3toG4MuonArmSD.h"          // for PHG3toG4MuonArmSD
#include "PHG3toG4MuonIDSD.h"           // for PHG3toG4MuonIDSD
#include "PHG3toG4MuonRPCSD.h"          // for PHG3toG4MuonRPCSD
#include "PHG3toG4SvxSD.h"              // for PHG3toG4SvxSD

#include <Geant4/G4SDManager.hh>
#include <Geant4/G4LogicalVolume.hh>
#include <Geant4/G4LogicalVolumeStore.hh>
#include <Geant4/G4UserLimits.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4ios.hh>
#include <Geant4/G4String.hh>                  // for G4String
#include <Geant4/G4Types.hh>                      // for G4double

#include <cstdlib>
#include <ostream>                      // for operator<<, basic_ostream
#include <stdexcept>

class TG4RootDetectorConstruction;

PHG3toG4PostDetConstruction *PHG3toG4PostDetConstruction::fgInstance = 0;

//______________________________________________________________________________
PHG3toG4PostDetConstruction::PHG3toG4PostDetConstruction()
{

  _userLimit_MASTER = false;
  _maxStep_MASTER = -99;
  _maxTof_MASTER = -99;
  _minKinEnergy_MASTER = -99;
  _minEnergyDep_MASTER = -99;

  _userLimit_SVX = false;
  _maxStep_SVX = -99;
  _maxTof_SVX = -99;
  _minKinEnergy_SVX = -99;
  _minEnergyDep_SVX = -99;

  _userLimit_MUI = false;
  _maxStep_MUI = -99;
  _maxTof_MUI = -99;
  _minKinEnergy_MUI = -99;
  _minEnergyDep_MUI = -99;

  _userLimit_MUT = false;
  _maxStep_MUT = -99;
  _maxTof_MUT = -99;
  _minKinEnergy_MUT = -99;
  _minEnergyDep_MUT = -99;

  _userLimit_MUPC = false;
  _maxStep_MUPC = -99;
  _maxTof_MUPC = -99;
  _minKinEnergy_MUPC = -99;
  _minEnergyDep_MUPC = -99;

  _userLimit_BBC = false;
  _maxStep_BBC = -99;
  _maxTof_BBC = -99;
  _minKinEnergy_BBC = -99;
  _minEnergyDep_BBC = -99;

  _userLimit_MAG = false;
  _maxStep_MAG = -99;
  _maxTof_MAG = -99;
  _minKinEnergy_MAG = -99;
  _minEnergyDep_MAG = -99;

  masterUserLimits = NULL;
  bbcSD = NULL;
  bbcUserLimits = NULL;
  svxSD = NULL;
  svxUserLimits = NULL;
  mutSD = NULL;
  mutUserLimits = NULL;
  muiSD = NULL;
  muiUserLimits = NULL;
  mupcSD = NULL;
  mupcUserLimits = NULL;
  //  magPD = NULL;
  magUserLimits = NULL;
}   

//______________________________________________________________________________
PHG3toG4PostDetConstruction::~PHG3toG4PostDetConstruction()
{
  if (masterUserLimits) delete masterUserLimits;
  delete bbcSD;
  if (bbcUserLimits) delete bbcUserLimits;
  delete svxSD;
  if (svxUserLimits) delete svxUserLimits;
  delete mutSD;
  if (mutUserLimits) delete mutUserLimits;
  delete muiSD;
  if (muiUserLimits) delete muiUserLimits;
  delete mupcSD;
  if (mupcUserLimits) delete mupcUserLimits;
  //  delete magPD;
  if (magUserLimits) delete magUserLimits;
}
   
//______________________________________________________________________________
PHG3toG4PostDetConstruction *PHG3toG4PostDetConstruction::GetInstance()
{
  // Returns self pointer.
   if (fgInstance) return fgInstance;
   fgInstance = new PHG3toG4PostDetConstruction();
   return fgInstance;
}
   
//______________________________________________________________________________
void PHG3toG4PostDetConstruction::Initialize(TG4RootDetectorConstruction *dc)
{
  //Set Translations from TGeo/G3 Names
  setSvxSDList();
  setMuonArmSDList();
  setMuonIDSDList();
  setMuonRPCSDList();
  setBbcSDList();
  setMagPDList();


  G4SDManager *pSDManager = G4SDManager::GetSDMpointer();


  const G4LogicalVolumeStore* store = G4LogicalVolumeStore::GetInstance();
  if( store == 0 )
    {
      throw std::runtime_error("G4LogicalVolumeStore* is NULL!");
      return;
    }

  if(_userLimit_MASTER)
    {
      G4double masterMaxStep = _maxStep_MASTER;
      G4double masterMaxTof = _maxTof_MASTER;
      G4double masterMinKinEnergy = _minKinEnergy_MASTER;
      masterUserLimits = new G4UserLimits();
      if(_maxStep_MASTER>0){ masterUserLimits->SetMaxAllowedStep(masterMaxStep*mm); G4cout << "MASTER max step set to " <<_maxStep_MASTER <<"mm" << G4endl;}
      if(_maxTof_MASTER>0){  masterUserLimits->SetUserMaxTime(masterMaxTof*ns); G4cout << "MASTER max tof set to " <<_maxTof_MASTER <<"ns"<< G4endl;}
      if(_minKinEnergy_MASTER>0){  masterUserLimits->SetUserMinEkine(masterMinKinEnergy*MeV); G4cout << "MASTER min kin energy set to " <<_minKinEnergy_MASTER <<"MeV"<< G4endl;}
    }
  
  //BBC
  bbcSD = new PHG3toG4BbcSD("MyBbcSDs");
  if(_minEnergyDep_BBC > 0){ bbcSD->SetMinEnergyDeposit(_minEnergyDep_BBC); G4cout << "BBC min energy deposit set to " <<_minEnergyDep_BBC <<"MeV" << G4endl;}
  if(_userLimit_BBC)
    {
      G4double bbcMaxStep = _maxStep_BBC;
      G4double bbcMaxTof = _maxTof_BBC;
      G4double bbcMinKinEnergy = _minKinEnergy_BBC;
      bbcUserLimits = new G4UserLimits();
      if(_maxStep_BBC>0){ bbcUserLimits->SetMaxAllowedStep(bbcMaxStep*mm); G4cout << "BBC max step set to " <<_maxStep_BBC <<"mm" << G4endl;}
      if(_maxTof_BBC>0){  bbcUserLimits->SetUserMaxTime(bbcMaxTof*ns); G4cout << "BBC max tof set to " <<_maxTof_BBC <<"ns"<< G4endl;}
      if(_minKinEnergy_BBC>0){  bbcUserLimits->SetUserMinEkine(bbcMinKinEnergy*MeV); G4cout << "BBC min kin energy set to " <<_minKinEnergy_BBC <<"MeV"<< G4endl;}
    }
  pSDManager->AddNewDetector(bbcSD);

  //SVX
  svxSD = new PHG3toG4SvxSD("MySvxSDs");
  if(_minEnergyDep_SVX > 0){ svxSD->SetMinEnergyDeposit(_minEnergyDep_SVX); G4cout << "SVX min energy deposit set to " <<_minEnergyDep_SVX <<"MeV" << G4endl;}
  svxUserLimits = NULL;
  if(_userLimit_SVX)
    {
      G4double svxMaxStep = _maxStep_SVX;
      G4double svxMaxTof = _maxTof_SVX;
      G4double svxMinKinEnergy = _minKinEnergy_SVX;
      svxUserLimits = new G4UserLimits();
      if(_maxStep_SVX>0){ svxUserLimits->SetMaxAllowedStep(svxMaxStep*mm); G4cout << "SVX max step set to " <<_maxStep_SVX<<"mm"<< G4endl;}
      if(_maxTof_SVX>0){  svxUserLimits->SetUserMaxTime(svxMaxTof*ns); G4cout << "SVX max tof set to " <<_maxTof_SVX<<"ns"<< G4endl;}
      if(_minKinEnergy_SVX>0){  svxUserLimits->SetUserMinEkine(svxMinKinEnergy*MeV); G4cout << "SVX min kin energy set to " <<_minKinEnergy_SVX<<"MeV"<< G4endl;}
    }
  pSDManager->AddNewDetector(svxSD);

  //MUT
  mutSD = new PHG3toG4MuonArmSD("MyMuonArmSDs");
  if(_minEnergyDep_MUT > 0){ mutSD->SetMinEnergyDeposit(_minEnergyDep_MUT); G4cout << "MUT min energy deposit set to " <<_minEnergyDep_MUT <<"MeV" << G4endl;}
  mutUserLimits = NULL;
  if(_userLimit_MUT)
    {
      G4double mutMaxStep = _maxStep_MUT;
      G4double mutMaxTof = _maxTof_MUT;
      G4double mutMinKinEnergy = _minKinEnergy_MUT;
      mutUserLimits = new G4UserLimits();
      if(_maxStep_MUT>0){ mutUserLimits->SetMaxAllowedStep(mutMaxStep*mm); G4cout << "MUT max step set to " <<_maxStep_MUT<<"mm"<< G4endl;}
      if(_maxTof_MUT>0){  mutUserLimits->SetUserMaxTime(mutMaxTof*ns); G4cout << "MUT max tof set to " <<_maxTof_MUT<<"ns"<< G4endl;}
      if(_minKinEnergy_MUT>0){  mutUserLimits->SetUserMinEkine(mutMinKinEnergy*MeV); G4cout << "MUT min kin energy set to " <<_minKinEnergy_MUT<<"MeV"<< G4endl;}
    }
  pSDManager->AddNewDetector(mutSD);

  //MUI
  muiSD = new PHG3toG4MuonIDSD("MyMuonIDSDs");
  if(_minEnergyDep_MUI > 0){ muiSD->SetMinEnergyDeposit(_minEnergyDep_MUI); G4cout << "MUI min energy deposit set to " <<_minEnergyDep_MUI <<"MeV" << G4endl;}
  muiUserLimits = NULL;
  if(_userLimit_MUI)
    {
      G4double muiMaxStep = _maxStep_MUI;
      G4double muiMaxTof = _maxTof_MUI;
      G4double muiMinKinEnergy = _minKinEnergy_MUI;
      muiUserLimits = new G4UserLimits();
      if(_maxStep_MUI>0){ muiUserLimits->SetMaxAllowedStep(muiMaxStep*mm); G4cout << "MUI max step set to " <<_maxStep_MUI<<"mm"<< G4endl;}
      if(_maxTof_MUI>0){  muiUserLimits->SetUserMaxTime(muiMaxTof*ns); G4cout << "MUI max tof set to " <<_maxTof_MUI<<"ns"<< G4endl;}
      if(_minKinEnergy_MUI>0){  muiUserLimits->SetUserMinEkine(muiMinKinEnergy*MeV);  G4cout << "MUI min kin energy set to " <<_minKinEnergy_MUI<<"MeV"<< G4endl;}
    }
  pSDManager->AddNewDetector(muiSD);

  //MUPC
  mupcSD = new PHG3toG4MuonRPCSD("MyMuonRPCSDs");
  if(_minEnergyDep_MUPC > 0){ mupcSD->SetMinEnergyDeposit(_minEnergyDep_MUPC); G4cout << "MUPC min energy deposit set to " <<_minEnergyDep_MUPC <<"MeV" << G4endl;}
  mupcUserLimits = NULL;
  if(_userLimit_MUPC)
    {
      G4double mupcMaxStep = _maxStep_MUPC;
      G4double mupcMaxTof = _maxTof_MUPC;
      G4double mupcMinKinEnergy = _minKinEnergy_MUPC;
      mupcUserLimits = new G4UserLimits();
      if(_maxStep_MUPC>0){ mupcUserLimits->SetMaxAllowedStep(mupcMaxStep*mm); G4cout << "MUPC max step set to " <<_maxStep_MUPC<<"mm"<< G4endl;}
      if(_maxTof_MUPC>0){  mupcUserLimits->SetUserMaxTime(mupcMaxTof*ns); G4cout << "MUPC max tof set to " <<_maxTof_MUPC<<"ns"<< G4endl;}
      if(_minKinEnergy_MUPC>0){  mupcUserLimits->SetUserMinEkine(mupcMinKinEnergy*MeV);  G4cout << "MUPC min kin energy set to " <<_minKinEnergy_MUPC<<"MeV"<< G4endl;}
    }
  pSDManager->AddNewDetector(mupcSD);

  ////////////
  //MAG
  //  magPD = new PHG3toG4MuonMAGPD("MyMuonMAGPDs");
  //  if(_minEnergyDep_MAG > 0){ magPD->SetMinEnergyDeposit(_minEnergyDep_MAG); G4cout << "MAG min energy deposit set to " <<_minEnergyDep_MAG <<"MeV" << G4endl;}
  magUserLimits = NULL;
  if(_userLimit_MAG)
    {
      G4double magMaxStep = _maxStep_MAG;
      G4double magMaxTof = _maxTof_MAG;
      G4double magMinKinEnergy = _minKinEnergy_MAG;
      magUserLimits = new G4UserLimits();
      if(_maxStep_MAG>0){ magUserLimits->SetMaxAllowedStep(magMaxStep*mm); G4cout << "MAG max step set to " <<_maxStep_MAG<<"mm"<< G4endl;}
      if(_maxTof_MAG>0){  magUserLimits->SetUserMaxTime(magMaxTof*ns); G4cout << "MAG max tof set to " <<_maxTof_MAG<<"ns"<< G4endl;}
      if(_minKinEnergy_MAG>0){  magUserLimits->SetUserMinEkine(magMinKinEnergy*MeV);  G4cout << "MAG min kin energy set to " <<_minKinEnergy_MAG<<"MeV"<< G4endl;}
    }
  //  pSDManager->AddNewDetector(magPD);
  //////////

  G4cout << ">>>> Setting sensitive detectors:" << G4endl;  
  for( G4LogicalVolumeStore::const_iterator ivolume = store->begin(); store->end() != ivolume; ++ivolume )
    {
      std::string G4VolName;
      G4LogicalVolume* vol = *ivolume;
      bool found = false;
      if( vol != 0 )
        {
	  G4VolName = vol->GetName();
	  //G4cout << G4VolName << "  " << vol->GetNoDaughters() << G4endl;
	  //SVX Volumes
	  
	  if(startsWith(G4VolName,"HALL"))
	    {
	      vol->SetUserLimits(masterUserLimits);
	    }

	  if(startsWith(G4VolName,"SI"))
	    {
	      for(unsigned int i = 0; i < _sensitiveVolumeListSvx.size(); i++)
		{
		  if(G4VolName == _sensitiveVolumeListSvx[i])
		    {
		      G4cout << "    " << G4VolName << " set as SVX sensitive detector" << G4endl;
		      vol->SetSensitiveDetector(svxSD); 
		      if(_userLimit_SVX) vol->SetUserLimits(svxUserLimits);
		      found = true; break;
		    }
		}
	    }
	  if(found) continue;

	  //Muon Tracker Volumes
	  if((startsWith(G4VolName,"MU") || startsWith(G4VolName,"MT")) && ! startsWith(G4VolName,"MUG") )
	    {
	      for(unsigned int i = 0; i < _sensitiveVolumeListMuonArm.size(); i++)
		{
		  if(G4VolName == _sensitiveVolumeListMuonArm[i])
		    {
		      G4cout << "    " << G4VolName << " set as MuonArm sensitive detector" << G4endl;
		      vol->SetSensitiveDetector(mutSD); 
                      if(_userLimit_MUT) vol->SetUserLimits(mutUserLimits);
		      found = true; break;
		    }
		}
	    }
	  if(found) continue;

	  //Muon ID Volumes
	  if(startsWith(G4VolName,"MUG"))
	    {
	      for(unsigned int i = 0; i < _sensitiveVolumeListMuonID.size(); i++)
		{
		  if(G4VolName == _sensitiveVolumeListMuonID[i])
		    {
		      G4cout << "    " << G4VolName << " set as MuonID sensitive detector" << G4endl;
		      vol->SetSensitiveDetector(muiSD);
                      if(_userLimit_MUI) vol->SetUserLimits(muiUserLimits);
		      found = true; break;
		    }
		}
	    }
	  if(found) continue;

	  //Muon RPC Volumes
	  if(startsWith(G4VolName,"UG"))
	    {
	      for(unsigned int i = 0; i < _sensitiveVolumeListMuonRPC.size(); i++)
		{
		  if(G4VolName == _sensitiveVolumeListMuonRPC[i])
		    {
		      G4cout << "    " << G4VolName << " set as MuonRPC sensitive detector" << G4endl;
		      vol->SetSensitiveDetector(mupcSD);
                      if(_userLimit_MUPC) vol->SetUserLimits(mupcUserLimits);
		      found = true; break;
		    }
		}
	    }
	  if(found) continue;

	  //Bbc Volumes
	  if(startsWith(G4VolName,"BBC"))
	    {
	      for(unsigned int i = 0; i < _sensitiveVolumeListBbc.size(); i++)
		{
		  if(G4VolName == _sensitiveVolumeListBbc[i])
		    {
		      G4cout << "    " << G4VolName << " set as Bbc sensitive detector" << G4endl;
		      vol->SetSensitiveDetector(bbcSD);
                      if(_userLimit_BBC) vol->SetUserLimits(bbcUserLimits);
		      found = true; break;
		    }
		}
	    }
	  if(found) continue;

	  //Magnet Volumes
	  if(startsWith(G4VolName,"MAG"))
	    {
	      for(unsigned int i = 0; i < _sensitiveVolumeListMag.size(); i++)
		{
		  if(G4VolName == _sensitiveVolumeListMag[i])
		    {
		       if(_userLimit_MAG)
			 {
			   vol->SetUserLimits(magUserLimits);
			   G4cout << "    " << G4VolName << " set Limits for Magnet Absorber" << G4endl;
			 }
		      found = true; break;
		    }
		}
	    }
	    if(found) continue; 

        }
      
    }  

}


bool PHG3toG4PostDetConstruction::startsWith(std::string input, std::string stringToCheck)
{
  return (input.substr(0, stringToCheck.size()) == stringToCheck); 
}



void PHG3toG4PostDetConstruction::setSvxSDList()
{
  //Sensitive layers in the Silicon
  
  /*   5 7 9 11          13 15 17 19  */
  /*   | | |   -----4-----   | | |    */
  /*   | | |  |-----3-----|  | | |    */
  /*   | | |  |-----2-----|  | | |    */
  /*   | | |  |-----1-----|  | | |    */
  /*                                  */
  /*   | | |  |-----1-----|  | | |    */
  /*   | | |  |-----2-----|  | | |    */
  /*   | | |  |-----3-----|  | | |    */
  /*   | | |   -----4-----   | | |    */
  /*   6 8 10 12         14 16 18 20  */
 
  _sensitiveVolumeListSvx.push_back("SISI");
  _sensitiveVolumeListSvx.push_back("SISN");
  
}

void PHG3toG4PostDetConstruction::setMuonArmSDList()
{
  //see http://p25ext.lanl.gov/~hubert/phenix/mutr/g4/

  //Station 1N 
  _sensitiveVolumeListMuonArm.push_back("MU11");
  _sensitiveVolumeListMuonArm.push_back("MU12");
  _sensitiveVolumeListMuonArm.push_back("MU13");
  _sensitiveVolumeListMuonArm.push_back("MT11");
  _sensitiveVolumeListMuonArm.push_back("MT12");
  _sensitiveVolumeListMuonArm.push_back("MT13");

  //Station 2N
  _sensitiveVolumeListMuonArm.push_back("MU21");
  _sensitiveVolumeListMuonArm.push_back("MU22");
  _sensitiveVolumeListMuonArm.push_back("MU23");
  _sensitiveVolumeListMuonArm.push_back("MT21");
  _sensitiveVolumeListMuonArm.push_back("MT22");
  _sensitiveVolumeListMuonArm.push_back("MT23");

  //Station 3N
  _sensitiveVolumeListMuonArm.push_back("MT31");
  _sensitiveVolumeListMuonArm.push_back("MT32");


  //Station 1S
  _sensitiveVolumeListMuonArm.push_back("MU41");
  _sensitiveVolumeListMuonArm.push_back("MU42");
  _sensitiveVolumeListMuonArm.push_back("MU43");
  _sensitiveVolumeListMuonArm.push_back("MT41");
  _sensitiveVolumeListMuonArm.push_back("MT42");
  _sensitiveVolumeListMuonArm.push_back("MT43");

  //Station 2S
  _sensitiveVolumeListMuonArm.push_back("MU51");
  _sensitiveVolumeListMuonArm.push_back("MU52");
  _sensitiveVolumeListMuonArm.push_back("MU53");
  _sensitiveVolumeListMuonArm.push_back("MT51");
  _sensitiveVolumeListMuonArm.push_back("MT52");
  _sensitiveVolumeListMuonArm.push_back("MT53");

  //Station 3S
  _sensitiveVolumeListMuonArm.push_back("MT61");
  _sensitiveVolumeListMuonArm.push_back("MT62");


}

void PHG3toG4PostDetConstruction::setMuonIDSDList()
{
  _sensitiveVolumeListMuonID.push_back("MUGS");
}

void PHG3toG4PostDetConstruction::setMuonRPCSDList()
{
  _sensitiveVolumeListMuonRPC.push_back("UG1N");
  _sensitiveVolumeListMuonRPC.push_back("UG3N");
  _sensitiveVolumeListMuonRPC.push_back("UG1S");
  _sensitiveVolumeListMuonRPC.push_back("UG3S");
}

void PHG3toG4PostDetConstruction::setBbcSDList()
{
  _sensitiveVolumeListBbc.push_back("BBCQ");
}

void PHG3toG4PostDetConstruction::setMagPDList()
{
  _sensitiveVolumeListMag.push_back("MAGD");
  _sensitiveVolumeListMag.push_back("MAGU");
}

void PHG3toG4PostDetConstruction::SetMaxTof(std::string detector, G4double v)
{
  if(detector == "HALL"){ _maxTof_MASTER = v; _userLimit_MASTER = true;}
  if(detector == "SVX"){ _maxTof_SVX = v; _userLimit_SVX = true;}
  if(detector == "MUI"){ _maxTof_MUI = v; _userLimit_MUI = true;}
  if(detector == "MUT"){ _maxTof_MUT = v; _userLimit_MUT = true;}
  if(detector == "MUPC"){ _maxTof_MUPC = v; _userLimit_MUPC = true;}
  if(detector == "BBC"){ _maxTof_BBC = v; _userLimit_BBC = true;}
  if(detector == "MAG"){ _maxTof_MAG = v; _userLimit_MAG = true;}
}

void PHG3toG4PostDetConstruction::SetMaxStep(std::string detector, G4double v)
{
  if(detector == "HALL"){ _maxStep_MASTER = v; _userLimit_MASTER = true;}
  if(detector == "SVX"){ _maxStep_SVX = v; _userLimit_SVX = true;}
  if(detector == "MUI"){ _maxStep_MUI = v; _userLimit_MUI = true;}
  if(detector == "MUT"){ _maxStep_MUT = v; _userLimit_MUT = true;}
  if(detector == "MUPC"){ _maxStep_MUPC = v; _userLimit_MUPC = true;}
  if(detector == "BBC"){ _maxStep_BBC = v; _userLimit_BBC = true;}
  if(detector == "MAG"){ _maxStep_MAG = v; _userLimit_MAG = true;}
}

void PHG3toG4PostDetConstruction::SetMinKinEnergy(std::string detector, G4double e)
{
  if(detector == "HALL"){ _minKinEnergy_MASTER = e; _userLimit_MASTER = true;}
  if(detector == "SVX"){ _minKinEnergy_SVX = e; _userLimit_SVX = true;}
  if(detector == "MUI"){ _minKinEnergy_MUI = e; _userLimit_MUI = true;}
  if(detector == "MUT"){ _minKinEnergy_MUT = e; _userLimit_MUT = true;}
  if(detector == "MUPC"){ _minKinEnergy_MUPC = e; _userLimit_MUPC = true;}
  if(detector == "BBC"){ _minKinEnergy_BBC = e; _userLimit_BBC = true;}
  if(detector == "MAG"){ _minKinEnergy_MAG = e; _userLimit_MAG = true;}
}

void PHG3toG4PostDetConstruction::SetMinEnergyDep(std::string detector, G4double e)
{
  if(detector == "HALL"){ _minEnergyDep_MASTER = e;}
  if(detector == "SVX"){ _minEnergyDep_SVX = e;}
  if(detector == "MUI"){ _minEnergyDep_MUI = e;}
  if(detector == "MUT"){ _minEnergyDep_MUT = e;}
  if(detector == "MUPC"){ _minEnergyDep_MUPC = e;}
  if(detector == "BBC"){ _minEnergyDep_BBC = e;}
  if(detector == "MAG"){ _minEnergyDep_MAG = e;}
}







