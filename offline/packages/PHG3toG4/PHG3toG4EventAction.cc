#include "PHG3toG4EventAction.h"

#include "PHG3toG4KinHit.h"                                 // for PHG3toG4K...
#include "PHG3toG4PriHit.h"                                 // for PHG3toG4P...

//PHG3toG4
#include "PHG3toG4RootManager.h"
#include "PHG3toG4SvxPara.h"
#include "PHG3toG4BbcPara.h"
#include "PHG3toG4MuonIDPara.h"
#include "PHG3toG4MuonArmPara.h"


//G4
#include <Geant4/G4Event.hh>
#include <Geant4/G4SDManager.hh>
#include <Geant4/G4HCofThisEvent.hh>
#include <Geant4/G4UnitsTable.hh>
#include <Geant4/G4SystemOfUnits.hh>
#include <Geant4/G4ExceptionSeverity.hh>                           // for FatalExce...
#include <Geant4/G4PrimaryVertex.hh>                               // for G4Primary...
#include <Geant4/G4String.hh>                                      // for G4String
#include <Geant4/G4THitsCollection.hh>                             // for G4THitsCo...
#include <Geant4/G4THitsMap.hh>                                    // for G4THitsMap
#include <Geant4/G4ThreeVector.hh>                                 // for G4ThreeVe...
#include <Geant4/G4ios.hh>                                         // for G4cout
#include <Geant4/G4Types.hh>                                       // for G4double
#include <Geant4/G4UserEventAction.hh>                             // for G4UserEve...
#include <Geant4/globals.hh>                                       // for G4Exception

//Encode to pisa
#include <PISAEvent.h>


#include <boost/date_time.hpp>
#include <boost/math/constants/constants.hpp>

#include <algorithm>                                        // for copy
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <iterator>                                         // for ostreambu...
#include <utility>
#include <iostream>
#include <locale>                                           // for locale
#include <map>                                              // for map, map<...
#include <memory>                                           // for allocator...
#include <string>
#include <sstream>

const double PI = boost::math::constants::pi<double>();

using namespace std;

PHG3toG4EventAction::PHG3toG4EventAction():
  G4UserEventAction(),
  _verbosity(0)
{
  eventCounter = 0;
  _theRootMgr = PHG3toG4RootManager::GetInstance();

  //Date/time for pisa event header --- is it really necessary?
  boost::gregorian::date d = boost::gregorian::day_clock::local_day();
  boost::gregorian::date_facet* facet = new boost::gregorian::date_facet("%y%m%d"); 
  std::ostringstream os;
  os.imbue(std::locale(os.getloc(),facet));
  os << d;
  theDate = atoi(os.str().c_str());

  //Para matching
  svxPara = new PHG3toG4SvxPara();
  bbcPara = new PHG3toG4BbcPara();
  muiPara = new PHG3toG4MuonIDPara();
  mutPara = new PHG3toG4MuonArmPara();
  for(int i = 0; i < 1000; i++)
    {
      if(i < 500)
	{
	  iData_svx[i] = -1;
	  fData_svx[i] = -1;
	  
	  iData_bbc[i] = -1;
	  fData_bbc[i] = -1;
	  
	  iData_mui[i] = -1;
	  fData_mui[i] = -1;

	}
      //more info to store
      iData_mut[i] = -1;
      fData_mut[i] = -1;
    }

  svxPara->InitArrays(iData_svx,fData_svx);
  bbcPara->InitArrays(iData_bbc,fData_bbc);
  muiPara->InitArrays(iData_mui,fData_mui);
  mutPara->InitArrays(iData_mut,fData_mut);


}


PHG3toG4EventAction::~PHG3toG4EventAction()
{
  delete svxPara;
  delete bbcPara;
  delete muiPara;
  delete mutPara;
}


G4THitsMap<G4double>* PHG3toG4EventAction::GetHitsCollection(G4int hcID, const G4Event* event) const
{
  G4THitsMap<G4double>* hitsCollection  = static_cast<G4THitsMap<G4double>*>(event->GetHCofThisEvent()->GetHC(hcID));
  
  if ( ! hitsCollection ) 
    {
      G4ExceptionDescription msg;
      msg << "Cannot access hitsCollection ID " << hcID; 
      G4Exception("PHG3toG4EventAction::GetHitsCollection()", "MyCode0003", FatalException, msg);
    }         
  return hitsCollection;
}    


G4double PHG3toG4EventAction::GetSum(G4THitsMap<G4double>* hitsMap) const
{
  G4double sumValue = 0;
  std::map<G4int, G4double*>::iterator it;
  for ( it = hitsMap->GetMap()->begin(); it != hitsMap->GetMap()->end(); it++) {
    sumValue += *(it->second);
  }
  return sumValue;  
}  

void PHG3toG4EventAction::BeginOfEventAction(const G4Event* event)
{

  if(_verbosity > 2) G4cout << "PHG3toG4EventAction::BeginOfEventAction - begin" << G4endl;

  _theRootMgr->ClearEvent();

  _theRootMgr->SetAbortEvent(false);

  SDman = G4SDManager::GetSDMpointer();
  //Get hit collection IDs
  svxHCID = SDman->GetCollectionID("svxHitCollection");
  mutHCID = SDman->GetCollectionID("muonArmHitCollection");
  muiHCID = SDman->GetCollectionID("muonIDHitCollection");
  mupcHCID = SDman->GetCollectionID("muonRPCHitCollection");
  bbcHCID = SDman->GetCollectionID("bbcHitCollection");
  
  eventCounter++;

  //PISAEvent header
  //initalize all to 0, then set what is "used"
  if(eventCounter == 1) for(int i = 0; i < 100; i++) Data.push_back(0);
  Data[0] = 1;//pisaInputRunNumber
  Data[1] = 1;//pisaOutputRunNumber
  Data[2] = 1;//pisaProjectNumber
  Data[3] = 4.10;//versionNumber
  Data[4] = 1;//GEANT run number
  Data[5] = eventCounter;// evt number
  Data[6] = event->GetEventID();// Geant evt number
  Data[7] = 1;//subevt number
  Data[8] = 1;//number of particles
  Data[20] = event->GetPrimaryVertex()->GetX0()*1000;//vtx x
  Data[21] = event->GetPrimaryVertex()->GetY0()*1000;//vtx y
  Data[22] = event->GetPrimaryVertex()->GetZ0()*1000;//vtx z
  Data[23] = theDate;
  boost::posix_time::ptime t = boost::posix_time::second_clock::local_time();
  boost::posix_time::time_facet* tfacet = new boost::posix_time::time_facet("%H%M");
  std::ostringstream ts;
  ts.imbue(std::locale(ts.getloc(), tfacet));
  ts << t;
  int theTime = atoi(ts.str().c_str());
  Data[24] = theTime;
  Data[28] = float(time(0));//time in seconds
  Data[29] = 1;//event code
  Data[89] = 8;//mapFileScale
  Data[90] = 1;
  Data[91] = -1;
  Data[94] = 55.694648;

  /*  
  //fill vectors for event header - e_put_dst.f
  Data.push_back(1);//pisaInputRunNumber
  Data.push_back(1);//pisaOutputRunNumber
  Data.push_back(1);//pisaProjectNumber
  Data.push_back(4.10);//versionNumber
  Data.push_back(1);//GEANT run number
  Data.push_back(eventCounter);//true event number
  Data.push_back(event->GetEventID());//GEANT event number
  Data.push_back(1);//sub-event in event number
  Data.push_back(1);//total number of subevents
  Data.push_back(0);//total number of particles (needs to be initialized)
  Data.push_back(0);//particles per subevent (needs to be intialized)
  Data.push_back(0);//bimevt*1000.
  Data.push_back(0);//atarg
  Data.push_back(0);//ztarg
  Data.push_back(0);//aproj
  Data.push_back(0);//zproj
  Data.push_back(0);//sqrt_s*1000
  Data.push_back(0);//bmin*1000.
  Data.push_back(0);//bmax*1000.
  Data.push_back(0);//t0start*1.e+15 femptosec
  Data.push_back(event->GetPrimaryVertex().GetX0());//event vertex x*1000
  Data.push_back(event->GetPrimaryVertex().GetY0());//event vertex y*1000
  Data.push_back(event->GetPrimaryVertex().GetY0());//event vertex z*1000
  Data.push_back(theDate);//date as YYMMDD (Y2K problem)
  
  boost::posix_time::ptime t = boost::posix_time::second_clock::local_time(); 
  boost::posix_time::time_facet* tfacet = new boost::posix_time::time_facet("%H%M");
  std::ostringstream ts;
  ts.imbue(tfacet);
  ts << t;
  int theTime = atoi(ts.str().c_str());
  time_t seconds = time(0);

  Data.push_back(theTime);//time as HHMM
  Data.push_back(0);//first random number seed 
  Data.push_back(0);//second random number seed
  Data.push_back(0);//starting GEANT random number sequence value
  Data.push_back(float(seconds));//itime_evt
  Data.push_back(0);//generator event code PYTHIA, HIJING etc
  Data.push_back(0);//number of binary collisions, introduced for HIJING1.37
  Data.push_back(0);//nrvphi_evt or nrvjpsi_evt or neutral_evt or pyth_proc_id
  Data.push_back(0);//nrvacc_evt or pyth_bjork(1)
  Data.push_back(0);//pyth_bjork(2)
  Data.push_back(0);//pyth_partstu(1)
  Data.push_back(0);//pyth_partstu(2)
  Data.push_back(0);//pyth_partstu(3)
  Data.push_back(0);//pyth_qsqr
  Data.push_back(0);//pyth_ptrans
  Data.push_back(0);//5 intermediate particles + px,py,pz
  //20
  Data.push_back(0);//71 - 83 retraction postions
  Data.push_back(0);//90 - Mag Field
  Data.push_back(0);//91 MAGF scale factor 
  Data.push_back(0);//92 MAGF R cutoff in gustep 
  Data.push_back(0);//93 MAGF Z cutoff in gustep
  Data.push_back(0);//94 MAGF momentum cutoff in gustep 
  Data.push_back(0);//95 -reactionPlaneAngle
  Data.push_back(0);//100 - 1 for zdc used
  */
  _theRootMgr->SetEventHeader(Data);


  if(_verbosity > 2) G4cout << "PHG3toG4EventAction::BeginOfEventAction - end" << G4endl;

}


void PHG3toG4EventAction::EndOfEventAction(const G4Event* event)
{  

  if(_verbosity > 2) G4cout << "PHG3toG4EventAction::EndOfEventAction - begin" << G4endl;


	G4HCofThisEvent * HCE = event->GetHCofThisEvent();

	if(HCE)
	{
		SvxHC = (PHG3toG4SvxHitsCollection*)(HCE->GetHC(svxHCID));
		MutHC = (PHG3toG4MuonArmHitsCollection*)(HCE->GetHC(mutHCID));
		MuiHC = (PHG3toG4MuonIDHitsCollection*)(HCE->GetHC(muiHCID));
		MupcHC = (PHG3toG4MuonRPCHitsCollection*)(HCE->GetHC(mupcHCID));
		BbcHC = (PHG3toG4BbcHitsCollection*)(HCE->GetHC(bbcHCID));
	}

	if(_verbosity > 5) 
	{
		if(SvxHC)
		{
			int n_hit = SvxHC->entries();
			cout << "     " << n_hit << " hits are stored in PHG3toG4SvxHitsCollection." << endl;
			G4double totE = 0;
			std::vector<std::pair<G4String,int> > theVolumes;
			for(int i=0;i<n_hit;i++)
			{
				(*SvxHC)[i]->Print();
				totE += (*SvxHC)[i]->GetdE(); 

				bool foundVol = false; int index;
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					if((*SvxHC)[i]->GetVolumeName() == theVolumes[j].first){foundVol = true; index = j; break;}
				}
				if(!foundVol)
				{
					std::pair<G4String,int> thePair = std::make_pair((*SvxHC)[i]->GetVolumeName(),1);
					theVolumes.push_back(thePair);
				}
				else{theVolumes[index].second += 1;} 

			}
			cout << "     Total energy deposition in SvxHC: " << totE / GeV << " (GeV)" << endl;
			if(_verbosity > 1) 
			{
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					cout << "Volume " << theVolumes[j].first << " has " << theVolumes[j].second << " hits " << endl;
				}
			}
		}

		if(MutHC)
		{
			int n_hit = MutHC->entries();
			cout << "     " << n_hit << " hits are stored in PHG3toG4MuonArmHitsCollection." << endl;
			G4double totE = 0;
			std::vector<std::pair<G4String,int> > theVolumes;
			for(int i=0;i<n_hit;i++)
			{
				//(*MutHC)[i]->Print();
				totE += (*MutHC)[i]->GetdE(); 
				bool foundVol = false; int index;
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					if((*MutHC)[i]->GetVolumeName() == theVolumes[j].first){foundVol = true; index = j; break;}
				}
				if(!foundVol)
				{
					std::pair<G4String,int> thePair = std::make_pair((*MutHC)[i]->GetVolumeName(),1);
					theVolumes.push_back(thePair);
				}
				else{theVolumes[index].second += 1;} 

			}
			cout << "     Total energy deposition in MutHC: " << totE / GeV << " (GeV)" << endl;
			if(_verbosity > 1) 
			{
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					cout << "Volume " << theVolumes[j].first << " has " << theVolumes[j].second << " hits " << endl;
				}
			}
		}
		if(MuiHC)
		{
			int n_hit = MuiHC->entries();
			cout << "     " << n_hit << " hits are stored in PHG3toG4MuonIDHitsCollection." << endl;
			G4double totE = 0;
			std::vector<std::pair<G4String,int> > theVolumes;
			for(int i=0;i<n_hit;i++)
			{
				//(*MuiHC)[i]->Print();
				totE += (*MuiHC)[i]->GetdE(); 
				bool foundVol = false; int index;
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					if((*MuiHC)[i]->GetVolumeName() == theVolumes[j].first){foundVol = true; index = j; break;}
				}
				if(!foundVol)
				{
					std::pair<G4String,int> thePair = std::make_pair((*MuiHC)[i]->GetVolumeName(),1);
					theVolumes.push_back(thePair);
				}
				else{theVolumes[index].second += 1;} 

			}
			cout << "     Total energy deposition in MuiHC: " << totE / GeV << " (GeV)" << endl;
			if(_verbosity > 1) 
			{
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					cout << "Volume " << theVolumes[j].first << " has " << theVolumes[j].second << " hits " << endl;
				}
			}
		}
		if(MupcHC)
		{
			int n_hit = MupcHC->entries();
			cout << "     " << n_hit << " hits are stored in PHG3toG4MuonRPCHitsCollection." << endl;
			G4double totE = 0;
			std::vector<std::pair<G4String,int> > theVolumes;
			for(int i=0;i<n_hit;i++)
			{
				//(*MupcHC)[i]->Print();
				totE += (*MupcHC)[i]->GetdE(); 
				bool foundVol = false; int index;
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					if((*MupcHC)[i]->GetVolumeName() == theVolumes[j].first){foundVol = true; index = j; break;}
				}
				if(!foundVol)
				{
					std::pair<G4String,int> thePair = std::make_pair((*MupcHC)[i]->GetVolumeName(),1);
					theVolumes.push_back(thePair);
				}
				else{theVolumes[index].second += 1;} 

			}
			cout << "     Total energy deposition in MupcHC: " << totE / GeV << " (GeV)" << endl;
			if(_verbosity > 1) 
			{
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					cout << "Volume " << theVolumes[j].first << " has " << theVolumes[j].second << " hits " << endl;
				}
			}
		}
		if(BbcHC)
		{
			int n_hit = BbcHC->entries();
			cout << "     " << n_hit << " hits are stored in PHG3toG4BbcHitsCollection." << endl;
			G4double totE = 0;
			std::vector<std::pair<G4String,int> > theVolumes;
			for(int i=0;i<n_hit;i++)
			{
				if((*BbcHC)[i]->GetDetID() > 0)
				{
					//(*BbcHC)[i]->Print();
					totE += (*BbcHC)[i]->GetdE(); 
					bool foundVol = false; int index;
					for(unsigned int j = 0; j < theVolumes.size(); j++)
					{
						if((*BbcHC)[i]->GetVolumeName() == theVolumes[j].first){foundVol = true; index = j; break;}
					}
					if(!foundVol)
					{
						std::pair<G4String,int> thePair = std::make_pair((*BbcHC)[i]->GetVolumeName(),1);
						theVolumes.push_back(thePair);
					}
					else{theVolumes[index].second += 1;} 

				}
			}
			cout << "     Total energy deposition in BbcHC: " << totE / GeV << " (GeV)" << endl;
			if(_verbosity > 1) 
			{
				for(unsigned int j = 0; j < theVolumes.size(); j++)
				{
					cout << "Volume " << theVolumes[j].first << " has " << theVolumes[j].second << " hits " << endl;
				}
			}
		}
	}//if(_verbosity > 0) 


	//If we aborted the event, dont write it out
	if(_theRootMgr->AbortEvent())
	{
		_theRootMgr->ClearEvent();
		return;
	}

	encodePisaEvt_Bbc(BbcHC);
	encodePisaEvt_Svx(SvxHC);
	encodePisaEvt_Mui(MuiHC);
	encodePisaEvt_Mut(MutHC);
	encodePisaEvt_Mupc(MupcHC);
	encodePisaEvt_Pri();
	encodePisaEvt_Kin();

	/////////////////////////////////////////////////////////////////
	//Fill the root tree and clear arrays
	_theRootMgr->EndEvent();

	if(_verbosity > 2) G4cout << "PHG3toG4EventAction::EndOfEventAction - end" << G4endl;
  
}  


//Encode SVX Hits
void PHG3toG4EventAction::encodePisaEvt_Svx(PHG3toG4SvxHitsCollection* hits)
{

  int kentries = hits->entries();
  int mctrack = -1;  // Will change in Off-Line
  int nfile = -1;

  for (int k=0; k<kentries; k++)
    {
      float xyzglobal[3];
      float pmomxyz[3];
      // Corrected by V. L. Rykov 09/03/2003
      float xyzlocalIn[3];
      float xyzlocalOut[3];
      // Added global in/out Jul 2006 Hubert van Hecke:
      float xyzglobalIn[3];
      float xyzglobalOut[3];
      int   hitVolume[9];
      
      xyzglobal[0]         = (*hits)[k]->GetWorldPosAvg().x();
      xyzglobal[1]         = (*hits)[k]->GetWorldPosAvg().y();
      xyzglobal[2]         = (*hits)[k]->GetWorldPosAvg().z();
      Float_t dele         = (*hits)[k]->GetdE();
      pmomxyz[0]           = (*hits)[k]->GetMomentumIn().x();
      Int_t siliID         = (*hits)[k]->GetParticleID();

      Int_t track          = (*hits)[k]->GetTrackID();
      Int_t layer          = (*hits)[k]->GetDetID();
      pmomxyz[1]           = (*hits)[k]->GetMomentumIn().y();
      pmomxyz[2]           = (*hits)[k]->GetMomentumIn().z();
      Float_t timeOfFlight = (*hits)[k]->GetTimeIn();
      
      xyzlocalIn[0]        = (*hits)[k]->GetLocalPosIn().x();
      xyzlocalIn[1]        = (*hits)[k]->GetLocalPosIn().y();
      xyzlocalIn[2]        = (*hits)[k]->GetLocalPosIn().z();
      xyzlocalOut[0]       = (*hits)[k]->GetLocalPosOut().x();
      xyzlocalOut[1]       = (*hits)[k]->GetLocalPosOut().y();
      xyzlocalOut[2]       = (*hits)[k]->GetLocalPosOut().z();
      
      xyzglobalIn[0]        = (*hits)[k]->GetWorldPosIn().x();
      xyzglobalIn[1]        = (*hits)[k]->GetWorldPosIn().y();
      xyzglobalIn[2]        = (*hits)[k]->GetWorldPosIn().z();
      xyzglobalOut[0]       = (*hits)[k]->GetWorldPosOut().x();
      xyzglobalOut[1]       = (*hits)[k]->GetWorldPosOut().y();
      xyzglobalOut[2]       = (*hits)[k]->GetWorldPosOut().z();
      
      hitVolume[0]         = (*hits)[k]->GetHitVolume(0);
      hitVolume[1]         = (*hits)[k]->GetHitVolume(1);
      hitVolume[2]         = (*hits)[k]->GetHitVolume(2);
      hitVolume[3]         = (*hits)[k]->GetHitVolume(3);
      hitVolume[4]         = (*hits)[k]->GetHitVolume(4);
      hitVolume[5]         = (*hits)[k]->GetHitVolume(5);
      hitVolume[6]         = (*hits)[k]->GetHitVolume(6);
      hitVolume[7]         = (*hits)[k]->GetHitVolume(7);
      hitVolume[8]         = (*hits)[k]->GetHitVolume(8);

      _theRootMgr->pisaevent->AddSvxHit( xyzglobal,  pmomxyz, dele, timeOfFlight,
					 xyzlocalIn, xyzlocalOut,
					 xyzglobalIn, xyzglobalOut,
					 hitVolume,
					 track,  layer,  siliID,
					 1 /*isubevent*/,   mctrack,  nfile);
      
    }
  
  //Match para
  _theRootMgr->pisaevent->AddSvxPara(iData_svx,fData_svx);

  return;
}


//Encode BBC Hits
void PHG3toG4EventAction::encodePisaEvt_Bbc(PHG3toG4BbcHitsCollection* hits)
{

  int argmctrack = -1;     // Will change in Off-Line
  int argnfile = -1;       // Will change in Off-line
  int kentries = hits->entries();


  for (int k=0; k<kentries; k++)
    {
      short argpmt = (*hits)[k]->GetDetID();//! PMT+1000*Side, Side 1=muon 2=other
      float argx =   (*hits)[k]->GetWorldPosIn().x();
      float argy =   (*hits)[k]->GetWorldPosIn().y();
      float argz =   (*hits)[k]->GetWorldPosIn().z();
      float argdel = (*hits)[k]->GetdE();
      float argtof = (*hits)[k]->GetTimeIn();
      short argpid = (*hits)[k]->GetParticleID();
      float argpx =  (*hits)[k]->GetMomentumIn().x();
      float argpy =  (*hits)[k]->GetMomentumIn().y();
      float argpz =  (*hits)[k]->GetMomentumIn().z();
      float arglen = (*hits)[k]->GetStepLength();
      int argtrack = (*hits)[k]->GetTrackID();
      
      _theRootMgr->pisaevent->AddBbcHit(argx,  argy,   argz,   argpx,  argpy,    argpz, argdel,
					argtof,arglen, argpmt, argpid, argtrack,  1 /*isubevent*/,
					argmctrack, argnfile);
      
    }


  //Match para
  _theRootMgr->pisaevent->AddBbcPara(iData_bbc,fData_bbc);
  

  return;

}


//Encode MUID Hits
void PHG3toG4EventAction::encodePisaEvt_Mui(PHG3toG4MuonIDHitsCollection* hits)
{

  int mctrack = -1;  // Will change in Off-Line
  int nfile = -1;    // will change offline
  float rhit[3];
  float phit[3];
  int kentries = hits->entries();

  for (int k=0; k<kentries; k++)
    {
      int side = 0;
      if((*hits)[k]->GetArmID() > 0) side = 1;
      else side = 2;

      int itrksub   = (*hits)[k]->GetTrackID();
      int plane_num = (*hits)[k]->GetLayerID()+1000*side;//1000*side+plane within side
      int trk_id    = (*hits)[k]->GetParticleID();
      float tof     = (*hits)[k]->GetTimeIn();
      float de      = (*hits)[k]->GetdE();
      rhit[0]       = (*hits)[k]->GetWorldPosIn().x();
      rhit[1]       = (*hits)[k]->GetWorldPosIn().y();
      rhit[2]       = (*hits)[k]->GetWorldPosIn().z();
      phit[0]       = (*hits)[k]->GetMomentumIn().x();
      phit[1]       = (*hits)[k]->GetMomentumIn().y();
      phit[2]       = (*hits)[k]->GetMomentumIn().z();
      
      _theRootMgr->pisaevent->AddMuiHit(itrksub, plane_num, trk_id, tof, de, rhit, 
					phit, mctrack, nfile, 1 /*isubevent*/);
      
    } 
  

  //Match para
  _theRootMgr->pisaevent->AddMuiPara(iData_mui,fData_mui);
  
  
  return;

}


//Encode MUT Hits
void PHG3toG4EventAction::encodePisaEvt_Mut(PHG3toG4MuonArmHitsCollection* hits)
{

  int mctrack = -1;  // Will change in Off-Line
  int nfile = -1;    // will change offline
  int kentries = hits->entries();
  
  for (int k=0; k<kentries; k++)
    {
      int track = (*hits)[k]->GetTrackID();
      short pid = (*hits)[k]->GetParticleID();
      float t   = (*hits)[k]->GetTimeIn();
      float e   = (*hits)[k]->GetdE();
      //float x   = (*hits)[k]->GetWorldPosIn().x();
      //float y   = (*hits)[k]->GetWorldPosIn().y();
      //float z   = (*hits)[k]->GetWorldPosIn().z();
      float x   = (*hits)[k]->GetWorldPosAvg().x();
      float y   = (*hits)[k]->GetWorldPosAvg().y();
      float z   = (*hits)[k]->GetWorldPosAvg().z();
      float px  = (*hits)[k]->GetMomentumIn().x();
      float py  = (*hits)[k]->GetMomentumIn().y();
      float pz  = (*hits)[k]->GetMomentumIn().z();
      int side = 1;
      if((*hits)[k]->GetArmID() < 0) side = 2;
      int station = (*hits)[k]->GetStationID();
      if(station > 3) station -= 3;
      double phihit = atan2(y,x);
      if (phihit < 0) phihit += 2*PI;
      int octant = (int)((phihit + (PI/8))/(PI/4)) + 1;
      if (octant == 9) octant = 1;
      int plane = side*1000 + octant*100 + station*10 + (*hits)[k]->GetLayerID(); //arm * 1000 + octant * 100 + station * 10 + plane

      

      _theRootMgr->pisaevent->AddMutHit(track, plane, pid, t, e, x, y, z, px, py, pz,
					mctrack, nfile, 1 /*isubevent*/);
      
    } 

  //Match para
  _theRootMgr->pisaevent->AddMutPara(iData_mut,fData_mut);


  return;

}


//Encode MU RPC Hits
void PHG3toG4EventAction::encodePisaEvt_Mupc(PHG3toG4MuonRPCHitsCollection* hits)
{

  int mctrack = -1;  // Will change in Off-Line
  int nfile = -1;    // will change offline
  int kentries = hits->entries();

  float xyzinloc[3];
  float xyzoutloc[3];
  float xyzinglo[3];

  for (int k=0; k<kentries; k++)
    {

      int ich            = (*hits)[k]->GetDetID();
      int ipc            = 0;
      if(ich <= 2) ipc = 1;
      if(ich>2 && ich<=4) ipc = 2;
      if(ich>4 && ich<=6) ipc = 3;

      int track          = (*hits)[k]->GetTrackID();
      xyzinloc[0]        = (*hits)[k]->GetLocalPosIn().x();
      xyzinloc[1]        = (*hits)[k]->GetLocalPosIn().y();
      xyzinloc[2]        = (*hits)[k]->GetLocalPosIn().z();
      xyzoutloc[0]       = (*hits)[k]->GetLocalPosOut().x();
      xyzoutloc[1]       = (*hits)[k]->GetLocalPosOut().y();
      xyzoutloc[2]       = (*hits)[k]->GetLocalPosOut().z();
      float tof          = (*hits)[k]->GetTimeIn();

      int id             = (*hits)[k]->GetParticleID();
      float dedx         = (*hits)[k]->GetdE();
      xyzinglo[0]        = (*hits)[k]->GetWorldPosIn().x();
      xyzinglo[1]        = (*hits)[k]->GetWorldPosIn().y();
      xyzinglo[2]        = (*hits)[k]->GetWorldPosIn().z();

      int arm = 1;
      if (xyzinglo[2] >= 0.0) arm = 0;   // Count from 0 as of Feb. 15, 1998
      float pathLength = (*hits)[k]->GetStepLength();


      // Original subevent output, kept for future use
      _theRootMgr->pisaevent->AddMuPCHit(xyzinloc, xyzoutloc, xyzinglo,
					 tof, dedx, pathLength, track, arm,
					 id, ipc, 1 /*isubevent*/,  mctrack, nfile );

    }


  return;

}


//Encode KinHits --- track info for all tracks 
//(but we just save ones that made hits, can have duplicates - faster than getting rid of them every event)
void PHG3toG4EventAction::encodePisaEvt_Kin()
{

  //int argevtrack = 0;
  int argtrue_track = -1;  // Will change in Off-Line
  int argnfile = -1;       // Will change in Off-line
  int kentries = _theRootMgr->KinHitSize();

  //_theRootMgr->FinishKinHits();//adds parent particle id .. not needed anymore

 
  for (int k=0; k<kentries; k++)
    {
      if(_verbosity > 10) 
	{
	  G4cout << "K " << _theRootMgr->GetKinHit(k).GetTrackID() << "  " << _theRootMgr->GetKinHit(k).GetParticleID() 
		 << "  " << _theRootMgr->GetKinHit(k).GetParentTrackID() << "  " << _theRootMgr->GetKinHit(k).GetParentPartID() << G4endl;
	}

      int argntrack = _theRootMgr->GetKinHit(k).GetTrackID();
      int argidpart = _theRootMgr->GetKinHit(k).GetParticleID();
      float argptot = _theRootMgr->GetKinHit(k).GetMomentum().mag(); //bad name - actually original momentum
      int argitparent = _theRootMgr->GetKinHit(k).GetParentTrackID();
      int argidparent = _theRootMgr->GetKinHit(k).GetParentPartID();
      float argpthet = _theRootMgr->GetKinHit(k).GetWorldPos().theta()*180/PI;
      float argpphi = _theRootMgr->GetKinHit(k).GetWorldPos().phi()*180/PI;
      float argr_vertex = sqrt(_theRootMgr->GetKinHit(k).GetVertexPos().x()*_theRootMgr->GetKinHit(k).GetVertexPos().x()+
			       _theRootMgr->GetKinHit(k).GetVertexPos().y()*_theRootMgr->GetKinHit(k).GetVertexPos().y());
      float argz_vertex = _theRootMgr->GetKinHit(k).GetVertexPos().z();
      float argth_vertx = _theRootMgr->GetKinHit(k).GetVertexPos().theta()*180/PI;
      float argph_vertx = _theRootMgr->GetKinHit(k).GetVertexPos().phi()*180/PI;



      //float argpx = _theRootMgr->GetKinHit(k).GetMomentum().x();
      //float argpy = _theRootMgr->GetKinHit(k).GetMomentum().y();
      //float argpz = _theRootMgr->GetKinHit(k).GetMomentum().z();

      //float argparent_px = _theRootMgr->GetKinHit(k).GetParentMomentum().x();
      //float argparent_py = _theRootMgr->GetKinHit(k).GetParentMomentum().y();
      //float argparent_pz = _theRootMgr->GetKinHit(k).GetParentMomentum().z();


      //G4cout << argntrack << "  " << argidpart << "  " << argitparent << "  " << argidparent << G4endl;
      
      _theRootMgr->pisaevent->AddKinHit(
					argtrue_track, 1 /*isubevent*/, argntrack, argidpart,
					argptot, argpthet, argpphi,
					argr_vertex, argz_vertex, argth_vertx,
					argph_vertx, argitparent, argidparent,
					argnfile);

      /*
      //Primary Particles
      if(argitparent == 0)
	{
	  argevtrack++;
	  _theRootMgr->pisaevent->AddPriHit(argtrue_track, 1, argntrack, argidpart,
					    argparent_px, argparent_py, argparent_pz, argevtrack, argnfile);

	}
      */
    }
  
  return;
  
}


void PHG3toG4EventAction::encodePisaEvt_Pri()
{

  int argevtrack = 0;
  int argtrue_track = -1;  // Will change in Off-Line
  int argnfile = -1;       // Will change in Off-line
  int kentries = _theRootMgr->PriHitSize();

 
  for (int k=0; k<kentries; k++)
    {

      if(_verbosity > 10) G4cout << "P " << _theRootMgr->GetPriHit(k).GetTrackID() << "  " << _theRootMgr->GetPriHit(k).GetParticleID() << G4endl;
      //G4cout << "P " << _theRootMgr->GetPriHit(k).GetTrackID() << "  " << _theRootMgr->GetPriHit(k).GetParticleID() << "  D: " << _theRootMgr->GetPriHit(k).GetDecayedFromTrackId() << G4endl;
      

      int argntrack = _theRootMgr->GetPriHit(k).GetTrackID();
      int argidpart = _theRootMgr->GetPriHit(k).GetParticleID();

      float argpx = _theRootMgr->GetPriHit(k).GetMomentum().x();
      float argpy = _theRootMgr->GetPriHit(k).GetMomentum().y();
      float argpz = _theRootMgr->GetPriHit(k).GetMomentum().z();
      float argptot = _theRootMgr->GetPriHit(k).GetMomentum().mag();

      _theRootMgr->pisaevent->AddPriHit(argtrue_track, 1/*isubevent*/, argntrack, argidpart,
					argpx, argpy, argpz, argevtrack, argnfile);

      //If this particle decayed from another particle, save other particle (e.g. J/Psi)
      if(_theRootMgr->GetPriHit(k).GetDecayedFromTrackId() == 0)
	{	  
	  //Make track ID negative of this particle
	  int dargntrack = -argntrack;
	  int dargidpart = _theRootMgr->GetPriHit(k).GetDecayedFromPdgId();

	  float dargpx = _theRootMgr->GetPriHit(k).GetDecayedFromP().x();
	  float dargpy = _theRootMgr->GetPriHit(k).GetDecayedFromP().y();
	  float dargpz = _theRootMgr->GetPriHit(k).GetDecayedFromP().z();
	  //float dargptot = _theRootMgr->GetPriHit(k).GetDecayedFromP().mag();
      
	  _theRootMgr->pisaevent->AddPriHit(argtrue_track, 1/*isubevent*/, dargntrack, dargidpart,
					    dargpx, dargpy, dargpz, argevtrack, argnfile);

	}

      //In order to fix a problem with the MCSingleMuons, we MUST add the primary particles to fKinHits
      //So we can tell which these are, we put it some dummy variables
      _theRootMgr->pisaevent->AddKinHit(argtrue_track, 1 /*isubevent*/, argntrack, argidpart,
                                        argptot, -99, -99, 0, 0, 0, -99, 0, 0, 1);

      argevtrack++;
      
    }
  
  return;
  
}
