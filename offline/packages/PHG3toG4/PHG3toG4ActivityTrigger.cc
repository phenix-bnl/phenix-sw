#include "PHG3toG4ActivityTrigger.h"
#include "PHG3toG4SvxHit.h"
#include "PHG3toG4MuonArmHit.h"
#include "PHG3toG4MuonIDHit.h"
#include "PHG3toG4MuonRPCHit.h"
#include "PHG3toG4BbcHit.h"

#include <Geant4/G4EventManager.hh>
#include <Geant4/G4Event.hh>
#include <Geant4/G4SDManager.hh>
#include <Geant4/G4HCofThisEvent.hh>
#include <Geant4/G4THitsCollection.hh>

#include <cstdlib>              // for abs
#include <iostream>              // for operator<<, cout, ostream, endl, bas...
#include <stdexcept>             // for runtime_error
#include <utility>                      // for pair

using namespace std;

//__________________________________________________________
PHG3toG4ActivityTrigger::PHG3toG4ActivityTrigger(const std::string &name):
  PHG3toG4GeneralTrigger(name)
{
  _verbosity = 0;
  _doAbsOnly = false;
  _anyParticles = false;
  minMom = 0;

  _allDetectors.push_back("BBC");
  _allDetectors.push_back("MUI");
  _allDetectors.push_back("MUT");
  _allDetectors.push_back("MUPC");
  _allDetectors.push_back("SVX");

}

//__________________________________________________________
PHG3toG4ActivityTrigger::~PHG3toG4ActivityTrigger()
{}


bool PHG3toG4ActivityTrigger::Apply() 
{

  G4SDManager *SDman = G4SDManager::GetSDMpointer();
  const G4Event *event = (G4EventManager::GetEventManager())->GetConstCurrentEvent();

  for(unsigned int i = 0; i < _theDetectors.size(); i++)
    {
      //Check for min hits requirement
      int minHits = 0;
      std::map<std::string,int>::iterator it = _theDetMinHits.find(_theDetectors[i]);
      if(it != _theDetMinHits.end()) minHits = it->second;

      int hitCollId = -1;
      //G4HitsCollection *hitCollection;
      //G4THitsMap<G4double>* hitCollection; 
      G4HCofThisEvent * HCE = event->GetHCofThisEvent();

      //This is ugly, but I cant find a generic class for the templated hit collections
      std::vector<int> hitCounters(_theParticles.size(),0);
      if(_theDetectors[i] == "MUI")
	{
	  hitCollId = SDman->GetCollectionID("muonIDHitCollection");
	  PHG3toG4MuonIDHitsCollection *hitCollection = (PHG3toG4MuonIDHitsCollection*)(HCE->GetHC(hitCollId));
	  int n_hit = hitCollection->entries();
	  if(_anyParticles) 
	    {
	      if( n_hit >= minHits ) return true;
	      else continue;
	    }
	  
	  for(int i = 0; i < n_hit; i++)
	    {
	      int pid = (*hitCollection)[i]->GetParticleID();
	      float mom = (*hitCollection)[i]->GetMomentumOut().mag();
	      if(_doAbsOnly) pid = abs((*hitCollection)[i]->GetParticleID());
	      //	      G4ThreeVector vmom = (*hitCollection)[i]->GetMomentumIn();
	      //	      float mom = vmom.mag();
	      //	      if ( mom < 1.0 ) continue;
	      
	      for(unsigned int p = 0; p < _theParticles.size(); p++)
		{
		  if( pid == _theParticles[p]   && mom > minMom)
		    {
		      hitCounters[p] += 1;
		      //When we surpass min hits, return true
		      if(hitCounters[p] > minHits) return true;
		    }
		}
	    }

	}
      else if(_theDetectors[i] == "MUT")
	{
	  hitCollId = SDman->GetCollectionID("muonArmHitCollection");
	  PHG3toG4MuonArmHitsCollection *hitCollection = (PHG3toG4MuonArmHitsCollection*)(HCE->GetHC(hitCollId));
	  int n_hit = hitCollection->entries();
          if(_anyParticles)
	    {
	      if( n_hit >= minHits ) return true;
	      else continue;
	    }

	  for(int i = 0; i < n_hit; i++)
	    {
              int pid = (*hitCollection)[i]->GetParticleID();
	      float mom = (*hitCollection)[i]->GetMomentumOut().mag();
	      if(_doAbsOnly) pid = abs((*hitCollection)[i]->GetParticleID());

	      for(unsigned int p = 0; p < _theParticles.size(); p++)
		{
		  if( pid == _theParticles[p]   && mom > minMom)
		    {
		      hitCounters[p] += 1;
		      //When we surpass min hits, return true
		      if(hitCounters[p] > minHits) return true;
		    }
		}
	    }

	}
      else if(_theDetectors[i] == "BBC")
	{
	  hitCollId = SDman->GetCollectionID("bbcHitCollection");
	  PHG3toG4BbcHitsCollection *hitCollection = (PHG3toG4BbcHitsCollection*)(HCE->GetHC(hitCollId));
	  int n_hit = hitCollection->entries();
          if(_anyParticles)
	    {
	      if( n_hit >= minHits ) return true;
	      else continue;
	    }

	  for(int i = 0; i < n_hit; i++)
	    {
              int pid = (*hitCollection)[i]->GetParticleID();
	      if(_doAbsOnly) pid = abs((*hitCollection)[i]->GetParticleID());

	      for(unsigned int p = 0; p < _theParticles.size(); p++)
		{
                  if( pid == _theParticles[p] )
		    {
		      hitCounters[p] += 1;
		      //When we surpass min hits, return true
		      if(hitCounters[p] > minHits) return true;
		    }
		}
	    }
	  
	}
      else if(_theDetectors[i] == "MUPC")
	{
	  hitCollId = SDman->GetCollectionID("muonRPCHitCollection");
	  PHG3toG4MuonRPCHitsCollection *hitCollection = (PHG3toG4MuonRPCHitsCollection*)(HCE->GetHC(hitCollId));
	  int n_hit = hitCollection->entries();
          if(_anyParticles)
	    {
	      if( n_hit >= minHits ) return true;
	      else continue;
	    }

	  for(int i = 0; i < n_hit; i++)
	    {
              int pid = (*hitCollection)[i]->GetParticleID();
	      if(_doAbsOnly) pid = abs((*hitCollection)[i]->GetParticleID());

	      for(unsigned int p = 0; p < _theParticles.size(); p++)
		{
                  if( pid == _theParticles[p] )
		    {
		      hitCounters[p] += 1;
		      //When we surpass min hits, return true
		      if(hitCounters[p] > minHits) return true;
		    }
		}
	    }

	}
      else if(_theDetectors[i] == "SVX")
	{
	  hitCollId = SDman->GetCollectionID("svxHitCollection");
	  PHG3toG4SvxHitsCollection *hitCollection = (PHG3toG4SvxHitsCollection*)(HCE->GetHC(hitCollId));
	  int n_hit = hitCollection->entries();
          if(_anyParticles)
	    {
	      if( n_hit >= minHits ) return true;
	      else continue;
	    }

	  for(int i = 0; i < n_hit; i++)
	    {
              int pid = (*hitCollection)[i]->GetParticleID();
	      float mom = (*hitCollection)[i]->GetMomentumOut().mag();
	      if(_doAbsOnly) pid = abs((*hitCollection)[i]->GetParticleID());

	      for(unsigned int p = 0; p < _theParticles.size(); p++)
		{
                  if( pid == _theParticles[p]  && mom > minMom)
		    {
		      hitCounters[p] += 1;
		      //When we surpass min hits, return true
		      if(hitCounters[p] > minHits) return true;
		    }
		}
	    }

	}
      

    }//for(unsigned int i = 0; i < _theDetectors; i++)
	
  

  return false;
}

void PHG3toG4ActivityTrigger::AddDetectorMinHits(std::string det,int minHits)
{
  std::map<std::string,int>::iterator it;
  it = _theDetMinHits.find(det);
  if (it != _theDetMinHits.end())
    {
      cout << "PHG3toG4ActivityTrigger::AddDetectorMinHits - Error! Detector "
	   << det << " already has an entry for minimum number of hits!" << endl;
      return;
    }
  else{
    _theDetMinHits.insert( std::pair<std::string,int>(det,minHits) );
  }


}

void PHG3toG4ActivityTrigger::PrintConfig()
{
  cout << "---------------- PHG3toG4ActivityTrigger::PrintConfig --------------------" << endl;
  cout << "   Particles: ";
  if(_anyParticles)
    {
      cout << " Any";
    }
  else{
    for(int i = 0; i < int(_theParticles.size()); i++) cout << _theParticles[i] << "  ";
  }
  cout << endl;

  cout << "   Parents: ";
  for(int i = 0; i < int(_theParents.size()); i++) cout << _theParents[i] << "  ";
  cout << endl;

  cout << "   Detectors(minHits): ";
  for(int i = 0; i < int(_theDetectors.size()); i++)
    {
      cout << _theDetectors[i] << "(";
      int minH = 0;
      std::map<std::string,int>::iterator it = _theDetMinHits.find(_theDetectors[i]);
      if(it != _theDetMinHits.end()) minH = it->second;
      cout << minH << ")  ";
    }
  cout << endl;

  if (minMom>0)
    {
      cout << "Minimum Total Momentum for the particle = " << minMom << " GeV/c" << endl;
    }
  cout << "-----------------------------------------------------------------------" << endl;
}


void PHG3toG4ActivityTrigger::CheckVectors()
{

  if(_doAbsOnly)
    {
      //Change to abs value of particles... easier to keep track
      for(unsigned int i = 0; i < _theParticles.size(); i++)
	{
	  _theParticles[i] = abs(_theParticles[i]);
	}

      //std::sort(_theParticles.begin(), _theParticles.end());
      //std::vector<int>::iterator it = std::unique(_theParticles.begin(), _theParticles.end());
      //_theParticles.resize( std::distance(_theParticles.begin(),it) );
    }

  if(_theParticles.size() == 0)
    {
      _anyParticles = true;
    }
  
  bool AllDet = false;
  for(unsigned int i = 0; i < _theDetectors.size(); i++)
    {
      std::string tmp = _theDetectors[i];
      //std::transform(tmp.begin(), tmp.end(), tmp.begin(), toupper);
      if(tmp == "ALL" || tmp == "all"){ AllDet = true; break;}
    }
  if(AllDet)
    {
      _theDetectors.clear();
      for(unsigned int i = 0; i < _allDetectors.size(); i++)
	{
	  _theDetectors.push_back(_allDetectors[i]);
	}
    }

  for(unsigned int i = 0; i < _theDetectors.size(); i++)
    {
      if(_theDetectors[i] != "BBC" && 
	 _theDetectors[i] != "MUI" &&
	 _theDetectors[i] != "MUT" &&
	 _theDetectors[i] != "SVX" &&
	 _theDetectors[i] != "MUPC")
	{
	  throw runtime_error("PHG3toG4ActivityTrigger - Unknown detector: "+_theDetectors[i]);
	}
    }

}

