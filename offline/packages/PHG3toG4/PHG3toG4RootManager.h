/************************************************************/
/*     Class: PHG3toG4RootManager                           */
/*                                                          */
/*     Desc: Handles the filling and writing of             */
/*           the PISAEvent tree                             */
/*           Also takes care of the KinHits (track info)    */
/*                                                          */
/*   Author: Matt Snowball (snowball.at.rcf.rhic.bnl.gov)   */
/*                                                          */
/************************************************************/

#ifndef __PHG3toG4RootManager_h__
#define __PHG3toG4RootManager_h__

#include "PHG3toG4KinHit.h"
#include "PHG3toG4PriHit.h"

#include <Geant4/G4Types.hh>        // for G4int

#include <string>
#include <vector>
#include <map>

class PISAEvent;
class TFile;
class TTree;


class PHG3toG4RootManager
{
public:
  

  PHG3toG4RootManager(std::string fileName = "PISAEvent.root", std::string treeName = "T");

  virtual ~PHG3toG4RootManager();
  static PHG3toG4RootManager *GetInstance();
  

  int Init();
  int SetEventHeader(std::vector<float> Data);
  int EndEvent();
  int ClearEvent();
  void SetAbortEvent(bool k = true);
  bool AbortEvent();
  int End();

  void AddKinHit(PHG3toG4KinHit *hit);
  PHG3toG4KinHit GetKinHit(int i);
  int KinHitSize();
  void FinishKinHits();
  bool IsKinHitRecorded(int ID);

  void AddPriHit(PHG3toG4PriHit *hit);
  PHG3toG4PriHit GetPriHit(int i);
  int PriHitSize();

  void Verbosity(int v)
  {_verbosity = v;}

  void CompressionLevel(int c)
  {_theCompressionLevel = c;}

  PISAEvent *pisaevent;

 private:
  static PHG3toG4RootManager *fgInstance; // Self pointer                                                                                                                                                                                                                      
  int _verbosity;
  bool _abortedEvent;
  std::string _theFileName;
  std::string _theTreeName;
  int _theCompressionLevel;
  
  TFile *hfile; 
  TTree *tree;
  long nbytes;

  PHG3toG4KinHitsCollection* fKinHitsCollection;
  G4int fKinCollectionSize;
  std::map<int, PHG3toG4KinHit*> fKinHitMap;

  PHG3toG4PriHitsCollection* fPriHitsCollection;
  G4int fPriCollectionSize;


};

#endif	

