#include <MasterRecalibratorManager.h>
#include <MasterRecalibrator.h>

#include <Fun4AllServer.h>

#include <cstring>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

MasterRecalibratorManager::MasterRecalibratorManager(const string& name) : SubsysReco(name) {
  topNodeName.clear();
  ClearMasterRecalVector();
  fillhistos = 1; // histo filling by recalibrators on by default
}

MasterRecalibratorManager::~MasterRecalibratorManager(){
  ClearMasterRecalVector();
}

void MasterRecalibratorManager::ClearMasterRecalVector(){
  vector<MasterRecalibrator*>::const_iterator masterIter = masterRecalibrator.begin();
  vector<MasterRecalibrator*>::const_iterator lastMasterIter = masterRecalibrator.end();
  for(; masterIter != lastMasterIter; ++masterIter){
    delete *masterIter;
  }
  masterRecalibrator.clear();
}

int MasterRecalibratorManager::Init(PHCompositeNode *topNode){
  //There is nothing to initialize except maybe clearing arrays?

  return 0;
}

int MasterRecalibratorManager::InitRun(PHCompositeNode *topNode){

  //this SubsysReco is attached to the default "TOP" topNode.
  //that is the input topNode is pretty useless in this function
  //we need to get the list of topNodes from the Fun4AllManager
  //and create a master recalibrator for each topNode

  topNodeName.clear();
  ClearMasterRecalVector();

  //grab the existing topNodes
  Fun4AllServer *se = Fun4AllServer::instance();
  int ntopNodes = se->GetTopNodes(topNodeName);
  ostringstream mrName;
  for(int inode=0; inode<ntopNodes; inode++){
    mrName.str("");
    mrName << "MASTERRECALIBRATOR_" << topNodeName[inode];
    MasterRecalibrator *mr = new MasterRecalibrator(mrName.str());
    mr->FillHistos(fillhistos);
    mr->Verbosity(this->Verbosity());
    masterRecalibrator.push_back(mr);
    masterRecalibrator[inode]->InitRun(se->topNode(topNodeName[inode]));
  }

  return 0;
}

int MasterRecalibratorManager::process_event(PHCompositeNode *topNode){
  Fun4AllServer *se = Fun4AllServer::instance();

  int ret = 0;
  for(unsigned int inode=0; inode<topNodeName.size(); inode++){
    ret = masterRecalibrator[inode]->process_event(se->topNode(topNodeName[inode]));
    if(ret!=0)
      return ret;
  }

  return ret;
}

int MasterRecalibratorManager::ResetEvent(PHCompositeNode *topNode){
  Fun4AllServer *se = Fun4AllServer::instance();

  int ret = 0;
  for(unsigned int inode=0; inode<topNodeName.size(); inode++){
    ret = masterRecalibrator[inode]->ResetEvent(se->topNode(topNodeName[inode]));
    if(ret!=0)
      return ret;
  }

  return ret;
}

int MasterRecalibratorManager::EndRun(int runno){
  int ret = 0;
  for(unsigned int inode=0; inode<topNodeName.size(); inode++){
    ret = masterRecalibrator[inode]->EndRun(runno);
    if(ret!=0)
      return ret;
  }

  return ret;
}

int MasterRecalibratorManager::End(PHCompositeNode *topNode){
  Fun4AllServer *se = Fun4AllServer::instance();

  int ret = 0;
  for(unsigned int inode=0; inode<topNodeName.size(); inode++){
    ret = masterRecalibrator[inode]->End(se->topNode(topNodeName[inode]));
    if(ret!=0)
      return ret;
  }

  return ret;
}

MasterRecalibrator *
MasterRecalibratorManager::GetMasterRecalibrator(const std::string &name)
{
  vector<MasterRecalibrator*>::const_iterator miter;
  for (miter = masterRecalibrator.begin(); miter != masterRecalibrator.end();miter++)
    {
      if (!strcmp((*miter)->Name(),name.c_str()))
	{
	  return *miter;
	}
    }
  cout << "Could not locate MasterRecalibrator with name " << name << endl;
  return 0;
}

