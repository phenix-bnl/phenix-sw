
#include <SvxMasterReco.h>

// PHENIX includes
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>

// SVX includes
#include <SvxParManager.h>
#include <SvxDecode.h>
#include <SvxSimulator.h>
#include <SvxReco.h>
#include <SvxStandAloneReco.h>

// standard includes
#include <iostream>

using namespace std;

/// This constructor establishes which modules will run and with
/// what settings.
SvxMasterReco::SvxMasterReco(const string &name): SubsysReco(name)
{
  // defaults
  _doClustering = true;
  _doTracking = true;
  _runSvxTrack = false;

  CreateModuleList();
}

void SvxMasterReco::CreateModuleList(bool readSimulationData)
{
  // clear
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      delete _modules[imodule];
    }

  _modules.clear();
  _hasClusterDep.clear();
  _hasTrackDep.clear();

  // geometry creation
  SvxParManager *parman = new SvxParManager();
  _modules.push_back(parman);
  _hasClusterDep.push_back(false);
  _hasTrackDep.push_back(false);

  if(!readSimulationData)
    {
      // convert PRDF packets into hits 
      SvxDecode *decode = new SvxDecode();
      decode->includePixel(true);
      decode->includeStripixel(true);
      decode->setAdcOffset(24);
      decode->setAdcCutoff(-24);
      _modules.push_back(decode);
      _hasClusterDep.push_back(false);
      _hasTrackDep.push_back(false);
    }
  else
    {
      SvxSimulator *simulator = new SvxSimulator();
      simulator->set_StripixelNoise(0.0);
      //simulator->set_ReadParFromFile(1);
      _modules.push_back(simulator);
      _hasClusterDep.push_back(false);
      _hasTrackDep.push_back(false);
    }

  // cluster hits
  SvxReco *reco = new SvxReco();
  //reco->set_ReadParFromFile(1);
  //reco->set_StripixelAdcThreshold(30);
  reco->set_StripixelAdcSumThreshold(30);
  _modules.push_back(reco);
  _hasClusterDep.push_back(true);
  _hasTrackDep.push_back(false);
  
  // create and fit tracks
  if(!_runSvxTrack)
    {
      SvxStandAloneReco *standalone = new SvxStandAloneReco();
      standalone->setVertexRecoFlag(2);
      _modules.push_back(standalone);
      _hasClusterDep.push_back(true);
      _hasTrackDep.push_back(true);
    }
  else
    {
      cout << "Implementing new interface here..." << endl;
    }
}

SvxMasterReco::~SvxMasterReco()
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      delete _modules[imodule];
    }

  return;
}

int SvxMasterReco::Init(PHCompositeNode *topNode)
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      if(_hasClusterDep[imodule] && !_doClustering) continue;
      if(_hasTrackDep[imodule] && !_doTracking) continue;

      int retval = _modules[imodule]->Init(topNode);

      if(retval != EVENT_OK)
	{
	  return retval;
	}
    }

  return EVENT_OK;
}

int SvxMasterReco::InitRun(PHCompositeNode *topNode)
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      if(_hasClusterDep[imodule] && !_doClustering) continue;
      if(_hasTrackDep[imodule] && !_doTracking) continue;

      int retval = _modules[imodule]->InitRun(topNode);

      if(retval != EVENT_OK)
	{
	  return retval;
	}
    }

  return EVENT_OK;
}

int SvxMasterReco::process_event(PHCompositeNode *topNode)
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      if(_hasClusterDep[imodule] && !_doClustering) continue;
      if(_hasTrackDep[imodule] && !_doTracking) continue;

      int retval = _modules[imodule]->process_event(topNode);

      if(retval != EVENT_OK)
	{
	  return retval;
	}
    }

  return EVENT_OK;
}

int SvxMasterReco::ResetEvent(PHCompositeNode *topNode)
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      if(_hasClusterDep[imodule] && !_doClustering) continue;
      if(_hasTrackDep[imodule] && !_doTracking) continue;

      int retval = _modules[imodule]->ResetEvent(topNode);

      if(retval != EVENT_OK)
	{
	  return retval;
	}
    }

  return EVENT_OK;
}

int SvxMasterReco::Reset(PHCompositeNode *topNode)
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      if(_hasClusterDep[imodule] && !_doClustering) continue;
      if(_hasTrackDep[imodule] && !_doTracking) continue;

      int retval = _modules[imodule]->Reset(topNode);

      if(retval != EVENT_OK)
	{
	  return retval;
	}
    }

  return EVENT_OK;
}

int SvxMasterReco::EndRun(const int runnumber)
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      if(_hasClusterDep[imodule] && !_doClustering) continue;
      if(_hasTrackDep[imodule] && !_doTracking) continue;

      int retval = _modules[imodule]->EndRun(runnumber);

      if(retval != EVENT_OK)
	{
	  return retval;
	}
    }

  return EVENT_OK;
}

int SvxMasterReco::End(PHCompositeNode *topNode)
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      if(_hasClusterDep[imodule] && !_doClustering) continue;
      if(_hasTrackDep[imodule] && !_doTracking) continue;

      int retval = _modules[imodule]->End(topNode);

      if(retval != EVENT_OK)
	{
	  return retval;
	}
    }

  return EVENT_OK;
}

void SvxMasterReco::Verbosity(int verbosity)
{
  for(unsigned int imodule = 0; imodule < _modules.size(); imodule++)
    {
      _modules[imodule]->Verbosity(verbosity);
    }

  return;
}
