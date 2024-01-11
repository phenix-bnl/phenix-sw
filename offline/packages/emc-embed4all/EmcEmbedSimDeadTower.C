#include "EmcEmbedSimDeadTower.h"
#include <cassert> 
#include <fstream>
//INCLUDECHECKER: Removed this line: #include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "emcBadModulesv1.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "emcDataManager.h"
#include "EmcIndexer.h"
//INCLUDECHECKER: Removed this line: #include "emcRejectList.h"
#include "RunHeader.h"
#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "getClass.h"
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"
//INCLUDECHECKER: Removed this line: #include "PHTimeStamp.h"
#include "recoConsts.h"

#include "TH2.h"

namespace
{
  bool checkdir(std::string dir)
  {
    dir += "/RejectList/reject.list";
    std::cout << dir << std::endl;
    std::ifstream in(dir.c_str());
    if (!in)
      {
        return false;
      }
    return true;
  }

  std::string histoName(int is, const char* suffix)
  {
    std::ostringstream name;

    name << EmcIndexer::EmcSectorId(is) << suffix;

    return name.str();
  }
}

//_____________________________________________________________________________
EmcEmbedSimDeadTower::EmcEmbedSimDeadTower(const char * realNode,
					   const char* simuNode)
  : fDataSource(emcManageable::kDB_Pg),   // postgres default value
    fBadModules(0),
    fRealNodeName(realNode),
    fSimuNodeName(simuNode)
{
  ThisName = "EmcEmbedSimDeadTower";

  recoConsts* rc = recoConsts::instance();

  if ( rc->FlagExist("EMCDATASOURCE") )
    {
      emcManageable::EStorage ds = emcManageable::GetStorage(rc->get_CharFlag("EMCDATASOURCE"));
      if ( ds == emcManageable::kNone )
        {
          std::cerr << "EmcEmbedSimDeadTower::EmcEmbedSimDeadTower : "
		    << "Flag EMCDATASOURCE="
		    << rc->get_CharFlag("EMCDATASOURCE")
		    << " is not valid. Using default="
		    << emcManageable::GetStorageName(fDataSource)
		    << std::endl;
          throw;
        }
      else
        {
          fDataSource = ds;

          if ( fDataSource == emcManageable::kFile_ASCII )
            {
              emcDataManager* dm = emcDataManager::GetInstance();
              const std::string dir = dm->GetSourceDir();
              bool ok = checkdir(dir);
              if (!ok)
                {
                  std::cerr << "EmcEmbedSimDeadTower::EmcEmbedSimDeadTower : "
			    << "The source directory "
			    << dir
			    << " is not valid (did not find "
			    << "RejectList/rejectlist file in there)."
			    << std::endl;
                  throw;
                }

              std::cout << "EmcEmbedSimDeadTower::EmcEmbedSimDeadTower : will use "
			<< dir << "/RejectList/reject.list file."
			<< std::endl;
            }
        }
    }
}

//_____________________________________________________________________________
EmcEmbedSimDeadTower::~EmcEmbedSimDeadTower()
{
  delete fBadModules;
}

//_____________________________________________________________________________
int
EmcEmbedSimDeadTower::Init(PHCompositeNode* topNode)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  for ( size_t is = 0; is < 8; ++is )
    {
      int nx = 72; // for PbSc
      int ny = 36;

      if ( is > 5 ) // for PbGl
        {
          nx = 96;
          ny = 48;
        }

      const char* names[] =
        { "_before", "_after"
        };

      for ( int i = 0; i < 2; ++i )
        {
          std::string name(histoName(is, names[i]));
          TH1* h = new TH2F(name.c_str(), name.c_str(),
                            nx, -0.5, nx - 0.5,
                            ny, -0.5, ny - 0.5);

          std::ostringstream hname;

          //	  hname << "EmcEmbedSimDeadTower/" << name;
          hname << name;

          se->registerHisto(hname.str().c_str(), h);
        }
    }

  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedSimDeadTower::InitRun(PHCompositeNode* topNode)
{
  delete fBadModules;
  Fun4AllServer* se = Fun4AllServer::instance();
  PHCompositeNode* topNode1 = se->topNode(fRealNodeName.c_str());
 
  RunHeader * run = findNode::getClass<RunHeader>(topNode1, "RunHeader");
  int runnumber = run->get_RunNumber();

  if ( verbosity>0 )
    {
      std::cout << PHWHERE << " RunNumber from Real RunHeader=" << runnumber
		<< std::endl;
    }

  if (runnumber < 0)
    {
      std::cout << PHWHERE << " bad run number " << runnumber << std::endl;
      return ABORTRUN;
    }

  PHTimeStamp t2(run->get_TimeStart());

  // To get the right PHTimeStamp (same as returned by Fun4All) 
  // we need to add 5 seconds.

  t2 = t2 + 5;

  if ( fDataSource == emcManageable::kFile_ASCII )
    {
      emcDataManager* dm = emcDataManager::GetInstance();
      std::cout << "EmcEmbedSimDeadTower::InitRun:sourcedir="
		<< dm->GetSourceDir()
		<< std::endl;

      fBadModules = new emcBadModulesv1(dm->GetSourceDir(),
                                        emcBadModules::kAll,
                                        true,
                                        "emcal");
    }
  else
    {

      fBadModules = new emcBadModulesv1(t2,
                                        emcBadModules::kAll,
					fDataSource,
                                        true,
                                        "emcal");
    }

  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedSimDeadTower::process_event(PHCompositeNode*)
{
  Fun4AllServer* se = Fun4AllServer::instance();
  PHCompositeNode* topNode = se->topNode(fSimuNodeName.c_str());
  if (!topNode)
    {
      std::cerr << PHWHERE << " Could not find my working topNode="
		<< fSimuNodeName << std::endl;
      return 0;
    }
  return my_process_event(topNode);
}

//_____________________________________________________________________________
int
EmcEmbedSimDeadTower::my_process_event(PHCompositeNode* topNode)
{
  Fun4AllServer* se = Fun4AllServer::instance();
  if (!topNode)
    {
      std::cerr << PHWHERE << " Could not find my working topNode="
		<< fSimuNodeName << std::endl;
      return 0;
    }
  emcTowerContainer* towers =
    findNode::getClass<emcTowerContainer>(topNode, "emcTowerContainer");

  if (!towers)
    {
      std::cerr << PHWHERE << " No emcTowerContainer object !"
		<< std::endl;
      return 0;
    }

  for ( size_t i = 0; i < towers->size(); ++i )
    {
      emcTowerContent* tow = towers->getTower(i);
      assert(tow != 0);

      int towerid = tow->TowerID();

      unsigned int cdead = (fBadModules->DeadmapFast(towerid) & 0x400) ;

      int is, iz, iy;

      EmcIndexer::decodeTowerId(towerid, is, iz, iy);

      if ( tow->Energy() > 0.)
        {
          TH2* h = dynamic_cast<TH2*>
	    (se->getHisto(histoName(is, "_before").c_str()));
          assert(h != 0);
          h->Fill(iz, iy);
        }


      //Updating the TowerContent ....

      if (cdead != 0) // bad tower
        {
          //std::cout << " Tower " << towerid << "  is a bad tower..." <<std::endl;
          tow->SetCalibrated(0.0, 0.0);  //set enregy and tof to 0
        }

      else
        {
          //std::cout << " Tower " << towerid << "  is a good tower..." <<std::endl;

        }
      if ( tow->Energy() > 0.)
        {
          TH2* h = dynamic_cast<TH2*>
	    (se->getHisto(histoName(is, "_after").c_str()));
          assert(h != 0);
          h->Fill(iz, iy);
        }

    }

  return 0;
}
