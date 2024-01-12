#include "EmcDeadRecalReco.h"

#include <cassert>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "emcBadModulesv1.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "emcDataManager.h"
#include "EmcIndexer.h"
#include "emcRejectList.h"

#include "Fun4AllServer.h"
#include "getClass.h"

#include "PHCompositeNode.h"
#include "PHTimeStamp.h"
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
EmcDeadRecalReco::EmcDeadRecalReco(const char* topNode, const char* dataNode)  
  :  fDataSource(emcManageable::kDB_Pg),
     fBadModules(0),
     fTopNodeName(topNode),
     fDataNodeName(dataNode)
{
  ThisName = "EmcDeadRecalReco";

  recoConsts* rc = recoConsts::instance();

  if ( rc->FlagExist("EMCDATASOURCE") )
    {
      emcManageable::EStorage ds = 
	emcManageable::GetStorage(rc->get_CharFlag("EMCDATASOURCE"));
      if ( ds == emcManageable::kNone )
	{
	  std::cerr << "EmcDeadRecalReco::EmcDeadRecalReco : "
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
		  std::cerr << "EmcDeadRecalReco::EmcDeadRecalReco : "
			    << "The source directory "
			    << dir
			    << " is not valid (did not find " 
			    << "RejectList/rejectlist file in there)."
			    << std::endl;
		  throw;
		}

	      std::cout << "EmcDeadRecalReco::EmcDeadRecalReco : will use "
			<< dir << "/RejectList/reject.list file."
			<< std::endl;
	    }
	}
    }
}

//_____________________________________________________________________________
EmcDeadRecalReco::~EmcDeadRecalReco()
{
  delete fBadModules;
}

//_____________________________________________________________________________
int
EmcDeadRecalReco::Init(PHCompositeNode* topNode)
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

      const char* names[] = { "_before","_after" };

      for ( int i = 0; i < 2; ++i ) 
	{
	  std::string name = histoName(is,names[i]); 
	  TH1* h = new TH2F(name.c_str(),name.c_str(),
			    nx,-0.5,nx-0.5,
			    ny,-0.5,ny-0.5);

	  std::ostringstream hname;

	  //	  hname << "EmcDeadRecalReco/" << name;
	  hname << name;

	  se->registerHisto(hname.str().c_str(),h);
	}
    }

  return 0;
}

//_____________________________________________________________________________
int
EmcDeadRecalReco::InitRun(PHCompositeNode* topNode)
{
  delete fBadModules;

  if ( fDataSource == emcManageable::kFile_ASCII )
    {
      emcDataManager* dm = emcDataManager::GetInstance();
      std::cout << "EmcDeadRecalReco::InitRun:sourcedir="
		<< dm->GetSourceDir()
		<< std::endl;

      fBadModules = new emcBadModulesv1(dm->GetSourceDir(),
					emcBadModules::kPhysics,
					true,
					"emcal");
    }
  else
    {
      recoConsts* rc = recoConsts::instance();

      PHTimeStamp t2(rc->get_TimeStamp());

      fBadModules = new emcBadModulesv1(t2,
					emcBadModules::kPhysics,
					fDataSource,
					true,
					"emcal");
    }

  return 0;
}

//_____________________________________________________________________________
int
EmcDeadRecalReco::process_event(PHCompositeNode*)
{
  Fun4AllServer* se = Fun4AllServer::instance();
  PHCompositeNode* topNode = se->topNode(fTopNodeName.c_str());
  if (!topNode)
    {
      std::cerr << PHWHERE << " Could not find my working topNode="
		<< fTopNodeName << std::endl;
      return 0;
    }
  return my_process_event(topNode);
}

//_____________________________________________________________________________
int 
EmcDeadRecalReco::my_process_event(PHCompositeNode* topNode)
{

  Fun4AllServer* se = Fun4AllServer::instance();
  emcClusterContainer* clusters = 
    findNode::getClass<emcClusterContainer>(topNode, fDataNodeName.c_str());

  if (!clusters)
    {
      std::cerr << PHWHERE << " No " << fDataNodeName.c_str() << " object !"
		<< std::endl;
      return 0;
    }

  const unsigned int cut3x3map[] = { 0xffe1ce70, 0x1ce70 }; // PbSc then PbGl

  for ( size_t i = 0; i < clusters->size(); ++i ) 
    {
      emcClusterContent* clu = clusters->getCluster(i);
      assert(clu!=0);

      int towerid = clu->towerid(0);

      unsigned int cdead = clu->deadmap() | fBadModules->DeadmapFast(towerid);
      unsigned int cwarn = clu->warnmap() | fBadModules->WarnmapFast(towerid);

      unsigned int cutmap = cut3x3map[clu->type()-1];

      int is,iz,iy;

      EmcIndexer::decodeTowerId(towerid,is,iz,iy);

      if ( (clu->deadmap() & cutmap) == 0 &&
	   (clu->warnmap() & cutmap) == 0 )
	{
	  TH2* h = dynamic_cast<TH2*>(se->getHisto(histoName(is,"_before")));
	  assert(h!=0);
	  h->Fill(iz,iy);
	}

      if ( verbosity>0 && 
	   ( cdead != clu->deadmap() || cwarn != clu->warnmap() ) )
	{  
	  std::ostream::fmtflags oldflags = std::cout.flags();
	  std::cout << PHWHERE << " Tower " << towerid << " has been updated "
		    << "from (dead,warn)=(" << std::hex
		    << clu->deadmap() << "," << std::hex
		    << clu->warnmap() << ") to (" << std::hex
		    << cdead << "," << std::hex
		    << cwarn << ")." 
		    << std::endl;
	  std::cout.setf(oldflags);
	}
		    
      clu->set_maps(cdead,cwarn);

      if ( (clu->deadmap() & cutmap) == 0 &&
	   (clu->warnmap() & cutmap) == 0 )
	{
	  TH2* h = dynamic_cast<TH2*>(se->getHisto(histoName(is,"_after")));
	  assert(h!=0);
	  h->Fill(iz,iy);
	}
    }

  return 0;
}
