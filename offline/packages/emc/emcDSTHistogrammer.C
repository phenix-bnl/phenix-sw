#include "emcDSTHistogrammer.h"
#include "ezdst.h"
#include <iostream>
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "emcObjectFillerManager.h"
#include <map>
#include <string>
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "EmcIndexer.h"
#include <sstream>
#include "TROOT.h"
#include <cassert>

using namespace std;

namespace
{
  typedef map<string,TH1*> MAP;
  static vector<MAP> histos;

}

//_____________________________________________________________________________
void histogram(const emcTowerContainer& tc)
{
  for ( size_t i = 0; i < tc.size(); ++i ) 
    {
      emcTowerContent* t = tc.getTower(i);
      assert(t!=0);
      int towerID = t->TowerID();
      int iS,x,y;
      EmcIndexer::decodeTowerId(towerID,iS,x,y);
      histos[iS]["tenergy"]->Fill(t->Energy());
      histos[iS]["ttof"]->Fill(t->ToF());
    }
}

//_____________________________________________________________________________
void histogram(const emcClusterContainer& cc)
{
  for ( size_t i = 0; i < cc.size(); ++i ) 
    {
      emcClusterContent* c = cc.getCluster(i);
      assert(c!=0);
      int iS = EmcIndexer::sectorOfflineToOnline(c->arm(),c->sector());
      assert(iS>=0&&iS<8);
      histos[iS]["cenergy"]->Fill(c->e());
      histos[iS]["ctof"]->Fill(c->tof());
    }
}

//_____________________________________________________________________________
void makeHistoMap()
{
  for ( size_t i = 0; i < 8; ++i ) 
    {
      MAP m;
      string name;
      string title;

      double emax = 25.6; // GeV
      int nebins = 512;

      double tmin = -150; // ns
      double tmax =  150; // ns
      int ntbins = 512;

      name = EmcIndexer::EmcSectorId(i) + string("_tenergy");
      title = string("Tower energies (GeV) for sector ") 
	    + EmcIndexer::EmcSectorId(i);

      TH1* h = m["tenergy"] = new TH1F(name.c_str(),
				      title.c_str(),
				      nebins,0,emax);
      
      name = EmcIndexer::EmcSectorId(i) + string("_cenergy");
      title = string("Cluster energies (GeV) for sector ") 
	+ EmcIndexer::EmcSectorId(i);

      h =  m["cenergy"] = new TH1F(name.c_str(),
				   title.c_str(),
				   nebins,0,emax);

      name =  EmcIndexer::EmcSectorId(i) + string("_ttof");
      title = string("Tower TOF (ns) for sector ") 
	+ EmcIndexer::EmcSectorId(i);

      h =  m["ttof"] = new TH1F(name.c_str(),
				title.c_str(),
				ntbins,tmin,tmax);

      name =  EmcIndexer::EmcSectorId(i) + string("_ctof");
      title = string("Cluster TOF (ns) for sector ") 
	+ EmcIndexer::EmcSectorId(i);

      h =  m["ctof"] = new TH1F(name.c_str(),
				title.c_str(),
				ntbins,tmin,tmax);

      histos.push_back(m);
    }

  for ( size_t i = 0; i < 8; ++i ) 
    {
      MAP& m = histos[i];
      MAP::const_iterator it;
      for ( it = m.begin(); it != m.end(); ++it ) 
	{
	  it->second->SetDirectory(gROOT);
	}
    }

  gROOT->ls();
}

//_____________________________________________________________________________
int process_event(DstContent* dst)
{
  if ( histos.empty() )
    {
      makeHistoMap();
    }

  emcTowerContainer* tc = static_cast<emcTowerContainer*>
    (dst->getClass("emcTowerContainer"));

  if (tc)
    {
      histogram(*tc);
    }

  emcClusterContainer* cc = static_cast<emcClusterContainer*>
    (dst->getClass("emcClusterContainer"));

  if (cc)
    {
      histogram(*cc);
    }

  return 0;
}

//_____________________________________________________________________________
void hsave(const char* filename)
{
  TFile f(filename,"RECREATE");
  for ( size_t i = 0; i < histos.size(); ++i ) 
    {
      MAP& m = histos[i];
      MAP::const_iterator it;
      for ( it = m.begin(); it != m.end(); ++it ) 
	{
	  it->second->Write();
	}
    }
  f.Close();

  // then reset the histos so they might be reused.

  for ( size_t i = 0; i < histos.size(); ++i ) 
    {
      MAP& m = histos[i];
      MAP::const_iterator it;
      for ( it = m.begin(); it != m.end(); ++it ) 
	{
	  it->second->Reset();
	}
    }
}
