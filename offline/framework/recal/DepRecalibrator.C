#include "DepRecalibrator.h"

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <RunHeader.h>
#include <PHGlobal.h>
#include <PHCompositeNode.h>
#include <recoConsts.h>

#include <PHString.h>
#include <TH2.h>

#include <iostream>

using namespace std;
using namespace findNode;

DepRecalibrator::DepRecalibrator(const string &name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  fillhistos = 0;
}

int DepRecalibrator::Init(PHCompositeNode *topNode)
{

  depfitmean0  = TF1("depfitmean0","0.987532+(-0.089152)/pow(x,1.025596) + (-0.001000)*x",0.5,5.0);
  depfitwidth0 = TF1("depfitwidth0","0.084674+(0.021487)/pow(x,1.669569) + (0.000855)*x",0.5,5.0);
  depfitmean1  = TF1("depfitmean1","1.011910+(-0.117106)/pow(x,0.729646) + (0.001000)*x",0.5,5.0);
  depfitwidth1 = TF1("depfitwidth1","0.060291+(0.036390)/pow(x,1.087173) + (0.005000)*x",0.5,5.0);
  depfitmean2  = TF1("depfitmean2","0.955367+(-0.031207)/pow(x,1.844949) + (-0.001000)*x",0.5,5.0);
  depfitwidth2 = TF1("depfitwidth2","0.031071+(0.061651)/pow(x,0.640164) + (0.005000)*x",0.5,5.0);
  depfitmean3  = TF1("depfitmean3","0.988141+(-0.021334)/pow(x,2.212720) + (-0.001000)*x",0.5,5.0);
  depfitwidth3 = TF1("depfitwidth3","0.068121+(0.032344)/pow(x,1.005323) + (0.002754)*x",0.5,5.0);
  depfitmean4  = TF1("depfitmean4","0.975240+(-0.020904)/pow(x,2.375890) + (-0.001000)*x",0.5,5.0);
  depfitwidth4 = TF1("depfitwidth4","0.071771+(0.025691)/pow(x,1.399602) + (0.005000)*x",0.5,5.0);
  depfitmean5  = TF1("depfitmean5","0.967993+(-0.034272)/pow(x,2.032255) + (-0.001000)*x",0.5,5.0);
  depfitwidth5 = TF1("depfitwidth5","0.081227+(0.027396)/pow(x,1.273899) + (0.005000)*x",0.5,5.0);
  depfitmean6  = TF1("depfitmean6","0.996241+(-0.044422)/pow(x,1.316069) + (-0.001000)*x",0.5,5.0);
  depfitwidth6 = TF1("depfitwidth6","0.089730+(0.015646)/pow(x,1.499989) + (0.004556)*x",0.5,5.0);
  depfitmean7  = TF1("depfitmean7","0.996327+(-0.028746)/pow(x,1.933068) + (-0.001000)*x",0.5,5.0);
  depfitwidth7 = TF1("depfitwidth7","0.084689+(0.024067)/pow(x,1.185714) + (0.005000)*x",0.5,5.0);

  if (fillhistos)
    {
      PHString nodename =  topNode->getName().getString();
      PHString histname = "Dep_e";
      histname += nodename;    
      hdep_pt_e = new TH2F(histname.getString(), "dep vs pt for electrons", 120,0,12.,120,-6,6);
      histname = "sDep_e";
      histname += nodename;    
      hsdep_pt_e = new TH2F(histname.getString(), "dep vs pt for electrons (swapped)", 120,0,12.,120,-6,6);
      histname = "Dep_p";
      histname += nodename;    
      hdep_pt_p = new TH2F(histname.getString(), "dep vs pt for positrons", 120,0,12.,120,-6,6);
      histname = "sDep_p";
      histname += nodename;    
      hsdep_pt_p = new TH2F(histname.getString(), "dep vs pt for positrons (swapped)", 120,0,12.,120,-6,6);

      Fun4AllServer *se = Fun4AllServer::instance();
      se->registerHisto(hdep_pt_e);
      se->registerHisto(hsdep_pt_e);
      se->registerHisto(hdep_pt_p);
      se->registerHisto(hsdep_pt_p);
      
    }
  return EVENT_OK;
}

int DepRecalibrator::InitRun(PHCompositeNode *topNode)
{

  RunHeader* d_run = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(!d_run){
    cout << PHWHERE << " ERROR: RunHeader not found." << endl;
    runNumber = 0;
    return EVENT_OK;
  }

  runNumber = d_run->get_RunNumber();
  return EVENT_OK;
}

int DepRecalibrator::isValidRun(const int runno) const
{

  // Run13 p+p at 510GeV run range
  if ( 386773<= runno && runno <=398149)
    {
      return 1;
    }

  return 0;
}

void DepRecalibrator::help() const
{
  cout << "===================================================================" << endl;
  cout << "DepRecalibrator description:"                    << endl;
  cout << "Author:  Sasha Lebedev (lebedev@iastate.edu)."                       << endl;
  cout << "Comment: Currently valid only for run13pp510 data set. "             << endl;
  cout << "         This method updates the dep value in PHCentralTrack. "      << endl;
  cout << "         pT range is > 0.5GeV                        "               << endl;
  cout << "===================================================================" << endl;
}

void DepRecalibrator::Print(const std::string&) const
{
  help();
}

int DepRecalibrator::process_event(PHCompositeNode *topNode)
{
  d_cnt    = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  if (!d_cnt || !d_global) return EVENT_OK;

  for (unsigned int itrk = 0; itrk < d_cnt->get_npart(); itrk++)
    {

      PHSnglCentralTrack *sngltrk = d_cnt->get_track(itrk);
      sngltrk->ShutUp();      
      if (
	  sngltrk->isImplemented(sngltrk->get_the0()) &&
	  sngltrk->isImplemented(sngltrk->get_mom()) &&
	  sngltrk->isImplemented(sngltrk->get_charge()) &&
	  sngltrk->isImplemented(sngltrk->get_dcarm()) &&
	  sngltrk->isImplemented(sngltrk->get_sect()) &&
	  sngltrk->isImplemented(sngltrk->get_ecore()) 
	  )
	{
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
	}
      else
	{
	  sngltrk->ShutUp(1);
	  if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
	  return 0;
	}
      
      sngltrk->ShutUp(1); // enable virtual warnings again


      float the0 = sngltrk->get_the0();
      float mom  = sngltrk->get_mom();
      float pt   = -9999.;
      if (the0 > -999.) { pt = mom * sin(the0); }
      
      int charge  = (int)sngltrk->get_charge();
      int dcarm   = (int) sngltrk->get_dcarm();
      int emcsect = (int)sngltrk->get_sect();
      int armsect = dcarm*4+emcsect;
      
      float ecore=float(sngltrk->get_ecore());
      float eop= -9999.;
      if(mom > 0. && ecore > -999.) { eop = ecore/mom; }
      
      float dep = calculate_dep(armsect, charge,  pt, eop);

      sngltrk->set_dep(dep);

      if (fillhistos)
	{
	  if(pt > 0.5)
	    {
	      if(sngltrk->get_n0() > 1) // only electron candidates 
		{
		  if(charge>0) hdep_pt_p->Fill(pt,dep);
		  else if(charge<0) hdep_pt_e->Fill(pt,dep);
		}
	      if(sngltrk->get_sn0() > 1) 
		{
		  if(charge>0) hsdep_pt_p->Fill(pt,dep);
		  else if(charge<0) hsdep_pt_e->Fill(pt,dep);
		}
	    }
	}
    }

  return EVENT_OK;
}


float DepRecalibrator::calculate_dep(const int armsect, const int charge, const float pt, const float ep)
{
 
  if(pt<0.5 || ep < -999. || armsect>=8 || armsect<0 || charge==0) return -9999.;  
  float mypt = pt;
  if(mypt>5.0) {mypt=5.0;}

  float mean  = 0.0;
  float sigma = 1.0;
  if(armsect==0) { mean = 1.0*depfitmean0.Eval(mypt); sigma = depfitwidth0.Eval(mypt); }
  if(armsect==1) { mean = 1.0*depfitmean1.Eval(mypt); sigma = depfitwidth1.Eval(mypt); }
  if(armsect==2) { mean = 1.0*depfitmean2.Eval(mypt); sigma = depfitwidth2.Eval(mypt); }
  if(armsect==3) { mean = 1.0*depfitmean3.Eval(mypt); sigma = depfitwidth3.Eval(mypt); }
  if(armsect==4) { mean = 1.0*depfitmean4.Eval(mypt); sigma = depfitwidth4.Eval(mypt); }
  if(armsect==5) { mean = 1.0*depfitmean5.Eval(mypt); sigma = depfitwidth5.Eval(mypt); }
  if(armsect==6) { mean = 1.0*depfitmean6.Eval(mypt); sigma = depfitwidth6.Eval(mypt); }
  if(armsect==7) { mean = 1.0*depfitmean7.Eval(mypt); sigma = depfitwidth7.Eval(mypt); }

  return (ep-mean)/sigma;

}



