#include "MasterRecalibrator.h"
#include "Recalibrator.h"

#include <recoConsts.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>

#include <PdbBankManager.hh>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHDataNode.h>
#include <PHObject.h>
#include <PHNode.h>
#include <PHPointerList.h>
#include <PHPointerListIterator.h>

#include <phool.h>

#include <TClass.h>
#include <TDirectory.h>
#include <TSystem.h>
#include <TROOT.h>

#include <cstdlib>
#include <iostream>
#include <algorithm>

using namespace std;

MasterRecalibrator::MasterRecalibrator(const string &name): SubsysReco(name)
{
  // IMPORTANT:
  // the order of the recalibrators is determined here!!!
  // so far:
  // Run5CuCu*CentralityReco have to be called before ReactionPlaneRecalReco
  // call BunchCross first since it discards events, having it first saves
  // a bit of cpu time (no point investing cpu time for recalibrating data 
  // which will be ditched by it)
  calibratorclasses.push_back("BunchCross");
  calibratorclasses.push_back("NoiseEvents");
  // call the ReadbackCompact* before recalibrating anything else so the 
  // "normal" CNT and PWGs are present
  // PWG needs to be called before CNT
  calibratorclasses.push_back("ReadbackCompactPWG");
  calibratorclasses.push_back("ReadbackCompactCNT");
  calibratorclasses.push_back("PhCglListRecalReco"); // This has to be before any charged recalibrator
  calibratorclasses.push_back("MatchrecalReco");
  calibratorclasses.push_back("MatchrecalRecoRun5");
  calibratorclasses.push_back("MatchrecalRecoRun6");
  calibratorclasses.push_back("Matchrecal_dAu_Reco");
  calibratorclasses.push_back("Run8dAu200GeVCentralityReco");
  calibratorclasses.push_back("Run7AuAu200GeVCentralityReco");
  calibratorclasses.push_back("Run10AuAu200GeVCentralityReco");
  calibratorclasses.push_back("Run10AuAu62GeVCentralityReco");
  calibratorclasses.push_back("Run10AuAu39GeVCentralityReco");
  calibratorclasses.push_back("Run10AuAu007GeVCentralityReco");
  calibratorclasses.push_back("Run11AuAu200GeVCentralityReco");
  calibratorclasses.push_back("Run11AuAu027GeVCentralityReco");
  calibratorclasses.push_back("Run11AuAu019GeVCentralityReco");
  calibratorclasses.push_back("Run14AuAu200GeVCentralityReco");
  calibratorclasses.push_back("Run14AuAu015GeVCentralityReco");
  calibratorclasses.push_back("Run12CuAu200GeVCentralityReco");
  calibratorclasses.push_back("Run12CuAu200GeVTOF_PC3_Matching");
  calibratorclasses.push_back("Run12UU193GeVCentralityReco");
  calibratorclasses.push_back("Run14He3Au200GeVCentralityReco");
  calibratorclasses.push_back("Run15pAu200GeVCentralityReco");
  calibratorclasses.push_back("Run15pAl200GeVCentralityReco");
  calibratorclasses.push_back("Run16dAu200GeVCentralityReco");
  calibratorclasses.push_back("Run16dAu62GeVCentralityReco");
  calibratorclasses.push_back("Run16dAu20GeVCentralityReco");
  calibratorclasses.push_back("Run16dAu39GeVCentralityReco");
  calibratorclasses.push_back("Run16AuAu200GeVCentralityReco");
  calibratorclasses.push_back("Bbcrecal");  
  calibratorclasses.push_back("SvxCentralTrackRecalReco");
  calibratorclasses.push_back("SvxCentralTrackReFit");
  calibratorclasses.push_back("SvxCentralTrackFitRecal");
  // MomChangeRecalReco has to be called before TofrecalReco
  // MomChangeRecalReco has to be called after SvxCentralTrackRecalReco
  calibratorclasses.push_back("MomChangeRecalReco");
  //calibratorclasses.push_back("MomChangeRecalReco_145");
  calibratorclasses.push_back("MomChangeRecal_dAu_Reco");
  calibratorclasses.push_back("TofrecalReco");
  calibratorclasses.push_back("TofwrecalReco");
  //tofw set the dz/dphi value for run7 matching
  calibratorclasses.push_back("MatchrecalRecoRun7");
  calibratorclasses.push_back("EmcGenericDeadRecalReco");

  //Tofw PC3 EMC matching re-calibrations for run 10
  calibratorclasses.push_back("TofwMatchRecalRun10");
  calibratorclasses.push_back("MatchrecalReco62GeVRun10");
  calibratorclasses.push_back("MatchrecalReco39GeVRun10");
  calibratorclasses.push_back("MatchrecalReco7GeVRun10");

  //PC3 EMC matching re-calibrations for run 11                               
  calibratorclasses.push_back("MatchrecalReco19GeVRun11");
  calibratorclasses.push_back("MatchrecalReco27GeVRun11");

  //PC3 TOF and (EMC) matching re-calibrations for run 14 HeAu
  calibratorclasses.push_back("MatchrecalRecoRun14HeAu");

  //PC3/PC2 matching re-calibrations for run 15
  calibratorclasses.push_back("MatchrecalRecoRun15pAu");
  calibratorclasses.push_back("MatchrecalRecoRun15pAl");
  calibratorclasses.push_back("MatchrecalRecoRun15pp");


  //EMC matching re-calibrations for run 15
  calibratorclasses.push_back("Run15ppEMCMatching");
  calibratorclasses.push_back("Run15pAuEMCMatching");

  //PC3/PC2 matching re-calibrations for run 14 AuAu15s
  calibratorclasses.push_back("MatchrecalRecoRun14AuAu15GeV");    

  //PC3/PC2 matching re-calibrations for run 14 AuAu200
  calibratorclasses.push_back("Run14AuAu200PC3MatchRecal");
  calibratorclasses.push_back("Run14AuAu200PC2MatchRecal");
  calibratorclasses.push_back("Run14AuAu200EMCMatchRecal");

  //PC3/PC2 matching re-calibrations for run 16 dAu
  calibratorclasses.push_back("MatchrecalReco200GeVRun16dAu");
  calibratorclasses.push_back("MatchrecalReco62GeVRun16dAu");
  calibratorclasses.push_back("MatchrecalReco39GeVRun16dAu");
  calibratorclasses.push_back("MatchrecalReco20GeVRun16dAu");
  calibratorclasses.push_back("MatchrecalRecoRun16AuAu200GeV");

  // EmctofrecalReco has to be called before EmcGenericEScaleRecalReco
  calibratorclasses.push_back("EmctofrecalReco");
  calibratorclasses.push_back("EmcTofWalkRecalReco"); //-- temp out while fixing. R.V.
  calibratorclasses.push_back("Run5PPEmctofRecalReco");
  calibratorclasses.push_back("EmcGenericEScaleRecalReco");
  // PidrecalReco has to be called after MomChangeRecalReco and TofrecalReco
  calibratorclasses.push_back("PidrecalReco");
  calibratorclasses.push_back("Pidrecal_dAu_Reco");
  calibratorclasses.push_back("EmcPidrecalReco");
  //  calibratorclasses.push_back("EmcPidrecal_dAu_Reco");
  calibratorclasses.push_back("Run5CuCu62GeVCentralityReco");
  calibratorclasses.push_back("Run5CuCu200GeVCentralityReco");
  calibratorclasses.push_back("PercentileRecalReco");
  calibratorclasses.push_back("Run4ReactionPlaneRecalReco");
  calibratorclasses.push_back("Run4ReactionPlaneReco");
  calibratorclasses.push_back("ReactionPlaneRecalReco");
  calibratorclasses.push_back("ReactionPlaneReco");
  calibratorclasses.push_back("RpParManager");
  calibratorclasses.push_back("SvxReactionPlaneMergeReco");
  calibratorclasses.push_back("SvxReactionPlaneRecalReco");
  calibratorclasses.push_back("SvxReactionPlaneReco");
  //  calibratorclasses.push_back("EmcTrkMatchingRecalReco");
  calibratorclasses.push_back("EmcElMatchingRecalReco");
  calibratorclasses.push_back("AccrecalReco");
  calibratorclasses.push_back("TecChargeRecal");
  calibratorclasses.push_back("TRDLikeReco"); //this has to be always next TecChargeRecal
  calibratorclasses.push_back("Run56PPEmcphotontofRecalReco"); // This has to be after EmcGenericEScaleRecalReco.  
  calibratorclasses.push_back("Run8PC3MatchRecal");
  calibratorclasses.push_back("Run12pp200PC3MatchRecal");
  calibratorclasses.push_back("Run12pp200EMCMatchRecal"); 
  calibratorclasses.push_back("TofwMatchRecal"); // this has to be after TofwrecalReco
  calibratorclasses.push_back("HbdRecalReco"); 
  calibratorclasses.push_back("HbdTrkMatchingRecalReco");
  calibratorclasses.push_back("Run11AuAu200GeVDepRecal");
  calibratorclasses.push_back("DepRecalibrator");
  calibratorclasses.push_back("Run11AuAu200ElectronEMCMatching");
  calibratorclasses.push_back("Run14AuAu200ElectronEMCMatching");
  calibratorclasses.push_back("Run14AuAu200DepRecal");
  calibratorclasses.push_back("Run15pp200DepRecal");
  calibratorclasses.push_back("Run15pp200ElectronEMCMatching");
  calibratorclasses.push_back("Run15pAu200ElectronEMCMatching");
  calibratorclasses.push_back("Run15pAu200DepRecal");
  calibratorclasses.push_back("MatchrecalRecoRun12UU192");
  //calibratorclasses.push_back("Run15pAu200_DCA_BeamAngle_Recal");

  locked = 1;
  fillhistos = 1; // fill histos is default
  return ;
}

MasterRecalibrator::~MasterRecalibrator()
{
  while(recalibrator.begin() != recalibrator.end())
    {
      delete recalibrator.back();
      recalibrator.pop_back();
    }
  while(deactivated_recalibrator.begin() != deactivated_recalibrator.end())
    {
      delete deactivated_recalibrator.back();
      deactivated_recalibrator.pop_back();
    }
  return ;
}

int MasterRecalibrator::Init(PHCompositeNode *topNode)
{
  static int ifirst = 1;
  if (ifirst)
    {
      ifirst = 0;
      return 0;
    }
  return DONOTREGISTERSUBSYSTEM;
}

int MasterRecalibrator::Reset(PHCompositeNode *topNode)
{
  int iret = 0;
  vector<Recalibrator *>::iterator recaliter;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      iret += (*recaliter)->Reset(topNode);
    }
  return iret;
}

int
MasterRecalibrator::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  vector<string> result;
  int iret = 0;
  int runno = rc->get_IntFlag("RUNNUMBER");
  vector<Recalibrator *>::iterator deliter, recaliter;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      if (! (*recaliter)->isValidRun(runno))
        {
          cout << "Recalibrator " << (*recaliter)->Name()
	       << " is not valid anymore for run " << runno
	       << endl;
          deactivated_recalibrator.push_back(*recaliter);
        }
    }
  for (deliter = deactivated_recalibrator.begin(); deliter != deactivated_recalibrator.end(); deliter++)
    {
      for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
        {
          if (*deliter == *recaliter)
            {
              recalibrator.erase(recaliter);
              break;
            }
        }
    }
  vector<string>::const_iterator citer;
  set
    <string>::const_iterator biter;
  // since calibratorclasses does not change we can use the ordering
  // of it given in the ctor as the order in which the recalibrators should be called
  int iorder = 0;
  for (citer = calibratorclasses.begin(); citer != calibratorclasses.end(); citer++)
    {
      iorder++;
      // here we waste some time to make new classes, maybe later I find an elegant way
      // to reuse the classes in the recalibrator/deactivated_recalibrator vectors
      // since it is not clear here if the recalibrators from deactivated_recalibrator will
      // be reactivated I cannot exclude these guys here. If the ctors are empty there is not
      // too much time wasted here
      TClass newclass((*citer).c_str());
      Recalibrator *newcal = (Recalibrator *) newclass.New();
      if (newcal->isValidRun(runno))
        {
          for (biter = (newcal->BaseClasses())->begin(); biter != (newcal->BaseClasses())->end(); biter++)
            {
              searchNodeTree(topNode, *biter, result);
              vector<string>::const_iterator riter;
              for (riter = result.begin(); riter != result.end(); riter++)
                {
                  string recalname = newcal->Name();
                  recalname += "_";
                  recalname += *riter;
                  if (!getRecalibrator(recalname))
                    {
                      int init = 1;
                      Recalibrator *recal = getDeactivatedRecalibrator(recalname);
                      if (recal)
                        {
			  vector<Recalibrator *>::iterator iter;
                          for (iter = deactivated_recalibrator.begin();
                               iter != deactivated_recalibrator.end();
                               iter++)
                            {
                              if (*iter == recal)
                                {
                                  cout << "removing " << (*iter)->Name() << endl;
                                  deactivated_recalibrator.erase(iter);
                                  break;
                                }
			      
                            }
                        }
                      else
                        {
                          init = 0;
                          recal = (Recalibrator *) newclass.New();
                          recal->SetInputNode(*riter);
                          recal->SetBaseClass(*biter);
                          recal->Name(recalname.c_str());
                          recal->Order(iorder);
			  recal->SetMasterRecalibrator(this);
                        }
                      registerRecalibrator(recal, topNode, recal->Order(), init);
                    }
                  else
                    {
                      if (verbosity > 0)
                        {
                          cout << "Recalibrator " << recalname
			       << " already registered" << endl;
                        }
                    }
                }
              result.clear();
            }
        }
      else
        {
          if (verbosity > 0)
            {
              cout << "Calibrator " << newcal->Name() << " not valid for run "
		   << runno << endl;
            }
        }

      delete newcal;

    }

  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      iret += (*recaliter)->InitRun(topNode);
      (*recaliter)->Print();
    }
  cout << "Running recalibrators: " << endl;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      cout << "Recalibrator: " << (*recaliter)->Name() << " order "
	   <<  (*recaliter)->Order() << endl;
    }
  return iret;
}

int
MasterRecalibrator::searchNodeTree(PHCompositeNode *startNode, const string &baseclass, vector<string> &result)
{
  PHNodeIterator nodeiter(startNode);
  PHPointerListIterator<PHNode> iterat(nodeiter.ls());
  PHNode *thisNode;
  while ((thisNode = iterat()))
    {
      if ((thisNode->getType() == "PHCompositeNode"))
        {
          searchNodeTree((PHCompositeNode*)thisNode, baseclass, result); // if this is a CompositeNode do this trick again
        }
      else
        {
          if ((thisNode->getType() == "PHIODataNode"))
            {
              PHIODataNode<PHObject> *mynode = dynamic_cast <PHIODataNode<PHObject> *>(thisNode);
              if (mynode)
                {
                  PHObject *tmp = static_cast<PHObject *> (mynode->getData());
                  if (tmp->InheritsFrom(baseclass.c_str()))
                    {
                      result.push_back(thisNode->getName().getString());
                    }
                }
	      else
	        {
		  PHIODataNode<TObject> *mynode1 = dynamic_cast <PHIODataNode<TObject> *>(thisNode);
		  if(mynode1)
		    {
		       TObject *tmp1 = static_cast<TObject *> (mynode1->getData());
   		       if (tmp1->InheritsFrom(baseclass.c_str()))
       		         {
			   result.push_back(thisNode->getName().getString());
	  	         }
		    }
		}
   	    }
        }
    }
  return 0;
}



int
MasterRecalibrator::registerRecalibrator(Recalibrator *recal, PHCompositeNode *topNode, const int iorder, const int init)
{

  if (!init)
    {
      // set the root dir where the histograms are created to
      // <MasterRecalibrator name>/<recalibratorname>
      // we need to use the global gDirectory pointer for this
      string currdir = gDirectory->GetPath();
      gROOT->cd(); // set dir to top dir in memory
      if (!gDirectory->FindObject(Name()))
        {
          gDirectory->mkdir(Name());
        }
      gDirectory->cd(Name());
      if (!gDirectory->FindObject(recal->Name()))
        {
          gDirectory->mkdir(recal->Name());
        }
      gDirectory->cd(recal->Name());
      recal->FillHistos(fillhistos);
      int iret = recal->Init(topNode);
      gROOT->cd(currdir.c_str());
      if (iret)
        {
          cout << PHWHERE << " Error initializing recalibrator "
	       << recal->Name() << ", return code: " << iret << endl;
          return iret;
        }
    }
  if (verbosity > 0)
    {
      cout << "Registering Recalibrator " << recal->Name() << endl;
    }
  vector<Recalibrator *>::iterator recaliter;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      if (recal->Order() < (*recaliter)->Order())
        {
          break;
        }
    }
  recalibrator.insert(recaliter, recal);
  return 0;
}

int MasterRecalibrator::process_event(PHCompositeNode *topNode)
{
  vector<Recalibrator *>::iterator recaliter;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      int iret = (*recaliter)->process_event(topNode);
      if (iret < 0)
        {
	  if (Verbosity() > 0)
	  {
	    cout << "module " << (*recaliter)->Name() << " returned "
		 << iret << endl;
	  }
          return iret;
        }
    }
  return EVENT_OK;
}

int MasterRecalibrator::ResetEvent(PHCompositeNode *topNode)
{
  vector<Recalibrator *>::iterator recaliter;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      (*recaliter)->ResetEvent(topNode);
    }
  return 0;
}

void MasterRecalibrator::Print(const string &what) const
{
  cout << "MasterRecalibrator Object" << endl;
  cout << "Known BaseClasses: " << endl;
  vector<string>::const_iterator siter;
  for (siter = calibratorclasses.begin(); siter != calibratorclasses.end(); siter++)
    {
      cout << *siter << endl;
    }
  cout << endl << "Currently registered Recalibrators:" << endl;
  vector<Recalibrator *>::const_iterator recaliter;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      (*recaliter)->Print(what);
    }
  cout << endl << "Currently decativated Recalibrators:" << endl;
  for (recaliter = deactivated_recalibrator.begin(); recaliter != deactivated_recalibrator.end(); recaliter++)
    {
      (*recaliter)->Print(what);
    }
  return ;
}

int
MasterRecalibrator::EndRun(const int runno)
{
  vector<Recalibrator *>::iterator recaliter;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      (*recaliter)->EndRun(runno);
    }
  PdbBankManager *pb = PdbBankManager::instance();
  map<string, set<int> > banks;
  pb->GetUsedBankRids(banks);
  if (banks.size() > 0)
    {
      cout << "The following PdbCal Banks were read by PdbCal:" << endl;
      map<string, set<int> >::const_iterator bankiter;
      for (bankiter = banks.begin(); bankiter != banks.end(); bankiter++)
        {
          set<int>::const_iterator siter;
          for (siter = (bankiter->second).begin(); siter != (bankiter->second).end(); siter++)
            {
              cout << "Bank: " << bankiter->first << " rid: " << *siter << endl;
            }
        }
      cout << "End PdbCal Bank Printout" << endl;
      pb->ClearUsedBankRids();
    }
  return 0;
}

int
MasterRecalibrator::End(PHCompositeNode *topNode)
{
  vector<Recalibrator *>::iterator recaliter;
  for (recaliter = recalibrator.begin(); recaliter != recalibrator.end(); recaliter++)
    {
      (*recaliter)->End(topNode);
    }
  
  return 0;
}

int
MasterRecalibrator::UseOnly(const std::string &recalname)
{
  int iret = 0;
  if (!locked)
    {
      calibratorclasses.clear();
      calibratorclasses.push_back(recalname);
    }
  else
    {
//       cout << PHWHERE << " MasterRecalibrator is locked, no changes possible"
// 	   << endl;
      iret = -1;
    }
  return iret;
}

int
MasterRecalibrator::AddRecalibrator(const std::string &recalname)
{
  int iret = 0;
  if (!locked)
    {
      calibratorclasses.push_back(recalname);
    }
  else
    {
//       cout << PHWHERE << " MasterRecalibrator is locked, no changes possible"
// 	   << endl;
      iret = -1;
    }
  return iret;
}

int
MasterRecalibrator::RemoveRecalibrator(const std::string &recalname)
{
  if (locked)
    return -1;

  std::cout << "Removing " << recalname << " from list of recalibrators" << std::endl;
  calibratorclasses.erase(
      std::remove(calibratorclasses.begin(), calibratorclasses.end(), recalname),
      calibratorclasses.end());
  return 0;
}

Recalibrator *
MasterRecalibrator::getRecalibrator(const std::string &name) const
{
  vector<Recalibrator *>::const_iterator iter;
  for (iter = recalibrator.begin(); iter != recalibrator.end(); iter++)
    {
      if (!strcmp((*iter)->Name(), name.c_str()))
	{
	  return *iter;
	}
    }
  return 0;
}

Recalibrator *
MasterRecalibrator::getDeactivatedRecalibrator(const std::string &name) const
{
  vector<Recalibrator *>::const_iterator iter;
  for (iter = deactivated_recalibrator.begin(); iter != deactivated_recalibrator.end(); iter++)
    {
      if (!strcmp((*iter)->Name(), name.c_str()))
          {
            return *iter;
          }
      }
    return 0;
  }

void
MasterRecalibrator::instance()
{
  cout << endl << endl;
  cout << "The Master Recalibrator Scheme has changed" << endl;
  cout << "and you need to modify your macro" << endl;
  cout << endl;
  cout << "Please exchange" << endl << endl;
  cout << "MasterRecalibrator *mr = MasterRecalibrator::instance();" << endl << endl;
  cout << "by" << endl << endl;
  cout << "MasterRecalibratorManager *mr = new MasterRecalibratorManager();" << endl << endl;
  cout << "in your macro. The registering with Fun4All stays identical" << endl;
  exit(1);
}

void
MasterRecalibrator::Unlock(const int i)
{
  if (i == 0)
    {
      cout << "MasterRecalibrator is now unlocked, you take control" << endl;
    }
  locked = i;
  return;
}
