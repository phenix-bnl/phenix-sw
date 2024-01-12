#include "MomChangeRecalReco.h"

#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankList.hh>
#include <PdbCalBank.hh>
#include <PdbParameter.hh>
#include <RunToTime.hh>
#include <RunHeader.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <getClass.h>
#include <Fun4AllServer.h>

#include <TH1.h>
#include <TProfile.h>

#include <cstdlib>
#include <iostream>
#include <memory>

using namespace std;

// PLEASE: use definitions from RunNumberRanges.h for run numbers. We can always add to them but we 
// want to avoid hardcoded runnumbers as much as possible


MomChangeRecalReco::MomChangeRecalReco(const string &name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  db            = 0;
  XOffsetW      = 0.;
  XOffsetE      = 0.;

  YOffsetW      = 0.;
  YOffsetE      = 0.;

  YOffset       = 0.;
  XOffset       = 0.;

  ScaleFactor   = 1.;
  ScaleFactorE   = 1.;
  ScaleFactorW   = 1.;

  databaseName  = "calibtestmomchanger";
  databaseName2 = "calibtestmomScaleFactor";
}

int MomChangeRecalReco::isValidRun(const int runno) const
{
  if (runno > (int) BEGIN_OF_RUN4 && runno < (int) BEGIN_OF_RUN5 )
    {
      return 1;
    }

  else if (runno > (int) BEGIN_OF_RUN5 && runno < (int) BEGIN_OF_RUN5_CUCU_62GEV)
    {
      return 1; //run5 CuCu 200GeV
    }

  else if (runno > (int) BEGIN_OF_RUN5_pp && runno < (int) BEGIN_OF_RUN7 )
    {
      return 1; //run5/6 pp
    }

  else if (runno >  (int) BEGIN_OF_RUN7 && runno < (int) BEGIN_OF_RUN8)
    {
      return 1;//run7AuAu200GeV
    }

  else if (runno >= (int) BEGIN_OF_RUN8 && runno < (int) BEGIN_OF_RUN10)
    {
      return 1;  // run8 pp and dAu 200 GeV, run9 500 and 200 GeV pp
    }

  else if (runno >= (int)BEGIN_OF_RUN10 && runno <= (int) BEGIN_OF_RUN10_62GEV)
    {
      return 1;  // run10 200GeV
    }
  else if(runno >= (int)BEGIN_OF_RUN12_PP200 && runno <= (int)END_OF_RUN12_PP200 )//added by jpperry
    {
      return 1;//run 12 pp200
    }
  else if(runno >= (int)BEGIN_OF_RUN12_PP510 && runno <= (int)END_OF_RUN12_PP510 )//added by amaresh
    {
      return 1;//run 12 pp510
    }
  else if(runno >= (int)BEGIN_OF_RUN13PP510 && runno <= (int)END_OF_RUN13PP510 ) // Sasha Lebedev 09/20/2013
    {
      return 1; // run13pp510
    }
  else if(runno >= (int)BEGIN_OF_RUN14AUAU200 && runno <= (int)END_OF_RUN14AUAU200 ) // Wenqing Fan 11/10/2018
    {
      return 1; // run14AuAu200
    }  
  else if(runno >= 421707 && runno <= 432008 ) // Roli Esha 04/10/2019
    {
      return 1; // run15pp200
    }
  else if(runno >= 432637 && runno <= 436647 ) // Roli Esha 04/10/2019
    {
      return 1; // run15pAu200
    }
  else if(runno >= 454774  && runno <= 455639 ) // Veronica Canoa 06/12/2019
    {
      return 1; // run16dAu200
    }
  else if(runno >= 455792  && runno <= 456283 ) // Veronica Canoa 07/08/2019
    {
      return 1; // run16dAu62
    }
    else if(runno >= 372402  && runno <= 377310 ) // Roli Esha 04/21/2021
    {
      return 1; // run12CuAu200
    }

  return 0;
}
//=================================================================
int MomChangeRecalReco::Init(PHCompositeNode *topNode)
{
  if (fillhistos)
    {
      Fun4AllServer *se = Fun4AllServer::instance();
      string Histoname = Name();
      Histoname += "_momorig";
      string nodename(topNode->getName().getString());
      Histoname += nodename;
      momold = new TH1F(Histoname.c_str(), "original momentum", 200, 0, 10);
      se->registerHisto(momold);

      Histoname = Name();
      Histoname += "_momnew";
      Histoname += nodename;
      momnew = new TH1F(Histoname.c_str(), "modified momentum", 200, 0, 10);
      se->registerHisto(momnew);

      Histoname = Name();
      Histoname += "_momdiff";
      Histoname += nodename;
      momdiff = new TH1F(Histoname.c_str(), "delta mom", 200, -10, 10);
      se->registerHisto(momdiff);

      Histoname = Name();
      Histoname += "_momprof";
      Histoname += nodename;
      momprof = new TProfile(Histoname.c_str(), "delta mom/mom vs mom", 200, 0, 10, -10, 10);
      se->registerHisto(momprof);

      Histoname = Name();
      Histoname += "_phi0orig";
      Histoname += nodename;
      phi0old = new TH1F(Histoname.c_str(), "original phi0", 200, -1, 4.2);
      se->registerHisto(phi0old);

      Histoname = Name();
      Histoname += "_phi0new";
      Histoname += nodename;
      phi0new = new TH1F(Histoname.c_str(), "recal phi0", 200, -1, 4.2);
      se->registerHisto(phi0new);

      Histoname = Name();
      Histoname += "_phi0diff";
      Histoname += nodename;
      phi0diff = new TH1F(Histoname.c_str(), "original-recal phi0", 200, -0.0012, 0.0012);
      se->registerHisto(phi0diff);

      Histoname = Name();
      Histoname += "_phi0prof";
      Histoname += nodename;
      phi0prof = new TProfile(Histoname.c_str(), "delta phi0 vs mom", 200, 0, 10, -1, 1);
      se->registerHisto(phi0prof);

    }
  return 0;
}
//===========================================================
int MomChangeRecalReco::InitRun(PHCompositeNode *topNode)
{

  RunHeader * d_runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");
  runnumber = d_runhdr->get_RunNumber();
  if (!d_runhdr)
    {
      cout << PHWHERE << "MomChangeRecalReco:: runhdr not in Node Tree" << endl;
      cout << PHWHERE << "You get zeroes for your Generic Recalibrations" << endl;
      return -1;
    }

  if (db == 0)
    {
      fetch(runnumber);
      fetchScaleFactor(runnumber);
    }

  return 0;
}
//======================================================================
int MomChangeRecalReco::process_event(PHCompositeNode *topNode)
{

  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  RunHeader *d_runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");
  runnumber = d_runhdr->get_RunNumber();

  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
        {

          PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
          int   charge                = sngltrk->get_charge();
          int   quality               = sngltrk->get_quality();
          float mom                   = sngltrk->get_mom ();
          float oldmom                = mom;
          float the0                  = sngltrk->get_the0 ();
          float pt                    = mom;
          if (the0 > -999)
            {
              pt = mom * sin(the0);
            }

          float  zed       = sngltrk->get_zed ();
          float  alpha     = sngltrk->get_alpha ();
          float  phi       = sngltrk->get_phi ();
          float  phi0      = sngltrk->get_phi0 ();
          int    arm       = sngltrk->get_dcarm();
          float  newalpha  = new_alpha(alpha, phi, arm, runnumber);
          float  del_alpha = newalpha - alpha;

          //  NOTE:  pt ~ Kq/alpha
          //         pt'~ Kq/newalpha
          //         pt'= pt*alpha/newalpha
          float newpt = pt;
          if (newalpha!=0) // new_alpha returns 0 if it doesn't know how to recalibrate this run
      {
        newpt *= fabs(alpha / newalpha);
      }
    else
      {
        newpt = NAN;
      }
          float newphi0 = delta_phi0(del_alpha, runnumber) + phi0;

          if (alpha > -999)
            {
              sngltrk->set_alpha(newalpha);
            }

          if (phi0 > -999)
            {
              sngltrk->set_phi0(newphi0);
            }

          if (the0 > -999)
            {
              mom = newpt / sin(the0);
            }

    if (runnumber >= (int) BEGIN_OF_RUN7 && runnumber <= (int) BEGIN_OF_RUN8)//run7AuAu200GeV
      {
        if (arm == 1)
    {
      mom = ScaleFactorW * mom;//west
    }
        else if (arm == 0)
    {
      mom = ScaleFactorE * mom;//east
    }
        else
    {
      cout << PHWHERE << " Invalid arm " << arm << endl;
      exit(1);
    }
            }
          else if(runnumber >= (int) BEGIN_OF_RUN13PP510 && runnumber <= (int) END_OF_RUN13PP510) // run13pp510
            {
              float scalefactor = 3096.88/(ScaleFactor0 + ScaleFactor1*newpt);
              if(scalefactor>1.0) {scalefactor=1.0;}
              mom = scalefactor * mom;
            }
          else
            {
              mom = ScaleFactor * mom;
            }

          sngltrk->set_mom(mom);
          if (fillhistos)
            {
        phi0old->Fill(phi0);
        phi0new->Fill(newphi0);
              momold->Fill(oldmom);
              momnew->Fill(mom);
            }
          if (mom < 0 and verbosity!=0)
            {
              cout << " new mom:      " << mom
                   << " old mom:      " << oldmom
                   << " del_alpha:    " << del_alpha
                   << " alpha:        " << alpha
                   << " charge:       " << charge
                   << " zed:          " << zed
                   << " the0:         " << the0
                   << " quality:      " << quality
                   << " arm:          " << arm
                   << endl;
            }
          if (fillhistos && oldmom < 10)
            {
              phi0diff->Fill(phi0-newphi0);
              phi0prof->Fill(mom,phi0-newphi0);
              momdiff->Fill(oldmom - mom);
              momprof->Fill(oldmom, (oldmom - mom) / oldmom);
            }

          //Check whether the charge sign of the particle has changed...
          if ( (alpha > 0 && newalpha < 0) ||
               (alpha<0 && newalpha>0) )
            {
              sngltrk->set_charge(-charge);
            }

        }

    }
  return 0;

}

//======New_alpha: just calculates the alpha offset based on the Xoffs and Yoff-
//------new TF1("alphafit","[0]*(sin(x)/220.0)+[1]*(cos(x)/220.)");
//------In Phenix convention: East arm=0, Westarm=1.
//======Note that the Emcal is exatly the opposite
float  MomChangeRecalReco::new_alpha(const float alpha, const float phi, const int arm, const int runnumber)
{

  float xp = sin(phi);
  float yp = cos(phi);

  if(runnumber >= 372402 && runnumber <= 377310 ) // Roli Esha 04/21/2021
    {
      if(arm == 0)//east
        {
    AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
    return (alpha - AlphaOffsetE);
        }
      else if(arm == 1)//west
        {
    AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
    return (alpha - AlphaOffsetW);
        }
    }// end run12CuAu200

 if(runnumber >= 455792 && runnumber <= 456283 ) // Veronica Canoa 07/08/2019
    {
      if(arm == 0)//east
        {
    AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
    return (alpha - AlphaOffsetE);
        }
      else if(arm == 1)//west
        {
    AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
    return (alpha - AlphaOffsetW);
        }
    }// end run16dAu62 (same as 200 GeV)

 if(runnumber >= 454774 && runnumber <= 455639 ) // Veronica Canoa 06/12/2019
    {
      if(arm == 0)//east
        {
    AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
    return (alpha - AlphaOffsetE);
        }
      else if(arm == 1)//west
        {
    AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
    return (alpha - AlphaOffsetW);
        }
    }// end run16dAu200

  if(runnumber >= 421707 && runnumber <= 432008) // Roli Esha 04/10/2019
    {
      if(arm == 0)//east
        {
    AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
    return (alpha - AlphaOffsetE);
        }
      else if(arm == 1)//west
        {
    AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
    return (alpha - AlphaOffsetW);
        }
    }// end run15pp200
    
  if(runnumber >= 432637 && runnumber <= 436647) // Roli Esha 04/10/2019
    {
      if(arm == 0)//east
        {
    AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
    return (alpha - AlphaOffsetE);
        }
      else if(arm == 1)//west
        {
    AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
    return (alpha - AlphaOffsetW);
        }
    }// end run15pAu200


  if(runnumber >= (int)BEGIN_OF_RUN14AUAU200 && runnumber <= (int)END_OF_RUN14AUAU200) // Wenqing Fan 11/10/2018
    {
      if(arm == 0)//east
        {
          AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
          return (alpha - AlphaOffsetE);
        }
      else if(arm == 1)//west
        {
          AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
          return (alpha - AlphaOffsetW);
        }
    }// end run14AuAu200

  if(runnumber >= BEGIN_OF_RUN13PP510 && runnumber <= END_OF_RUN13PP510) // Sasha Lebedev09/20/2013
    { // Sookhyun Lee 02/24/2018  use correct formula : '-' sign in front of YOffset 
      // 04/02/2018 revert the sign to use values on database 
      if(arm == 0)//east
        {
          AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
          return (alpha - AlphaOffsetE);
        }
      else if(arm == 1)//west
        {
          AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
          return (alpha - AlphaOffsetW);
        }
    }// end run13pp510

  if(runnumber >= BEGIN_OF_RUN12_PP510 && runnumber <= END_OF_RUN12_PP510)//run12 pp510; added by amaresh, May 2013
    {
      if(arm == 0)//east
  {
    AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
    return (alpha - AlphaOffsetE);
  }
      else if(arm == 1)//west
  {
    AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
    return (alpha - AlphaOffsetW);
  }
    }//run12 pp 510 GeV
  else if(runnumber >= BEGIN_OF_RUN12_PP200 && runnumber <= END_OF_RUN12_PP200)//run12 pp200; added by jpperry, June 2013
    {
      if(arm == 0)//east
  {
    AlphaOffsetE = (XOffsetE * xp/220. + YOffsetE * yp/220.);
    return (alpha - AlphaOffsetE);
  }
      else if(arm == 1)//west
  {
    AlphaOffsetW = (XOffsetW * xp/220. + YOffsetW * yp/220.);
    return (alpha - AlphaOffsetW);
  }
    }//run12 pp 200 GeV
  else if (runnumber >= BEGIN_OF_RUN10 && runnumber < BEGIN_OF_RUN10_62GEV)//run10 at 200GeV
    {
      if (arm == 0)
        {
          AlphaOffsetE = (XOffsetE * xp / 220. + YOffsetE * yp / 220.);
          return (alpha - AlphaOffsetE);
        }
      else if (arm == 1)
        {
          AlphaOffsetW = (XOffsetW * xp / 220. + YOffsetW * yp / 220.);
          return (alpha - AlphaOffsetW);
        }

    }//run10 200 GeV

  else if (runnumber >= (int) BEGIN_OF_RUN9 && runnumber < (int) BEGIN_OF_RUN10)
    // Use separate E and W offsets for bothe X and Y.
    // Formula corrected : negative sign in front of YOffset.  
    // modified by Sookhyun Lee 
    {
      if (arm == 0)
        {
          AlphaOffsetE = (XOffsetE * xp / 220. - YOffsetE * yp / 220.);
          return (alpha - AlphaOffsetE);
        }
      else if (arm == 1)
        {
          AlphaOffsetW = (XOffsetW * xp / 220. - YOffsetW * yp / 220.);
          return (alpha - AlphaOffsetW);
        }
    }//run9

  else if (runnumber >= (int) BEGIN_OF_RUN7 && runnumber < (int) BEGIN_OF_RUN9)
    // Use separate E and W offsets for both X and Y.
    // modified by Ruizhe Yang for run8, by C. Aidala for run9
    // Run 9 taken out of this loop to use correct formula by Sookhyun Lee 
    {
      if (arm == 0)
        {
          AlphaOffsetE = (XOffsetE * xp / 220. + YOffsetE * yp / 220.);
          return (alpha - AlphaOffsetE);
        }
      else if (arm == 1)
        {
          AlphaOffsetW = (XOffsetW * xp / 220. + YOffsetW * yp / 220.);
          return (alpha - AlphaOffsetW);
        }
    }//run8

  else if (runnumber > (int) BEGIN_OF_RUN6 && runnumber < (int) BEGIN_OF_RUN7)
    {
      if (arm == 0)
        {
          AlphaOffsetE = (XOffsetE * xp / 220. + YOffset * yp / 220.);
          return (alpha - AlphaOffsetE);
        }
      else if (arm == 1)
        {
          AlphaOffsetW = (XOffsetW * xp / 220. + YOffset * yp / 220.);
          return (alpha - AlphaOffsetW);
        }
    }//run6pp200

  else if (runnumber < (int) BEGIN_OF_RUN6 )
    {
      AlphaOffset = (XOffset * xp / 220. + YOffset * yp / 220.);
      return (alpha - AlphaOffset);
    }//before the run6
  return 0;
}

//=====================
float MomChangeRecalReco::delta_phi0(const float del_alpha, const int runnumber)
{
  if (runnumber > BEGIN_OF_RUN9_200GEV && runnumber < BEGIN_OF_RUN11)
    {
      return 0.756*del_alpha;
    }
  else if ( runnumber > BEGIN_OF_RUN7  && runnumber < BEGIN_OF_RUN8)
    {
      if (runnumber > BEGIN_OF_RUN7_PLUSPLUS && runnumber < END_OF_RUN7_PLUSPLUS ) // short ++ field running in Run7
  {
           return 2.0195*del_alpha;
  }
      return 0.756*del_alpha;
    }
  else if (runnumber > BEGIN_OF_RUN9 && runnumber < END_OF_RUN9_PLUSMINUS) // first part of Run9 at +/- field
    {
      return 0.756*del_alpha;
    }
  return 2.0195*del_alpha;

  //this number comes from the slope of phi0-phi vs alpha
  //only the change in phi0 contributes to the slope since phi
  //and alpha are orthognal.  The run7 number is for the +/-, -/+
  //fields and the other number is valid for ++, -- fields
  // Chris Pinkenburg: This is a very optimistic view of dealing with a 2d mapping. The real distribution looks
  // more like a flattish figure 8 and using the fitted slope gives a distorted representation of this. Also
  // the +/- slope is not reproducible (tried 2 runs and got 0.770 and 0.765). Luckily it
  // doesn't change a damn thing, this correction is tiny (0.09 degrees) and being systematically off by 5% 
  // is hopefully no problem.
}

//===========Update Offsets=====

void  MomChangeRecalReco::update(const int runnumber, const int beginrun, const int endrun)

{ // this updates the slave calibrations db by default
  // (ie phnxdbrcf1.rcf.bnl.gov )
  // you can see the slave with isql calibrations
  // or look up the db table name by more-ing /opt/phenix/etc/odbc.ini
  // (as of 8/25/06 calibrations_phnxdbrcf1_A )
  // to update the master calibrations db you need to
  // check the wiki
  //
  //  These are a so-called "parameter bank" meaning
  //  that it is essentially a flat set of named numbers.
  //  I will spice it up to allow for as follows:
  //
  //  h  /  scheme  == 1 now now, can indicate new scheme later
  //  d  \  entries == 2
  //
  //  b   /
  //  d   |   XOffset
  //  y   \   YOffset
  //
  //

  //--- Run-07, XOffset Yoffset separated into West and East: XOffsetW, XOffsetE and YOffsetW, YOffsetE
  //--- Run-06, XOffset separated into West and East: XOffsetW, XOffsetE
  //----Run-05 and earlier are not separated

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  //---The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      ts    = runTime->getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application  = bankManager->getApplication();

  if (!application->startUpdate())
    {
      PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  //-------------------------------
  //-------Run8 or Run9, bankID3
  //-------------------------------
  //if (runnumber >= (int) BEGIN_OF_RUN8 && runnumber <= 310454 ) // run8pp, Ruizhe Yang, run8dAu Nathan Grau, run9pp Christine Aidala and Sookhyun Lee
                                                                //run10auau200gev Shawn Whitaker
  if (runnumber >= (int) BEGIN_OF_RUN8 && runnumber <= 456283) // run8pp, Ruizhe Yang, run8dAu Nathan Grau, run9pp Christine Aidala and Sookhyun Lee, run10auau200gev Shawn Whitaker, run12pp510 Amaresh Datta and Inseok Yoon, run12pp200 Josh Perry, run15pp200, Run15pAu200 Roli Esha, Run16dAu200 and Run16dAu62 Veronica Canoa
    {
      char descrip[60];
      
      if(runnumber >=372402  && runnumber <= 377310 ) // Roli Esha 04/21/2021
        {
    cout << "Updating Database for Run12CuAu200 Dch offsets. " << endl;
    cout << "  run range: " << beginrun << " " << endrun << endl;
    cout << "  TimeStamp range: "; Tstart.print(); cout << " "; Tstop.print(); cout << endl;
    sprintf(descrip, "Offsets for Run12 Cu+Au at 200 GeV");
        }

      if(runnumber >=455792  && runnumber <= 456283 ) // Veronica Canoa 07/08/2019
        {
    cout << "Updating Database for Run16dAu62 Dch offsets. " << endl;
    cout << "  run range: " << beginrun << " " << endrun << endl;
    cout << "  TimeStamp range: "; Tstart.print(); cout << " "; Tstop.print(); cout << endl;
    sprintf(descrip, "Offsets for Run16 d+Au at 62 GeV");
        }
      
      if(runnumber >=454774  && runnumber <= 455639 ) // Veronica Canoa 06/12/2019
        {
    cout << "Updating Database for Run16dAu200 Dch offsets. " << endl;
    cout << "  run range: " << beginrun << " " << endrun << endl;
    cout << "  TimeStamp range: "; Tstart.print(); cout << " "; Tstop.print(); cout << endl;
    sprintf(descrip, "Offsets for Run16 d+Au at 200 GeV");
        }

      if(runnumber >= 421707 && runnumber <= 432008) // Roli Esha 04/10/2019
        {
    cout << "Updating Database for Run15pp200 Dch offsets. " << endl;
    cout << "  run range: " << beginrun << " " << endrun << endl;
    cout << "  TimeStamp range: "; Tstart.print(); cout << " "; Tstop.print(); cout << endl;
    sprintf(descrip, "Offsets for Run15 p+p at 200 GeV");
        }
        
      if(runnumber >= 432637 && runnumber <= 436647) // Roli Esha 04/10/2019
        {
    cout << "Updating Database for Run15pAu200 Dch offsets. " << endl;
    cout << "  run range: " << beginrun << " " << endrun << endl;
    cout << "  TimeStamp range: "; Tstart.print(); cout << " "; Tstop.print(); cout << endl;
    sprintf(descrip, "Offsets for Run15 p+Au at 200 GeV");
        }


      if(runnumber >= (int)BEGIN_OF_RUN14AUAU200 && runnumber <= (int)END_OF_RUN14AUAU200) // Wenqing Fan 11/10/2018 
        {
          cout << "Updating Database for Run14AuAu200 Dch offsets. " << endl;
          cout << "  run range: " << beginrun << " " << endrun << endl;
          cout << "  TimeStamp range: "; Tstart.print(); cout << " "; Tstop.print(); cout << endl;
          sprintf(descrip, "Offsets for Run14 Au+Au at 200 GeV");
        }

      if(runnumber >= (int)BEGIN_OF_RUN13PP510 && runnumber <= (int)END_OF_RUN13PP510) // Sasha Lebedev (lebedev@iastate.edu) 09/20/2013 
        {
          cout << "Updating Database for run13pp510 Dch offsets. " << endl;
          cout << "  run range: " << beginrun << " " << endrun << endl;
          cout << "  TimeStamp range: "; Tstart.print(); cout << " "; Tstop.print(); cout << endl;
          sprintf(descrip, "Offsets for Run13 p+p at 510 GeV");
        }

      if(runnumber >= (int)BEGIN_OF_RUN12_PP510 && runnumber <= (int)END_OF_RUN12_PP510)//amaresh, May 2013
  {
    cout << "This is run 12 pp at 510 GeV : " << runnumber << endl;
    sprintf(descrip, "Entries for Run12 pp at 510 GeV based on zero-fied data");
  }
      if(runnumber >= (int)BEGIN_OF_RUN12_PP200 && runnumber <= (int)END_OF_RUN12_PP200)//jpperry, June 2013
  {
    cout << "This is run 12 pp at 200 GeV : " << runnumber << endl;
    sprintf(descrip, "Entries for Run12 pp at 200 GeV based on zero-fied data");
  }

      if (runnumber >= (int)BEGIN_OF_RUN10 && runnumber < (int)BEGIN_OF_RUN10_62GEV) //updated to '< BEGINOF_RUN10_62GEV' from '<= 310454' to ensure it's for run10 pp200; amaresh
  {
    cout << "This is run 10 at 200 GeV: " << runnumber << endl;
    sprintf(descrip,"Entries for Run10 at 200 GeV based on zero-field data");
  }

      if (runnumber >= (int) BEGIN_OF_RUN9 && runnumber < (int)BEGIN_OF_RUN10) {
  cout << "This is run 9 : " << runnumber << endl;  
  sprintf(descrip,"Entries for Run9 based on zero-field data"); 
      }
      else if (runnumber >= (int) BEGIN_OF_RUN8 && runnumber < (int)BEGIN_OF_RUN9) {
  cout << "This is run 8 : " << runnumber << endl;
  sprintf(descrip,"These are the entries for RUN-8--Testing");
      }

      //Make a bank ID.  bankID3=separate West and East XOffsets Yoffsets

      PdbBankID bankID;
      bankID.setInternalValue(3);


      PdbCalBank *momrecalBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop,
               databaseName.c_str());

      int length = 4 + 2; //arrays XOffsetW, XOffsetE, YOffsetW, YoffsetE + 2 header val.
      momrecalBank->setLength(length);

      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( XOffsetE );
      parameter->setName("XOffsetE");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( XOffsetW );
      parameter->setName("XOffsetW");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( YOffsetE );
      parameter->setName("YOffsetE");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( YOffsetW );
      parameter->setName("YOffsetW");
      application->commit(); //must be put here or the update does not work@ shengli

    }//-----(run >= BEGIN_OF_RUN8 && run <=  (int)END_OF_RUN14AUAU200)

  //-------------------------------
  //-------Run7 and bankID3
  //-------------------------------
  if (runnumber >= (int) BEGIN_OF_RUN7 && runnumber <= 241000 ) //put run7 by shengli.huang@vanderbilt.edu
    {

      cout << "This is run 7 : " << runnumber << endl;
      //Make a bank ID.  bankID3=separate West and East XOffsets Yoffsets

      PdbBankID bankID;
      bankID.setInternalValue(3);

      const char *descrip = "These are the entries for RUN-7--Testing";

      PdbCalBank *momrecalBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop,
               databaseName.c_str());

      int length = 4 + 2; //arrays XOffsetW, XOffsetE, YOffsetW, YoffsetE + 2 header val.
      momrecalBank->setLength(length);

      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( XOffsetE );
      parameter->setName("XOffsetE");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( XOffsetW );
      parameter->setName("XOffsetW");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( YOffsetE );
      parameter->setName("YOffsetE");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( YOffsetW );
      parameter->setName("YOffsetW");
      application->commit(); //must be put here or the update does not work@ shengli

    }//-----(run > BEGIN_OF_RUN7 && run<  BEGIN_OF_RUN9

  if (runnumber >= (int) BEGIN_OF_RUN6 && runnumber < (int) BEGIN_OF_RUN7 )
    {

      cout << "This is run 6 : " << runnumber << endl;

      //Make a bank ID.  bankID2=separate West and East XOffsets
      PdbBankID bankID;
      bankID.setInternalValue(2);

      const char *descrip = "These are the entries for RUN-6";

      PdbCalBank *momrecalBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop,
               databaseName.c_str());
      int length = 3 + 2; //arrays XOffsetW, XOffsetE, YOffset + 2 header val.
      momrecalBank->setLength(length);

      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( XOffsetE );
      parameter->setName("XOffsetE");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( XOffsetW );
      parameter->setName("XOffsetW");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( YOffset );
      parameter->setName("YOffset");

      application->commit(); //must be put here or the update does not work@ shengli

    }//-----(run > BEGIN_OF_RUN6 && run<  BEGIN_OF_RUN7

  //-------------------------------
  //-------Run5 and earlier= bankID1
  //-------------------------------

  if (runnumber < (int) BEGIN_OF_RUN6)
    {
      cout << "this is run5 and earlier" << beginrun << endl;

      PdbBankID bankID;
      bankID.setInternalValue(1);
      const char *descrip = "Single XOffsets --Testing";

      PdbCalBank *momrecalBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop,
               databaseName.c_str());

      int length = 2 + 2; //arrays XOffset, YOffset + 2 header val.
      momrecalBank->setLength(length);

      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( XOffset );
      parameter->setName("XOffset");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( YOffset );

      application->commit(); //must be put here or the update does not work@ shengli

    }//run5 and earlier


}//Updating slave calibrations

//=============Update Scale Factor=========
void MomChangeRecalReco::updateScaleFactor(const int runnumber, const int beginrun, const int endrun)
{

  /*
    These are a so-called "parameter bank" meaning
    that it is essentially a flat set of named numbers.
    I will spice it up to allow for as follows:

    h  /  scheme  == 1 now now, can indicate new scheme later
    d  \  entries == 2

    b   /
    d   |   ScaleFactor
    y   \

  */
if (verbosity) {
  cout << "Staring MomChangeRecalReco::updateScaleFactor()..." << endl;
  cout << "  for run range: " << beginrun << " " << endrun << endl;
}

  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  if (endrun > 0)
    {
      ts = runTime->getEndTime(endrun);
      Tstop = *ts;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

if (verbosity) {
  cout << "  TimeStamp range: "; 
  Tstart.print(); cout << " - "; Tstop.print(); cout << endl;
}

  //------another Bankid1, this time for scale
  PdbBankManager *bankManager = PdbBankManager::instance();

  PdbBankID bankID;

  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate())
    {
      PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not writable");
      application->abort();
    }

  if (runnumber < (int) BEGIN_OF_RUN7 || runnumber >= (int)BEGIN_OF_RUN8)
    {
      if(runnumber >= (int)BEGIN_OF_RUN13PP510 && runnumber <= (int)END_OF_RUN13PP510) {
        bankID.setInternalValue(3);
        const char *descrip = "Momentum scale factor for run13pp510 dataset.";
        if (verbosity) { cout << descrip << endl; }

        PdbCalBank *momrecalBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop, databaseName2.c_str());

        int length = 2 + 2; // array + 2 hdr
        momrecalBank->setLength(length);

        PdbParameter *parameter;
        int index = 0;
        parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
        parameter->setParameter(3.0);
        parameter->setName("scheme");

        parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
        parameter->setParameter(length - 2);
        parameter->setName("entries");

        parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
        parameter->setParameter( ScaleFactor0 );
        parameter->setName("ScaleFactor0");

        parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
        parameter->setParameter( ScaleFactor1 );
        parameter->setName("ScaleFactor1");
      }
      else { 
        bankID.setInternalValue(1);
        const char *descrip = "Parameters submitted by generic recal object";

        PdbCalBank *momrecalBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop,
               databaseName2.c_str());

        int length = 1 + 2; // array + 2 hdr
        momrecalBank->setLength(length);

        PdbParameter *parameter;
        int index = 0;
        parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
        parameter->setParameter(1.0);
        parameter->setName("scheme");

        parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
        parameter->setParameter(length - 2);
        parameter->setName("entries");

        parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
        parameter->setParameter( ScaleFactor );
        parameter->setName("ScaleFactor");
      }

      application->commit(); //must be put here or the update does not work@ shengli
    }

  else if (runnumber >= (int)BEGIN_OF_RUN7 && runnumber < 241000)
    {
      bankID.setInternalValue(2);
      const char *descrip = "Parameters submitted by generic recal object";

      PdbCalBank *momrecalBank = bankManager->createBank("PdbParameterBank", bankID, descrip, Tstart, Tstop,
               databaseName2.c_str());

      int length = 2 + 2; // array + 2 hdr
      momrecalBank->setLength(length);

      PdbParameter *parameter;
      int index = 0;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(1.0);
      parameter->setName("scheme");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter(length - 2);
      parameter->setName("entries");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( ScaleFactorE );
      parameter->setName("ScaleFactorE");

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      parameter->setParameter( ScaleFactorW );
      parameter->setName("ScaleFactorW");

      application->commit();//must be put here or the update does not work@ shengli
    }

}//update scale factor

//================FETCH===-
void MomChangeRecalReco::fetch(const int runnumber)
{
  /*

  h  /  scheme  == 1 now now, can indicate new scheme later
  d  \  entries == number of entries that *SHOULD* follow

  b   /   XOffset
  od  |   YOffset
  y   \

  For Run-06, XOffset separated into West and East: XOffsetW, XOffsetE

  */

  if (runnumber >= 372402 && runnumber <= 377310)
    { // Roli Esha 04/21/2021
      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();
        
      if (!application->startRead())
        {
    PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
    application->abort();
        }
        
      PdbBankID bankID;
      bankID.setInternalValue(3);
      PdbParameter *parameter;
      int index = 0;
        
      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
      if (!momrecalBank)
        {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " << runnumber << " from " << databaseName << endl;
    cout<<" do not abort session, use default values for momentum offset correction"<<endl;
    XOffsetW = 0.;
    XOffsetE = 0.;
            
    YOffsetW = 0.;
    YOffsetE = 0.;
    cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
         << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
    return;
        }
      int length = 4 + 2; // 2 header values + array of XOffsetW, XOffsetE, YOffsetw, YoffsetE
        
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
    cout << PHWHERE;
    cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
    cout << "                                            expected length: " << length << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
    cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
    cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
    exit(1);
        }
        
      // OK, ALL Checks passed...get the parameters...
        
      cout << "MomChangeRecalReco::READING from Database  Beam Shift Corrections 2 Xoff's and 2 Yoff..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetE = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetW = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetE = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetW = parameter->getParameter();
        
      cout << "This is the Beam shift Calibration for RUN 12 CU+AU at 200 GeV" <<endl;
      cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
     << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
        
      delete momrecalBank;
    }

  if (runnumber >=455792 && runnumber <=456283 )
    { // Veronica Canoa 07/08/2019
      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();
      
      if (!application->startRead())
        {
    PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
    application->abort();
        }
      
      PdbBankID bankID;
      bankID.setInternalValue(3);
      PdbParameter *parameter;
      int index = 0;
        
      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
      if (!momrecalBank)
        {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " << runnumber << " from " << databaseName << endl;
    cout<<" do not abort session, use default values for momentum offset correction"<<endl;
    XOffsetW = 0.;
    XOffsetE = 0.;
            
    YOffsetW = 0.;
    YOffsetE = 0.;
    cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
         << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
    return;
        }
      int length = 4 + 2; // 2 header values + array of XOffsetW, XOffsetE, YOffsetw, YoffsetE
        
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
    cout << PHWHERE;
    cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
    cout << "                                            expected length: " << length << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
    cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
    cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
    exit(1);
        }
      // OK, ALL Checks passed...get the parameters...
      cout << "MomChangeRecalReco::READING from Database  Beam Shift Corrections 2 Xoff's and 2 Yoff..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetE = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetW = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetE = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetW = parameter->getParameter();
        
      cout << "This is the Beam shift Calibration for RUN 16 dAu at 62 GeV" <<endl;
      cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
     << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
        
      delete momrecalBank;
    }

  if (runnumber >= 454774 && runnumber <= 455639 )
    { // Veronica Canoa 06/12/2019
      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();
        
      if (!application->startRead())
        {
    PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
    application->abort();
        }
        
      PdbBankID bankID;
      bankID.setInternalValue(3);
      PdbParameter *parameter;
      int index = 0;
        
      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
      if (!momrecalBank)
        {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " << runnumber << " from " << databaseName << endl;
    cout<<" do not abort session, use default values for momentum offset correction"<<endl;
    XOffsetW = 0.;
    XOffsetE = 0.;
            
    YOffsetW = 0.;
    YOffsetE = 0.;
    cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
         << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
    return;
        }
      int length = 4 + 2; // 2 header values + array of XOffsetW, XOffsetE, YOffsetw, YoffsetE
        
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
    cout << PHWHERE;
    cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
    cout << "                                            expected length: " << length << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
    cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
    cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
    exit(1);
        }
        
      // OK, ALL Checks passed...get the parameters...
        
      cout << "MomChangeRecalReco::READING from Database  Beam Shift Corrections 2 Xoff's and 2 Yoff..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetE = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetW = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetE = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetW = parameter->getParameter();
        
      cout << "This is the Beam shift Calibration for RUN 16 dAu at 200 GeV" <<endl;
      cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
     << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
        
      delete momrecalBank;
    }

  if (runnumber >= 421707 && runnumber <= 436647)
    { // Roli Esha 04/10/2019
      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();
        
      if (!application->startRead())
        {
    PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
    application->abort();
        }
        
      PdbBankID bankID;
      bankID.setInternalValue(3);
      PdbParameter *parameter;
      int index = 0;
        
      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
      if (!momrecalBank)
        {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " << runnumber << " from " << databaseName << endl;
    cout<<" do not abort session, use default values for momentum offset correction"<<endl;
    XOffsetW = 0.;
    XOffsetE = 0.;
            
    YOffsetW = 0.;
    YOffsetE = 0.;
    cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
         << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
    return;
        }
      int length = 4 + 2; // 2 header values + array of XOffsetW, XOffsetE, YOffsetw, YoffsetE
        
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
    cout << PHWHERE;
    cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
    cout << "                                            expected length: " << length << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
    cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
    cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
    exit(1);
        }
        
      // OK, ALL Checks passed...get the parameters...
        
      cout << "MomChangeRecalReco::READING from Database  Beam Shift Corrections 2 Xoff's and 2 Yoff..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetE = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetW = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetE = parameter->getParameter();
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetW = parameter->getParameter();
        
      cout << "This is the Beam shift Calibration for RUN 15 P+P/AU at 200 GeV" <<endl;
      cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
     << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
        
      delete momrecalBank;
    }


  if (runnumber >= (int)BEGIN_OF_RUN14AUAU200 && runnumber <= (int)END_OF_RUN14AUAU200)
  { // Wenqing Fan 12/15/2018
    PdbBankManager *bankManager = PdbBankManager::instance();
    PdbApplication *application = bankManager->getApplication();

    if (!application->startRead())
    {
      PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

    PdbBankID bankID;
    bankID.setInternalValue(3);
    PdbParameter *parameter;
    int index = 0;

    PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
    if (!momrecalBank)
    {
      cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " << runnumber << " from " << databaseName << endl;
      cout<<" do not abort session, use default values for momentum offset correction"<<endl;
      XOffsetW = 0.;
      XOffsetE = 0.;

      YOffsetW = 0.;
      YOffsetE = 0.;
      cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
         << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
      return;
    }
    int length = 4 + 2; // 2 header values + array of XOffsetW, XOffsetE, YOffsetw, YoffsetE

    int truelength = momrecalBank->getLength();
    if (length != truelength)
    {
      cout << PHWHERE;
      cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
      cout << "                                            expected length: " << length << endl;
      exit(1);
    }

    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    int scheme = (int)parameter->getParameter();
    if (scheme != 1)
    {
      cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
      exit(1);
    }

    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    int entries = (int)parameter->getParameter();
    if (entries != length - 2)
    {
      cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
      exit(1);
    }

    // OK, ALL Checks passed...get the parameters...

    cout << "MomChangeRecalReco::READING from Database  Beam Shift Corrections 2 Xoff's and 2 Yoff..." << endl;
    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    XOffsetE = parameter->getParameter();

    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    XOffsetW = parameter->getParameter();

    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    YOffsetE = parameter->getParameter();

    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    YOffsetW = parameter->getParameter();

    cout << "This is the Beam shift Calibration for RUN 14 AU+AU at 200 GeV" <<endl;
    cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
           << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;

    delete momrecalBank;
  }

  if (runnumber >= (int)BEGIN_OF_RUN8 && runnumber <= (int)END_OF_RUN13PP510)//amaresh, May,2013, Sasha Lebedev 09/23/2013
    {

      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();

      if (!application->startRead())
        {
          PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
          application->abort();
        }

      //For Run-08 and Run-09, separate XOffsets and Yoffsets for West and East; bankID = 3

      PdbBankID bankID;
      bankID.setInternalValue(3);
      PdbParameter *parameter;
      int index = 0;

      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
      if (!momrecalBank)
  {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " 
               << runnumber << " from " << databaseName << endl;
    exit(1);
  } 

      int length = 4 + 2; // 2 header values + array of XOffsetW, XOffsetE, YOffsetw, YoffsetE

      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
          cout << "                                            expected length: " << length << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
          exit(1);
        }

      // OK, ALL Checks passed...get the parameters...

      cout << "MomChangeRecalReco::READING from Database  Beam Shift Corrections 2 Xoff's and 2 Yoff..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetE = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetW = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetE = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetW = parameter->getParameter();

      if (runnumber >= (int)BEGIN_OF_RUN8 && runnumber < (int)BEGIN_OF_RUN9)
  {
    cout << "This is the Beam shift Calibration for RUN 8" << endl;
  }
      else if (runnumber >= (int)BEGIN_OF_RUN9 && runnumber < (int)BEGIN_OF_RUN10)
  {
    cout << "This is the Beam shift Calibration for RUN 9" << endl;
  }
      else if (runnumber >=(int)BEGIN_OF_RUN10 && runnumber < 310454)
  {
    cout << "This is the Beam shift Calibration for RUN 10 at 200GeV" <<endl;
  }
  else if(runnumber >= (int)BEGIN_OF_RUN12_PP200 && runnumber <= (int)END_OF_RUN12_PP200)//jpperry : June,2013
  {
    cout << "This is the Beam shift Calibration for RUN 12 P+P at 200 GeV" <<endl;
  }
      else if(runnumber >= (int)BEGIN_OF_RUN12_PP510 && runnumber <= (int)END_OF_RUN12_PP510)//amaresh : May,2013
  {
    cout << "This is the Beam shift Calibration for RUN 12 P+P at 510 GeV" <<endl;
  }
      else if(runnumber >= (int)BEGIN_OF_RUN13PP510 && runnumber <= (int)END_OF_RUN13PP510) // Sasha Lebedev 09/23/2013
        {
          cout << "This is the Beam shift Calibration for RUN 13 P+P at 510 GeV" <<endl;
        }

      if (verbosity > 0) 
  {
    cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
         << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
  }

      delete momrecalBank;
    }//run8 or run9 or run 10 at 200 GeV or run12 pp at 510 GeV


  //----fetching run7
  if (runnumber > 228000 && runnumber < 241000)   //only 228000 ~ 241000 be calibrated for run7
    {

      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();

      if (!application->startRead())
        {
          PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
          application->abort();
        }

      //For Run-07, separate XOffsets and Yoffsets for West and East; bankID = 3

      PdbBankID bankID;
      bankID.setInternalValue(3);
      PdbParameter *parameter;
      int index = 0;

      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
      if (!momrecalBank)
  {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " 
               << runnumber << " from " << databaseName << endl;
    exit(1);
  } 

      int length = 4 + 2; // 2 header values + array of XOffsetW, XOffsetE, YOffsetw, YoffsetE

      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
          cout << "                                            expected length: " << length << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
          exit(1);
        }

      // OK, ALL Checks passed...get the parameters...

      cout << "MomChangeRecalReco::READING from Database  Beam Shift Corrections 2 Xoff's and 2 Yoff..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetE = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetW = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetE = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffsetW = parameter->getParameter();

      cout << "This is the Beam shift Calibration for RUN 7!" << endl;

      if (verbosity > 0)
        {
          cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW
               << ", YOffsetE = " << YOffsetE << ", YOffsetW = " << YOffsetW << endl;
        }

      delete momrecalBank;
    }//run7

  //----fetching run6
  else if (runnumber > BEGIN_OF_RUN6 && runnumber < BEGIN_OF_RUN7)
    {

      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();

      if (!application->startRead())
        {
          PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
          application->abort();
        }
      //fetch from bankid2
      //For Run-06, separate XOffsets for West and East; bankID = 2


      PdbBankID bankID;
      bankID.setInternalValue(2);
      PdbParameter *parameter;
      int index = 0;

      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
      if (!momrecalBank)
  {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " 
               << runnumber << " from " << databaseName << endl;
    exit(1);
  } 

      int length = 3 + 2; // 2 header values + array of XOffsetW, XOffsetE, YOffset

      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
          cout << "                                            expected length: " << length << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
          exit(1);
        }

      // OK, ALL Checks passed...get the parameters...

      cout << "MomChangeRecalReco::READING from Database  Beam Shift Corrections 2 Xoff's and 1 Yoff..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetE = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffsetW = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffset = parameter->getParameter();
      cout << "This is the Beam shift Calibration for RUN 6!" << endl;
      if (verbosity > 0)
        {
          cout << " XOffsetE = " << XOffsetE << ", XOffsetW = " << XOffsetW <<  ", YOffset = " << YOffset << endl;
        }

      delete momrecalBank;
    }//run6


  //----fetching run5 and earlier bankID1

  else if (runnumber < 180000)
    {

      PdbBankManager *bankManager = PdbBankManager::instance();
      PdbApplication *application = bankManager->getApplication();

      if (!application->startRead())
        {
          PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
          application->abort();
        }


      PdbBankID bankID;
      bankID.setInternalValue(1);
      PdbParameter *parameter;
      int index = 0;

      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName.c_str(), runnumber);
      if (!momrecalBank)
  {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " 
               << runnumber << " from " << databaseName << endl;
    exit(1);
  } 

      int length = 2 + 2; //  XOffset, YOffset

      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
          cout << "                                            expected length: " << length << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
          exit(1);
        }

      // OK, ALL Checks passed...get the parameters...

      cout << "MomChangeRecalReco::READING from RUN 5 and earlier Database Beam Shift Corrections.." << endl;

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      XOffset = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      YOffset = parameter->getParameter();

      if (verbosity > 0)
        {
          cout << " XOffset = " << XOffset <<  ", YOffset = " << YOffset << endl;
        }

      delete momrecalBank;

    }//run5 and earlier
}//fetching Offsets


//=================Fetching Scale factor

void MomChangeRecalReco::fetchScaleFactor(const int runnumber)
{
  /*

  h  /  scheme  == 1 now now, can indicate new scheme later
  d  \  entries == number of entries that *SHOULD* follow

  b   /   XOffset
  od  |   YOffset
  y   \

  */


  //if (runnumber <  BEGIN_OF_RUN7 || (runnumber >= BEGIN_OF_RUN8 && runnumber <= 310454))//for run6 and earlier, Run8 dAu/p+p, Run9, Run10 200GeV
  if (runnumber <  (int)BEGIN_OF_RUN7 || (runnumber >= (int)BEGIN_OF_RUN8 && runnumber <= (int)END_OF_RUN12_PP510))//for run6 and earlier, Run8 dAu/p+p, Run9, Run10 200GeV, Run12 pp 510 GeV : updated the end of range by amaresh : May,2013; Run12pp200GeV updated by jpperry
    {
      PdbBankManager *bankManager = PdbBankManager::instance();

      PdbApplication *application = bankManager->getApplication();

      if (!application->startRead())
        {
          PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
          application->abort();
        }

      PdbBankID bankID;
      bankID.setInternalValue(1);
      PdbParameter *parameter;
      int index = 0;

      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName2.c_str(), runnumber);
      if (!momrecalBank)
  {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " 
               << runnumber << " from " << databaseName2 << endl;
    exit(1);
  } 

      int length = 1 + 2;
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
          cout << "                                            expected length: " << length << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
          exit(1);
        }


      cout << "MomChangeRecalReco::READING from Database..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactor = parameter->getParameter();

      //      if (verbosity > 0)
      {
        cout << " ScaleFactor = " << ScaleFactor << endl;
      }

      delete momrecalBank;
    }

  else if ( runnumber > 228000 && runnumber < 241000) // for run7
    {
      PdbBankManager *bankManager = PdbBankManager::instance();

      PdbApplication *application = bankManager->getApplication();

      if (!application->startRead())
        {
          PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
          application->abort();
        }

      PdbBankID bankID;
      bankID.setInternalValue(2);//bankID set to 2 for run7 due to different scale for east and west
      PdbParameter *parameter;
      int index = 0;

      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName2.c_str(), runnumber);
      if (!momrecalBank)
  {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run " 
               << runnumber << " from " << databaseName2 << endl;
    exit(1);
  } 

      int length = 2 + 2;
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
          cout << "                                            expected length: " << length << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
          cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
          exit(1);
        }


      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << "MomChangeRecalReco:: FATAL...wrong entries DB read for delta-t" << endl;
          exit(1);
        }


      cout << "MomChangeRecalReco::READING from Database..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactorE = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactorW = parameter->getParameter();
      if (verbosity > 0)
        {
          cout << " ScaleFactorE = " << ScaleFactorE << ", ScaleFactorW = " << ScaleFactorW << endl;
        }

      delete momrecalBank;
    }


  else if ( runnumber >= BEGIN_OF_RUN13PP510 && runnumber <= END_OF_RUN13PP510) // for run13pp510
    {
      PdbBankManager *bankManager = PdbBankManager::instance();

      PdbApplication *application = bankManager->getApplication();

      if (!application->startRead())
        {
          PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
          application->abort();
        }

      PdbBankID bankID;
      bankID.setInternalValue(3);//bankID set to 3 for run13pp510: non-constant scale factor
      PdbParameter *parameter;
      int index = 0;

      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName2.c_str(), runnumber);
      if (!momrecalBank)
        {
          cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run "
               << runnumber << " from " << databaseName2 << endl;
          exit(1);
        }

      int length = 2 + 2;
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
          cout << PHWHERE;
          cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
          cout << "                                            expected length: " << length << endl;
          exit(1);
        }

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 3)
        {
          cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
          exit(1);
        }


      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
          cout << "MomChangeRecalReco:: FATAL...wrong number of entries in DB." << endl;
          exit(1);
        }


      cout << "MomChangeRecalReco::READING from Database..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactor0 = parameter->getParameter();

      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactor1 = parameter->getParameter();
      if (verbosity > 0)
        {
          cout << " ScaleFactor1 = " << ScaleFactor0 << ", ScaleFactor1 = " << ScaleFactor1 << endl;
        }

      delete momrecalBank;
    }

  else if ( runnumber >= (int)BEGIN_OF_RUN14AUAU200 && runnumber <= (int)END_OF_RUN14AUAU200) // for run14AuAu200, Wenqing Fan 12/15/2018
  {
    PdbBankManager *bankManager = PdbBankManager::instance();

    PdbApplication *application = bankManager->getApplication();

    if (!application->startRead())
    {
      PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
      application->abort();
    }

    PdbBankID bankID;
    bankID.setInternalValue(1);// dummy, scale factor not yet set for Run14AuAu
    PdbParameter *parameter;
    int index = 0;

    PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName2.c_str(), runnumber);
    if (!momrecalBank)
    {
      cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run "
           << runnumber << " from " << databaseName2 << endl;
      cout<<" do not abort session, use default values for momentum scale correction"<<endl;
      ScaleFactor = 1.;
      cout << " ScaleFactor = " << ScaleFactor << endl;
      return;
    }
    int length = 1 + 2;
    int truelength = momrecalBank->getLength();
    if (length != truelength)
    {
      cout << PHWHERE;
      cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
      cout << "                                            expected length: " << length << endl;
      exit(1);
    }

    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    int scheme = (int)parameter->getParameter();
    if (scheme != 1)
    {
      cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
      exit(1);
    }

    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    int entries = (int)parameter->getParameter();
    if (entries != length - 2)
    {
      cout << "MomChangeRecalReco:: FATAL...wrong number of entries in DB." << endl;
      exit(1);
    }

    cout << "MomChangeRecalReco::READING from Database..." << endl;
    parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
    ScaleFactor = parameter->getParameter();
    cout << " ScaleFactor = " << ScaleFactor << endl;

    delete momrecalBank;
  }

  else if ( runnumber >= 421707 && runnumber <= 432008) // for run15pp200, Roli Esha 05/09/2019
    {
      PdbBankManager *bankManager = PdbBankManager::instance();
        
      PdbApplication *application = bankManager->getApplication();
        
      if (!application->startRead())
        {
    PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
    application->abort();
        }
        
      PdbBankID bankID;
      bankID.setInternalValue(1);// dummy, scale factor not yet set for Run14AuAu
      PdbParameter *parameter;
      int index = 0;
        
      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName2.c_str(), runnumber);
      if (!momrecalBank)
        {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run "
         << runnumber << " from " << databaseName2 << endl;
    cout<<" do not abort session, use default values for momentum scale correction"<<endl;
    ScaleFactor = 1.;
    cout << " ScaleFactor = " << ScaleFactor << endl;
    return;
        }
      int length = 1 + 2;
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
    cout << PHWHERE;
    cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
    cout << "                                            expected length: " << length << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
    cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
    cout << "MomChangeRecalReco:: FATAL...wrong number of entries in DB." << endl;
    exit(1);
        }
        
      cout << "MomChangeRecalReco::READING from Database..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactor = parameter->getParameter();
      cout << " ScaleFactor = " << ScaleFactor << endl;
        
      delete momrecalBank;
    }

  else if ( runnumber >= 432637 && runnumber <= 436647) // for run15pAu200, Roli Esha 05/09/2019
    {
      PdbBankManager *bankManager = PdbBankManager::instance();
        
      PdbApplication *application = bankManager->getApplication();
        
      if (!application->startRead())
        {
    PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
    application->abort();
        }
        
      PdbBankID bankID;
      bankID.setInternalValue(1);// dummy, scale factor not yet set for Run14AuAu
      PdbParameter *parameter;
      int index = 0;
        
      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName2.c_str(), runnumber);
      if (!momrecalBank)
        {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run "
         << runnumber << " from " << databaseName2 << endl;
    cout<<" do not abort session, use default values for momentum scale correction"<<endl;
    ScaleFactor = 1.;
    cout << " ScaleFactor = " << ScaleFactor << endl;
    return;
        }
      int length = 1 + 2;
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
    cout << PHWHERE;
    cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
    cout << "                                            expected length: " << length << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
    cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
    cout << "MomChangeRecalReco:: FATAL...wrong number of entries in DB." << endl;
    exit(1);
        }
        
      cout << "MomChangeRecalReco::READING from Database..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactor = parameter->getParameter();
      cout << " ScaleFactor = " << ScaleFactor << endl;
        
      delete momrecalBank;
    }
  
  else if ( runnumber >= 454774 && runnumber <= 455639) // for run16dAu200, Veronica 6/12/2019
    {
      PdbBankManager *bankManager = PdbBankManager::instance();
        
      PdbApplication *application = bankManager->getApplication();
        
      if (!application->startRead())
        {
    PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
    application->abort();
        }
        
      PdbBankID bankID;
      bankID.setInternalValue(1);// dummy, scale factor not yet set for Run16dAu
      PdbParameter *parameter;
      int index = 0;
        
      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName2.c_str(), runnumber);
      if (!momrecalBank)
        {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run "
         << runnumber << " from " << databaseName2 << endl;
    cout<<" do not abort session, use default values for momentum scale correction"<<endl;
    ScaleFactor = 1.;
    cout << " ScaleFactor = " << ScaleFactor << endl;
    return;
        }
      int length = 1 + 2;
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
    cout << PHWHERE;
    cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
    cout << "                                            expected length: " << length << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
    cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
    cout << "MomChangeRecalReco:: FATAL...wrong number of entries in DB." << endl;
    exit(1);
        }
        
      cout << "MomChangeRecalReco::READING from Database..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactor = parameter->getParameter();
      cout << " ScaleFactor = " << ScaleFactor << endl;
        
      delete momrecalBank;
    }
  
    else if ( runnumber >= 372402 && runnumber <= 377310) // for run12CuAu200, Roli Esha 04/21/2021
    {
      PdbBankManager *bankManager = PdbBankManager::instance();
        
      PdbApplication *application = bankManager->getApplication();
        
      if (!application->startRead())
        {
    PHMessage("MomChangeRecalReco::", PHError, "Aborting ... Database not readable");
    application->abort();
        }
        
      PdbBankID bankID;
      bankID.setInternalValue(1);// dummy, scale factor not yet set for Run14AuAu
      PdbParameter *parameter;
      int index = 0;
        
      PdbCalBank *momrecalBank = bankManager->fetchBank("PdbParameterBank", bankID, databaseName2.c_str(), runnumber);
      if (!momrecalBank)
        {
    cout << PHWHERE << " could not fetch bank " << bankID.getInternalValue() << " for run "
         << runnumber << " from " << databaseName2 << endl;
    cout<<" do not abort session, use default values for momentum scale correction"<<endl;
    ScaleFactor = 1.;
    cout << " ScaleFactor = " << ScaleFactor << endl;
    return;
        }
      int length = 1 + 2;
      int truelength = momrecalBank->getLength();
      if (length != truelength)
        {
    cout << PHWHERE;
    cout << "MomChangeRecalReco:: FATAL...wrong length DB read for RecalData t: " << truelength << endl;
    cout << "                                            expected length: " << length << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int scheme = (int)parameter->getParameter();
      if (scheme != 1)
        {
    cout << "MomChangeRecalReco:: FATAL...unknown scheme DB read for offsets:" << scheme << endl;
    exit(1);
        }
        
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      int entries = (int)parameter->getParameter();
      if (entries != length - 2)
        {
    cout << "MomChangeRecalReco:: FATAL...wrong number of entries in DB." << endl;
    exit(1);
        }
        
      cout << "MomChangeRecalReco::READING from Database..." << endl;
      parameter = (PdbParameter *) & momrecalBank->getEntry(index++);
      ScaleFactor = parameter->getParameter();
      cout << " ScaleFactor = " << ScaleFactor << endl;
        
      delete momrecalBank;
    }
}
