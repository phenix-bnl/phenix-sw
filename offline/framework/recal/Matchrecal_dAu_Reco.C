#include <Matchrecal_dAu_Reco.h>
#include <Fun4AllReturnCodes.h>

#include <PHSnglCentralTrack.h>
#include <getClass.h>

#include <PHCompositeNode.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>

#include <iostream>
#include <phool.h>

#include <PHCentralTrack.h>
#include <PHGlobal.h>

#include <Fun4AllServer.h>
#include <getClass.h>

#include <TH1.h>
#include <TH2.h>
using namespace std;
using namespace findNode;

Matchrecal_dAu_Reco::Matchrecal_dAu_Reco(const char* name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
}

int
Matchrecal_dAu_Reco::InitRun(PHCompositeNode *topNode)
{
  run_Burn_pc3_match = 2;
  return 0;
}

int
Matchrecal_dAu_Reco::isValidRun(const int runno) const
{
  if (runno < (int) BEGIN_OF_RUN3 || runno > (int) BEGIN_OF_RUN4)
    {
      return 0;
    }

  return 1;
}


int
Matchrecal_dAu_Reco::process_event(PHCompositeNode *topNode)
{
  // For run 3 d+Au gold data, this recaliberation
  // looks at only the sigma (not absolute) values.
  // The only change made is a shift of the pc3sdphi. 
  d_cnt = getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  if (!d_cnt)
    {
      cout << PHWHERE << "Dude, you are missing nodes!!!" << endl;
      return 0;
    }


  if (!run_Burn_pc3_match)
    {
      if (verbosity > 0)
        {
          cout << ThisName << " d+Au PC3 burning disabled" << endl;
        }
      return 0;
    }

  


  for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
      sngltrk->ShutUp();
      if (run_Burn_pc3_match == 2)
        {
          if (sngltrk->isImplemented(sngltrk->get_mom()) &&
              sngltrk->isImplemented(sngltrk->get_the0()) &&
              sngltrk->isImplemented(sngltrk->get_pc3sdphi()) &&
              sngltrk->isImplemented(sngltrk->get_charge()) &&
              sngltrk->isImplemented(sngltrk->get_dcarm()) &&
              sngltrk->isImplemented(sngltrk->get_dcside()))

            {
              if (verbosity > 0)
                {
                  cout << ThisName << " d+Au PC3 burning workable" << endl;
                }
              run_Burn_pc3_match = 1;
            }
          else
            {
              run_Burn_pc3_match = 0;
              sngltrk->ShutUp(1); // enable virtual warnings again
              if (verbosity > 0)
                {
                  cout << ThisName << " d+Au PC3 burning gave invalid" << endl;
                }
              return 0;
            }
        }

      float mom  = sngltrk->get_mom();
      float the0 = sngltrk->get_the0();
      float pt = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }
      short west     = sngltrk->get_dcarm()   ;//1 for west 0 for east
      short north    = sngltrk->get_dcside()  ;//1 for north 0 for south
      short charge   = sngltrk->get_charge()  ;
      float pc3sdphi = sngltrk->get_pc3sdphi();
    
      //make the corrections by region and charge
      if(north && west)
	{
	  if (charge>0)
	    {
	     pc3sdphi =pc3sdphi - (-1*exp(-1.61963 + (-0.891664 * pt)) + 0.48);
	    }
	  else 
	    {
	     pc3sdphi =pc3sdphi - (-1*exp(-0.287085 + (-1.25049 * pt)) + 0.35);	      
	    }
      
	}
      if(north && !west)
	{
	  if (charge>0)
	    {
	     pc3sdphi =pc3sdphi - (exp(-0.65812 + (-1.36986 * pt)) - 0.11);
	    }
	  else
	    {
	     pc3sdphi =pc3sdphi + 0.281309;
	    }
	}
      if(!north && west)
	{
	  if (charge>0)
	    {
	     pc3sdphi = pc3sdphi - 0.350219;
	    }
	  else
	    {
	     pc3sdphi = pc3sdphi - (-1*exp(-0.438291 + (-1.25049 * pt)) + 0.19);
	    }
	}
      if(!north && !west)
	{
	  if (charge>0)
	    {
	      //pc3sdphi = pc3sdphi - exp(-0.186454 + (-1.36986 * pt));
	    }
	  else
	    {
	     pc3sdphi = pc3sdphi + 0.191603;
	    }
	}

      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->ShutUp(1); 
    } 
      
  return EVENT_OK;
}
