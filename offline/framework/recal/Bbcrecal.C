#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <Bbcrecal.h>

#include <PHGlobal.h>
#include <RunHeader.h>

//#include "Fun4AllReturnCodes.h"
#include <getClass.h>
#include <PHCompositeNode.h>

#include "TriggerHelper.h"

#include <iostream>


using namespace std;

Bbcrecal::Bbcrecal() : Recalibrator("Bbcrecal")
{
  //baseclasses.insert("PHGlobal");
  baseclasses.insert("PHCentralTrackv23");
  return ;
}

int Bbcrecal::isValidRun(const int runno) const
{
  if (runno < 227016 || runno > 240121)
    {
      return 0;
    }

  return 1;
}

int Bbcrecal::InitRun(PHCompositeNode *topNode)
{

  RunHeader* d_run = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(!d_run){
    cout << PHWHERE << " RunHeader not found" << endl;
    return 0;
  }

  int runnumber = d_run->get_RunNumber();

  if(227016<=runnumber && runnumber<=240121)
    {
      cout << "Bbcrecal getBbcTimeZero() function available" << endl;
    }

  return 0;
}

int Bbcrecal::process_event(PHCompositeNode *topNode)
{
  PHGlobal* d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if(!d_global){
    cout << PHWHERE << "PHGlobal not found" << endl;
    return 0;
  }
  float cent = (float)d_global->getCentrality();
  float bbct0 = d_global->getBbcTimeZero();
//    cout << bbct0 << "old  ";

  float bbct0new = bbct0-4.05555e-02
                        +1.23523e-03*cent
                        +7.47574e-05*cent*cent
                        -4.52631e-06*pow(cent, 3)
                        +1.28953e-07*pow(cent, 4)
                        -1.50628e-09*pow(cent, 5)
                        +6.07081e-12*pow(cent, 6);

  d_global->setBbcTimeZero(bbct0new);

  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  if (d_cnt)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
	{
	  PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
	  //tofw
	  int strip = sngltrk->get_striptofw();
	  if (strip >= 0 && strip < 512)
	    {
	      float ttofw  = sngltrk->get_ttofw();
	      ttofw = ttofw + bbct0 - bbct0new;
	      sngltrk->set_ttofw(ttofw);
	    }

	  //tofe
	  int slat = sngltrk->get_slat();
	  if (slat >= 0 && slat < 960)
	    {
	      float ttof  = sngltrk->get_ttof();
	      ttof = ttof + bbct0 - bbct0new;
	      sngltrk->set_ttof(ttof);
	    }

	  //emc ( pb scintirator )
	  int arm  = sngltrk->get_dcarm();
	  int sect = sngltrk->get_sect();
	  if (arm==1 || (arm==0 && sect>=2)) 
	    {
	      float temc  = sngltrk->get_temc();
	      temc = temc + bbct0 - bbct0new;
	      sngltrk->set_temc(temc);
	    }
	}
    }
//    cout << d_global->getBbcTimeZero() << "new" << endl;

  return 0;
}
