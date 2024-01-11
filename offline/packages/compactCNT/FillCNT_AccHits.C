#include "FillCNT_AccHits.h"
#include "setIntflag.h"

#include <PHCentralTrackv24.h>
#include <PHSnglCentralTrackv24.h>

#include <Fun4AllReturnCodes.h>
#include <AccHitMapEntry.h>
#include <AccHitMap.h>
#include <PHGlobal.h>
#include <RunHeader.h>
#include <id_detector.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>
#include <recoConsts.h>

#include <cstdlib>
#include <sstream>

using namespace std;

FillCNT_AccHits::FillCNT_AccHits(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillcnt_acchits.dump");
#endif

  return;
}

int
FillCNT_AccHits::InitRun(PHCompositeNode *topNode)
{
  RunHeader *d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");
  int runnumber = d_runhdr->get_RunNumber();
  // Optionally, the analyzer can set a flag with an earlier run number
  // This accomodates runs that are produced but are not yet in the RCF database
  recoConsts *rc = recoConsts::instance();
  if( (rc->FlagExist( "compactCNTRUN" ) && rc->get_IntFlag("compactCNTRUN") > 0 ) )
    {
      runnumber = rc->get_IntFlag("compactCNTRUN");
    }
  cout << "FillCNT_AccHits using Run number " << runnumber << endl;

  return EVENT_OK;
}


int
FillCNT_AccHits::process_event(PHCompositeNode *topNode)
{
  AccHitMap *accmap;
  accmap  = findNode::getClass<AccHitMap>(topNode, "AccHit_comp");
  if (!accmap)
    {
      return EVENT_OK;
    }


  PHCentralTrackv24 *cnt = findNode::getClass<PHCentralTrackv24>(topNode, "PHCentralTrack");
  if (!cnt)
    {
      cout << PHWHERE << "Failed to find PHCentralTrack, quit!" << endl;
      exit(1);
    }

  // Get the list of acc hits 

  unsigned int nacchits = accmap->GetNentries();

#ifdef DUMP
  dumpfile << "PHCentralTrackv24 has " << cnt->get_npart() << " tracks " << endl;
  dumpfile << "accmap has Nentries " << nacchits << endl;
#endif
  
  if(nacchits < 1) 
    {
      return EVENT_OK;
    }

  // Get the acc clusters and print them to the dumpfile

  for(unsigned int i=0;i<nacchits;i++)
    {
      const AccHitMapEntry *acchit = accmap->GetHit(i);
      if(acchit)
	{  

#ifdef DUMP
	  int hitid = acchit->get_hitid();
	  int hitconfig = acchit->get_hitconfig();
	  dumpfile << "Acc Hits for this event: " << endl
		   << " cluster id " << i
		   << " hitid " << hitid
		   << " hitconfig " << hitconfig
		   << endl;
#endif

	  // float ph1[4];
	  // float ph2[4];
	  // float t1[4];
	  // float t2[4];
	  for(int ibox=0;ibox<4;ibox++)
	    {
	      // ph1[ibox] = acchit->get_ph1(ibox);
	      // ph2[ibox] = acchit->get_ph2(ibox);
	      // t1[ibox] = acchit->get_t1(ibox);
	      // t2[ibox] = acchit->get_t2(ibox);

#ifdef DUMP
	      dumpfile << "  ibox " << ibox
		       << " ph1 " << ph1[ibox]
		       << " ph2 " << ph2[ibox]
		       << " t1 " << t1[ibox]
		       << " t2 " << t2[ibox]
		       << endl;
#endif
	    }
	}
      else
	{
	  cout << PHWHERE << " AccHitMap Hit " << i << " does not have a valid AccHitMapEntry - Quit!" << endl; 
	  exit(1);
	}
    }
  
  // Now find the acc parameters for each Aerogel associated track 

  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      
      short int aerindex = sngl->get_aerindex();
      short int aersindex = sngl->get_aersindex();
      
      // If there are no acc clusters associated with this track we are done with it!

#ifdef DUMP      
      dumpfile << "Track " << i << " aerindex " << aerindex << " aersindex " << aersindex << endl;
#endif

      if(aerindex >= 0 || aersindex >= 0)
	{
	  // There is an acc cluster associated with this track, get the parameters
	  
	  // The value of aerindex or aersindex is the acc cluster id number 
	  
	  // Get the acc clusters
	  for(unsigned int iacc=0;iacc<nacchits;iacc++)
	    {
	      const AccHitMapEntry *acchit = accmap->GetHit(iacc);
	      if(acchit)
		{  
		  if( (int) iacc == aerindex || (int) iacc == aersindex)
		    {
		      // This is the acc cluster associated with this track
		      
#ifdef DUMP
		      int hitid = acchit->get_hitid();
		      int hitconfig = acchit->get_hitconfig();
		      dumpfile << " Found for track " << i;
		      if(aerindex >= 0)
			dumpfile << " aerindex " << aerindex;
		      if(aersindex >= 0)
			dumpfile << " aersindex " << aersindex;
		      dumpfile << " acc cluster " << iacc
			       << endl;
		      dumpfile << "      Acc Hits : " << endl
			       << " hitid " << hitid
			       << " hitconfig " << hitconfig
			       << endl;
#endif		      
		      
		      // float ph1[4];
		      // float ph2[4];
		      // float t1[4];
		      // float t2[4];
		      for(int ibox=0;ibox<4;ibox++)
			{
			  // ph1[ibox] = acchit->get_ph1(ibox);
			  // ph2[ibox] = acchit->get_ph2(ibox);
			  // t1[ibox] = acchit->get_t1(ibox);
			  // t2[ibox] = acchit->get_t2(ibox);
			  
#ifdef DUMP
			  dumpfile << "          ibox " << ibox
				   << " ph1 " << ph1[ibox]
				   << " ph2 " << ph2[ibox]
				   << " t1 " << t1[ibox]
				   << " t2 " << t2[ibox]
				   << endl;
#endif
			}
		    }

		  // There are no entries for PHCentralTrack, this is done in the analysis macro 
		  // using PHCentralTrack and AccCluster
		  // So we are done here!

		}
	    }
	}
      
    }


  return EVENT_OK;
}

int
FillCNT_AccHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}

