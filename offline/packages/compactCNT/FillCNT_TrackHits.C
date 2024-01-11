#include <FillCNT_TrackHits.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <Fun4AllReturnCodes.h>
#include <PHCentralTrackv24.h>
#include <PHSnglCentralTrackv24.h>

#include <TrackHitsMap.h>
#include <TrackHitsMapEntry.h>


#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <cstdlib>

using namespace std;

FillCNT_TrackHits::FillCNT_TrackHits(const std::string &name): SubsysReco(name)
{
  inputnodename[0] = "CglTrackHits_comp";
  inputnodename[1] = "CglTrackBackHits_comp";

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/fillcnt_trackhits.dump");
#endif

  return;
}

int
FillCNT_TrackHits::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_TrackHits::process_event(PHCompositeNode *topNode)
{
  PHCentralTrackv24 *cnt = findNode::getClass<PHCentralTrackv24>(topNode, "PHCentralTrack");
  if(!cnt)
    {
      return EVENT_OK;
    }

  for (int k = 0; k < 2;k++)
    {
      TrackHitsMap *trkmap = findNode::getClass<TrackHitsMap>(topNode, inputnodename[k]);
      if (!trkmap)
        {
          return EVENT_OK;
        }
      if (k==0)
	{
	  for (unsigned int i = 0; i < cnt->get_npart(); i++)
	    {
	      PHSnglCentralTrack *sngl = cnt->get_track(i);
	      sngl->set_aerindex(-9999);
	      sngl->set_dchid(-1);
	      sngl->set_emcid(-1);
	      sngl->set_pc1id(-1);
	      sngl->set_pc2id(-1);
	      sngl->set_pc3id(-1);
	      sngl->set_tecid(-1);
	      sngl->set_tofeid(-1);
	      sngl->set_tofwid(-1);
	      sngl->set_aersindex(-9999);
	      sngl->set_semcid(-1);
	      sngl->set_spc1id(-1);
	      sngl->set_spc2id(-1);
	      sngl->set_spc3id(-1);
	      sngl->set_stecid(-1);
	      sngl->set_stofeid(-1);
	      sngl->set_stofwid(-1);
	      sngl->set_ring(-1);
	      sngl->set_sring(-1);
              for(int ilayer=0; ilayer<4; ilayer++) {
	        sngl->set_svxid(ilayer, -1);
	        sngl->set_svxsid(ilayer, -1);
              }
	    }
	}
      for (unsigned int i = 0; i < cnt->get_npart(); i++)
        {
          PHSnglCentralTrack *sngl = cnt->get_track(i);
          TrackHitsMapEntry *sngltrkmap = trkmap->GetTrack(i);
          if (!sngltrkmap)
            {
              cout << "could not find track " << i << " in track hits map" << endl;
              exit(1);
            }

#ifdef DUMP
	  dumprecover << "k = " << k << " track  " << i << endl; 
#endif
	  int detsize = (int) sngltrkmap->getSize();
	  short int *alldets = sngltrkmap->getMap();

	  for(short int idet=0;idet<detsize;idet++)
            {
	      if(idet > 0)
		alldets++;

	      // Check the index, skip if it is not set
	      if(*alldets < -9000)
		continue;

#ifdef DUMP
	      dumprecover << "  idet " << idet << " hitid " << *alldets << endl; 
#endif

              if (k == 0)
                {
                  switch (idet)
                    {
                    case id_detector::id_acc:
                      sngl->set_aerindex(*alldets);
                      break;
                    case id_detector::id_dch:
                      sngl->set_dchid(*alldets);
                      break;
                    case id_detector::id_emc:
                      sngl->set_emcid(*alldets);
                      break;
                    case id_detector::id_pc1:
                      sngl->set_pc1id(*alldets);
                      break;
                    case id_detector::id_pc2:
                      sngl->set_pc2id(*alldets);
                      break;
                    case id_detector::id_pc3:
                      sngl->set_pc3id(*alldets);
                      break;
                    case id_detector::id_tec:
                      sngl->set_tecid(*alldets);    // this may happen multiple times per track
                      break;
                    case id_detector::id_tofe:
                      sngl->set_tofeid(*alldets);
                      break;
                    case id_detector::id_tofw:
                      sngl->set_tofwid(*alldets);
                      break;
                    case id_detector::id_crk:
                      sngl->set_ring(*alldets);
                      break;
                    case id_detector::id_svx0:
                      sngl->set_svxid(0, *alldets);
                      break;
                    case id_detector::id_svx1:
                      sngl->set_svxid(1, *alldets);
                      break;
                    case id_detector::id_svx2:
                      sngl->set_svxid(2, *alldets);
                      break;
                    case id_detector::id_svx3:
                      sngl->set_svxid(3, *alldets);
                      break;
                    }
                }
              else
                {
                  switch (idet)
                    {
                    case id_detector::id_acc:
                      sngl->set_aersindex(*alldets);
                      break;
                    case id_detector::id_dch:
                      sngl->set_sdchid(*alldets);
                      break;
                    case id_detector::id_emc:
                      sngl->set_semcid(*alldets);
                      break;
                    case id_detector::id_pc1:
                      sngl->set_spc1id(*alldets);
                      break;
                    case id_detector::id_pc2:
                      sngl->set_spc2id(*alldets);
                      break;
                    case id_detector::id_pc3:
                      sngl->set_spc3id(*alldets);
                      break;
                    case id_detector::id_tec:
                      sngl->set_stecid(*alldets);   // this may happen multiple times per track
                      break;
                    case id_detector::id_tofe:
                      sngl->set_stofeid(*alldets);
                      break;
                    case id_detector::id_tofw:
                      sngl->set_stofwid(*alldets);
                      break;
		    case id_detector::id_crk:
                      sngl->set_sring(*alldets);
                      break;
		    case id_detector::id_svx0:
                      sngl->set_svxsid(0, *alldets);
                      break;
		    case id_detector::id_svx1:
                      sngl->set_svxsid(1, *alldets);
                      break;
		    case id_detector::id_svx2:
                      sngl->set_svxsid(2, *alldets);
                      break;
		    case id_detector::id_svx3:
                      sngl->set_svxsid(3, *alldets);
                      break;
                    }

                }
            }
        }
    }

  return EVENT_OK;
}

