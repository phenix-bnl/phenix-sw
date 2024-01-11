#include <FillCNT_TrackProjections.h>
#include <id_detector.h>
#include "setIntflag.h"

#include <Fun4AllReturnCodes.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include <TrackProjectionMap.h>
#include <TrackProjectionMapEntry.h>


#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <cstdlib>

using namespace std;

FillCNT_TrackProjections::FillCNT_TrackProjections(const std::string &name): SubsysReco(name)
{

#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/fillcnt_trackprojections.dump");
#endif

  return;
}

int
FillCNT_TrackProjections::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_TrackProjections::process_event(PHCompositeNode *topNode)
{

  TrackProjectionMap *trkproj = findNode::getClass<TrackProjectionMap>(topNode, "TrackProjection_comp");
  if (!trkproj)
    {
      return EVENT_OK;
    }

  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");

  for (unsigned int i = 0; i < cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      sngl->set_pemcx(-99999.9);
      sngl->set_pemcy(-99999.9);
      sngl->set_pemcz(-99999.9);
      sngl->set_spemcx(-99999.9);
      sngl->set_spemcy(-99999.9);
      sngl->set_spemcz(-99999.9);

      sngl->set_ppc1x(-1000000.);
      sngl->set_ppc1y(-1000000.);
      sngl->set_ppc1z(-1000000.);

      sngl->set_ppc2x(-1000000.);
      sngl->set_ppc2y(-1000000.);
      sngl->set_ppc2z(-1000000.);
      sngl->set_ppc3x(-1000000.);
      sngl->set_ppc3y(-1000000.);
      sngl->set_ppc3z(-1000000.);
      sngl->set_sppc1x(-1000000.);
      sngl->set_sppc1y(-1000000.);
      sngl->set_sppc1z(-1000000.);
      sngl->set_sppc2x(-1000000.);
      sngl->set_sppc2y(-1000000.);
      sngl->set_sppc2z(-1000000.);
      sngl->set_sppc3x(-1000000.);
      sngl->set_sppc3y(-1000000.);
      sngl->set_sppc3z(-1000000.);
      sngl->set_ptofex(-1000000.);
      sngl->set_ptofey(-1000000.);
      sngl->set_ptofez(-1000000.);
      sngl->set_sptofex(-1000000.);
      sngl->set_sptofey(-1000000.);
      sngl->set_sptofez(-1000000.);
      sngl->set_ptofwx(-1000000.);
      sngl->set_ptofwy(-1000000.);
      sngl->set_ptofwz(-1000000.);
      sngl->set_sptofwx(-1000000.);
      sngl->set_sptofwy(-1000000.);
      sngl->set_sptofwz(-1000000.);
      //
      // Adding HBD projections
      //
      sngl->set_phbdx(-1000000.);
      sngl->set_phbdy(-1000000.);
      sngl->set_phbdz(-1000000.);
//       sngl->set_sphbdx(-1000000.);
//       sngl->set_sphbdy(-1000000.);
//       sngl->set_sphbdz(-1000000.);
      // 4 layers of SVX
      for(int ilayer=0; ilayer<4; ilayer++) {
        sngl->set_psvxx(ilayer, -1000000.);
        sngl->set_psvxy(ilayer, -1000000.);
        sngl->set_psvxz(ilayer, -1000000.);
        sngl->set_spsvxx(ilayer, -1000000.);
        sngl->set_spsvxy(ilayer, -1000000.);
        sngl->set_spsvxz(ilayer, -1000000.);
      }

    }

  for (unsigned int i = 0; i < cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      TrackProjectionMapEntry *sngltrkproj = trkproj->GetTrack(i);
      if (!sngltrkproj)
        {
          cout << "could not find track " << i << " in proj map" << endl;
          exit(1);
        }

#ifdef DUMP
      dumprecover << "Track  " << i << endl;
#endif

      int detsize = (int) sngltrkproj->getSize();
      float *alldets = sngltrkproj->getMap();
      //cout << PHWHERE << " Track " << i << " detsize " << detsize << " *alldets " << *alldets << endl;
      for (int idet = 0;idet<detsize; idet++)
        {
	  if(idet > 0)
	    alldets += 3;

	  if( *alldets < -9000.0)
	    continue;

#ifdef DUMP
	  dumprecover << "  idet " << idet 
		      << " X " << *alldets
		      << " Y " << *(alldets+1)
		      << " Z " << *(alldets+2)
		      << endl;
#endif

	  switch (idet)
            {
            case id_detector::id_emc:
              sngl->set_pemcx(*alldets);
              sngl->set_pemcy(*(alldets+1));
              sngl->set_pemcz(*(alldets+2));
              sngl->set_spemcx(*alldets);
              sngl->set_spemcy(*(alldets+1));
              sngl->set_spemcz(flipandslide( *(alldets+2) ));
              break;
            case id_detector::id_pc1:
              sngl->set_ppc1x(*alldets);
              sngl->set_ppc1y(*(alldets+1));
              sngl->set_ppc1z(*(alldets+2));
              sngl->set_sppc1x(*alldets);
              sngl->set_sppc1y(*(alldets+1));
              sngl->set_sppc1z(flipandslide(*(alldets+2)));
              break;
            case id_detector::id_pc2:
              sngl->set_ppc2x(*alldets);
              sngl->set_ppc2y(*(alldets+1));
              sngl->set_ppc2z(*(alldets+2));
              sngl->set_sppc2x(*alldets);
              sngl->set_sppc2y(*(alldets+1));
              sngl->set_sppc2z(flipandslide(*(alldets+2)));
              break;
            case id_detector::id_pc3:
              sngl->set_ppc3x(*alldets);
              sngl->set_ppc3y(*(alldets+1));
              sngl->set_ppc3z(*(alldets+2));
              sngl->set_sppc3x(*alldets);
              sngl->set_sppc3y(*(alldets+1));
              sngl->set_sppc3z(flipandslide(*(alldets+2)));
              break;
            case id_detector::id_tofe:
              sngl->set_ptofex(*alldets);
              sngl->set_ptofey(*(alldets+1));
              sngl->set_ptofez(*(alldets+2));
              sngl->set_sptofex(*alldets);
              sngl->set_sptofey(*(alldets+1));
              sngl->set_sptofez(flipandslide(*(alldets+2)));
              break;
            case id_detector::id_tofw:
              sngl->set_ptofwx(*alldets);
              sngl->set_ptofwy(*(alldets+1));
              sngl->set_ptofwz(*(alldets+2));
              sngl->set_sptofwx(*alldets);
              sngl->set_sptofwy(*(alldets+1));
              sngl->set_sptofwz(flipandslide(*(alldets+2)));
              break;
	      //
	      // Adding HBD projections
	      //
            case id_detector::id_hbd:
              sngl->set_phbdx(*alldets);
              sngl->set_phbdy(*(alldets+1));
              sngl->set_phbdz(*(alldets+2));
//               sngl->set_sphbdx(*alldets);
//               sngl->set_sphbdy(*(alldets+1));
//               sngl->set_sphbdz(flipandslide(*(alldets+2)));
              break;
              // 4 layers of SVX
            case id_detector::id_svx0:
              sngl->set_psvxx(0, *alldets);
              sngl->set_psvxy(0, *(alldets+1));
              sngl->set_psvxz(0, *(alldets+2));
              sngl->set_spsvxx(0, *alldets);
              sngl->set_spsvxy(0, *(alldets+1));
              sngl->set_spsvxz(0, flipandslide(*(alldets+2)));
              break;
            case id_detector::id_svx1:
              sngl->set_psvxx(1, *alldets);
              sngl->set_psvxy(1, *(alldets+1));
              sngl->set_psvxz(1, *(alldets+2));
              sngl->set_spsvxx(1, *alldets);
              sngl->set_spsvxy(1, *(alldets+1));
              sngl->set_spsvxz(1, flipandslide(*(alldets+2)));
              break;
            case id_detector::id_svx2:
              sngl->set_psvxx(2, *alldets);
              sngl->set_psvxy(2, *(alldets+1));
              sngl->set_psvxz(2, *(alldets+2));
              sngl->set_spsvxx(2, *alldets);
              sngl->set_spsvxy(2, *(alldets+1));
              sngl->set_spsvxz(2, flipandslide(*(alldets+2)));
              break;
            case id_detector::id_svx3:
              sngl->set_psvxx(3, *alldets);
              sngl->set_psvxy(3, *(alldets+1));
              sngl->set_psvxz(3, *(alldets+2));
              sngl->set_spsvxx(3, *alldets);
              sngl->set_spsvxy(3, *(alldets+1));
              sngl->set_spsvxz(3, flipandslide(*(alldets+2)));
              break;
            }
        }

    }

  return EVENT_OK;
}

float
FillCNT_TrackProjections::flipandslide(const float z) const
{

  static const float SLIDINGFLIPPING = 35;

  if (fabs(z) > SLIDINGFLIPPING)
      {
        return -z;
      }
    if (z >= 0)
      {
        return z -SLIDINGFLIPPING;
      }
    return z + SLIDINGFLIPPING;
  }

