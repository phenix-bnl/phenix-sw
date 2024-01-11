#include <FillCNT_CrkHits.h>
#include "setIntflag.h"

#include <PHCentralTrackv24.h>
#include <PHSnglCentralTrackv24.h>

#include <Fun4AllReturnCodes.h>
#include <CrkHitMapEntry.h>
#include <CrkHitMap.h>
#include <PHGlobal.h>
#include <RunHeader.h>
#include <CrkPID.hh>
#include <id_detector.h>
#include <TrackLineProjectionMap.h>
#include <TrackLineProjectionMapEntry.h>
#include <CrkAssocHitsEntry.h>
#include <CrkAssocHits.h>

#include <PHAngle.h>
#include <PHLine.h>
#include <PHPoint.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>
#include <recoConsts.h>

#include <sstream>

using namespace std;

FillCNT_CrkHits::FillCNT_CrkHits(const std::string &name): SubsysReco(name)
{
  crkpid = 0;
#ifdef DUMP
  dumprecover.open("/phenix/scratch/frawley/fillcnt_crkhits.dump");
#endif

  return;
}

FillCNT_CrkHits::~FillCNT_CrkHits(){
  if(crkpid)
    delete crkpid;
}

int
FillCNT_CrkHits::InitRun(PHCompositeNode *topNode)
{
  RunHeader *d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");
  int runnumber = d_runhdr->get_RunNumber();

  // Optionally, the analyzer can set a flag with an earlier run number
  // This accomodates runs that are produced but are not yet in the RCF database
  recoConsts *rc = recoConsts::instance();
  if( (rc->FlagExist( "compactCNTRUN" ) && rc->get_IntFlag("compactCNTRUN") > 0 ) )
    runnumber = rc->get_IntFlag("compactCNTRUN");

  cout << "FillCNT_CrkHits using Run number " << runnumber << " to get geometry from database" << endl;

  // Get an instance of CrkPID to do the track association
  crkpid = new CrkPID(runnumber);

  // Create a node to output the CRK hits associated with each track to

  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

  CrkAssocHits *crkassochits = new CrkAssocHits();

  PHIODataNode<PHObject> *PHObjectIONode = new PHIODataNode<PHObject>(crkassochits, "CrkAssocHits", "PHObject");
  dstNode->addNode(PHObjectIONode);
  
  return EVENT_OK;
}

int
FillCNT_CrkHits::process_event(PHCompositeNode *topNode)
{
  CrkHitMap *crkmap;
  crkmap  = findNode::getClass<CrkHitMap>(topNode, "CrkHit_comp");
  if (!crkmap)
    {
      return EVENT_OK;
    }

  TrackLineProjectionMap *trklinemap;
  trklinemap  = findNode::getClass<TrackLineProjectionMap>(topNode, "TrackLineProjection_comp");
  if (!trklinemap)
    {
      cout << PHWHERE << "Did not find TrackLineProjection_comp, return and do nothing" << endl;
      return EVENT_OK;
    }

  PHCentralTrackv24 *cnt = findNode::getClass<PHCentralTrackv24>(topNode, "PHCentralTrack");
  if (!cnt)
    {
      cout << PHWHERE << "Failed to find PHCentralTrack, quit!" << endl;
      exit(1);
    }

#ifdef DUMP
  dumprecover << "PHCentralTrack has " << cnt->get_npart() << " tracks " << endl;
#endif

  // begin by initializing all crk fields to -9999 for all tracks
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);
      sngl->set_n0   ( -9999);
      sngl->set_npe0 ( -9999);
      sngl->set_n1   ( -9999);
      sngl->set_npe1 ( -9999);
      sngl->set_n2   ( -9999);
      sngl->set_npe2 ( -9999);
      sngl->set_n3   ( -9999);
      sngl->set_npe3 ( -9999);
      sngl->set_chi2 ( -9999);
      sngl->set_disp ( -9999);
      sngl->set_tcrk ( -9999);
      sngl->set_cross_phi ( -9999);
      sngl->set_cross_z   ( -9999);
      sngl->set_center_phi( -9999);
      sngl->set_center_z  ( -9999);

      sngl->set_sn0   ( -9999);
      sngl->set_snpe0 ( -9999);
      sngl->set_sn1   ( -9999);
      sngl->set_snpe1 ( -9999);
      sngl->set_sn2   ( -9999);
      sngl->set_snpe2 ( -9999);
      sngl->set_sn3   ( -9999);
      sngl->set_snpe3 (-9999);
      sngl->set_schi2 (-9999);
      sngl->set_sdisp (-9999);
      sngl->set_stcrk (-9999);

    }

  // Get the list of RICH hits and put them in a form suitable for use by CrkPID::Associate()

  unsigned int ncrkhits = crkmap->GetNentries();

  if(ncrkhits < 1) 
    {
      return EVENT_OK;
    }

  DCRKHIT_ST *pmt = new DCRKHIT_ST[ncrkhits];  //pointer to PMT hits

  for(unsigned int i=0;i<ncrkhits;i++)
    {
      const CrkHitMapEntry *crkhit = crkmap->GetHit(i);
      if(crkhit)
	{  
	  int pmtid = crkhit->get_pmtid();
	  float npe = crkhit->get_npe();
	  float time = crkhit->get_time();
	  
	  pmt[i].pmt = pmtid;
	  pmt[i].npe = npe;
	  pmt[i].time = time;
	}
      else
	{
	  cout << PHWHERE << " CrkHitMap Hit " << i << " does not have a valid CrkHitMapEntry - Quit!" << endl; 
	  exit(1);
	}
    }

  // Create a pointer to the node where the associated RICH hits will be stored
  CrkAssocHits *crkassochits = findNode::getClass<CrkAssocHits>(topNode, "CrkAssocHits");
  
  // Now find the crk parameters for each RICH associated track and add them to PHCentralTrack for straight & swapped tracks

  // We need to initialize CrkPID with the BBC time zero

  PHGlobal *glb = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  float TimeZero = glb->getBbcTimeZero();

#ifdef DUMP
  dumprecover << "Time zero = " << TimeZero << endl;
#endif

  crkpid->SetBbcT0(TimeZero);
  
  // Note: We do not set the pointer to the crk hits in CrkPID here, we will supply that in the call to CrkPID::Associate

  int trindex=0;
  
  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);

      // These have already been set by FillCNT_TrackHits, which gets the real or swapped 
      // index from whether the track is found in "CglTrackHits" or "CglTrackBackHits"
      short int crkid = sngl->get_ring();
      short int scrkid = sngl->get_sring();

      // If there are no RICH rings associated with this track we are done with it!

      if (crkid >= 0 || scrkid >=0)
        {
	  // If there are, we get the track line projections to the RICH from the node "TrackLineProjections_comp"

	  PHPoint pstart;    // for straight tracks
	  PHPoint pend;
	  PHPoint spstart;   // for swapped tracks
	  PHPoint spend;

	  TrackLineProjectionMapEntry *sngltrklineproj = trklinemap->GetTrackLine(i);  // Line projection object for this track
	  if (!sngltrklineproj)
	    {
	      // projection should be there if we got here!
	      cout << "could not find track " << i << " in trackline proj map, fatal!" << endl;  
	      exit(1);
	    }
	  
	  map<short int, vector<float> > *alldets = sngltrklineproj->getMap();
	  map<short int, vector<float> >::const_iterator iter;
	  for (iter = (*alldets).begin(); iter != (*alldets).end(); iter++)
	    {
	      switch (iter->first)
		{
		case id_detector::id_crk:

		  float px1 = (iter->second)[0];
		  float py1 = (iter->second)[1];
		  float pz1 = (iter->second)[2];
		  float px2 = (iter->second)[3];
		  float py2 = (iter->second)[4];
		  float pz2 = (iter->second)[5];

		  if(crkid >= 0)
		    {
		      //pstart = PHPoint( (iter->second)[0], (iter->second)[1], (iter->second)[2] );
		      //pend = PHPoint( (iter->second)[3], (iter->second)[4], (iter->second)[5] );
		      pstart = PHPoint( px1, py1, pz1 );
		      pend = PHPoint( px2,py2,pz2 );
		    }
		  if(scrkid >= 0)
		    {
		      //spstart = PHPoint( (iter->second)[0], (iter->second)[1], (iter->second)[2] );
		      //spend = PHPoint( (iter->second)[3], (iter->second)[4], (iter->second)[5] );
		      spstart = PHPoint( px1,py1,pz1 );
		      spend = PHPoint( px2,py2,pz2 );
		    }
		  break;
		}
	    }	
	  
	  if(pstart.getX() == 0 && spstart.getX() == 0)
	    {
	      std::cout << PHWHERE << "WARNING: Dropped the ball for track " << i << " no entry for track line but crkid = " 
			  << crkid << " and scrkid = "  << scrkid << std::endl;
	    }

	  if(crkid >= 0)
	    {
	      // There is a real ring associated with this track, get the parameters

	      PHLine track(pstart,pend);
	      track.normalize();

	      CrkPIDout result;
	      
	      if(crkpid->Associate(track, ncrkhits, pmt, &result))
		{
		  //cout << "Associated electron track: " << endl;

		  // Copy the RICH ring parameters into PHCentralTrack	  
		  
		  sngl->set_n0   ( result.npmt0);
		  sngl->set_npe0 ( result.npe0);
		  sngl->set_n1   ( result.npmt1);
		  sngl->set_npe1 ( result.npe1);
		  sngl->set_n2   ( result.npmt2);
		  sngl->set_npe2 ( result.npe2);
		  sngl->set_n3   ( result.npmt3);
		  sngl->set_npe3 ( result.npe3);
		  sngl->set_chi2 ( result.chi2);
		  sngl->set_disp ( result.disp);
		  sngl->set_tcrk ( result.time);
		  sngl->set_cross_phi ( result.cross_phi);
		  sngl->set_cross_z   ( result.cross_z);
		  
		  if (result.center[0] != 0)
		    {
		      if(atan2(result.center[1], result.center[0]) < -0.5*M_PI){
			sngl->set_center_phi(atan2(result.center[1], result.center[0])+2*M_PI);
		      }else{
			sngl->set_center_phi(atan2(result.center[1], result.center[0]));
		      }
		    }
		  sngl->set_center_z(result.center[2]);
		  
#ifdef DUMP
		  dumprecover << "Track " << i
			      << " crkid " << crkid 
			      << " rich pars:"
			      << " " << sngl->get_n0()
			      << " " << sngl->get_npe0()
			      << " " << sngl->get_n1()
			      << " " << sngl->get_npe1()
			      << " " << sngl->get_n2()
			      << " " << sngl->get_npe2()
			      << " " << sngl->get_n3()
			      << " " << sngl->get_npe3()
			      << " " << sngl->get_chi2()
			      << " " << sngl->get_disp()
			      << " " << sngl->get_tcrk()
			      << " " << sngl->get_cross_phi()
			      << " " << sngl->get_cross_z()
			      << " " << sngl->get_center_phi()
			      << " " << sngl->get_center_z()
			      << endl;

		  dumprecover << "  projection:  crkid " << crkid
			      << " line " << pstart.getX()
			      << " " << pstart.getY()
			      << " " << pstart.getZ()
			      << " " << pend.getX()
			      << " " << pend.getY()
			      << " " << pend.getZ()
			      << endl;
#endif

		  
		  // Following a request by the HBD group (Ilia) capture the hits associated with this track
		  // These are stored by CrkPID::Associate" in a DCRK_HITS structure that is a member of "result"
		  // Hits were stored for all of the associated PMT's in npmt1, so keep the radius of the pmt too
		  
		  // These are for the straight tracks
		  
		  CrkAssocHitsEntry crkassocentry;

		  crkassocentry.set_trackid(i);

		  // swapped tracks are identified by sngl->get_sring() being set
		  // we add them in the same way, but we have to identify this entry
		  // as a swapped track, since a given track can have both staright and 
		  // swapped RICH hits - in that case it will have two entries in CrkAssocHits

		  bool swapped = false;
		  crkassocentry.set_swapped(swapped);


		  int npmt1 = result.npmt1;
		  // npmt1 can be greater than result.MAXPMTHIT, but "result" is truncated and dimensioned only to MAXPMTHIT! 
		  // This prevents a segfault!
		  if(npmt1 > result.MAXPMTHIT) npmt1=result.MAXPMTHIT;

		  crkassocentry.set_npmts(npmt1);
		  
		  /*
		  cout << "trackid " << i << " npmt1 " << result.npmt1 << endl;
		  cout << "from entry: trackid " << crkassocentry.get_trackid()
		       << " npmts " << crkassocentry.get_npmts()
		       << endl;
		  */

		  for(int ipmt = 0;ipmt<npmt1;ipmt++)
		    {
		      // Add the hit information for this pmt
		      // We want pmtid, npe, time, and radius from track
		      
		      crkassocentry.set_pmtid(ipmt, result.pmt[ipmt]->pmt);
		      crkassocentry.set_npe(ipmt, result.pmt[ipmt]->npe);
		      crkassocentry.set_time(ipmt, result.pmt[ipmt]->time);
		      crkassocentry.set_rpmt(ipmt, result.rpmt[ipmt]);
		      /*
		      cout << "	  ipmt " << ipmt
			   << " pmtid " << result.pmt[ipmt]->pmt
			   << " npe " << result.pmt[ipmt]->npe
			   << " time " << result.pmt[ipmt]->time
			   << " rpmt " << result.rpmt[ipmt]
			   << endl;
		      */
		    }
		  	      
		  crkassochits->AddHit(trindex,crkassocentry);
		  trindex++;

		  /*
		  cout << "  Checking:" << endl;
		  cout << "    CrkAssocHits has Nentries " << crkassochits->GetNentries()
		       << " central n0 " << sngl->get_n0()
		       << " central npe0 " << sngl->get_npe0()
		       << " central n1 " << sngl->get_n1()
		       << " central npe1 " << sngl->get_npe1()
		       << endl;
		  */		
		}
	    }
	  
	  if(scrkid >= 0)
	    {
	      // There is a ring associated with this swapped track, get the parameters
	      
	      PHLine strack(spstart,spend);
              strack.normalize();
	      strack = ReflectInZ(strack);

	      CrkPIDout sresult;      	      
	      
	      if(crkpid->Associate(strack, ncrkhits, pmt, &sresult))
		{
		  // Copy the RICH ring parameters into PHCentralTrack
		  
		  //  sn0, snpe0, sn1, snpe1, sn2, snpe2, sn3, snpe3, schi2, sdisp, stcrk
		  
		  sngl->set_sn0   ( sresult.npmt0);
		  sngl->set_snpe0 ( sresult.npe0);
		  sngl->set_sn1   ( sresult.npmt1);
		  sngl->set_snpe1 ( sresult.npe1);
		  sngl->set_sn2   ( sresult.npmt2);
		  sngl->set_snpe2 ( sresult.npe2);
		  sngl->set_sn3   ( sresult.npmt3);
		  sngl->set_snpe3 ( sresult.npe3);
		  sngl->set_schi2 ( sresult.chi2);
		  sngl->set_sdisp ( sresult.disp);
		  sngl->set_stcrk ( sresult.time);
		  
#ifdef DUMP
		  dumprecover << "Track " << i 
			      << " scrkid " << scrkid 
			      << " srich pars: "
			      << " " << sresult.npmt0
			      << " " << sresult.npe0
			      << " " << sresult.npmt1
			      << " " << sresult.npe1
			      << " " << sresult.npmt2
			      << " " << sresult.npe2
			      << " " << sresult.npmt3
			      << " " << sresult.npe3
			      << " " << sresult.chi2
			      << " " << sresult.disp
			      << " " << sresult.time
			      << " " << sresult.cross_phi
			      << " " << sresult.cross_z
			      << endl;
		  
		  dumprecover << "  projection: scrid " << scrkid
			      << " (unflipped) line " << spstart.getX()
			      << " " << spstart.getY()
			      << " " << spstart.getZ()
			      << " " << spend.getX()
			      << " " << spend.getY()
			      << " " << spend.getZ()
			      << endl;
#endif

		  //===============================================
		  // Following a request by the HBD group (Ilia) capture the hits associated with this track
		  // These are stored by CrkPID::Associate" in a DCRK_HITS structure that is a member of "result"
		  // Hits were stored for all of the associated PMT's in npmt1, so keep the radius of the pmt too
		  //===============================================		  


		  CrkAssocHitsEntry crkassocentry;
		
		  crkassocentry.set_trackid(i);

		  // swapped tracks are identified by sngl->get_sring() being set
		  // we add them in the same way, but we have to identify this entry
		  // as a swapped track, since a given track can have both staright and 
		  // swapped RICH hits - in that case it will have two entries in CrkAssocHits

		  bool swapped = true;
		  crkassocentry.set_swapped(swapped);

		  int npmt1 = sresult.npmt1;
		  // npmt1 can be greater than result.MAXPMTHIT, but result is dimensioned only to MAXPMTHIT! 
		  if(npmt1 > sresult.MAXPMTHIT) npmt1=sresult.MAXPMTHIT;

		  crkassocentry.set_npmts(npmt1);

		  //cout << "swapped trackid " << i << " npmt1 " << sresult.npmt1 << endl;
  
		  for(int ipmt = 0;ipmt<npmt1;ipmt++)
		    {
		      // Add the hit information for this pmt
		      // We want pmtid, npe, time, and radius from track
		      
		      crkassocentry.set_pmtid(ipmt, sresult.pmt[ipmt]->pmt);
		      crkassocentry.set_npe(ipmt, sresult.pmt[ipmt]->npe);
		      crkassocentry.set_time(ipmt, sresult.pmt[ipmt]->time);
		      crkassocentry.set_rpmt(ipmt, sresult.rpmt[ipmt]);

		      /*
		      cout << "	  ipmt " << ipmt
			   << " pmtid " << sresult.pmt[ipmt]->pmt
			   << " npe " << sresult.pmt[ipmt]->npe
			   << " time " << sresult.pmt[ipmt]->time
			   << " rpmt " << sresult.rpmt[ipmt]
			   << endl;
		      */

		    }
		  
		  crkassochits->AddHit(trindex,crkassocentry);
		  trindex++;

		  /*
		  cout << "  Checking:" << endl;
		  cout << "    CrkAssocHits has Nentries " << crkassochits->GetNentries()
		       << endl;		
		  */

		}
	    }
	}
    }

  delete [] pmt;
  
  return EVENT_OK;
}

int
FillCNT_CrkHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumprecover.close();
#endif

  return 0;
}

PHLine
FillCNT_CrkHits::ReflectInZ(const PHLine &trk)
{
  PHPoint base_zref = trk.getBasepoint();
  PHVector dir_zref = trk.getDirection();
  base_zref.setZ( -1.0*base_zref.getZ());
  dir_zref.setZ( -1.0*dir_zref.getZ());
  return PHLine(base_zref, dir_zref);
}
