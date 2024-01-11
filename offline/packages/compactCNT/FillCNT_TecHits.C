#include <FillCNT_TecHits.h>
#include "setIntflag.h"

#include <RunHeader.h>

#include <Fun4AllReturnCodes.h>
#include <TecHitMapEntry.h>
#include <TecHitMap.h>
#include <PHCentralTrackv24.h>
#include <PHSnglCentralTrackv24.h>
#include <TrackLineProjectionMap.h>
#include <id_detector.h>

#include <TecClusterV1.hh>
#include <TecClusterContainerV1.hh>
#include <mTecUtilities.h>
#include <TecProjv1.hh>
#include <TecCalibrationObject.hh>
#include <TecGeometryObject.hh>

#include <PHAngle.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>
#include <recoConsts.h>

#include <cstdlib>
#include <sstream>

using namespace std;

FillCNT_TecHits::FillCNT_TecHits(const std::string &name): SubsysReco(name)
{
  cluscont = new TecClusterContainerV1();
  clus = new TecClusterV1();

#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/fillcnt_techits.dump");
#endif

  TGO = 0;
  TCO = 0;

  return;
}

FillCNT_TecHits::~FillCNT_TecHits(){
  delete cluscont;
  delete clus;
  if(TGO)
    delete TGO;
  if(TCO)
    delete TCO;
}

int
FillCNT_TecHits::InitRun(PHCompositeNode *topNode)
{
  RunHeader *d_runhdr = findNode::getClass<RunHeader>(topNode,"RunHeader");
  int runnumber = d_runhdr->get_RunNumber();

  // Optionally, the analyzer can set a flag with an earlier run number
  // This accomodates runs that are produced but are not yet in the RCF database
  recoConsts *rc = recoConsts::instance();
  if( ( rc->FlagExist( "compactCNTRUN" ) && rc->get_IntFlag("compactCNTRUN") > 0 ))
    runnumber = rc->get_IntFlag("compactCNTRUN");
  
  cout << "FillCNT_TecHits using Run number " << runnumber << " to get geometry from database" << endl;
  if (verbosity > 1)
    cout << "Fetching Tec Geometry Object " << endl;

  TGO = new TecGeometryObject();
  TGO->setRunNumber(runnumber);
  TGO->Fetch();

  if (verbosity > 1)
    {
      cout << "Fetching Tec Calibration Object " << endl;
    }
  TCO = new TecCalibrationObject();
  TCO->setRunNumber(runnumber);
  TCO->Fetch();

  return EVENT_OK;
}

int
FillCNT_TecHits::process_event(PHCompositeNode *topNode)
{

  //=========================================================================================
  // Get the TEC clusters from TecHitMap and write them to a TecClusterContainer that is a 
  // a member of this class. The TecClusterContainer will be used later in this module during 
  // track re-association
  //==========================================================================================

  TecHitMap *tecmap;
  tecmap  = findNode::getClass<TecHitMap>(topNode, "TecHit_comp");
  if (!tecmap)
    {
      //cout << PHWHERE << "Did not find TecHit_comp, return and do nothing" << endl;
      return EVENT_OK;
    }

  // This is a TecClusterContainer object that we use to hold the Tec hits for 
  // track re-association

  cluscont->Clear();

  unsigned int nclus = tecmap->GetNentries();

#ifdef DUMP
  dumpfile << "TecHit has nentries = " << tecmap->GetNentries() << endl; 
#endif

  for(unsigned int i=0;i<nclus;i++)
    {
#ifdef DUMP
      dumpfile << "  get cluster " << i << endl;
#endif
      const TecHitMapEntry *techit = tecmap->GetHit(i);
      if(techit)
        {
          //short int id = techit->get_id();
	  short int index = techit->get_index();
	  short int wire = techit->get_wire();
	  short int ntimebins = techit->get_ntimebins();
	  short int avgtime = techit->get_avgtime();
	  float charge = techit->get_charge();

#ifdef DUMP
	  dumpfile << "TecHit number " << i
		   << " index " << index
		   << " wire " << wire
		   << " ntimebins " << ntimebins
		   << " avgtime " << avgtime
		   << " charge " << charge
		   << endl;
#endif

	  clus->set_index(index);
	  clus->set_wire(wire);
	  clus->set_ntimebins(ntimebins);
	  clus->set_avgtime(avgtime);
	  clus->set_charge(charge);

#ifdef DUMP
	  dumpfile << "  clus parameters: index " << clus->get_index()
		   << " wire " << clus-> get_wire()
		   << " ntimebins " << clus-> get_ntimebins()
		   << " avgtime " << clus->get_avgtime()
		   << " charge " << clus->get_charge()
		   << endl;
#endif

	    cluscont->AddTecCluster(*clus);
	}
    }

#ifdef DUMP
  dumpfile << "Number of TEC clusters added to TecClusterContainer is " << cluscont->getNClusters() << endl;
#endif

  //=========================================================================================
  // Now get the tracks from PHCentralTrack and the TEC track line projections 
  // from TrackLineProjectionMap 
  //==========================================================================================

  TrackLineProjectionMap *trklinemap;
  trklinemap  = findNode::getClass<TrackLineProjectionMap>(topNode, "TrackLineProjection_comp");
  if (!trklinemap)
    {
      cout << PHWHERE << "Did not find TrackLineProjection_comp, return and do nothing" << endl;
      return EVENT_OK;
    }

  // Initialize all fields to a large negative number for all tracks in this event

  PHCentralTrackv24 *cnt = findNode::getClass<PHCentralTrackv24>(topNode, "PHCentralTrack");

#ifdef DUMP
  dumpfile << "PHCentralTrack has ntracks = " << cnt->get_npart() << endl;
#endif

  //==========================================
  // Now re-associate TEC clusters with tracks
  // if there is a track line projection for
  // that track
  //==========================================

  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);

#ifdef DUMP
      if(sngl->get_tecid() > 0)
	dumpfile << "Track " << i << " has tecid " << sngl->get_tecid() << endl;

      if(sngl->get_stecid() > 0)
	dumpfile << "Track " << i << " has stecid " << sngl->get_stecid() << endl;
#endif

      // If this track has no TEC associated hits, forget it

      if( !(sngl->get_tecid() > 0) && !(sngl->get_stecid() > 0) ) 
	continue;

      // Get the track line projections for this track to the TEC from 
      // the node "TrackLineProjections"

      TrackLineProjectionMapEntry *sngltrklineproj = trklinemap->GetTrackLine(i);
      if (!sngltrklineproj)
	{
	  // At least an empty projection map should be there for every track
	  cout << "could not find track " << i << " in trackline proj map, fatal!" << endl;
	  exit(1);
	}
      
      float px1 = 0.0;
      float px2 = 0.0;
      float py1 = 0.0,py2 = 0.0;
      float pz1 = 0.0,pz2 = 0.0;
      
      map<short int, vector<float> > *alldets = sngltrklineproj->getMap();
      map<short int, vector<float> >::const_iterator iter;
      for (iter = (*alldets).begin(); iter != (*alldets).end(); iter++)
	{
	  switch (iter->first)
	    {
	    case id_detector::id_tec:
	      
	      px1 = (iter->second)[0];
	      py1 = (iter->second)[1];
	      pz1 = (iter->second)[2];
	      px2 = (iter->second)[3];
	      py2 = (iter->second)[4];
	      pz2 = (iter->second)[5];
	      
#ifdef DUMP
	      dumpfile << "Found TEC track projection for Track " << i
		       << " with px1 " << px1
		       << " py1 " << py1
		       << " pz1 " << pz1
		       << " px2 " << px2
		       << " py2 " << py2
		       << " pz2 " << pz2
		       << endl;
#endif
	      
	      // This projection is for straight, swapped, or possibly both
	      
	      break;
	    }
	}
      
      if(px1 < 0.0001 && px1 > -0.0001)
	{
	  cout << PHWHERE << " Panic: there is no track line projection and there should be! Quit." << endl;
	  exit(1);
	}

      PHPoint pstart;
      PHPoint pend;
      pstart = PHPoint( px1, py1, pz1 );
      pend = PHPoint( px2,py2,pz2 );
      PHLine track(pstart,pend);
      
      // Now associate this track with TEC hits
      
      float min_distance_cut = 4.0;

      // This is used to hold the results of track re-association
      // and return them to here

      TecProj *tecproj = new TecProjv1(); 
      tecproj->Reset();

      //=============================================
      // Do the association with the unswapped track
      //=============================================

      if(sngl->get_tecid() > 0)
	{
	  bool is_swapped = false;
	  TecUtilities::Associate(i, track, cluscont, TCO, TGO, tecproj, is_swapped, min_distance_cut);
	  
	  if(tecproj->get_TecNProj() > 0)
	    {
#ifdef DUMP
	      dumpfile << "Found match for straight track " << i << " which has tecid " << sngl->get_tecid() << endl; 
#endif
	      
	      // Found matched hits for this track, capture variables for PHCentralTrack
	      
	      float charge[6] = {-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9};
	      int ntimebins[6] = {-9999,-9999,-9999,-9999,-9999,-9999};
	      int avgtimebin[6] = {-9999,-9999,-9999,-9999,-9999,-9999};
	      float dphi[6] =  {-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9};
	      
	      for(unsigned int iplane=0;iplane<6;iplane++)
		{
		  if(tecproj->get_teclusterid(0,iplane) > 0)
		    {
		      int clusid = tecproj->get_teclusterid(0,iplane);
		      
		      charge[iplane] = cluscont->getTecCluster(clusid)->get_charge();
		      ntimebins[iplane] = cluscont->getTecCluster(clusid)->get_ntimebins();
		      avgtimebin[iplane] = cluscont->getTecCluster(clusid)->get_avgtime();
		      
		      // Have to calculate dphi for each plane
		      
		      int ichamber = cluscont->getTecCluster(clusid)->get_index();
		      PHPoint trkpoint = TecUtilities::get_projection(track, ichamber, TGO);
		      PHPoint tecpoint(cluscont->getTecCluster(clusid)->get_xyz_global(TCO,TGO,0),
				       cluscont->getTecCluster(clusid)->get_xyz_global(TCO,TGO,1),
				       trkpoint.getZ());
		      
		      // Calculate dphi using trkpoint and the cluster position
		      dphi[iplane] = atan( (tecpoint.getY()-trkpoint.getY()) / trkpoint.getX());
		      
#ifdef DUMP
		      dumpfile << " iplane " << iplane 
			       << " clusterid " << clusid
			       << " index  " << cluscont->getTecCluster(clusid)->get_index()
			       << " wire  " << cluscont->getTecCluster(clusid)->get_wire()
			       << " avgtime  " << avgtimebin[iplane]
			       << " ntimebins  " << ntimebins[iplane]
			       << " charge  " << charge[iplane]
			       << " dphi " << dphi[iplane]
			       << endl;
#endif
		    }
		}
	      
	      // Now fill the TEC fields in PHCentralTrack
	      
	      for(unsigned int iplane=0;iplane<6;iplane++)
		{
		  sngl->set_teccharge(iplane,charge[iplane]);
		  sngl->set_tecntimebins(iplane,ntimebins[iplane]);
		  sngl->set_tecavgtimebin(iplane,avgtimebin[iplane]);
		  sngl->set_tecdphiplane(iplane,dphi[iplane]);
		  
#ifdef DUMP
		  if(sngl->get_teccharge(iplane) > 0)
		    dumpfile << "    from PHCentralTrack for plane " << iplane << ": " 
			     << " teccharge " << sngl->get_teccharge(iplane)
			     << " tecntimebins " << sngl->get_tecntimebins(iplane)
			     << " tecavgtimebin " << sngl->get_tecavgtimebin(iplane)
			     << " tecdphiplane " << sngl->get_tecdphiplane(iplane)
			     << endl;
#endif
		}
	      
	    }
	}
      
      //===============================================
      // Then do the association with the swapped track
      //===============================================

      if(sngl->get_stecid() >0)
	{      
	  tecproj->Reset();
	  bool is_swapped = true;
	  TecUtilities::Associate(i, track, cluscont, TCO, TGO, tecproj, is_swapped, min_distance_cut);
	  
	  if(tecproj->get_TecNProj() > 0)
	    {
#ifdef DUMP
	      dumpfile << "Found match for swapped track " << i << " which has stecid " << sngl->get_stecid() << endl; 
#endif
	      
	      // OK, we got a match. Capture the swapped variables that we want to write into PHCentralTrack
	      
	      float charge[6] = {-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9};
	      int ntimebins[6] = {-9999,-9999,-9999,-9999,-9999,-9999};
	      int avgtimebin[6] = {-9999,-9999,-9999,-9999,-9999,-9999};
	      float dphi[6] =  {-9999.9,-9999.9,-9999.9,-9999.9,-9999.9,-9999.9};
	      
	      for(unsigned int iplane=0;iplane<6;iplane++)
		{
		  if(tecproj->get_teclusterid(0,iplane) > 0)
		    {
		      int clusid = tecproj->get_teclusterid(0,iplane);
		      
		      charge[iplane] = cluscont->getTecCluster(clusid)->get_charge();
		      ntimebins[iplane] = cluscont->getTecCluster(clusid)->get_ntimebins();
		      avgtimebin[iplane] = cluscont->getTecCluster(clusid)->get_avgtime();
		      
		      // Have to calculate dphi for each plane
		      
		      int ichamber = cluscont->getTecCluster(clusid)->get_index();
		      PHPoint trkpoint = TecUtilities::get_projection(track, ichamber, TGO);
		      PHPoint tecpoint(cluscont->getTecCluster(clusid)->get_xyz_global(TCO,TGO,0),
				       cluscont->getTecCluster(clusid)->get_xyz_global(TCO,TGO,1),
				       -trkpoint.getZ());
		      
		      // Calculate dphi using trkpoint and the cluster position
		      dphi[iplane] = atan( (tecpoint.getY()-trkpoint.getY()) / trkpoint.getX());
		      
#ifdef DUMP
		      dumpfile << " iplane " << iplane 
			       << " clusterid " << clusid
			       << " index  " << cluscont->getTecCluster(clusid)->get_index()
			       << " wire  " << cluscont->getTecCluster(clusid)->get_wire()
			       << " avgtime  " << avgtimebin[iplane]
			       << " ntimebins  " << ntimebins[iplane]
			       << " charge  " << charge[iplane]
			       << " dphi " << dphi[iplane]	
			       << endl;
#endif
		    }
		}
	      
	      for(unsigned int iplane=0;iplane<6;iplane++)
		{
		  sngl->set_steccharge(iplane,charge[iplane]);
		  sngl->set_stecntimebins(iplane,ntimebins[iplane]);
		  sngl->set_stecavgtimebin(iplane,avgtimebin[iplane]);
		  sngl->set_stecdphiplane(iplane,dphi[iplane]);
		  
#ifdef DUMP
		  if(sngl->get_steccharge(iplane) > 0)
		    dumpfile << "    from PHCentralTrack for plane " << iplane << ": " 
			     << " steccharge " << sngl->get_steccharge(iplane)
			     << " stecntimebins " << sngl->get_stecntimebins(iplane)
			     << " stecavgtimebin " << sngl->get_stecavgtimebin(iplane)
			     << " stecdphiplane " << sngl->get_stecdphiplane(iplane)
			     << endl;
#endif
		}
	    }
	  
	}
      delete tecproj;
    }
  
  return EVENT_OK;
}

int
FillCNT_TecHits::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumpfile.close();
#endif

  return 0;
}
