/*
 * QAMut.C
 * $Id: QAMut.C,v 1.13 2016/04/20 14:04:35 pinkenbu Exp $
 *
 * Book and fill MUT histograms.
 * History:
 * 4-19-2004: Moved ntuples into debug mode.  xhe & hq
 * 7:19:2010 : added few more checks.  Abhisek Sen
*/


#include "QAMut.h"
#include "QADefs.h"

#include <Fun4AllHistoManager.h>
#include <Fun4AllServer.h>

// MUTOO includes
#include <TMutHitMap.h>
#include <TMutClusMap.h>
#include <TMutCoordMap.h>
#include <TMutTrkMap.h>
#include <TMuiRoadMapO.h>
#include <TMutGapCoordMap.h>
#include <TMutVtxMap.h>

#include <MUTOO.h>
#include <MUTOO_FEM.h>
#include <MutGeom.h>
#include <MutStrip.h>

#include <MutCalibStrip.hh>
#include <MutCalib.h>

#include <TriggerHelper.h>
#include <RunHeader.h>
#include <getClass.h>
#include <BbcOut.h>
#include <getClass.h>

#include <TMinuit.h>
#include <TF1.h>

using namespace std;

const int NUMPLANEST[QAMut::NUMSTATION] = { 6, 6, 4 };
const int TOTPLANEST[QAMut::NUMSTATION] = { 48, 48, 32 };
const int NUMSTRIPS[QAMut::NUMARM*QAMut::NUMSTATION] = { 51, 80, 120, 51, 96, 160 };
const int TOTPACKETARM[QAMut::NUMARM] = { 168, 192 };

//___________________________________________________________________________________________________________
int QAMut::InitRun(PHCompositeNode* top_node)
{
  Fun4AllServer *se = Fun4AllServer::instance();

  try
    {
      TMutNode<TMutClusMap>::find_node(top_node, "TMutClusMap");
      TMutNode<TMutTrkMap>::find_node(top_node, "TMutTrkMap");
      TMutNode<TMutHitMap>::find_node(top_node, "TMutHitMap");
      TMutNode<TMutCoordMap>::find_node(top_node, "TMutCoordMap");
      TMutNode<TMutGapCoordMap>::find_node(top_node, "TMutGapCoordMap");
      TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
    }
  catch ( exception& e)
    {
      se->unregisterSubsystem(this);
      return 0;
    }

  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
    {
      hm = new Fun4AllHistoManager(HistoManagerName);
      se->registerHistoManager(hm);
    }

  char id[128], title[128];

  // init histograms, use maximum limits
  int arm, sta, oct, gap, cath;
  for (arm = 0; arm < NUMARM; arm++)
  {
   // ClusterQPeak/arm
   sprintf(id, "MutCathClustQPeak[%d]", arm);
   sprintf(title, "MUT%d cathode cluster charge peak all stations", arm);
   MutCathClustQPeak[arm] = new TH1F(id, title,HISTMAXPEAKCHARGE,-0.5, HISTMAXPEAKCHARGE - 0.5);
   MutCathClustQPeak[arm]->SetXTitle("Cluster peak charge");
   hm->registerHisto(MutCathClustQPeak[arm]);

   // NumMutTracks/arm
    sprintf(id, "MutNumTracks[%d]", arm);
    sprintf(title, "MUT%d number of tracks per event", arm);
    MutNumTracks[arm] = new TH1F(id, title,HISTMAXNTRACKS,-0.5, HISTMAXNTRACKS - 0.5);
    hm->registerHisto(MutNumTracks[arm]);

    //MutTracksMom/arm
    sprintf(id, "MutTrackMom[%d]", arm);
    sprintf(title, "MUT%d fitted track total momentum", arm);
    MutTrackMom[arm] = new TH1F(id, title, 180, 0.0, 36.0);
    hm->registerHisto(MutTrackMom[arm]);

    //MutrackChisquare/ndf
    sprintf(id,"MutTrackChisqNdf[%d]", arm);
    sprintf(title,"MUT arm%d Track Chisq/ndf", arm);
    MutTrkChisqNdf[arm]= new TH1F(id,title,100,0,100);
    hm->registerHisto(MutTrkChisqNdf[arm]);

    //Vtx chisquare/ndf
    sprintf(id,"VtxChisqNdf[%d]", arm);
    sprintf(title,"MUT arm%d Vtx Chisq/ndf", arm);
    VtxChisqNdf[arm]= new TH1F(id,title,100,0,100);
    hm->registerHisto(VtxChisqNdf[arm]);


    //MutCluster Chisquare
     sprintf(id,"MutClusChisquare[%d]",arm);
     sprintf(title,"MUT Arm%d Cluster Chisquare", arm);
     MutClusChisq[arm]= new TH1F(id,title,100,0,100);
     hm->registerHisto(MutClusChisq[arm]);

    // HitsinPacket/arm
    sprintf(id, "MutHitsPerPacketArm[%d]", arm);
    sprintf(title, "MUT%d hits per packet per event ", arm);
    MutHitsPerPacketArm[arm] = new TProfile(id, title,TOTPACKETARM[arm],-0.5, TOTPACKETARM[arm]);
    hm->registerHisto(MutHitsPerPacketArm[arm]);

    // add amu difference error check per packet
    sprintf(id, "MutAmuErrorPerPacketArm[%d]", arm);
    sprintf(title, "MUT%d Ave. number of AMU-cell errors per event ", arm);
    MutAmuErrorPerPacketArm[arm] = new TProfile(id, title,TOTPACKETARM[arm],0., TOTPACKETARM[arm]);
    hm->registerHisto(MutAmuErrorPerPacketArm[arm]);
    
     for (sta = 0; sta < NUMSTATION; sta++)
     {
    // Clusterwidth/arm
     sprintf(id, "MutCathClustWidth[%d][%d]", arm, sta);
     sprintf(title, "MUT arm%d station%d cathode strip cluster width", arm, sta);
     MutCathClustWidth[arm][sta] = new TH1F(id, title,HISTMAXWIDTH,-0.5, HISTMAXWIDTH - 0.5);
     hm->registerHisto(MutCathClustWidth[arm][sta]);


  
      // Number of clusters/arm/sta
      sprintf(id, "MutNumCathClustersSt[%d][%d]", arm, sta);
      sprintf(title, "MUT%d number of fitted cathode strip clusters/plane/event (Sta.%d)", arm, sta);
      MutNumCathClustersSt[arm][sta] = new TH1F(id, title,HISTMAXFITNSTRIPS,0, HISTMAXFITNSTRIPS);
      MutNumCathClustersSt[arm][sta]->SetXTitle("# cathode clusters");
      hm->registerHisto(MutNumCathClustersSt[arm][sta]);

      // MutHits/arm/sta
      sprintf(id, "MutHitsPerPlaneSt[%d][%d]", arm, sta);
      sprintf(title, "MUT%d hits per plane per event St%d", arm, sta);
      MutHitsPerPlaneSt[arm][sta] = new TProfile(id, title,TOTPLANEST[sta],-0.5, TOTPLANEST[sta]);
      MutHitsPerPlaneSt[arm][sta]->SetXTitle("Plane number");
      MutHitsPerPlaneSt[arm][sta]->SetYTitle("Average number of hits");
      hm->registerHisto(MutHitsPerPlaneSt[arm][sta]);

      
       for (oct = 0; oct < NUMOCTANT; oct++)
       for (gap = 0; gap < NUMGAP; gap++)
        {
         if(sta==2 && gap ==2) continue;                // Station 3 have only 2 gaps

         // ClusterCharge/arm/sta/oct/gap as landau parameters are defined in simulation
         sprintf(id, "MutCathClustQPeakSt[%d][%d][%d][%d]", arm, sta, oct, gap);
         sprintf(title, "MUT%d cathode cluster charge peak St%d octant%d gap%d", arm, sta, oct, gap);
         MutCathClustQPeakSt[arm][sta][oct][gap] = new TH1F(id, title,300,0, HISTMAXPEAKCHARGE);
         MutCathClustQPeakSt[arm][sta][oct][gap]->SetXTitle("Cluster peak charge");
         hm->registerHisto(MutCathClustQPeakSt[arm][sta][oct][gap]);

        // MutAdcPulses/arm/sta/oct/gap
         sprintf(id, "MutPulseSamples[%d][%d][%d][%d]", arm, sta, oct, gap);
         sprintf(title, "MUT arm%dsta%doct%dgap%d Four samples of cathode pulse", arm,sta,oct,gap);
         MutAdcPulses[arm][sta][oct][gap] = new TH1F(id, title, 2000, 0, 2000);
         hm->registerHisto(MutAdcPulses[arm][sta][oct][gap]);
	}


        for(gap=0; gap< NUMGAP; gap++){
        if(sta==2 && gap==2) continue;      
        sprintf(id, "MutHitCoord[%d][%d][%d]", arm, sta, gap);
        sprintf(title, "Mutr Hit Coordinate - Arm[%d] Sta[%d] Gap[%d]", arm, sta, gap);
        if ( sta == 0 )
        {
          MutHitCoord[arm][sta][gap] = new TH2F(id, title, 300, -150, 150, 300, -150, 150);
          hm->registerHisto(MutHitCoord[arm][sta][gap]);
        }
        if ( sta == 1 )
        {
          MutHitCoord[arm][sta][gap] = new TH2F(id, title, 600, -300, 300, 600, -300, 300);
          hm->registerHisto(MutHitCoord[arm][sta][gap]);
        }
        if ( sta == 2 )
        {
          MutHitCoord[arm][sta][gap] = new TH2F(id, title, 1000, -500, 500, 1000, -500, 500);
          hm->registerHisto(MutHitCoord[arm][sta][gap]);
         }
        }

	// plane level histograms
	for( oct = 0; oct < NUMOCTANT; oct++ ) for( gap = 0; gap < NUMGAP; gap++ )
	  for( cath = 0; cath < NUMCATH; cath++ ){	   
	    if( sta == 2 && gap == 2 ) continue;// Station 3 have only 2 gaps

	    // MutQtotPlane/arm/sta/oct/gap/cath
	    sprintf( id, "MutQTotPlane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d total q at trkass clstr",
		     arm, sta, oct, gap, cath );
	    MutQTotPlane[ arm ][ sta ][ oct ][ gap ][ cath ] 
	      = new TH1F( id, title, 200, +0.0, +200.0 );
	    hm->registerHisto( MutQTotPlane[ arm ][ sta ][ oct ][ gap ][ cath ] );

	    // MutPkClszPlane/arm/sta/oct/gap/cath
	    sprintf( id, "MutPkClszPlane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d clstr size at trkass clstr",
		     arm, sta, oct, gap, cath );
	    MutPkClszPlane[ arm ][sta ][ oct ][ gap ][ cath ]
	      = new TH1F( id, title, 9, -0.5, 8.5 );
	    hm->registerHisto( MutPkClszPlane[ arm ][sta ][ oct ][ gap ][ cath ] );

	    // MutPkWPlane/arm/sta/oct/gap/cath
	    sprintf( id, "MutPkWPlane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d w dist at trkass clstr",
		     arm, sta, oct, gap, cath );
	    MutPkWPlane[ arm ][ sta ][ oct ][ gap ][ cath ]
	      = new TH1F( id, title, 100, -0.6, +0.6 );
	    hm->registerHisto( MutPkWPlane[ arm ][ sta ][ oct ][ gap ][ cath ] );

	    // MutQSubQPkPlane/arm/sta/oct/gap/cath
	    sprintf( id, "MutQSubQPkPlane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d (qtot-qpk)/qpk at trkass clstr",
		     arm, sta, oct, gap, cath );
	    MutQSubQPkPlane[ arm ][ sta ][ oct ][ gap ][ cath ]
	      = new TH1F( id, title, 100, +0.0, +1.1 );
	    hm->registerHisto( MutQSubQPkPlane[ arm ][ sta ][ oct ][ gap ][ cath ] );

	    // MutQPk20QPk2Plane/arm/sta/oct/gap/cath
	    sprintf( id, "MutQPk20QPk2Plane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d (qpk_2-qpk_0)/qpk_2 at pkstrp",
		     arm, sta, oct, gap, cath );
	    MutQPk20QPk2Plane[ arm ][ sta ][ oct ][ gap ][ cath ]
	      = new TH1F( id, title, 100, -1.5, +1.5 );
	    hm->registerHisto( MutQPk20QPk2Plane[ arm ][ sta ][ oct ][ gap ][ cath ] );

	    // MutTimePkPlane/arm/sta/oct/gap/cath
	    sprintf( id, "MutTimePkPlane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d 4 sample peak timing at pkstrp",
		     arm, sta, oct, gap, cath );
	    MutTimePkPlane[ arm ][ sta ][ oct ][ gap ][ cath ]
	      = new TH1F( id, title, 100, 0.0, +15.0 );
	    hm->registerHisto( MutTimePkPlane[ arm ][ sta ][ oct ][ gap ][ cath ] );

	    // MutQ20Q2Plane/arm/sta/oct/gap/cath
	    sprintf( id, "MutQ20Q2Plane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d (q_2-q_0)/q_2",
		     arm, sta, oct, gap, cath );
	    MutQ20Q2Plane[ arm ][ sta ][ oct ][ gap ][ cath ]
	      = new TH1F( id, title, 100, -1.5, +1.5 );
	    hm->registerHisto( MutQ20Q2Plane[ arm ][ sta ][ oct ][ gap ][ cath ] );

	    // MutClszPlane/arm/sta/oct/gap/cath
	    sprintf( id, "MutClszPlane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d clstr size at coordass clstr",
		     arm, sta, oct, gap, cath );
	    MutClszPlane[ arm ][sta ][ oct ][ gap ][ cath ]
	      = new TH1F( id, title, 9, -0.5, 8.5 );
	    hm->registerHisto( MutClszPlane[ arm ][sta ][ oct ][ gap ][ cath ] );

	    // MutWPlane/arm/sta/oct/gap/cath
	    sprintf( id, "MutWPlane[%d][%d][%d][%d][%d]", arm, sta, oct, gap, cath );
	    sprintf( title, "MUT arm%dsta%doct%dgap%dcath%d w dist at at coordass clstr",
		     arm, sta, oct, gap, cath );
	    MutWPlane[ arm ][ sta ][ oct ][ gap ][ cath ]
	      = new TH1F( id, title, 100, -0.6, +0.6 );
	    hm->registerHisto( MutWPlane[ arm ][ sta ][ oct ][ gap ][ cath ] );
	  }

      }   // for station
     }   // for arm

  return 0;
}

//___________________________________________________________________________________________________________
int QAMut::process_event(PHCompositeNode *top_node)
{
  _counter++;
  
  PHIODataNode<RunHeader>::iterator fRunHeader(top_node);
  if ( !fRunHeader.find("RunHeader") ) { cout << "QAMut::mutHistFill RunHeader Not Found." << endl; }
  else { _run_number = (*fRunHeader).get_RunNumber(); }

  // fill clusters related histograms
  fill_mut_clusters_hists( top_node );

  // fill tracks related histograms
  fill_mut_tracks_hists( top_node );

  // fill packets related histograms
  fill_mut_packets_hists( top_node );

  // fill plane level histograms
  fill_mut_plane_hists( top_node );

  // fill vtx fitting histograms
  fill_vtx_hists( top_node );

  return 0;
}

//________________________________________________________________________________
void QAMut::fill_mut_clusters_hists( PHCompositeNode *top_node )
{
  
  // clusters
  try
  {
    // first, look at things on cluster level
    TMutClusMap* clus_map = TMutNode<TMutClusMap>::find_node(top_node, "TMutClusMap");
    TMutClusMap::const_iterator clus_iter = clus_map->range();
    
    int nclusters[NUMARM][NUMSTATION];
    memset(nclusters,0,sizeof(nclusters));
    int hitsperplane[NUMARM][NUMSTATION][NUMOCTANT][MAXNUMPLANE];
    
    // set all elements in multi-dim arrays : hitperplane and nhits, to 0
    memset(hitsperplane, 0, sizeof(hitsperplane));
    
    while (TMutClusMap::const_pointer clus_ptr = clus_iter.next())
    {
      //nclusters++;
      
      int arm = clus_ptr->get()->get_arm();
      int station = clus_ptr->get()->get_station();
      int octant = clus_ptr->get()->get_octant();
      //int halfoctant = clus_ptr->get()->get_half_octant();
      int gap = clus_ptr->get()->get_gap();
      int cath = clus_ptr->get()->get_cathode();
      


     // BbcOut* _bbcout = TMutNode<BbcOut>::find_io_node( top_node, "BbcOut" );
     // double bbc_charge = _bbcout->get_ChargeSum(0) + _bbcout->get_ChargeSum(1);
     // if( bbc_charge > 300) 
     // {
        float total_clustercharge=0;   
        TMutHitMap::const_key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
        while(TMutHitMap::const_pointer hit_ptr = hit_iter.next())
        {
        // m = hit_ptr->get()->get_strip();
        
      /*  const PdbMutCalibStrip *dbstrip = MutCalib()->getPdbMutCalibStrip(arm,
          station,
          octant,
          halfoctant,
          gap, 
          cath,
          hit_ptr->get()->get_strip()); */
        float qstrip = hit_ptr->get()->get_q();
        total_clustercharge += qstrip;
    
      // MutCathClustQPeak[(int) arm]->Fill(qstrip*dbstrip->get_gain());
      // MutCathClustQPeakSt[(int) arm][(int) station][(int) octant][(int) gap]->Fill(qstrip*dbstrip->get_gain());
      }
     
      MutCathClustQPeak[(int) arm]->Fill(total_clustercharge);
      MutCathClustQPeakSt[(int) arm][(int) station][(int) octant][(int) gap]->Fill(total_clustercharge);
    //  }
    
     float cluswid = clus_ptr->get()->get_n_strip();
      MutCathClustWidth[arm][station]->Fill(cluswid);


      float cluschisq= clus_ptr->get()->get_chi_square();
      MutClusChisq[arm]->Fill(cluschisq);

      int nfit = clus_ptr->get()->get_n_centroid();
      
      
      int plane = gap * NUMCATH + cath;
      nclusters[arm][station]++;
      
     hitsperplane[arm][station][octant][plane] += nfit;

    }
    
    // done with cluster loop, now fill counter histograms
      for ( int arm = 0; arm < NUMARM; arm++)
      for ( int station = 0; station < NUMSTATION; station++)
      {
       if(nclusters[arm][station] > 0) MutNumCathClustersSt[arm][station]->Fill( nclusters[arm][station]/NUMPLANEST[station]);    
       for( int octant=0; octant <NUMOCTANT; octant++)
       for( int plane=0; plane < NUMPLANEST[station]; plane++)
       {
        int pla = octant* NUMPLANEST[station] + plane;
        MutHitsPerPlaneSt[arm][station]->Fill(pla, (Axis_t) hitsperplane[arm][station][octant][plane]);        
       }

      }
        
  } catch ( std::exception &e ) { cout << e.what() << endl; }
}

//_______________________________________________
void QAMut::fill_mut_tracks_hists( PHCompositeNode* top_node )
{
  // tracks
  try
  {
    
    // next, on to the track part/level
    
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(top_node, "TMutTrkMap");
    TMutTrkMap::const_iterator trk_iter = trk_map->range();
    
    int ngoodtrk[NUMARM] = {0};
    
    while (TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
    {
      TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
      while (TMuiRoadMapO::const_pointer road_ptr = road_iter.next())
        if (!road_ptr->get
        ()->get_ghost_flag())
      {
       int arm = trk_ptr->get()->get_arm();
       float ptot = trk_ptr->get()->get_trk_par()->get_ptot();
       double chisq =  trk_ptr->get()->get_chi_square();
       size_t ndf =  trk_ptr->get()->get_ndf();

        ngoodtrk[arm]++;
        MutTrackMom[arm]->Fill(ptot);
        if(ndf !=0){ MutTrkChisqNdf[arm]->Fill(chisq/ndf); }
        else{  MutTrkChisqNdf[arm]->Fill(9999); }
      }
      
    }
     
    // done with track level, fill counter histos
   for ( int arm = 0; arm < NUMARM; arm++)
     if(ngoodtrk[arm]>0)MutNumTracks[arm]->Fill(ngoodtrk[arm]);
   }
  catch ( std::exception &e )
  {
    cout << e.what() << endl;
  }
  
}

//_______________________________________________________
void QAMut::fill_mut_packets_hists( PHCompositeNode *top_node )
{
  
  // strips sample check and FEM problem check
  try
  {
    
    //hit  per packet
    int hitsperpacket[NUMARM][MAXNUMPACKET];
    unsigned int packetId( 0 );
    
    // amu diff per packet
    int Amuerrorperpacket[NUMARM][MAXNUMPACKET];
    
    memset(hitsperpacket, 0, sizeof(hitsperpacket));
    memset(Amuerrorperpacket, 0, sizeof(Amuerrorperpacket));
    
    
    TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(top_node, "TMutHitMap");
    TMutHitMap::const_iterator hit_iter = hit_map->range();
    
    while (TMutHitMap::const_pointer hit_ptr = hit_iter.next())
    {
      
      packetId = 0;
      
      int arm = hit_ptr->get()->get_arm();
      int station = hit_ptr->get()->get_station();
      int octant = hit_ptr->get()->get_octant();
      int halfoctant = hit_ptr->get()->get_half_octant();
      int gap = hit_ptr->get()->get_gap();
      int cath = hit_ptr->get()->get_cathode();
      int strip = hit_ptr->get()->get_strip();
      
      int amu0 = hit_ptr->get()->get_amu(0);
      int amu1 = hit_ptr->get()->get_amu(1);
      int amu2 = hit_ptr->get()->get_amu(2);
      int amu3 = hit_ptr->get()->get_amu(3);
      
      int amudif10 = abs(amu1 - amu0);
      int amudif21 = abs(amu2 - amu1);
      int amudif32 = abs(amu3 - amu2);
      
      
      MutArm* arm_ptr( (arm == MUTOO::South) ? SouthArm() : NorthArm());
      
      /*
      check hit indexes validity
      this is needed since the arm_ptr access will not throw any exception
      */
      
      BOUNDS_CHECK( station, MUTOO::NumberOfStations );
      BOUNDS_CHECK( octant, MUTOO::NumberOfOctants );
      BOUNDS_CHECK( halfoctant, MUTOO::NumberOfHalfOctants );
      
      int n_gaps(arm_ptr->f_pMutStations[station]
        ->f_pMutOctants[octant]
        ->f_pMutHalfOctants[halfoctant]->getNumberOfGaps() );
      BOUNDS_CHECK( gap, n_gaps );
      
      BOUNDS_CHECK( cath, MUTOO::NumberOfCathodePlanes );
      
      unsigned int cathode_id( ( cath == 0 ) ? MUTGEOM::Cathode1 : MUTGEOM::Cathode2 );
      int n_strips(arm_ptr->f_pMutStations[station]
        ->f_pMutOctants[octant]
        ->f_pMutHalfOctants[halfoctant]
        ->f_pMutGaps[gap]
        ->f_pMutPlanes[cathode_id]->f_pMutStrips.size( ) );
      BOUNDS_CHECK( strip, n_strips );
      
      packetId = arm_ptr->f_pMutStations[station]
        ->f_pMutOctants[octant]
        ->f_pMutHalfOctants[halfoctant]
        ->f_pMutGaps[gap]
        ->f_pMutPlanes[cathode_id]
        ->f_pMutStrips[strip]
        ->getPacket_ID();
      
      packetId = ( arm == 0 ) ? packetId - 11001 :packetId - 11171;
      // packetId is unsigned int, checking for >=0 does not do anything
      //      if (packetId >= 0)
      {
        
        hitsperpacket[arm][packetId]++;
        
        if ( !((amudif10 == 5 || amudif10 == 59 ) && (amudif21 == 1 || amudif21 == 63) && ( amudif32 == 63 || amudif32 == 1 )))
        { Amuerrorperpacket[arm][packetId]++; }
        
      }
      
      
      // use {pedestal - adc(2)} > 100 check 4 sample
      
      float pedestal = 0.;
      
      int p = hit_ptr->get()->get_adc(2);
      
      const PdbMutCalibStrip *dbstrip = MutCalib()->getPdbMutCalibStrip(arm,
        station,
        octant,
        halfoctant,
        gap,
        cath,
        strip);
      
      if (dbstrip) pedestal = dbstrip->get_pedestal();
      
      if ((pedestal - p) > 100)
      {
        for (int isample = 0; isample < NUMSAMPLES; isample++)
        {
          MutAdcPulses[arm][station][octant][gap]->Fill( TMath::Abs(pedestal-(hit_ptr->get()->get_adc(isample))) );
        }
      }
      
    }
    
    
    // fill hits per packet and amu diff error per packet
    
    for (int arm = 0; arm < NUMARM; arm++)
    {
      for (int packet = 0; packet < TOTPACKETARM[arm]; packet++)
      {
        MutHitsPerPacketArm[arm]->Fill(packet, (Axis_t) hitsperpacket[arm][packet]);
        if (hitsperpacket[arm][packet] > 0)
        {
          if (Amuerrorperpacket[arm][packet] > 0)
            MutAmuErrorPerPacketArm[arm]->Fill(packet, 1);
          else
            MutAmuErrorPerPacketArm[arm]->Fill(packet, 0);
        }
      }
    }
    
  }
  catch ( std::exception &e )
  {
    cout << e.what() << endl;
  }

  // This code is for radiograph
  
  // Loop over gap_coord
  //
  TMutGapCoordMap* gap_coord_map = TMutNode<TMutGapCoordMap>::find_node(top_node, "TMutGapCoordMap");
  TMutGapCoordMap::const_iterator gap_coord_iter = gap_coord_map->range();
  while (TMutGapCoordMap::const_pointer gap_coord_ptr = gap_coord_iter.next())
  {
    int arm = gap_coord_ptr->get()->get_arm();
    int station = gap_coord_ptr->get()->get_station();
    int gap = gap_coord_ptr->get()->get_gap();
    float coord_x = (gap_coord_ptr->get()->get_coord()).getX();
    float coord_y = (gap_coord_ptr->get()->get_coord()).getY();
    MutHitCoord[arm][station][gap]->Fill(coord_x, coord_y);
    
  }
}

//_______________________________________________________________________
void QAMut::fill_vtx_hists( PHCompositeNode *top_node )
{

 TMutVtxMap* vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
 TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
 
 while( TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next() )
  {
    double vtx_chisq = vtx_ptr->get()->get_chi_square();
    double vtx_ndf = vtx_ptr->get()->get_ndf();
    int arm = vtx_ptr->get()->get_arm();
    if( vtx_ndf >0 )VtxChisqNdf[arm]->Fill(vtx_chisq/vtx_ndf);
  }

}

//_______________________________________________________________________
void QAMut::fill_mut_plane_hists( PHCompositeNode *top_node )
{

  TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(top_node, "TMutTrkMap");
  TMutHitMap* hit_map = TMutNode<TMutHitMap>::find_node(top_node, "TMutHitMap");
  TMutCoordMap* coord_map = TMutNode<TMutCoordMap>::find_node(top_node, "TMutCoordMap");

  TMutTrkMap::iterator itr_trk = trk_map->range();
  while( TMutTrkMap::pointer ptr_trk = itr_trk.next() ){
    TMutTrk* mut_trk = ptr_trk->get();

    if( mut_trk->get_ghost() ) continue;

    int arm_ = mut_trk->get_arm();
    int oct_ = mut_trk->get_octant();
    bool phit_[ 16 ] = { false };
     
    TMutCoordMap::key_iterator kit_coord 
      = mut_trk->get_associated< TMutCoord >();
    while( TMutCoordMap::pointer ptr_coord = kit_coord.next() ){
      TMutCoord* mut_coord = ptr_coord->get();
      
      int st_ = mut_coord->get_station();
      int hoct_ = mut_coord->get_half_octant();
      int gap_ = mut_coord->get_gap();
      int cath_ = mut_coord->get_cathode();
      phit_[ cath_ + 2 * gap_ + 6 * st_ ] = true;

      int pkstrp_ = mut_coord->get_peak_strip();
      double qtot_ = mut_coord->get_q_tot();
      double pkw_ = mut_coord->get_w();
      double qpk_ = mut_coord->get_q_peak();

      int clsz_ = -1;
      TMutClusMap::key_iterator kit_clus 
	= mut_coord->get_associated< TMutClus >();
      while( TMutClusMap::pointer ptr_clus = kit_clus.next() ){
	TMutClus* mut_clus = ptr_clus->get();
	clsz_ = mut_clus->get_n_strip();
      }//TMutClusMap (trkass)

      // reserve information on effective charge (trk ass.) 
      MutQTotPlane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]->Fill( qtot_ );
      MutPkClszPlane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]->Fill( clsz_ );
      if( clsz_ > 1 && clsz_ < 6 ){
	MutPkWPlane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]->Fill( pkw_ );
	MutQSubQPkPlane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]
	  ->Fill( ( qtot_ - qpk_ ) / qpk_ );
      }

      double q4_[ 4 ];

      // reserve 4sample-shape information for all/peak hits in the plane
      TMutHitMap::iterator itr_hit 
	= hit_map->get( arm_, st_, oct_, hoct_, gap_, cath_ );
      while( TMutHitMap::pointer ptr_hit = itr_hit.next() ){
	TMutHit* mut_hit = ptr_hit->get();

	for( int is = 0; is < 4; is++ )
	  q4_[ is ] = this->get_calibed( mut_hit, is ); 

	MutQ20Q2Plane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]
	  ->Fill( ( q4_[ 2 ] - q4_[ 0 ] ) / q4_[ 2 ] );

	if( mut_hit->get_strip() == pkstrp_ ){
	  MutQPk20QPk2Plane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]
	    ->Fill( ( q4_[ 2 ] - q4_[ 0 ] ) / q4_[ 2 ] );

	  vector< double > fitpar_ = this->get_fitpar( mut_hit );
	  if( fitpar_.size() )
	    MutTimePkPlane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]
	      ->Fill( fitpar_[ 1 ] );
	}

      }//TMutHitMap
      
      // more hists for raw-like (coordass) clsz & w distribution
      TMutCoordMap::iterator itr_coord_t
	= coord_map->get( arm_, st_, oct_, hoct_, gap_, cath_ );
      while( TMutCoordMap::pointer ptr_coord_t = itr_coord_t.next() ){
	TMutCoord* mut_coord_t = ptr_coord_t->get();

	double pkw_t_ = mut_coord_t->get_w();

	int clsz_t_ = -1;
	TMutClusMap::key_iterator kit_clus_t 
	  = mut_coord_t->get_associated< TMutClus >();
	while( TMutClusMap::pointer ptr_clus_t = kit_clus_t.next() )
	  clsz_t_ = ptr_clus_t->get()->get_n_strip();

	MutClszPlane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]->Fill( clsz_t_ );
	if( clsz_t_ > 1 && clsz_t_ < 6 )
	  MutWPlane[ arm_ ][ st_ ][ oct_ ][ gap_ ][ cath_ ]->Fill( pkw_t_ );
      }//TMutCoordMap

    }//TMutCoordMap (trkass)

    // fill 0 for naive hit inefficiency 
    for( int pid = 0; pid < 16; pid++ ) if( !phit_[ pid ] )
      MutPkClszPlane[ arm_ ][ pid/6%3 ][ oct_ ][ pid/2%3 ][ pid%2 ]->Fill( 0 );

  }//TMutTrkMap

}

double QAMut::get_calibed( TMutHit* mut_hit, const int& i ){

  const PdbMutCalibStrip* dbstrip_ 
    = MutCalib()->getPdbMutCalibStrip( mut_hit->get_arm(),
				       mut_hit->get_station(),
				       mut_hit->get_octant(),
				       mut_hit->get_half_octant(),
				       mut_hit->get_gap(),
				       mut_hit->get_cathode(),
				       mut_hit->get_strip() );
  if( !( dbstrip_ && dbstrip_->isValid() ) ){
    cout << "warning: calib db tracking fail, return gain=1.0" << endl;
    return -1.0;
  }

  double adc_ = mut_hit->get_adc( i );

  int DACSaturation = dbstrip_->getSaturation();
  int ADCSaturation = dbstrip_->getAdc( DACSaturation );

  // set saturation flag
  if( adc_ == MUTOO_FEM::ADC_MIN || adc_ <= ADCSaturation )
    return -1.0;

  // apply the calibration including non-linear correction 
  // ( ped - adc ) + non-liner corr. / gain 
  // and obtain the resultant q ( adc ) sample
  double calibed_q_ = dbstrip_->getCharge( adc_ );

  // negative charge means that
  // adc could not be converted into charge
  if( calibed_q_ < 0 ) return -1.0;
    
  return calibed_q_;
}

Double_t QAMut::pol2fit( Double_t* x, Double_t *par ){
  return par[ 0 ] * pow( x[ 0 ] - par[ 1 ], 2.0 ) + par[ 2 ];
};

vector< double > QAMut::get_fitpar( TMutHit* mut_hit ){

  TH1F tmp_( "tmp_", "tmp_", 15, -2.5, 12.5 );

  tmp_.Fill( 0.0, this->get_calibed( mut_hit, 0 ) );
  tmp_.Fill( 5.0, this->get_calibed( mut_hit, 1 ) );
  tmp_.Fill( 6.0, this->get_calibed( mut_hit, 2 ) );
  tmp_.Fill( 7.0, this->get_calibed( mut_hit, 3 ) );
  /*
  tmp_.Fill( 0.0, mut_hit->get_adc( 0 ) );
  tmp_.Fill( 5.0, mut_hit->get_adc( 1 ) );
  tmp_.Fill( 6.0, mut_hit->get_adc( 2 ) );
  tmp_.Fill( 7.0, mut_hit->get_adc( 3 ) )
  */

  // f1 initialization
  TF1 f1_( "f1", &QAMut::pol2fit, 0.0, 10.0, 3 );
  f1_.SetParameters( -0.1, 7, 15 );
  f1_.SetParLimits( 0, -10.0, 0.0 );
  /*
  TF1 f1_( "f1", &QAMut::pol2fit, 0.0, 10.0, 3 );
  f1_.SetParameters( 1.0, 7, 1650 );
  f1_.SetParLimits( 0, 0.0, 100.0 );
  */

  tmp_.Fit( "f1", "IRQOWN" );

  double a_, b_, c_; int d_, e_, f_;
  gMinuit->mnstat( a_, b_, c_, d_, e_, f_ );
  /*
    FMIN: the best function value found so far // chi2??

    ISTAT: a status integer indicating how good is the covariance matrix:  
    0= not calculated at all
    1= approximation only, not accurate
    2= full matrix, but forced positive-definite
    3= full accurate covariance matrix
  */

  vector< double > tmppar_( 4, 0.0 );

  // requires full accuracy
  if( f_ < 3 ){ tmppar_.resize( 0 ); return tmppar_; }

  for( int i = 0; i < 3; i++ )
    tmppar_[ i ] = f1_.GetParameter( i );

  tmppar_[ 3 ] = f1_.GetChisquare();

  return tmppar_;
}

//________________________________________________________________________
int QAMut::End(PHCompositeNode *topNode)
{
  if( _counter <= 0 )
  {
    cerr 
      << "QAMut::End -  exiting; invalid counter value: "
      << _counter << endl;
    return 0;
  }
 
  return 0;
}
