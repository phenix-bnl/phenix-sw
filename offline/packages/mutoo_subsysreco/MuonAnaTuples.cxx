// $Id: MuonAnaTuples.cxx,v 1.58 2017/10/25 19:54:56 shlim Exp $

/*!
  \file    MuonAnaTuples.cxx
  \ingroup supermodules
  \brief   basic ntuples for data evaluation and mutoo reconstruction
  \author  Hugo Pereira
  \version $Revision: 1.58 $
  \date    $Date: 2017/10/25 19:54:56 $
*/

#include <PHTFileServer.h>
#include <MutCalib.h>
#include <TNtuple.h>
#include <EventHeader.h>
#include <Fun4AllReturnCodes.h>

#include <TMuiRoadMapO.h>
#include <TMui1DRoadMapO.h>
#include <TMuiClusterMapO.h>
#include <TMuiHitMapO.h>
#include <TMuiPseudoBLTMapO.h>

#include <TMutTrackUtil.h>
#include <TMutHitMap.h>
#include <TMutClusMap.h>
#include <TMutCoordMap.h>
#include <TMutStubMap.h>
#include <TMutTrkMap.h>
#include <TMutVtxMap.h>
#include <TMutGeo.h>

#include <MUTOO_FEM.h>
#include <MUTGEOM.h>

#include <TMuiRoadMapO.h>
#include <MuiGeomClasses.hh>
#include <MutGeom.h>
#include <MutStrip.h>
#include <MuonUtil.h>
#include <PhMutooDisplay.h>
#include <PHTimer.h>
#include <ReactionPlaneObject.h>
#include <ReactionPlaneSngl.h>
#include <VtxOut.h>

#include <bitset>
#include <fstream>
#include <sstream>

// Database
#include <TMutDatabaseInit.h>

#include "MuonAnaTuples.h"

using namespace std;

//_________________________________________________________________
MuonAnaTuples::MuonAnaTuples( const char* name,  const char* filename ):
  SubsysReco( name ),
  _flags( ALL_NTUPLES ),
  _event_stream_ptr( 0 ),
  _pp_flag( false ),
  vertexname( "BBC" ),
  rp_code( 18 ),
  _timer( PHTimeServer::get()->insert_new( name) )
{
  _filename = filename ? filename:"muon_ana_ntuples.root";

  // initialize ntuples
  _muioo = 0;
  _dimu_reco = 0;
  _stpp = 0;
  _strip_data = 0;
  _event = 0;
  _muid_hits = 0;
  _mutr_hits = 0;
  _ClustData2 = 0;
  _trk_stpp = 0;
  _clus_reco = 0;

  return ;
}

//_________________________________________________________________
int MuonAnaTuples::Init(PHCompositeNode *topNode)
{

  MUTOO::PRINT( cout, "MuonAnaTuples::Init" );

  // only create TFile if there is at least one ntuple filled
  if( _flags & ALL_NTUPLES )
  {
    cout << "opening TFile " << _filename << endl;
    PHTFileServer::get().open( _filename, "RECREATE" );
  }

  if( _flags & RECO )
  {

    _dimu_reco = new TNtuple("dimu_reco","dimu_reco","mass_reco:sign_reco:ntrk:nvtx:chi_vtx:ptot1:xst1:yst1"
      ":depth1:chi1:nhit1:ptot2:xst2:yst2:depth2:chi2:nhit2:blt_fire:"
      "event:centc:centp");

    cout << "_dimu_reco ntuple booked" << endl;
  }

  if( _flags & MUID_HITS )
  {
    _muid_hits = new TNtuple("muid_hits","muid_hits","arm:plane:panel:orient:offset:event:ntot");
    cout << "_muid_hits ntuple booked" << endl;
  }

  if( _flags & MUIOO )
  {
    _muioo = new TNtuple("muioo","muioo","arm:rx:ry:rz:depth:has_trk");
    cout << "_muioo ntuple booked" << endl;
  }

  if( _flags & STRIP )
  {
    _strip_data = new TNtuple("strip_data","strip_data","arm:station:octant:half:gap:cath:"
      "strip:adc1:adc2:adc3:adc4:vpeak:tzero:event:chifit:pedestal:gain:rms:"
      "packet:dcmchan:amu1:amu2:amu3:amu4:xbegin:"
      "ybegin:xend:yend:cluswid:lastgap:pz:track_nhit:chiq:mass_reco:sign_reco:vtx_chiq:cent");
    cout << "_strip_data ntuple booked" << endl;
  }

  if( _flags & STPP )
  {
    _stpp = new TNtuple("stpp","stpp","arm:sta:oct:hoct:gap:cath:"
      "qpeak:qtot:offset:x1:y1:x2:y2:x3:y3:"
      "q1:adc1a:adc1b:adc1c:ped1:gain1:"
      "q2:adc2a:adc2b:adc2c:ped2:gain2:"
      "q3:adc3a:adc3b:adc3c:ped3:gain3");
    cout << "_stpp ntuple booked" << endl;
  }

  if( _flags & TRK_STPP )
  {
    _trk_stpp = new TNtuple("trk_stpp","trk_stpp","arm:sta:oct:hoct:gap:cath:cluswid:" //7
      "qpeak:qtot:offset:" //3
      "charge:ndf:chiq:nbit:nhit:lastgap:pt:pz:x1:y1:x2:y2:x3:y3");	 //5
    cout << "_trk_stpp ntuple booked" << endl;
  }

  if( _flags & CLUSTER )
  {
    _clus_reco = new TNtuple("clus_reco","clus_reco",
      "event:arm:station:octant:half:"   //5
      "gap:cath:cluswid:chi:nfit:"       //5
      "badmiddlestrip:badedgestrip:leftedgedet:rightedgedet:" //4
      "saturation:atten:badcalib:nocalib:" //4
      "strip0:strip1:strip2:strip3:strip4:strip5:strip6:strip7:" //8
      "q1:q2:q3:q4:q5:q6:q7:q8:"         //8
      "qe1:qe2:qe3:qe4:qe5:qe6:qe7:qe8:" //8
      "xfit1:xfit2:xfit3:xfit4:"         //4
      "we1:we2:we3:we4:"                 //4
      "qpeak1:qpeak2:qpeak3:qpeak4:"     //4
      "qtotal1:qtotal2:qtotal3:qtotal4:hastrk");     //4
    cout << "_clus_reco ntuple booked" << endl;
  }

  if( _flags & CLUSTER2 )
  {
    _ClustData2= new TNtuple("ClustData2","ClustData2","arm:sta:oct:hoct:gap:coord_x:coord_y:coord_z:nhits");
    cout << "_ClustData2 ntuple booked" << endl;
  }

  if( _flags & EVENT )
  {
    _event = new TNtuple("event","event","mutnhits_S:mutnhits_N:"
			 "s1g1c1S:s1g1c2S:s1g2c1S:s1g2c2S:s1g3c1S:s1g3c2S:"
			 "s2g1c1S:s2g1c2S:s2g2c1S:s2g2c2S:s2g3c1S:s2g3c2S:"
			 "s3g1c1S:s3g1c2S:s3g2c1S:s3g2c2S:"
			 "s1g1c1N:s1g1c2N:s1g2c1N:s1g2c2N:s1g3c1N:s1g3c2N:"
			 "s2g1c1N:s2g1c2N:s2g2c1N:s2g2c2N:s2g3c1N:s2g3c2N:"
			 "s3g1c1N:s3g1c2N:s3g2c1N:s3g2c2N:event:zvertex:centc:centp:b:"
			 "s1Sadc:s2Sadc:s3Sadc:s1Nadc:s2Nadc:s3Nadc:bbcn:bbcs:"
			 "muihitsS1:muihitsS2:muihitsS3:muihitsS4:muihitsS5:"
			 "muihitsN1:muihitsN2:muihitsN3:muihitsN4:muihitsN5:"
			 "dtime_mut:dtime_muid:dtime_fvtx:dtime_total");
    cout << "_event ntuple booked" << endl;
  }

  return 0;

}

//_________________________________________________________________
int MuonAnaTuples::process_event(PHCompositeNode *top_node)
{
  //  _timer.get()->restart();
  PHTypedNodeIterator<PHGlobal> global_iter(top_node);
  PHIODataNode<PHGlobal> *global_ndst = global_iter.find("PHGlobal");
  if(global_ndst) {
    _global = global_ndst->getData();
  } else {
    _global = 0;
  }

  if( _flags & RECO ) write_reco_ntuple(top_node);
  if( _flags & MUIOO ) write_muioo_ntuple(top_node);
  if( _flags & MUID_HITS ) write_muid_hits(top_node);
  if( _flags & STPP ) write_stpp_ntuple(top_node);
  if( _flags & TRK_STPP ) write_trk_stpp_ntuple(top_node);
  if( _flags & STRIP ) write_strip_data(top_node);
  if( _flags & CLUSTER ) write_clus_ntuple(top_node);
  if( _flags & CLUSTER2 ) write_ClustData2_ntuple(top_node);
  if( _flags & EVENT ) write_event_data(top_node);

  if( _flags & (VERTEX|CENTRALITY|REACTION_PLANE) ) dump_event( top_node );

  // wants to catch all time spent in the event
  //  _timer.get()->stop();

  return EVENT_OK;
}

//_________________________________________________________________
int MuonAnaTuples::End(PHCompositeNode* top_node)
{
//   _timer.get()->print_stat();

  // write ntuple only if at least one ntuple has been booked
  if( _flags & ALL_NTUPLES )
  { PHTFileServer::get().write( _filename ); }

  // close event stream, if any
  if( _event_stream_ptr ) {
    _event_stream_ptr->close();
    delete _event_stream_ptr;
  }

	return 0;
}


//_________________________________________________________________
void MuonAnaTuples::write_muid_hits(PHCompositeNode* top_node)
{
  TMuiClusterMapO*  clus_map = 0;
  try{
    clus_map = TMutNode<TMuiClusterMapO>::find_node(top_node,"TMuiClusterMapO");
  } catch(std::exception& e){
    return;
  }

  for(int arm=0; arm<2; ++arm)
  {
    for(int plane=0; plane<5; ++plane)
    {
      for(int panel=0; panel<6; ++panel)
      {
        for(int orient=0; orient<2; ++orient)
        {
          TMuiClusterMapO::const_iterator clus_iter = clus_map->get(arm,plane,panel,orient);
          while(TMuiClusterMapO::const_pointer clus_ptr = clus_iter.next())
          {
            float ntvar[100] = {0};
            ntvar[0] = clus_ptr->get()->get_arm();
            ntvar[1] = clus_ptr->get()->get_plane();
            ntvar[2] = clus_ptr->get()->get_panel();
            ntvar[3] = clus_ptr->get()->get_orientation();
            ntvar[4] = (clus_ptr->get()->get_orientation() == kHORIZ)  ? clus_ptr->get()->get_coord_end().getY() : clus_ptr->get()->get_coord_end().getX();
            // Event number
            //
            try {
              EventHeader* evt = TMutNode<EventHeader>::find_io_node(top_node,"EventHeader");
              ntvar[5] = evt->get_EvtSequence();
            } catch (std::exception& e) {}
            ntvar[6] = clus_iter.count();
            _muid_hits->Fill(ntvar);
          }
        }
      }
    }
  }
}

//_________________________________________________________________
void MuonAnaTuples::write_muioo_ntuple(PHCompositeNode* top_node)
{
  TMuiRoadMapO*  road_map = 0;
  try{
    road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
  }
  catch(std::exception& e){
    return;
  }

  TMuiRoadMapO::const_iterator road_iter = road_map->range();
  while(TMuiRoadMapO::const_pointer road_ptr = road_iter.next())
  {
    float ntvar[100] = {0};
    ntvar[0] = road_ptr->get()->get_arm();
    ntvar[1] = road_ptr->get()->get_gap0_point().getX();
    ntvar[2] = road_ptr->get()->get_gap0_point().getY();
    ntvar[3] = road_ptr->get()->get_gap0_point().getZ();
    ntvar[4] = road_ptr->get()->get_depth();
    ntvar[5] = road_ptr->get()->get_associated<TMutTrk>().count();
    _muioo->Fill(ntvar);
  }
}

//_________________________________________________________________
void MuonAnaTuples::write_reco_ntuple(PHCompositeNode* top_node)
{
  // Clear comparison array
  //
  _comp_var.assign(0.0);
  _nvtx_o=0;
  _nvtx_n=0;

  double cent( MuonUtil::get_centrality( top_node ) );

  TMutTrkMap*  trk_map = 0;
  TMutVtxMap*  vtx_map = 0;

  static int EventNum=1;

  // Reconstructed di-muons ntuple
  //
  try{
    trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
    vtx_map = TMutNode<TMutVtxMap>::find_node(top_node,"TMutVtxMap");
  }
  catch(std::exception& e){
    return;
  }

  TMutVtxMap::const_iterator vtx_iter = vtx_map->range();
  //  MUTOO::PRINT(std::cout,"MuonAnaTuples::write_reco_ntuple");
  while(TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next())
  {
    float nt_vars[100] = {0};
    nt_vars[0] = vtx_ptr->get()->get_mass();
    nt_vars[1] = vtx_ptr->get()->get_sign();
    nt_vars[2] = trk_map->size();
    nt_vars[3] = vtx_map->size();
    nt_vars[4] = vtx_ptr->get()->get_chi_square();
    nt_vars[5] = vtx_ptr->get()->get_ptot1();
    TMutTrkMap::const_key_iterator trk_iter = vtx_ptr->get()->get_associated<TMutTrk>();
    std::bitset<16> trk_p_hits;
    std::bitset<16> trk_n_hits;

    if(vtx_ptr->get()->get_sign()==0) {
      _comp_var[0] = vtx_ptr->get()->get_mass();
      ++_nvtx_n;
    }

    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
    {
      if(trk_ptr->get()->get_charge() == 1)
      {
        nt_vars[6] = trk_ptr->get()->get_trk_par()->get_x();
        nt_vars[7] = trk_ptr->get()->get_trk_par()->get_y();
        TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
        if(!road_iter.at_end()) nt_vars[8] = road_iter->get()->get_depth();
        nt_vars[9] = trk_ptr->get()->get_w_chi_square()/trk_ptr->get()->get_ndf();
        nt_vars[10] = trk_ptr->get()->get_ndf();
        trk_p_hits = get_hit_bitset(trk_ptr);
        if(vtx_ptr->get()->get_sign()==0)
        {
          _comp_var[2] = trk_p_hits.to_ulong();
          _comp_var[4] = trk_p_hits.count();
        }

      } else {
        nt_vars[11] = vtx_ptr->get()->get_ptot2();
        nt_vars[12] = trk_ptr->get()->get_trk_par()->get_x();
        nt_vars[13] = trk_ptr->get()->get_trk_par()->get_y();
        TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
        if(!road_iter.at_end()) nt_vars[14] = road_iter->get()->get_depth();
        nt_vars[15] = trk_ptr->get()->get_w_chi_square()/trk_ptr->get()->get_ndf();
        nt_vars[16] = trk_ptr->get()->get_ndf();

        // If 1d or 2d trigger fired
        //
        try {
          TMuiPseudoBLTMapO*  blt_map = TMutNode<TMuiPseudoBLTMapO>::find_node(top_node,"TMuiPseudoBLTMapO");
          TMuiPseudoBLTMapO::const_iterator blt_iter = blt_map->get(trk_ptr->get()->get_arm());
          if(!blt_iter.at_end()) nt_vars[17] = (blt_iter->get()->is_1D1S_fired() || blt_iter->get()->is_2D_fired()) ? 1.0 : 0.0;
        } catch(std::exception& e) {}

        // Write hits bitset into compare data members
        //
        trk_n_hits = get_hit_bitset(trk_ptr);
        if(vtx_ptr->get()->get_sign()==0) {
          _comp_var[6] = trk_n_hits.to_ulong();
          _comp_var[8] = trk_n_hits.count();
          _comp_var[10] = trk_ptr->get()->get_arm();
          _comp_var[12] = trk_ptr->get()->get_arm();
        }

      }
    }

    //    UShort_t arm = vtx_ptr->get()->get_arm();
    //    if(!vtx_ptr->get()->get_sign()){
    //      std::cout << "mass:" << nt_vars[0] << " arm:" << arm << std::endl;
    //      std::cout << "   oct_p:" << oct_p <<
    //        " trk_p:MuID<-[" << trk_p_hits << "]<-IP" << std::endl;
    //      std::cout << "   oct_n:" << oct_n <<
    //        " trk_n:MuID<-[" << trk_n_hits << "]<-IP" << std::endl;
    //    }

    nt_vars[18] = (float)EventNum;
    if (_global && !_pp_flag )
    {
  
      nt_vars[19] = cent;
      nt_vars[20] = cent;
    }

    _dimu_reco->Fill(nt_vars);
  }

  EventNum++;

  //  MUTOO::PRINT(std::cout,"**");
}


//_________________________________________________________________
std::bitset<16> MuonAnaTuples::get_hit_bitset(TMutTrkMap::const_pointer trk_ptr)
{
  std::bitset<16> hits;
  TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next())
    {
      UShort_t bit = coord_ptr->get()->get_station()*6 + coord_ptr->get()->get_gap()*2 + coord_ptr->get()->get_cathode();
      hits.set(bit);      
    }
  return hits;
}

//_________________________________________________________________
void MuonAnaTuples::write_event_data(PHCompositeNode* top_node)
{

  static int EventNum = 1;
  // s3g1c1N:s3g1c2N:s3g2c1N:s3g2c2N:zvertex:centc:centp:b");

  double cent( MuonUtil::get_centrality( top_node ) );

  float ntvar[100] = {0};

  TMutHitMap*  hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
  int hitsN = hit_map->get( MUTOO::North ).count();
  int hitsS = hit_map->get( MUTOO::South ).count();
  ntvar[0] = hitsS;
  ntvar[1] = hitsN;

  int hit_station = 0;

  for (int iarm=0; iarm<2; iarm++)
  {
    for (int istation=0; istation<3; istation++)
    {
      hit_station = 0;
      for (int ioctant=0; ioctant<8; ioctant++)
      {
        for (int ihalf=0; ihalf<2; ihalf++)
        {
          for (int igap=0; igap<3; igap++)
          {
            for (int icath=0; icath<2; icath++)
            {
              MUTOO::cathode_locator location(boost::make_tuple(iarm, istation, ioctant, ihalf, igap, icath) );
              TMutHitMap::iterator hit_iter = hit_map->get(location);
              ntvar[iarm*16+istation*6+igap*2+icath+2] += hit_iter.count();
              while(TMutHitMap::const_pointer hit_ptr = hit_iter.next())
              {

                const PdbMutCalibStrip *dbstrip = MutCalib()->getPdbMutCalibStrip(
                  hit_ptr->get()->get_arm(),
                  hit_ptr->get()->get_station(),
                  hit_ptr->get()->get_octant(),
                  hit_ptr->get()->get_half_octant(),
                  hit_ptr->get()->get_gap(),
                  hit_ptr->get()->get_cathode(),
                  hit_ptr->get()->get_strip());
                ntvar[iarm*3+istation+39] +=
                  dbstrip->get_pedestal() - hit_ptr->get()->get_adc(0);
              }
              hit_station += hit_iter.count();
            }
          }
        }
      }
      if (hit_station > 0)ntvar[iarm*3+istation+39] /= hit_station;
      else ntvar[iarm*3+istation+39] = 0.0;
    }  // Loop over stations
  }  // Loop over arms
  ntvar[34] = EventNum;

  if (_global)
  {
    ntvar[35] = _global->getBbcZVertex();
    if(!_pp_flag)
      {  ntvar[36] = cent;
        ntvar[37] = cent;
    } else{
        ntvar[36] = 0;
        ntvar[37] = 0;
   }
    ntvar[45] = _global->getBbcChargeN();
    ntvar[46] = _global->getBbcChargeS();
  }

  TMuiHitMapO*  mui_hit_map = TMutNode<TMuiHitMapO>::find_node(top_node,"TMuiHitMapO");
  for (int iarm=0; iarm<2; iarm++)
  {
    for (int ipl=0; ipl<6; ipl++)
    {
      int temp_hits = 0;
      // for this arm and plane - loop over all panels and orientations
      for (int ii=0; ii<6; ii++)
      {
        for (int jj=0; jj<2; jj++)
        {
          temp_hits +=   mui_hit_map->get(iarm,ipl,ii,jj).count();
        }
      }
      ntvar[47 + iarm*5 + ipl] = temp_hits;
    }
  }

  // time spent in all muon reconstruction
  PHTimeServer* muon_timer = PHTimeServer::get();
  double dtime_mut=0, dtime_muid=0, dtime_fvtx=0, dtime_total=0;
  if (muon_timer->get_timer("MUONDEV").get())
    dtime_mut = muon_timer->get_timer("MUONDEV").get()->elapsed();
  if (muon_timer->get_timer("MUIOORECO").get())
    dtime_muid = muon_timer->get_timer("MUIOORECO").get()->elapsed();
  if (muon_timer->get_timer("MUONANATUPLES").get())
    dtime_total = muon_timer->get_timer("MUONANATUPLES").get()->elapsed();
	/*
	//why we need FvtxReco timer here ?
  if (muon_timer->get_timer("FvtxReco").get())
    dtime_fvtx = muon_timer->get_timer("FvtxReco").get()->elapsed();
		*/

  _timer.get()->restart();

  ntvar[57] = dtime_mut;
  ntvar[58] = dtime_muid;
  ntvar[59] = dtime_fvtx;
  ntvar[60] = dtime_total;

  _event->Fill(ntvar);

  EventNum++;
}

//_________________________________________________________________
void MuonAnaTuples::write_strip_data(PHCompositeNode* top_node)
{

  static int EventNum=1;

  double cent( MuonUtil::get_centrality( top_node ) );

  TMutHitMap*  hit_map = TMutNode<TMutHitMap>::find_node(top_node,"TMutHitMap");
  TMutHitMap::const_iterator hit_iter = hit_map->range();

  PHPoint StripBegin, StripEnd;
  int CathodePlane;

  while(TMutHitMap::const_pointer hit_ptr = hit_iter.next())
  {

    float ntvar[100] = {0};
    int icount = 0;
    ntvar[icount++] = hit_ptr->get()->get_arm();
    ntvar[icount++] = hit_ptr->get()->get_station();
    ntvar[icount++] = hit_ptr->get()->get_octant();
    ntvar[icount++] = hit_ptr->get()->get_half_octant();
    ntvar[icount++] = hit_ptr->get()->get_gap();
    ntvar[icount++] = hit_ptr->get()->get_cathode();
    ntvar[icount++] = hit_ptr->get()->get_strip();

    if ( hit_ptr->get()->get_cathode()==0) CathodePlane = MUTGEOM::Cathode1;
    else CathodePlane = MUTGEOM::Cathode2;

    for (int i=0; i<4; i++) ntvar[icount++] = hit_ptr->get()->get_adc(i);

    ntvar[icount++] = hit_ptr->get()->get_q();
    ntvar[icount++] = hit_ptr->get()->get_t();
    ntvar[icount++] = EventNum;   //event number
    ntvar[icount++] = hit_ptr->get()->get_error_q();
    const PdbMutCalibStrip *dbstrip = MutCalib()->getPdbMutCalibStrip(hit_ptr->get()->get_arm(),
      hit_ptr->get()->get_station(),
      hit_ptr->get()->get_octant(),
      hit_ptr->get()->get_half_octant(),
      hit_ptr->get()->get_gap(),
      hit_ptr->get()->get_cathode(),
      hit_ptr->get()->get_strip());

    ntvar[icount++] = dbstrip->get_pedestal();
    ntvar[icount++] = dbstrip->get_gain();
    ntvar[icount++] = dbstrip->get_rms();
    MutArm* geometry = (hit_ptr->get()->get_arm() == MUTOO::South) ? SouthArm() : NorthArm();
    ntvar[icount++] = geometry->f_pMutStations[hit_ptr->get()->get_station()]
      ->f_pMutOctants[hit_ptr->get()->get_octant()]
      ->f_pMutHalfOctants[hit_ptr->get()->get_half_octant()]
      ->f_pMutGaps[hit_ptr->get()->get_gap()]
      ->f_pMutPlanes[CathodePlane]
      ->f_pMutStrips[hit_ptr->get()->get_strip()]->getPacket_ID();
    ntvar[icount++] = geometry->f_pMutStations[hit_ptr->get()->get_station()]
      ->f_pMutOctants[hit_ptr->get()->get_octant()]
      ->f_pMutHalfOctants[hit_ptr->get()->get_half_octant()]
      ->f_pMutGaps[hit_ptr->get()->get_gap()]
      ->f_pMutPlanes[CathodePlane]
      ->f_pMutStrips[hit_ptr->get()->get_strip()]->getDCMChannel();

    for (int i=0; i<4; i++) ntvar[icount++] = hit_ptr->get()->get_amu(i);

    StripBegin = geometry->f_pMutStations[hit_ptr->get()->get_station()]
      ->f_pMutOctants[hit_ptr->get()->get_octant()]
      ->f_pMutHalfOctants[hit_ptr->get()->get_half_octant()]
      ->f_pMutGaps[hit_ptr->get()->get_gap()]
      ->f_pMutPlanes[CathodePlane]
      ->f_pMutStrips[hit_ptr->get()->get_strip()]->getGlobalPositionBegin();
    StripEnd = geometry->f_pMutStations[hit_ptr->get()->get_station()]
      ->f_pMutOctants[hit_ptr->get()->get_octant()]
      ->f_pMutHalfOctants[hit_ptr->get()->get_half_octant()]
      ->f_pMutGaps[hit_ptr->get()->get_gap()]
      ->f_pMutPlanes[CathodePlane]
      ->f_pMutStrips[hit_ptr->get()->get_strip()]->getGlobalPositionEnd();
    ntvar[icount++] = StripBegin.getX();
    ntvar[icount++] = StripBegin.getY();
    ntvar[icount++] = StripEnd.getX();
    ntvar[icount++] = StripEnd.getY();

    int cluswid = 0;
    int lastgap = 0;
    float pz = 0;
    int nhit = 0;
    float chiq = -10.0;
    float mass_reco = -100.0;
    int sign_reco = -10;
    float vtx_chiq = -10.0;

    // check for any associated cluster
    TMutClusMap::const_key_iterator clus_iter( hit_ptr->get()->get_associated<TMutClus>() );
    while ( TMutClusMap::const_pointer clus_ptr = clus_iter.next() )
      {
	cluswid = clus_ptr->get()->get_associated<TMutHit>().count();

	// check for any associated track
	TMutCoordMap::const_key_iterator coord_iter = clus_ptr->get()->get_associated<TMutCoord>();
	while ( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
	  {
	    TMutTrkMap::const_key_iterator trk_iter = coord_ptr->get()->get_associated<TMutTrk>();
	    while ( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
	      {
		pz = trk_ptr->get()->get_trk_par()->get_pz();
		nhit = get_hit_bitset(trk_ptr).count();
		chiq = trk_ptr->get()->get_w_chi_square()/trk_ptr->get()->get_ndf();
		
		//check for any associated MuID road
		TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
		if(!road_iter.at_end()) lastgap = road_iter->get()->get_depth();
		
		//checkfor any associated dimuon
		TMutVtxMap::const_key_iterator vtx_iter = trk_ptr->get()->get_associated<TMutVtx>();
		while( TMutVtxMap::const_pointer vtx_ptr = vtx_iter.next() )
		  {
		    mass_reco = vtx_ptr->get()->get_mass();
		    sign_reco = vtx_ptr->get()->get_sign();
		    vtx_chiq = vtx_ptr->get()->get_chi_square();
		  } // loop over dimuons
	      } // loop over tracks
	  } // loop over coordinates
      } // loop over clusters

    ntvar[icount++] = cluswid;
    ntvar[icount++] = lastgap;
    ntvar[icount++] = pz;
    ntvar[icount++] = nhit;
    ntvar[icount++] = chiq;
    ntvar[icount++] = mass_reco;
    ntvar[icount++] = sign_reco;
    ntvar[icount++] = vtx_chiq;
    ntvar[icount++] = cent;

    _strip_data->Fill(ntvar);
  }

  EventNum++;
}

//_________________________________________________________________
void MuonAnaTuples::write_stpp_ntuple(PHCompositeNode* top_node)
{
   TMutClusMap*  clus_map = 0;
   try{
     clus_map = TMutNode<TMutClusMap>::find_node(top_node,"TMutClusMap");
   }
   catch(std::exception& e){
     return;
   }

  TMutClusMap::const_iterator clus_iter = clus_map->range();
  while(TMutClusMap::const_pointer clus_ptr = clus_iter.next())
  {

    // Require no bad status bits
    //
    if(clus_ptr->get()->get_peak_bound() ||
       clus_ptr->get()->get_low_charge() ||
       clus_ptr->get()->get_high_charge()) continue;

    // Require two or three wide
    //

    float ntvar[100] = {0};
    ntvar[0] = clus_ptr->get()->get_arm();
    ntvar[1] = clus_ptr->get()->get_station();
    ntvar[2] = clus_ptr->get()->get_octant();
    ntvar[3] = clus_ptr->get()->get_half_octant();
    ntvar[4] = clus_ptr->get()->get_gap();
    ntvar[5] = clus_ptr->get()->get_cathode();

    // Peak, total, offset
    for( size_t index = 0; index < clus_ptr->get()->get_n_centroid(); index ++)
    {
      const TMutClusCentroid *centroid( clus_ptr->get()->get_centroid( index ) );
      if( !centroid ) continue;

      ntvar[6] = centroid->get_q_peak();
      ntvar[7] = centroid->get_q_tot();
      ntvar[8] = centroid->get_w();
    }

    float x[3] = {0};
    float y[3] = {0};

    TMutCoordMap::const_key_iterator coord_iter( clus_ptr->get()->get_associated<TMutCoord>() );
    while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
      {
	TMutTrkMap::key_iterator trk_iter = coord_ptr->get()->get_associated<TMutTrk>();
	while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
	  {
	    // Track parameters at gap 0 of each station
	    for (int sta=0; sta<3; ++sta)
	      {
		const TMutTrkPar* sta_trk_par = trk_ptr->get()->get_trk_par_station(sta);
		if( sta_trk_par )
		  {
		    x[sta] = sta_trk_par->get_x();
		    y[sta] = sta_trk_par->get_y();
		  }
	      }
	  }
      }

    ntvar[9] = x[0];
    ntvar[10] = y[0];
    ntvar[11] = x[1];
    ntvar[12] = y[1];
    ntvar[13] = x[2];
    ntvar[14] = y[2];

    // Data from associated TMutHit
    //
    size_t iwrite=15;
    TMutHitMap::const_key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
    while(TMutHitMap::const_pointer hit_ptr = hit_iter.next()){

      if (iwrite>25) break;

      ntvar[iwrite++] = hit_ptr->get()->get_q();
      ntvar[iwrite++] = hit_ptr->get()->get_adc(0);
      ntvar[iwrite++] = hit_ptr->get()->get_adc(1);
      ntvar[iwrite++] = hit_ptr->get()->get_adc(2);

      // Gain and pedistal from the calibration DB
      const PdbMutCalibStrip *dbstrip = MutCalib()->getPdbMutCalibStrip(hit_ptr->get()->get_arm(),
								  hit_ptr->get()->get_station(),
								  hit_ptr->get()->get_octant(),
								  hit_ptr->get()->get_half_octant(),
								  hit_ptr->get()->get_gap(),
								  hit_ptr->get()->get_cathode(),
								  hit_ptr->get()->get_strip());

      ntvar[iwrite++] = dbstrip->get_pedestal();
      ntvar[iwrite++] = dbstrip->get_gain();
    }

     _stpp->Fill(ntvar);

  }
}

//_________________________________________________________________
void MuonAnaTuples::write_ClustData2_ntuple(PHCompositeNode* top_node)
{

  float nt_vars[100]={0};
  TMutGapCoordMap* gap_coord_map = 0;
  // Loop over gap_coord
  //
  try{
    gap_coord_map = TMutNode<TMutGapCoordMap>::find_node(top_node,"TMutGapCoordMap");
  }
  catch(std::exception& e){
    return;
  }

  TMutGapCoordMap::const_iterator gap_coord_iter = gap_coord_map->range();
  while(TMutGapCoordMap::const_pointer gap_coord_ptr = gap_coord_iter.next()){
    nt_vars[0] = gap_coord_ptr->get()->get_arm();
    nt_vars[1] = gap_coord_ptr->get()->get_station();
    nt_vars[2] = gap_coord_ptr->get()->get_octant();
    nt_vars[3] = gap_coord_ptr->get()->get_half_octant();
    nt_vars[4] = gap_coord_ptr->get()->get_gap();
    nt_vars[5] = (gap_coord_ptr->get()->get_coord()).getX();
    nt_vars[6] = (gap_coord_ptr->get()->get_coord()).getY();
    nt_vars[7] = (gap_coord_ptr->get()->get_coord()).getZ();
    nt_vars[8] = gap_coord_map->get(gap_coord_ptr->get()->get_arm(),
      gap_coord_ptr->get()->get_station(),
      gap_coord_ptr->get()->get_octant(),
      gap_coord_ptr->get()->get_half_octant(),
      gap_coord_ptr->get()->get_gap()).count();
    _ClustData2->Fill(nt_vars);
  }
}

//_________________________________________________________________
void MuonAnaTuples::write_trk_stpp_ntuple(PHCompositeNode* top_node)
{

  // Clear comparison array
  _comp_var.assign(0.0);
  _nvtx_o=0;
  _nvtx_n=0;

  float ntvar[100] = {0};
  float charge;
  float px,py,pz,road_depth,chiq,ndf;
  std::bitset<16> trk_hits;

  // Reconstructed track ntuple which has all the information to the associated clusters
  //
  TMutTrkMap* trk_map = 0;
  try{
    trk_map = TMutNode<TMutTrkMap>::find_node(top_node, "TMutTrkMap");
  }
  catch(std::exception& e){
    return;
  }

  TMutTrkMap::const_iterator trk_iter = trk_map->range();
  //  MUTOO::PRINT(std::cout,"MuonAnaTuples::write_trk_stpp_ntuple");

  while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next())
  {

    charge = trk_ptr->get()->get_charge();
    px     = trk_ptr->get()->get_trk_par()->get_px();
    py     = trk_ptr->get()->get_trk_par()->get_py();
    pz     = trk_ptr->get()->get_trk_par()->get_pz();

    road_depth = 0;
    TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
    if(!road_iter.at_end()) road_depth = road_iter->get()->get_depth();
    chiq = trk_ptr->get()->get_w_chi_square()/trk_ptr->get()->get_ndf();
    ndf  = trk_ptr->get()->get_ndf();
    trk_hits = get_hit_bitset(trk_ptr);
    //cout <<"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"<< endl;
    //cout << "TRK Ntuple : "<< arm <<octant<<":"<<charge<<":"<<chiq<<":"<<ndf<<endl;
    ntvar[10] = charge;
    ntvar[11] = ndf;
    ntvar[12] = chiq;
    ntvar[13] = trk_hits.to_ulong();
    ntvar[14] = trk_hits.count();
    ntvar[15] = road_depth;
    ntvar[16] = sqrt(px*px + py*py);
    ntvar[17] = pz;

    float x[3] = {0};
    float y[3] = {0};
    // Track parameters at gap 0 of each station
    for (int sta=0; sta<3; ++sta)
    {
      const TMutTrkPar* sta_trk_par = trk_ptr->get()->get_trk_par_station(sta);
      if( sta_trk_par )
      {
        x[sta] = sta_trk_par->get_x();
	y[sta] = sta_trk_par->get_y();
      }
    }

    ntvar[18] = x[0];
    ntvar[19] = y[0];
    ntvar[20] = x[1];
    ntvar[21] = y[1];
    ntvar[22] = x[2];
    ntvar[23] = y[2];

    TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
    while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next())
    {

      //cout << "TMutCoordMap :"<< q_peak << endl;
      TMutClusMap::const_key_iterator     clus_iter     = coord_ptr->get()->get_associated<TMutClus>();
      while(TMutClusMap::const_pointer clus_ptr = clus_iter.next())
      {

        //cout << "   TMutClusdMap :"<< clus_ptr->get()->get_arm() << endl;

        // Require no bad status bits
        if(
          clus_ptr->get()->get_peak_bound() ||
          clus_ptr->get()->get_low_charge() ||
          clus_ptr->get()->get_high_charge()) continue;

        // Require two or three wide
        //

        ntvar[0] = clus_ptr->get()->get_arm();
        ntvar[1] = clus_ptr->get()->get_station();
        ntvar[2] = clus_ptr->get()->get_octant();
        ntvar[3] = clus_ptr->get()->get_half_octant();
        ntvar[4] = clus_ptr->get()->get_gap();
        ntvar[5] = clus_ptr->get()->get_cathode();
        ntvar[6] = clus_ptr->get()->get_n_strip();

        // Peak, total, offset
        for( size_t index = 0; index < clus_ptr->get()->get_n_centroid(); index ++)
        {
          const TMutClusCentroid *centroid( clus_ptr->get()->get_centroid( index ) );
          if( !centroid ) continue;
          ntvar[7] = centroid->get_q_peak();
          ntvar[8] = centroid->get_q_tot();
          ntvar[9] = centroid->get_w();
        }

        _trk_stpp->Fill(ntvar);
      } //TMutClusMap
    } //TMutCoordMap
  } //TMutTrkMap

  //  MUTOO::PRINT(std::cout,"**");

}


//_________________________________________________________________
void MuonAnaTuples::write_clus_ntuple(PHCompositeNode* top_node)
{

  static int EventNum = 1;
  MutCalibStrip *CalibPointer = MutCalib();
  TMutClusMap*  clus_map = 0;
  try{
    clus_map = TMutNode<TMutClusMap>::find_node(top_node,"TMutClusMap");
  } catch(std::exception& e){
    return;
  }

  TMutClusMap::const_iterator clus_iter = clus_map->range();

  // Reconstructed cluster ntuple
  while(TMutClusMap::const_pointer clus_ptr = clus_iter.next())
  {

    // Require no bad status bits
    //
    if(clus_ptr->get()->get_peak_bound() ||
      clus_ptr->get()->get_low_charge() ||
      clus_ptr->get()->get_high_charge()) continue;

    // check whether or not there is one track associated to this cluster
    // 1- get associated coordinates
    // 2- check whether one of the associated to the tracks or not.
    bool has_track( false );
    {
      TMutCoordMap::const_key_iterator coord_iter( clus_ptr->get()->get_associated<TMutCoord>() );
      while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
      {
        if( coord_ptr->get()->get_associated<TMutTrk>().count() )
        {
          has_track = true;
          break;
        }
      }
    }

    Int_t arm     = clus_ptr->get()->get_arm();
    Int_t station = clus_ptr->get()->get_station();
    Int_t octant  = clus_ptr->get()->get_octant();
    Int_t half    = clus_ptr->get()->get_half_octant();
    Int_t gap     = clus_ptr->get()->get_gap();
    Int_t   cathode = clus_ptr->get()->get_cathode();
    TMutHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
    Float_t nhits   = hit_iter.count();

    // double check if associated TMutHits are ordered left to right...???? ( )
    // check when confirmed july 3, 2004 j.nagle

    Int_t BadMiddleStrip=0;
    Int_t BadEdgeStrip=0;
    bool  LeftEdgeDetector = false;
    bool  RightEdgeDetector = false;
    Int_t Saturation = 0;
    Int_t AttenStrip = 0;
    Int_t BadCalib = 0;
    Int_t NoCalib = 0;

    Int_t   Strip_number[8] = {0,0,0,0,0,0,0,0};
    Float_t Strip_q[8] = {0,0,0,0,0,0,0,0};
    Float_t Strip_q_err[8] = {0,0,0,0,0,0,0,0};

    Int_t index = 0;
    // Loop over all strips associated with the cluster

    while(TMutHitMap::pointer hit_ptr = hit_iter.next())
    {

      // only store up to 8 values
      if (index == 8) break;

      Strip_number[index] = hit_ptr->get()->get_strip();
      Strip_q[index] = hit_ptr->get()->get_q();
      Strip_q_err[index] = hit_ptr->get()->get_error_q();

      Int_t strip = hit_ptr->get()->get_strip();
      // For each strip check a number of issues - go through list below

      // (1) is there a bad (q_error) middle strip? - if it is one strip wide then both an edge and middle!
      if (hit_iter.count()==1 || !(index==0 || index==static_cast<int>(hit_iter.count())-1))
      {
        if (hit_ptr->get()->get_error_q() > 20.0)
        { BadMiddleStrip++; }
      }

      // (2) is there a bad (q_error) edge strip?
      if (index==0 || index==static_cast<int>(hit_iter.count())-1)
      {
        // double check this logic count-1
        if (hit_ptr->get()->get_error_q() > 20.0) { BadEdgeStrip++; }
      }

      // (3) is the cluster at the edge of the detector (left edge),(right edge)?
      Int_t NumberOfStrips =  TMutGeo::get_n_strip(arm, station, octant, half, gap, cathode) - 1;
      if (index==0)
      { if (strip == 1) LeftEdgeDetector = true; }

      if (index==static_cast<int>(hit_iter.count())-1)
      {
        if (strip == NumberOfStrips) RightEdgeDetector = true;
      }

      // (3 1/2) is there a bad calibration
      // set calib pointer
      const PdbMutCalibStrip *StripCalib = CalibPointer->getPdbMutCalibStrip(arm, station, octant, half, gap, cathode, strip);
      if (StripCalib) {
        if (! (StripCalib->isValid()) ) BadCalib++;
      } else NoCalib++;

      // (4) is there an attenuated strip? - should we add which one? (edge or middle?????)
      MutStrip* strip_ptr;
      strip_ptr = TMutGeo::get_strip_geom(arm, station, octant, half, gap, cathode, strip);
      if (strip_ptr) {
        if (strip_ptr->UseAttenuation()) AttenStrip++;
      }

      // (5) is there a saturated strip?
      Int_t DACSaturation, ADCSaturation;
      bool this_strip_saturation = false;
      if (StripCalib)
      {

        Float_t gain = StripCalib->get_gain();

        DACSaturation = StripCalib->getSaturation();
        ADCSaturation = StripCalib->getAdc(DACSaturation);

        // loop over all ADC samples and see if any are saturated...
        for (UShort_t j=0; j<4; ++j)
        {

          UShort_t adc = hit_ptr->get()->get_adc(j);
          // set saturation bool if adc is out of range or gain is below
          // minimum value.  (gain conditions should be handled by bad
          // channel map)
          //
          /*
          if (adc==MUTOO_FEM::ADC_MIN || gain < _mod_par->get_min_gain()
          || (adc<=ADCSaturation && adc<=MUTOO_FEM::SATURATED_ADC_ERROR)
          || !StripCalib->isValid() ){
          */
          // I believe the mMutCalibratePar.h sets min_gain(0) by default and is never reset
          if (adc==MUTOO_FEM::ADC_MIN || gain < 0 // hard code this value until you get _mod_par working :::  _mod_par->get_min_gain()
            || (adc<=ADCSaturation && adc<=MUTOO_FEM::SATURATED_ADC_ERROR)
            || !StripCalib->isValid() )
          {
            this_strip_saturation = true;
          }

        }
      }
      if (this_strip_saturation) Saturation++;

      index++;  // increment to next strip

    } // end loop over all hits in this cluster

    // loop over all coordinates associated with the cluster
    TMutCoordMap::key_iterator coord_iter = clus_ptr->get()->get_associated<TMutCoord>();
    Float_t num_coord =  coord_iter.count();  // number of coordinates fit for this cluster
    Float_t clus_fit_chisq = clus_ptr->get()->get_chi_square();

    Int_t ncoord  = 0;

    Float_t w_value[4] = {0,0,0,0};     // store up to four w fit values
    Float_t w_value_err[4] = {0,0,0,0}; // store all four even though they all get the same error
    Float_t q_peak[4] = {0,0,0,0};     // store up to four qpeak values
    Float_t q_total[4] = {0,0,0,0};     // store up to four qtotal values

    while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){

      if (ncoord < 4) {
        w_value[ncoord] = coord_ptr->get()->get_w();
        w_value_err[ncoord] = coord_ptr->get()->get_error();
        q_peak[ncoord] = coord_ptr->get()->get_q_peak();
        q_total[ncoord] = coord_ptr->get()->get_q_tot();
      }
      ncoord++;

    } // end loop over coordinates(centroids)

    // now fill the ntuple entries ------------------------------------------------------
    boost::array<float,100> nt_vars;
    nt_vars.assign(0);

    nt_vars[0] = (float) EventNum;
    nt_vars[1] = arm;
    nt_vars[2] = station;
    nt_vars[3] = octant;
    nt_vars[4] = half;

    nt_vars[5] = gap;
    nt_vars[6] = cathode;
    nt_vars[7] = nhits;

    // same as the width of the cluster
    nt_vars[8] = clus_fit_chisq;

    // double check that this is per dof ( )
    nt_vars[9] = num_coord;

    // number of coordinates fit to the cluster
    nt_vars[10] = BadMiddleStrip;
    nt_vars[11] = BadEdgeStrip;
    nt_vars[12] = LeftEdgeDetector;
    nt_vars[13] = RightEdgeDetector;

    nt_vars[14] = Saturation;
    nt_vars[15] = AttenStrip;
    nt_vars[16] = BadCalib;
    nt_vars[17] = NoCalib;

    // store 8 strip values, 8 q values, 8 qerror values
    for (Int_t k=0;k<8;k++) nt_vars[18+k] = Strip_number[k];
    for (Int_t k=0;k<8;k++) nt_vars[26+k] = Strip_q[k];
    for (Int_t k=0;k<8;k++) nt_vars[34+k] = Strip_q_err[k];

    // store 4 w values, 4 w_error values, 4 w_true values
    for (Int_t k=0;k<4;k++) nt_vars[42+k] = w_value[k];
    for (Int_t k=0;k<4;k++) nt_vars[46+k] = w_value_err[k];
    for (Int_t k=0;k<4;k++) nt_vars[50+k] = q_peak[k];
    for (Int_t k=0;k<4;k++) nt_vars[54+k] = q_total[k];

    nt_vars[58] = has_track;

    _clus_reco->Fill(&nt_vars[0]);

  } // end loop over clusters

  EventNum++;

}

//_______________________________________
void MuonAnaTuples::dump_event( PHCompositeNode* top_node )
{

  // store stream in member pointer
  if( !_event_stream_ptr )
  {
    cout << "MuonAnaTuples::dump_event - writting to " << _event_file << endl;
    _event_stream_ptr = new ofstream( _event_file.c_str() );
  }

  VtxOut* vtx = TMutNode<VtxOut>::find_io_node(top_node, "VtxOut" );
  if (!vtx)
    {
      cout << "MuonAnalysis:: VtxOut not in Node Tree" << endl;
      return;
    }

  // store vertex
  double vtx_x( ( _flags & VERTEX ) ? vtx->get_Vertex(vertexname.c_str()).getX():0 );
  double vtx_y( ( _flags & VERTEX ) ? vtx->get_Vertex(vertexname.c_str()).getY():0 );
  double vtx_z( ( _flags & VERTEX ) ? vtx->get_Vertex(vertexname.c_str()).getZ():0 );

  // store centrality
  double centrality( (_flags & CENTRALITY) ? MuonUtil::get_centrality( top_node ):0 );

  // store reaction plane
  double reaction_plane = 0;
  if( _flags & REACTION_PLANE ) try
  {

    ReactionPlaneObject* rp_object = TMutNode<ReactionPlaneObject>::find_io_node( top_node, "ReactionPlaneObject" );
    reaction_plane = rp_object->getRXNrp18();
    if (rp_code != 18)
      {
	ReactionPlaneSngl* rpsngl = rp_object->getReactionPlane( rp_code );
	if (!rpsngl)
	  {
	    cout << "Reaction plane code " << rp_code << " not available" << endl;
	    return;
	  }
	reaction_plane = rpsngl->GetPsi();
      }

  } catch( exception &e ) { cout << e.what() << endl;}

  // dump event information
  static unsigned int event = 1;
  char line[512];
  if( _flags & REACTION_PLANE ) sprintf( line, "%10i %10.5f %10.5f %10.5f %10.5f %10.5f", (int)event, (float)vtx_x, (float)vtx_y, (float)vtx_z, (float)centrality, (float)reaction_plane );
  else sprintf( line, "%10i %10.5f %10.5f %10.5f %10.5f", (int)event, (float)vtx_x, (float)vtx_y, (float)vtx_z, (float)centrality );

  // write line to output stream
  (*_event_stream_ptr) << line << endl;

  // increment event
  event ++;

}
