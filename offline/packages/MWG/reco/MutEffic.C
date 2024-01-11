// $Id: MutEffic.C,v 1.2 2009/07/21 16:13:57 hpereira Exp $
/*!
  \file MutEffic.cxx
  \ingroup supermodules 
  \brief reads nanoDST and derive MuTR plane efficiency based on hit pattern
  \author H. Pereira
  \version $Revision: 1.2 $
  \date $Date: 2009/07/21 16:13:57 $
*/

#include <assert.h>
#include <PHCompositeNode.h>
#include <PHMuoTracksOut.h>
#include <PHTFileServer.h>

#include <Tools.h>

#include "MutEffic.h"
#include "MuonUtil.h"

#include <MutGeom.h>
#include <MutAnodeMap.h>
#include <MutStrip.h>
#include <MutWire.h>
#include <MutOctant.h>
#include <MutGap.h>
#include <TMutDatabaseInit.h>
#include <TMutIndex.h>
#include <RunToTime.hh>

using namespace std;

//___________________________________________________________________
MutEffic::MutEffic( const char* name, const char* file ):
  SubsysReco( name ),
  _filename( file ? file:"mut_effic_ntuple.root" ),
  _tree(0),
  _anode(boost::extents[MUTOO::MAX_STATION][MUTOO::MAX_GAP][6]),
  _cathode(boost::extents[MUTOO::MAX_STATION][MUTOO::MAX_GAP][MUTOO::MAX_CATHODE][6]),
  _geom_init(false),
  _verbosity(None),
  _timer( PHTimeServer::get()->insert_new(name) )
{
  _centrality = 0;
  reset_track_variables(); 
}


//_____________________________________
int MutEffic::Init(PHCompositeNode *top_node)
{

  MUTOO::PRINT( cout, "MutEffic::Init" );
  PHTFileServer::get().open( _filename, "RECREATE" );
  cout << "writing to file \"" << _filename << "\"" << endl;

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  assert( !_tree );
  _tree = new TTree( "mut_effic", "mut_effic" );
  _tree->SetAutoSave( AUTO_SAVE );
  
  _tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _tree->Branch( "centrality", &_centrality, "centrality/I", BUFFER_SIZE );
  _tree->Branch( "x", &_x[0], "x[3]/F", BUFFER_SIZE );
  _tree->Branch( "y", &_y[0], "y[3]/F", BUFFER_SIZE );
  _tree->Branch( "z", &_z[0], "z[3]/F", BUFFER_SIZE );
  _tree->Branch( "px", &_px[0], "px[3]/F", BUFFER_SIZE );
  _tree->Branch( "py", &_py[0], "py[3]/F", BUFFER_SIZE );
  _tree->Branch( "pz", &_pz[0], "pz[3]/F", BUFFER_SIZE );
  _tree->Branch( "p_vtx", &_p_vtx[0], "p_vtx[3]/F", BUFFER_SIZE );
  _tree->Branch( "octant", &_octant, "octant/I", BUFFER_SIZE );
  _tree->Branch( "pattern", &_pattern, "pattern/I", BUFFER_SIZE );
  _tree->Branch( "chisquare", &_chisquare, "chisquare/F", BUFFER_SIZE );
  _tree->Branch( "depth", &_depth, "depth/I", BUFFER_SIZE );
  _tree->Branch( "dg0", &_dg0, "dg0/F", BUFFER_SIZE );
  _tree->Branch( "ddg0", &_ddg0, "ddg0/F", BUFFER_SIZE );
  _tree->Branch( "anode", &_anode[0][0][0], "anode[3][3][6]/D", BUFFER_SIZE );
  _tree->Branch( "cathode", &_cathode[0][0][0][0], "cathode[3][3][2][6]/D", BUFFER_SIZE );
  cout << "_tree booked" << endl;

  MUTOO::PRINT( cout, "**" );
  return 0;
  
}

//___________________
int MutEffic::End(PHCompositeNode* top_node)
{
  _timer.get()->print_stat();
  PHTFileServer::get().write( _filename );
  return 0;
}

//___________________________________________________________________
int MutEffic::process_event( PHCompositeNode* top_node )
{
  
  _timer.get()->restart();
  
  try {
  
    // store centrality
    _centrality = (int) (MuonUtil::get_centrality( top_node ) );
    
    // find PHMuoTracksOut node
    PHMuoTracksOut* muo_tracks = TMutNode<PHMuoTracksOut>::find_io_node( top_node, "PHMuoTracksOO" );
    
    // loop over tracks in PHMuoTracks node
    for( unsigned int i=0; i < muo_tracks->get_npart(); i++ )
    {
     
      // check track
      if( !accept_track( muo_tracks, i ) ) continue;
      fill_tree( muo_tracks, i );
      
    }
  
  }  catch( exception &e ) { cout << e.what() << endl; }
  _timer.get()->stop();
 
  return 0;
 
}

//___________________________________________________________________
void MutEffic::reset_track_variables( void )
{
  // initialize variables (to make valgrind happy)
  _arm = 0;
  _x.assign(0);
  _y.assign(0);
  _p_vtx.assign(0);
  _octant = 0;
  _pattern = 0;
  _chisquare = 0;
  _depth = -1;
  _dg0 = -1;
  _ddg0 = -1;
}

//___________________________________________________________________
bool MutEffic::accept_track( PHMuoTracksOut* muo, int id ) const
{ return true; }

//___________________________________________________________________
void MutEffic::fill_tree( PHMuoTracksOut* muo, int id ) 
{ 
  reset_track_variables();
  assert( _tree );
    
  // x and y position vs station
  for( int station = 0; station < MUTOO::MAX_STATION; station++ )
  {
    _x[station] = muo->get_xpos(station+1,id);
    _y[station] = muo->get_ypos(station+1,id);
    _z[station] = muo->get_zpos(station+1,id);
    _px[station] = muo->get_px(station+1,id);
    _py[station] = muo->get_py(station+1,id);
    _pz[station] = muo->get_pz(station+1,id);
  }
  
  // position at vertex
  _p_vtx[0] = muo->get_px( 0, id );
  _p_vtx[1] = muo->get_py( 0, id );
  _p_vtx[2] = muo->get_pz( 0, id );
  
  // arm
  _arm = (_p_vtx[2] < 0 ) ? MUTOO::South:MUTOO::North;

  // chisquare and pattern
  _chisquare = muo->get_chisquare(id);
  _octant = get_octant( muo, id, 0 );

  if (_verbosity == Alot )
    std::cout << "octant: " << _octant 
	      << " x:" << _x[0] 
	      << " y:" << _y[0] 
	      << std::endl;

  _pattern = muo->get_muTRhits(id);
  
  // select deepest road
  // the deepest road is kept
  int best_road( -1 );
  for( int i_road = 0; i_road<3; i_road++ )
  { if( muo->get_muIDOOhits( i_road, id ) ) best_road = i_road; }
  
  // check if one road was found
  if( best_road > 0 )
  {
    _depth = best_road + 2;
    _dg0 = Tools::DG0( muo, id, best_road );
    _ddg0 = Tools::DDG0( muo, id, best_road );
  }
  
  // Now figure out which anode card and wire this was
  get_anodes();

  // Now figure out which cathode channels we have
  get_cathodes();
  
  // fill tree
  _tree->Fill();

  return; 
}

//_______________________________________
int MutEffic::get_octant( PHMuoTracksOut* muo, int ipart , int station)
{  
  return get_octant( muo->get_xpos( station+1,ipart ),
		     muo->get_ypos( station+1,ipart ) );
}

//_______________________________________
int MutEffic::get_octant( double x, double y )
{  
  
  static vector<double> phi_min;
  static vector<double> phi_max;

  if( phi_min.empty() )
    for( int octant = 0; octant < 8; octant++ )
      {
	if( octant <= 4 )
	  {
	    phi_min.push_back( M_PI/8*(2*octant-1) );
	    phi_max.push_back( M_PI/8*(2*octant+1) );
	  } else {
	    phi_min.push_back( M_PI/8*(2*octant-17) );
	    phi_max.push_back( M_PI/8*(2*octant-15) );
	  }    
      } 
  
  double phi = atan2(y,x);
  
  for( int octant=0; octant < 8; octant++ )
    if( octant != 4 && phi>=phi_min[octant]&&phi<=phi_max[octant] ) 
      return octant;
    else if( octant == 4 && ( phi >= phi_min[octant] || phi <= phi_max[octant] - 2*M_PI ) ) 
      return octant;
  return -1;
  
}   

//_______________________________________
int MutEffic::get_half_octant( PHMuoTracksOut* muo, int ipart, int station )
{  
  return get_half_octant( muo->get_xpos( station,ipart ),
			  muo->get_ypos( station,ipart ) );
}
//_______________________________________
int MutEffic::get_half_octant( double x, double y)
{  

  
  int octant = get_octant(x,y);
  //my angle
  double phi_me = atan2(y,x);

  // a value that makes no sense
  int half_octant = -1; 
  double phi_min;
  double phi_max;

  if( octant <= 4 )
    {
      phi_min=( M_PI/8*(2*octant-1) );
      phi_max=( M_PI/8*(2*octant+1) );
    } else {
      phi_min=( M_PI/8*(2*octant-17) );
      phi_max=( M_PI/8*(2*octant-15) );
    }

  double phi_half=phi_min+M_PI/8;
  if (phi_me < 0 &&
      octant == 4 )
    phi_me+=2*M_PI;

  if ( phi_max > phi_me  && phi_me > phi_half )
    half_octant=1;
  else 
    half_octant=0;

  if (  _verbosity == Alot) 
    std::cout << "MutEffic::get_half_octant: " << half_octant << " " 
	      << x << " "
	      << y << " " 
	      << octant << " "
	      << half_octant << " "
	      << phi_me << " "
	      << phi_min << " " 
	      << phi_max << " " 
	      << phi_half << std::endl;

  return half_octant;
}

//_______________________________________
void MutEffic::init_mut_geom(int runnumber)
{
  // Pick some timestamp to use to load DB
  PHTimeStamp *time_stamp_ptr = RunToTime::instance()->getBeginTime( abs(runnumber) );
  if( time_stamp_ptr ){
    TMutDatabaseInit::set_time_stamp( *time_stamp_ptr );
    delete time_stamp_ptr;
    
  } else {
    // pick some random time during run 7 and print an error
    PHTimeStamp fSearchTime(2007,5,1,0,0,0);
    TMutDatabaseInit::set_time_stamp(fSearchTime);
  }
  
  TMutDatabaseInit::initialize();
  
  _geom_init=true;

}


//_______________________________________
PHPoint MutEffic::project_to_planez( const PHPoint& start, 
				      const PHVector& momentum,
				      const double& z_end)
{
  PHVector end;
  PHVector startv(start);
  double delta_z = fabs ( z_end - start.getZ());
  PHVector momentum_scaled = momentum * ( 1.0 / momentum.getZ() );
  if ( z_end > start.getZ() )
    {
      end = startv + momentum_scaled*delta_z;
    }
  else if ( z_end < start.getZ() )
    {
      end = startv - momentum_scaled*delta_z;
    }

  return PHPoint(end);
}

//_______________________________________
void MutEffic::get_anodes( int runnumber )
{
  //!initialize he geometry.
  if (! _geom_init) init_mut_geom(runnumber);
  
  //we already know what octant we are in.
  //! now loop over the stations and get the x,y position of the track
  for( int station = 0; station < MUTOO::MAX_STATION; station++ )
    {
      if ( _verbosity == Alot) 
	std::cout << "======================================"<< std::endl;
      
      // I should be able to calculate the half octant from position.
      int half = get_half_octant(_x[station],_y[station]);

      //loop over the gaps 
      for( int gap = 0; 
	   gap < ( ( station == MUTOO::Station3 ) ? 
		   MUTOO::MAX_GAP-1 : MUTOO::MAX_GAP ); 
	   gap++ )
	{
	  TMutIndex index( _arm, station, _octant, half, gap );
	  
	  // retrieve/check arm structure
	  MutArm* arm_ptr( (index.arm()==MUTOO::South) ? SouthArm():NorthArm() ); 
	  if( !arm_ptr ) 
	    {
	      std::cout << "MutEff::get_anode - invalid arm pointer.\n";
	      return;
	    }
	  
	  // retrieve/check octant object
	  MutOctant* octant_ptr = arm_ptr->f_pMutStations[index.station()]->f_pMutOctants[index.octant()];
	  
	  if( !octant_ptr ) 
	    {
	      std::cout << "MutEff::get_anode - invalid octant pointer.\n";
	      return;
	    } 
	  
	  // retrieve/check gap object  
	  MutGap* gap_ptr = octant_ptr->f_pMutHalfOctants[index.half()]->f_pMutGaps[index.gap()];
	  
	  if( !gap_ptr ) 
	    {
	      std::cout << "MutEff::get_anode - invalid gap pointer.\n";
	      return;
	    }
// This code can be used to figure out if we had a hit in a given
// cathode or not....
// 	  for (int cathode = 0 ; cathode < MUTOO::NumberOfCathodePlanes ; cathode++ )
// 	    {
// 	      //Figure out which bit should be set
// 	      int index = cathode + MUTOO::NumberOfCathodePlanes * 
// 		( gap + MUTOO::NumberOfGaps * ( station ) );
// 	      //Shift up to that bit a one
// 	      index = ( 1 << index);
// 	      std::cout << "s:" << station << " " 
// 			<< "g:" << gap << " " 
// 			<< "c:" << cathode << " " 
// 			<< index << " " 
// 			<< (index & _pattern ) << " " 
// 			<< std::endl;
// 	      if ( (index & _pattern ) == index )
// 		std::cout << "we had a hit in this cathode plane" << std::endl;
// 	    }

	  // retrieve number of wires for this 
	  MutPlane *plane_ptr = gap_ptr->f_pMutPlanes[ MUTGEOM::Wire ];
	  
	  if( !plane_ptr )
	    { 
	      cout << "MutEff::get_anode - invalid plane" << endl;
	      return;
	    }

	  //here is where we call this function to get the new point
	  //by projection at the actual anode plane
	  //	  PHPoint point_plane = plane_ptr->getGlobalPosition();

	  //Seems that the z position of the plane is not neccesarily
	  //the same as the wire is this a bug? --Alex

	  //To test this i moved to the actual wire loop to do the
	  //projection...

// 	  PHPoint  point_station(_x[station], _y[station],_z[station]);
// 	  PHVector mom_station(_px[station],_py[station],_pz[station]);
// 	  PHPoint  point_proj = project_to_planez( point_station, 
// 						   mom_station, 
// 						   point_plane.getZ());
// 	  if ( _verbosity == Alot )
// 	    std::cout << "start:" << point_station << " " 
// 		      << " momentum: " << mom_station << " " 
// 		      <<  " end: " << point_proj << std::endl;
	  
	  // set the distance to some huge un-physical number so that the
	  // first check will pass
	  double dist = 99999.0;
	  
	  //Now we find the closest wire in this gap by calculating
	  //the distance
	  for(unsigned int iwire=0; 
	      iwire<plane_ptr->f_pMutWires.size(); 
	      iwire++ )
	    {
	      MutWire* wire = plane_ptr->f_pMutWires[iwire];

	      for(unsigned int icard=0; 
		  icard<MutAnodeMap::get_num_cards(index.arm(), index.station()); 
		  icard++ )
		{
		  
		  
		  if(static_cast<int>(iwire) < MutAnodeMap::get_start_wire( index.arm(), index.station(), index.octant(), icard) ||
		     static_cast<int>(iwire) > MutAnodeMap::get_end_wire( index.arm(), index.station(), index.octant(), icard) )
		    continue;

		  //The start and end points of the anode wire
		  PHPoint point_begin( wire->getGlobalPositionBegin() );
		  PHPoint point_end( wire->getGlobalPositionEnd() );

		  // project to the z position of the anode wires
		  PHPoint  point_station( _x[station],_y[station],_z[station]);

		  PHVector mom_station( _px[station],_py[station],_pz[station]);

		  PHPoint  point_proj = project_to_planez( point_station, 
							   mom_station, 
							   point_begin.getZ());

		  //now calculate the distance away and pick
		  //the closest anode?
		  PHLine anode_line(point_begin, point_end);
		  
		  double dist_tmp =  PHGeometry::distanceLinePoint(anode_line,point_proj);

		  if ( dist_tmp < dist )
		    {
		      dist=dist_tmp;
		      if ( _verbosity == Alot )
			std::cout << "distance[" << icard 
				  << "][" << iwire << "]: " << dist  
				  << " anode: " 
				  << wire->getGlobalPositionBegin() 
				  << " "  
				  << wire->getGlobalPositionEnd()
				  << " hit-proj: " << point_proj << std::endl;
		      
		      _anode[station][gap][0] = icard;
		      _anode[station][gap][1] = iwire;
		      _anode[station][gap][2]= dist;
		      _anode[station][gap][3]= point_proj.getX();
		      _anode[station][gap][4]= point_proj.getY();
		      _anode[station][gap][5]= point_proj.getZ();
		      
		    } //test for distance		      
		} //card loop
	    } //wire loop
	} //gap loop
    } //station loop

  return;
}

//_______________________________________
void MutEffic::get_cathodes( int runnumber )
{
  //!initialize he geometry.
  if (! _geom_init) init_mut_geom(runnumber);
  
  //we already know what octant we are in.
  //! now loop over the stations and get the x,y position of the track
  for( int station = 0; station < MUTOO::MAX_STATION; station++ )
    {
      if ( _verbosity == Alot) 
	std::cout << "======================================"<< std::endl;
      
      // I should be able to calculate the half octant from position.
      int half = get_half_octant(_x[station],_y[station]);

      //loop over the gaps 
      for( int gap = 0; 
	   gap < ( ( station == MUTOO::Station3 ) ? 
		   MUTOO::MAX_GAP-1 : MUTOO::MAX_GAP ); 
	   gap++ )
	{
	  TMutIndex index( _arm, station, _octant, half, gap );

	  // retrieve/check arm structure
	  MutArm* arm_ptr( (index.arm()==MUTOO::South) ? SouthArm():NorthArm() ); 
	  if( !arm_ptr ) 
	    {
	      std::cout << "MutEff::get_cathodes - invalid arm pointer.\n";
	      return;
	    }
	  
	  // retrieve/check octant object
	  MutOctant* octant_ptr = arm_ptr->f_pMutStations[index.station()]->f_pMutOctants[index.octant()];
	  
	  if( !octant_ptr ) 
	    {
	      std::cout << "MutEff::get_cathodes - invalid octant pointer.\n";
	      return;
	    } 
	  
	  // retrieve/check gap object  
	  MutGap* gap_ptr = octant_ptr->f_pMutHalfOctants[index.half()]->f_pMutGaps[index.gap()];
	  
	  if( !gap_ptr ) 
	    {
	      std::cout << "MutEff::get_cathodes - invalid gap pointer.\n";
	      return;
	    }

	  
	  MUTGEOM::PlaneNumber planes[2]= { MUTGEOM::Cathode1,
					    MUTGEOM::Cathode2 };
	  for ( int icath=0; icath < 2 ; icath++ )
	    {
	      // retrieve number of strip for the first cathode
	      MutPlane *plane_ptr = gap_ptr->f_pMutPlanes[ planes[icath] ];
	      
	      if( !plane_ptr )
		{ 
		  cout << "MutEff::get_cathodes - invalid plane" << endl;
		  return;
		}
	      //set the distance to something insanely large
	      double dist = 99999.0;
	      
	      //Now we find the closest strip
	      for(unsigned int istrip=0; 
		  istrip < plane_ptr->f_pMutStrips.size(); 
		  istrip++ )
		{
		  MutStrip * strip = plane_ptr->f_pMutStrips[istrip];
		  PHPoint point_begin( strip->getGlobalPositionBegin() );
		  PHPoint point_end( strip->getGlobalPositionEnd() );
		  // project to the z position of the anode wires
		  PHPoint  point_station( _x[station],_y[station],_z[station]);
		  
		  PHVector mom_station( _px[station],_py[station],_pz[station]);
		  
		  PHPoint  point_proj = project_to_planez( point_station, 
							   mom_station, 
							   point_begin.getZ());

		  PHLine cathode_line(point_begin, point_end);
		  
		  double dist_tmp =  PHGeometry::distanceLinePoint(cathode_line,point_proj);
		  if ( _verbosity == Alot )
		    {
		      if ( _octant == 4 &&
			   half == 1 )
			std::cout << cathode_line << " "
				  << point_station << " " 
				  <<  mom_station << " " 
				  << point_proj << " " 
			      << std::endl;
		    }
		  if ( dist_tmp < dist )
		    {
		      dist=dist_tmp;
		      _cathode[station][gap][icath][0] = strip->getPacket_ID();
		      _cathode[station][gap][icath][1] = istrip;
		      _cathode[station][gap][icath][2]= dist;
		      _cathode[station][gap][icath][3]= point_proj.getX();
		      _cathode[station][gap][icath][4]= point_proj.getY();
		      _cathode[station][gap][icath][5]= point_proj.getZ();
		    }
		  //end distance test
		}
	      //end strip loop
	    }
	  //end cathode loop
	}
      //end gap loop
    }
  //end station loop
}
