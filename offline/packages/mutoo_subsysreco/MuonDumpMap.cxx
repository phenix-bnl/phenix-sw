// $Id: MuonDumpMap.cxx,v 1.10 2011/07/14 22:27:09 pinkenbu Exp $

/*!
	\file		MuonDumpMap.cxx 
	\ingroup supermodules
	\brief	 utility class to dump mutoo/muioo/rpcoo maps at each event
	\author	Hugo Pereira
	\version $Revision: 1.10 $
	\date		$Date: 2011/07/14 22:27:09 $
*/

#include "MuonDumpMap.h"
#include <PHTimer.h>
#include <PHMapBase.h>
#include <TMutNode.h>
#include <iostream>

using namespace std;

//__________________________________________________________________________
MuonDumpMap::MuonDumpMap( const char* name ) : 
	SubsysReco( name ),
	_timer( PHTimeServer::get()->insert_new( name ) )
{ }

//__________________________________________________________________________
int MuonDumpMap::InitRun(PHCompositeNode *top_node)
{
	MUTOO::PRINT( cout, "MuonDumpMap::InitRun" );
	dump_registered_maps();
	return 0;
}

//__________________________________________________________________________
int MuonDumpMap::process_event(PHCompositeNode *top_node)
{
	
	_timer.get()->restart();	
	MUTOO::PRINT( cout, string("MuonDumpMap::process_event (") + ThisName + ")" );
	
	PHNodeIterator node_iter(top_node);
	string node_name( "" );
	PHCompositeNode *node = top_node;
	for( MapInfoSet::const_iterator iter = _maps.begin(); iter != _maps.end(); iter++ )
	{
		try {
			
			// if node_name has changed, reload
			if( node_name != iter->_node_name )
			{
				
				node_name = iter->_node_name;
				if( !node_name.empty() ) 
				{
					node = static_cast<PHCompositeNode*>(node_iter.findFirst("PHCompositeNode", node_name.c_str() ));
					if( !node ) throw runtime_error( DESCRIPTION( string( "cannot access node ") + node_name ) );						
				}
				
				// loop over maps registered for this node
				MUTOO::PRINT( cout, string("MuonDumpMap::process_event - Node ") + (node_name.empty() ? "TOP":node_name ) );

			}
			
			PHMapBase* map_base( TMutNode<PHMapBase>::find_node( node, iter->_map_name ) );
			if( iter->_mode & SIZE ) cout << "map_name: " << map_base->get_name() << " size: " << map_base->count() << endl;
			if( iter->_mode & FULL && map_base->count() ) map_base->print();
			cout << endl;
		} catch( exception& e ) { cout << e.what() << endl; }
	}
	
	_timer.get()->stop();
	MUTOO::PRINT( cout, "**" );
	return 0;

}

//__________________________________________________________________________
void MuonDumpMap::add_map( const string& map_name, const string& node_name, const unsigned int& mode )
{ _maps.insert( MapInfo( node_name, map_name, mode ) ); }

//__________________________________________________________________________
void MuonDumpMap::dump_registered_maps( void ) const
{
	
	MUTOO::PRINT( cout, "MuonDumpMap::dump_registered_maps" );
	for( MapInfoSet::const_iterator iter = _maps.begin(); iter != _maps.end(); iter++ )
	cout << *iter << endl;
	MUTOO::PRINT( cout, "**" );
	
}

//__________________________________________________________________________
int MuonDumpMap::End(PHCompositeNode* top_node) 
{
// 	_timer.get()->print_stat();
	return 0;
}
