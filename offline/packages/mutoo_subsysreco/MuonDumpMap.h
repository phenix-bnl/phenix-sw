// $Id: MuonDumpMap.h,v 1.6 2008/10/21 11:19:51 hpereira Exp $
#ifndef MuonDumpMap_h
#define MuonDumpMap_h

/*!
	\file		MuonDumpMap.h	
	\ingroup supermodules
	\brief	 utility class to dump mutoo/muioo/rpcoo maps at each event
	\author	Hugo Pereira
	\version $Revision: 1.6 $
	\date		$Date: 2008/10/21 11:19:51 $
*/

#include <SubsysReco.h>
#include <string>
#include <set>
#include <map>
#include <iostream>

// Forward declerations
class PHCompositeNode;

#ifndef __CINT__
#include <PHTimeServer.h>
#endif

/*!
	\class	 MuonDumpMap
	\ingroup supermodules
	\brief	 utility class to dump mutoo/muioo/rpcoo maps at each event
*/
class MuonDumpMap: public SubsysReco
{
	public:

	//! constructor
	MuonDumpMap( const char* name = "MUONDUMPMAP" );
	
	//! destructor
	~MuonDumpMap()
	{};
	
	//! full initialization
	int InitRun(PHCompositeNode *topNode);
	
	//! event method
	int process_event(PHCompositeNode *topNode);

	//! end method
	int End(PHCompositeNode *topNode);
				 
	//! printout mode
	enum Mode
	{
		SIZE = 1<<0,
		FULL = 1<<1
	};
	
	//! register a map to be dumped
	void add_map( const std::string& map_name, const std::string& node_name = "", const unsigned int& mode = MuonDumpMap::FULL );
	
	//! dump registered maps names
	void dump_registered_maps( void ) const;
	
	protected:

	//! map info class
	class MapInfo
	{
		
		public:
				
		//! constructor
		MapInfo( const std::string& node_name = "", const std::string& map_name = "", const unsigned int& mode = MuonDumpMap::FULL ):
				_node_name( node_name ),
				_map_name( map_name ),
				_mode( mode )
		{}
		
		//! equal to operator
		bool operator == (const MapInfo& info ) const
		{
			return _map_name == info._map_name && _node_name == info._node_name; 
		}
		
		//! less than operator
		bool operator < (const MapInfo& info ) const
		{
			return 
				( _node_name < info._node_name ) ||
				( _node_name == info._node_name && _map_name < info._map_name );
		}
		
		//! streamer
    friend std::ostream &operator << (std::ostream &out,const MapInfo& map_info )
		{
			out 
				<< "node: " << map_info._node_name 
				<< " map: " << map_info._map_name 
				<< " mode: " << map_info._mode;
			return out;
		} 
		
		//! node name
		std::string _node_name;
		
		//! map name
		std::string _map_name;
		
		//! printout mode
		unsigned int _mode;
		
	};
											
	#ifndef __CINT__
  
	//! module timer
	PHTimeServer::timer _timer;
	
	//! MapInfo set
	typedef std::set< MapInfo > MapInfoSet;
	
	//! MapInfo set
	MapInfoSet _maps;
	
	#endif
	
};

#endif
