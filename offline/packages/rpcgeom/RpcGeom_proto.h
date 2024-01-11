
#ifndef _RpcGeom_proto_h_
#define _RpcGeom_proto_h_

#ifndef __CINT__
#include <boost/array.hpp>
#endif

#include "RPCPROTOGEOM.h"
#include "RpcGeom.h"
#include "RpcArm_proto.h"

class PHCompositeNode;
class RpcArm_proto;

//! RPC geometry manager singleton. 
/*! 
  RPC geometry manager singleton.	
  Initialize and provide access to RPC strip geometry. 
*/
class RpcGeom_proto
{
	
 public:
  
  //!@name accessors
  //@{
	  
  //! retrieve arm from index
  static RpcArm_proto* get_arm( unsigned int arm );
 
  //! retrieve pointer to north arm
  static RpcArm_proto* north_arm( void );
	
  //! retrieve pointer to south arm
  static RpcArm_proto* south_arm( void );
  
  //! print
  static void print( std::ostream& out = std::cout )
    {
      RPCPROTOGEOM::PRINT( out, std::string("RpcGeom_proto::print") );
      south_arm()->print(out);
      north_arm()->print(out);
      RPCPROTOGEOM::PRINT( out, "**" );
    }
	
  //!@}
	
	
  //!@name IOs
  //@{
	
  //! stationType1 array node name
  static std::string node_name( void )
    { return "RpcStation"; }
	
  //! arm 'local' initialization
  static void create_arms( void )
    { create_arms_v1(); }
	
  //! initialization from PHNode
  static void read_arms( PHCompositeNode* node );
	
  //! write arms to node
  static void write_arms( PHCompositeNode* node );
	
  //!@}
			
  private:

  //! initialization from local geometry
  static void create_arms_v1( void );
  
  //! RPC north arm static pointers
  static RpcArm_proto* _north_arm;
	
  //! RPC south arm static pointers
  static RpcArm_proto* _south_arm;		
  
};

#endif
