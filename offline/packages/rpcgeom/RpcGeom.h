
#ifndef _RpcGeom_h_
#define _RpcGeom_h_

#ifndef __CINT__
#include <boost/array.hpp>
#endif

#include "RPCGEOM.h"
#include "RpcArm.h"

class PHCompositeNode;
class RpcArm;

//! RPC geometry manager singleton. 
/*! 
  RPC geometry manager singleton.	
  Initialize and provide access to RPC strip geometry. 
*/
class RpcGeom
{
	
 public:
					
  //!@name accessors
  //@{
					
  //! retrieve arm from index
  static RpcArm* get_arm( unsigned int arm );
		
  //! retrieve pointer to north arm
  static RpcArm* north_arm( void );
	
  //! retrieve pointer to south arm
  static RpcArm* south_arm( void );
	
  //! print
  static void print( std::ostream& out = std::cout )
    {
      RPCGEOM::PRINT( out, std::string("RPCGeom::print") );
      south_arm()->print(out);
      north_arm()->print(out);
      RPCGEOM::PRINT( out, "**" );
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
  static RpcArm* _north_arm;
	
  //! RPC south arm static pointers
  static RpcArm* _south_arm;		

};

#endif
