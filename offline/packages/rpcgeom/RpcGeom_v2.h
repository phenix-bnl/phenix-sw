
#ifndef _RpcGeom_v2_h_
#define _RpcGeom_v2_h_

#ifndef __CINT__
#include <boost/array.hpp>
#endif

#include "RPCFINALGEOM.h"
#include "RpcGeom.h"
#include "RpcArm_v2.h"

class PHCompositeNode;
class RpcArm_v2;

//! RPC geometry manager singleton. 
/*! 
  RPC geometry manager singleton.	
  Initialize and provide access to RPC strip geometry. 
*/
class RpcGeom_v2
{
	
 public:
  
  //!@name accessors
  //@{
	  
  //! retrieve arm from index
  static RpcArm_v2* get_arm( unsigned int arm );
 
  //! retrieve pointer to north arm
  static RpcArm_v2* north_arm( void );
	
  //! retrieve pointer to south arm
  static RpcArm_v2* south_arm( void );
  
  //! print
  static void print( std::ostream& out = std::cout )
    {
      RPCFINALGEOM::PRINT( out, std::string("RpcGeom_v2::print") );
      south_arm()->print(out);
      north_arm()->print(out);
      RPCFINALGEOM::PRINT( out, "**" );
    }
	
  //!@}
	
	
  //!@name IOs
  //@{
	
  //! stationType1 array node name
  static std::string node_name( void )
    { return "RpcStation"; }
	
  //! arm 'local' initialization
  static void create_arms( void )
    { create_arms_v2(); }
	
  //! initialization from PHNode
  static void read_arms( PHCompositeNode* node );
	
  //! write arms to node
  static void write_arms( PHCompositeNode* node );
	
  //!@}
			
  private:

  //! initialization from local geometry
  static void create_arms_v2( void );
  
  //! RPC north arm static pointers
  static RpcArm_v2* _north_arm;
	
  //! RPC south arm static pointers
  static RpcArm_v2* _south_arm;		
  
};

#endif
