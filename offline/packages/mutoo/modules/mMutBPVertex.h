#ifndef __MMUTBPVERTEX_HH__
#define __MMUTBPVERTEX_HH__

#include<PHTimeServer.h>
#include<TMutTrkMap.h>
#include<TMutVtxMap.h>

class mMutBPVertexPar;
class PHCompositeNode;

/*! \ingroup modules */
/*!

INSERT MODULE DESCRIPTION HERE 

<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutBPVertexPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>

INSERT ADDITIONAL IOC SPECIFICATIONS HERE

</table>
*/

class mMutBPVertex
{
 public: 

  mMutBPVertex(); 
  virtual ~mMutBPVertex(){}
  virtual PHBoolean event(PHCompositeNode*);

 private:  

  // private methods
  //
  void set_interface_ptrs(PHCompositeNode* top_node);
  void vtx_loop();

  // Interface pointers
  //
  const mMutBPVertexPar* _mod_par;           // parameter table

  //  INSERT ADDITIONAL IOC POINTERS HERE

  TMutVtxMap* _vtx_map;

  // Timer
  //
  PHTimeServer::timer _timer;

};

#endif /* __MMUTBPVERTEX_HH__ */

