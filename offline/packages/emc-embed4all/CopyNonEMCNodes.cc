/*
 * see header for description
 *
 */


#include <iostream>
#include <set>
#include <stack>
#include <string>
#include <iostream>
#include <iomanip>

#include <PHObject.h>
#include <PHIODataNode.h>
#include <PHNodeOperation.h>
#include <PHNodeIterator.h>
#include <Fun4AllReturnCodes.h>

#include <CopyNonEMCNodes.h>



using namespace std;



ClassImp(CopyNonEMCNodes);



#define XCOUT cout << "CopyNonEMCNodes: "
#define XCERR cerr << "CopyNonEMCNodes: "
#define XNAMETYPE(x) (x)->getName() << "(" << (x)->getType() << ")"
#define XNAMETYPEOBJ(x) (x)->getName() << "(" << (x)->getType() << ", objtype=" << (x)->getObjectType() << ")"

namespace {

  class copy_nonemc_nodes: public PHNodeOperation {
  public:
    copy_nonemc_nodes(PHCompositeNode * from, PHCompositeNode * to, std::set<std::string> * banlist){
      topfrom = from;
      topto = to;
      failed = false;
      this->banlist = banlist;
    }

    void perform(PHNode * from){
      // is this the TOP node
      if(from == topfrom) return;

      // is this an emc node?
      if( from->getName().find("emc") || from->getName().find("Emc") || from->getName().find("EMC") ) return;

      // is this node banned?
      if( banlist->find(from->getName().getString()) != banlist->end() ) return;
      
      // build up path from this node to top node
      stack<PHNode *> path;
      for(PHNode * tmp = from->getParent(); tmp != topfrom; tmp = tmp->getParent() ) path.push(tmp);



      // get the top-most composite node
      PHCompositeNode * to = topto;
      while( !path.empty() ){
	PHNode * node = PHNodeIterator(to).findFirst( path.top()->getName() );
	if(node == NULL){
	  XCERR << "error finding " << path.top()->getName() << " in " << to->getName() << endl;
	  failed |= true; return;
	}
	to = dynamic_cast<PHCompositeNode *>( node );
	if( to == NULL ){
	  XCERR << "error converting " << XNAMETYPE(node) << " into PHCompositeNode" << endl; 
	  failed |= true; return;
	}
	path.pop();
      }


      
      // find final node
      PHNode * tonode = PHNodeIterator( to ).findFirst( from->getName() );
      
      
      // directory, create if missing
      if( from->getType() == "PHCompositeNode" ){
	if( tonode == NULL ){
	  tonode = new PHCompositeNode( from->getName() );
	  to->addNode( tonode );
	}
      }

      // test if original node is a PHObject. if it is, than we are fine.
      else if( from->getObjectType() != "PHObject" ){
	XCOUT << from->getParent()->getName() << "/" << from->getName() 
	      << " is not PHObject (but " << from->getObjectType() << ") "
	      << "banning node." << endl;
	banlist->insert( from->getName().getString() );
	failed |= true; return;
      }


      else if( from->getType() == "PHIODataNode" ){
	PHIODataNode<PHObject> * fromnode = static_cast< PHIODataNode<PHObject> * >( from );
	if( fromnode == NULL ){
	  XCERR << "error converting " << XNAMETYPE(from) << " into PHIODataNode<PHObject>" << endl; 
	  failed |= true; return;
	}	  

	PHObject * object = fromnode->getData()->clone();
	if(object == NULL){
	  XCERR << "PHObject::clone() returned NULL.. banning " << from->getName() << endl;
	  banlist->insert( from->getName().getString() );
	  return;
	}

	if(tonode == NULL){
	  tonode = new PHIODataNode<PHObject>(NULL, from->getName(), from->getObjectType());
	  to->addNode( tonode );
	}
	
	PHIODataNode<PHObject> * ionode = dynamic_cast< PHIODataNode<PHObject> * >( tonode );
	if( ionode == NULL ){
	  XCERR << "error converting " << XNAMETYPE(tonode) << " into PHIODataNode<PHObject>" << endl; 
	  failed |= true; return;
	}	  
	if( ionode->getData() != NULL ) delete ionode->getData();
	ionode->setData( object );
      }


      else {
	XCOUT << "not handling " << XNAMETYPEOBJ(from) << endl;
	banlist->insert( from->getName().getString() );
      }
    }
	
    
    
    PHCompositeNode * topfrom;
    PHCompositeNode * topto;
    bool failed;
    std::set<std::string> * banlist;
  };


}






CopyNonEMCNodes::CopyNonEMCNodes(PHCompositeNode * from, const char * name): SubsysReco(name){ 
  this->from = from; 
};





int CopyNonEMCNodes::InitRun(PHCompositeNode * root){

  copy_nonemc_nodes op1(from, root, &banlist);
  PHNodeIterator( from ).for_each( op1 );

  return EVENT_OK;
}





int CopyNonEMCNodes::process_event(PHCompositeNode * root){

  copy_nonemc_nodes op1(from, root, &banlist);
  PHNodeIterator( from ).for_each( op1 );

  return EVENT_OK;
}



