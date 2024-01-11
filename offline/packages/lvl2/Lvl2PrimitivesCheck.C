#include <Lvl2PrimitivesCheck.h>
#include <Event.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>
#include <L2DecisionHelper.h> // offline/packages/lvl2
#include <TrigRunLvl1.h>
#include <TrigLvl1.h>
#include <TrigRunLvl2.h>
#include <recoConsts.h>
#include <Lvl2OutArrayv1.h>

#include <getClass.h>
#include <iomanip>
#include <cstdlib>

using namespace std;

typedef PHIODataNode<Lvl2OutArray> Lvl2OutArrayNode_t;
static Lvl2OutArray *lvl2outarray = 0;

Lvl2PrimitivesCheck::Lvl2PrimitivesCheck(const char *name): SubsysReco(name)
{
  nevt = 0;

  // Do we want to read primitives from ATP or from offline?
  if(!strcmp(name,"ATPLVL2"))
    {
      strcpy(lvl2outarraynodename,"Lvl2OutArray");
    }
  else
    {
      strcpy(lvl2outarraynodename,"Lvl2OutArrayCal");
    }
  
}

int Lvl2PrimitivesCheck::Init(PHCompositeNode *topNode)
{
  return 0;
}
  
void Lvl2PrimitivesCheck::identify(ostream& out) const
{
  cout << "LVL2PRIMITIVESCHECK" << endl;
  return;
}


int Lvl2PrimitivesCheck::process_event(PHCompositeNode *topNode)
{
  // verbosity is defined in SubsysReco

  /////////////////////////////////////////////////////////////////
  // Increment and set the event number counter
  ////////////////////////////////////////////////////////////////

  nevt++;
  if(nevt%1 == 0 && verbosity>0)
    {
      cout << "Lvl2PrimitivesCheck: Nevts = " << nevt << endl;
    } 
 
  /////////////////////////////////////////////////////////////////
  // Check to see if this is a data event
  /////////////////////////////////////////////////////////////////

  PHNodeIterator iter(topNode);
  
  Event *evt = findNode::getClass<Event>(topNode,"PRDF");
  
      if (!evt)
	{
	  cout << PHWHERE << "NULL Event Pointer" << endl;
	  return -1;
	}  

  // If this is not a data event, skip it
  if( evt->getEvtType() != 1 )
    {
      cout << "Lvl2PrimitivesCheck: Not a data event, skip it - event type = " 
	   << evt->getEvtType() << endl;
      return 0;
    }
  
  /////////////////////////////////////////////////////////////
  // Find the Lvl2OutArray object
  ////////////////////////////////////////////////////////////

  PHCompositeNode *dstNode = NULL;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if(!dstNode)
    {
      cout << "Lvl2PrimitivesCheck::process_event: No DST node, do nothing and return!"
	   << endl;
      return 1;
    }
 
  //finding the Lvl2OutArray on the dst node
  
  PHTypedNodeIterator<Lvl2OutArray> l2iter(dstNode);
  Lvl2OutArrayNode_t *lvl2node = l2iter.find(lvl2outarraynodename);
  
  if (!lvl2node)
    {
      cout << "Lvl2PrimitivesCheck: no " << lvl2outarraynodename << " Node" << endl;
      return -1;
    }
  
  lvl2outarray = lvl2node->getData();
  
  if ( !lvl2outarray )
    {
      cout << "Lvl2PrimitivesCheck: no Lvl2OutArray object in " << lvl2outarraynodename << endl;
      return -1;
    }

  ///////////////////////////////////////////////////////////////  
  // Now loop over all of the primitives
  ///////////////////////////////////////////////////////////////

  int Nprim=50;

  cout << "Event: " << nevt << " examine primitives from " << lvl2outarraynodename << endl; 
  for (int iprim=0;iprim<Nprim;iprim++)
    {
      if(lvl2outarray->getdatalength(iprim) >= 0)
	{

	  cout << "    Read iprim " << iprim << " name " << lvl2outarray->getname(iprim) << " data length " << lvl2outarray->getdatalength(iprim) << endl;

	  if(verbosity>0)
	    {
	      PHDWORD* outword_ptr = lvl2outarray->getdata(iprim);
	      for(int il=0;il<lvl2outarray->getdatalength(iprim);il++)
		{
		  cout << "     word " << il << " value " << *outword_ptr << endl;
		  outword_ptr++;
		  
		}
	    }
	}
    }
  
  if(verbosity>0)
    cout << "Leaving Lvl2PrimitivesCheck:: process_event" << endl;

  return 0;
}

int Lvl2PrimitivesCheck::BeginRun(const int runno) 
{
  return 0;
}

int Lvl2PrimitivesCheck::EndRun(const int runno) 
{
  cout << endl << "Lvl2PrimitivesCheck::EndRun: Total events = " << nevt
       << endl << endl;


  return 0;
}
