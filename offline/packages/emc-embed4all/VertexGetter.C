#include "VertexGetter.h"

#include "Fun4AllServer.h"
#include "VtxOut.h"
#include "PHGlobal.h"
#include "getClass.h"

#include <cassert>
#include <cmath>
#include <cstdlib>
#include <iostream>

//_____________________________________________________________________________
float
VertexGetter::getVertex(const std::string& topnodename)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* topnode = se->topNode(topnodename.c_str());
  assert(topnode!=0);

  VtxOut* vtxout = findNode::getClass<VtxOut>(topnode,"VtxOut");

  if (!vtxout)
    {
      // tries PHGlobal then
      PHGlobal* phglobal = findNode::getClass<PHGlobal>(topnode,"PHGlobal");
      if (!phglobal)
	{
	  std::cerr << PHWHERE << topnodename << " do not contain "
		    << "VtxOut nor PHGlobal. That's a fatal error. Bye."
		    << std::endl;
	  exit(1);
	}
      if ( phglobal->isValid() )
	{
	  return phglobal->getBbcZVertex();
	}
      else
	{
	  return sqrt(-1.0);
	}
    }
  else
    {
      return vtxout->get_ZVertex();
    }
}
