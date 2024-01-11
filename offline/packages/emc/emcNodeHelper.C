#include "emcNodeHelper.h"

#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include <cassert>
#include <string>
#include <vector>

//_____________________________________________________________________________
void splitPath(const char* path,
	       std::vector<std::string>& paths)
{
  // Given a path e.g. /A/B/C, will return 
  // a vector of string with A,B and C.

  paths.clear();

  std::string str = path;

  if ( str.empty() )
    {
      return;
    }

  std::vector<size_t> slashes_pos;

  if ( str[0] != '/' ) 
    { 
      str.insert(str.begin(),'/');
    }

  if ( str[str.size()-1] != '/' ) 
    {
      str.push_back('/');
    }

  for (size_t i = 0 ; i < str.size() ; i++) 
    {
      if ( str[i] == '/' ) 
	{ 
	  slashes_pos.push_back(i);
	}
    }
  
  if ( slashes_pos.size() > 0 ) 
    {
      for (size_t i = 0 ; i < slashes_pos.size()-1 ; i++) 
	{
	  paths.push_back(str.substr(slashes_pos[i]+1,
				     slashes_pos[i+1]-slashes_pos[i]-1));
	}
    }  
}

//_____________________________________________________________________________
emcNodeHelper::emcNodeHelper()
{
}

//_____________________________________________________________________________
PHCompositeNode*
emcNodeHelper::findCompositeNode(PHCompositeNode* topNode, 
				 const char* path)
{
  if ( !path || !topNode ) return 0;

  std::vector<std::string> pathList;

  splitPath(path,pathList);

  PHNodeIterator it(topNode);

  PHCompositeNode* rv = 0;

  if ( pathList.size() == 1 ) 
    {
      rv = static_cast<PHCompositeNode*>
	(it.findFirst("PHCompositeNode",pathList[0].c_str()));
    }
  else
    {
      for ( size_t i = 0; i < pathList.size()-1 ; ++i ) 
	{
	  std::string name = pathList[i];
	  bool ok = it.cd(name.c_str());
	  if (!ok) return 0;
	}
      rv= static_cast<PHCompositeNode*>
	(it.findFirst("PHCompositeNode",pathList[pathList.size()-1].c_str()));
    }
  return rv;
}

//_____________________________________________________________________________
bool
emcNodeHelper::makeDSTnodes(PHCompositeNode* topNode)
{
  PHCompositeNode* dstNode = findCompositeNode(topNode,"DST");
  PHCompositeNode* parNode = findCompositeNode(topNode,"PAR");
  PHCompositeNode* dcmNode = findCompositeNode(topNode,"DCM");
  PHCompositeNode* evaNode = findCompositeNode(topNode,"EVA");
  PHCompositeNode* geaNode = findCompositeNode(topNode,"GEA");

  if ( !dstNode || !parNode || !dcmNode || !evaNode || !geaNode ) 
    {
      return false;
    }

  topNode->addNode(new PHCompositeNode("EMC"));
  topNode->addNode(new PHCompositeNode("EMC2"));

  return true;
}

//_____________________________________________________________________________
bool
emcNodeHelper::makeCompositeNode(PHCompositeNode* topNode,
				 const char* path, const char* opt)
{
  if (!path || !topNode) return false;

  std::vector<std::string> pathList;

  splitPath(path,pathList);

  PHNodeIterator it(topNode);

  std::string sopt = opt;

  bool create = (sopt=="-p");

  for ( size_t i = 0; i < pathList.size(); ++i ) 
    {
      std::string nodename = pathList[i];
      bool ok = it.cd(nodename.c_str());
      if ( !ok ) 
	{
	  if ( create )
	    {
	      it.addNode(new PHCompositeNode(nodename.c_str()));
	      bool ok = it.cd(nodename.c_str());	      
	      assert(ok==true);
	    }
	  else
	    {
	      return false;
	    }
	}
    }

  return true;
}
