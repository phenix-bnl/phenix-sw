#ifndef __emcNodeHelper_h__
#define __emcNodeHelper_h__

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTable.hh>
#include <PHNodeIterator.h>
#include <PHObject.h>

#include <string>
#include <iostream>
#include <cassert> 

/** Utility class to ease object nodes manipulation. 
@ingroup helper
*/

class emcNodeHelper
{
public:
  emcNodeHelper();

  template <class T>
  static
  T* addTable(PHCompositeNode* node,
	      const char* tablename,
	      unsigned int tablesize,
	      bool transient = false,
	      const char* nodename="");
 
  template <class T>
  static
  T* addObject(PHCompositeNode* node,
	       const char* objectName,
	       bool transient = false,
	       const char* nodename="");

  template <class T>
  static
  void insertObject(PHCompositeNode* node,
		    T* object,
		    const char* objectName,  
		    bool transient = false,
		    const char* nodename="");

  static PHCompositeNode* findCompositeNode(PHCompositeNode* topNode, 
					    const char* path);

  template <class T>
  static
  T* getTable(const char* tableName, PHCompositeNode* topNode);

  template <class T>
  static
  T* getObject(const char* objectName, PHCompositeNode* topNode);

  static bool makeDSTnodes(PHCompositeNode* topNode);

  static bool makeCompositeNode(PHCompositeNode* topNode,
				const char* path, const char* opt);
};

//_____________________________________________________________________________
template <class T>
T* 
emcNodeHelper::addObject(PHCompositeNode* node,
			 const char* objectName,
			 bool transient,
			 const char* _nodename)
{
  std::string nodename = _nodename;

  if ( nodename.empty() )
    {
      nodename = objectName;
    }

  // First check if object is already there

  T* object = getObject<T>(nodename.c_str(),node);

  if ( !object )
    {
      object = new T;
      PHObject* ph = dynamic_cast<PHObject*>(object);
      if (!ph)
	{
	  std::cerr << __FILE__ << ":" << __LINE__ 
	       << " object is not a PHObject !"
	       << std::endl;
	  delete object;
	  return 0;
	}

      PHIODataNode<T>* onode = 
	new PHIODataNode<T>(object,nodename.c_str(),"PHObject");

      if ( transient ) 
	{
	  onode->makeTransient();
	}

      node->addNode(onode);
    }
  else
    {      
      // object alreay exists, we do nothing.
      std::cout << __FILE__ << ":" << __LINE__ << " emcNodeHelper::addObject : "
	   << "Object " << objectName << " is already in node "
	   << nodename << ". Doing nothing." 
	   << std::endl;
    }

  return object;
}

//_____________________________________________________________________________
template <class T>
void
emcNodeHelper::insertObject(PHCompositeNode* node,
			    T* object,
			    const char* objectName,
			    bool transient,
			    const char* _nodename)
{
  std::string nodename = _nodename;
  
  if ( nodename.empty() )
    {
      nodename = objectName;
    }

  // Check it's really a PHObject
  PHObject* test = dynamic_cast<PHObject*>(object);
  if ( !test && object ) 
    {
      std::cerr << __FILE__ << ":" << __LINE__ 
	   << " object is not a PHObject!"
	   << std::endl;
      return;
    }

  // First check if object is already there
  
  T* cobject = getObject<T>(nodename.c_str(),node);

  if ( !cobject )
    {
      PHIODataNode<T>* onode = 
	new PHIODataNode<T>(object,nodename.c_str(),"PHObject");
      
      if ( transient ) 
	{
	  onode->makeTransient();
	}

      node->addNode(onode);
    }
  else
    {      
      // object alreay exists, we do nothing.
      std::cout << __FILE__ << ":" << __LINE__ << " emcNodeHelper::insertObject : "
	   << "Object " << objectName << " is already in node "
	   << nodename << ". Doing nothing." 
	   << std::endl;
    }
}


//_____________________________________________________________________________
template <class T>
T* 
emcNodeHelper::addTable(PHCompositeNode* node,
			const char* tablename,
			unsigned int tablesize, 
			bool transient,
			const char* _nodename)
{
  std::string nodename = _nodename;

  if ( nodename.empty() )
    {
      nodename = tablename;
    }

  // First check if table is already there

  T* table = getTable<T>(nodename.c_str(),node);

  if ( !table )
    {
      table = new T(tablename,tablesize);
      PHTable* test = dynamic_cast<PHTable*>(table);
      if (!test)
	{
	  std::cerr << __FILE__ << ":" << __LINE__ 
	       << " table is not a PHTable !"
	       << std::endl;
	  delete table;
	  return 0;
	}

      PHIODataNode<PHTable>* tnode = 
	new PHIODataNode<PHTable>(table,nodename.c_str(),"PHTable");

      if ( transient ) 
	{
	  tnode->makeTransient();
	}

      node->addNode(tnode);
    }
  else
    {      
      // table alreay exists, we do nothing.
      std::cout << __FILE__ << ":" << __LINE__ << " emcNodeHelper::addTable : "
	   << "Table " << tablename << " is already in node "
	   << nodename << ". Doing nothing." 
	   << std::endl;
    }

  return table;


}

//_____________________________________________________________________________
template<class T>
T*
emcNodeHelper::getObject(const char* objectName, PHCompositeNode* topNode)
{
  assert(topNode!=0);
  PHNodeIterator iter(topNode);

  PHIODataNode<T>* node = 
    static_cast<PHIODataNode<T>*>(iter.findFirst("PHIODataNode",
						 objectName));

  T* rv=0;

  if ( node ) 
    {
      //      rv = static_cast<T*>(node->getData());
      rv = node->getData();
    }
  return rv;
}

//_____________________________________________________________________________
template<class T>
T*
emcNodeHelper::getTable(const char* tableName, PHCompositeNode* topNode)
{
  assert(topNode!=0);
  PHNodeIterator iter(topNode);

  PHIODataNode<PHTable>* node = 
    static_cast<PHIODataNode<PHTable>*>(iter.findFirst("PHIODataNode",
						       tableName));

  T* rv=0;

  if ( node ) 
    {
      rv = static_cast<T*>(node->getData());
    }
  return rv;
}






#define getGeaObject(geatype, basetype, name, composite) \
  dynamic_cast< geatype * >( emcNodeHelper::getObject< basetype >(name, composite) )
    

#define EMCNODEASSERT(x) \
  if( (x) == NULL ){ \
    std::cerr << __PRETTY_FUNCTION__ << ": node " #x " not found." << std::endl; \
    return ABORTRUN; \
  }


#endif
