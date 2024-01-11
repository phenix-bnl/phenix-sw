#ifndef __PHNodeHelper_h__
#define __PHNodeHelper_h__

#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHIODataNode.h>
#include <PHTable.hh>
#include <cassert>
#include <string>

template<class T>
class PHNodeHelper
{
public:

  static
  void createTable(const char* tableName, int tableSize,
		   PHCompositeNode* attachTo,
		   const char* tableNodeName,
		   const char* className);
  static
  void createObject(const char* objectName, 
		    PHCompositeNode* attachTo,
		    const char* objectNodeName,
		    const char* className);

  static
  bool createTableNode(const char* tableNodeName, PHCompositeNode* attachTo);
  static
  bool createObjectNode(const char* objectNodeName, PHCompositeNode* attachTo);

  static
  T* getTable(const char* tableName, PHCompositeNode* from);
  static
  T* getObject(const char* objectName, PHCompositeNode* from);

  static
  PHIODataNode<T>* getTableNode(const char* tableName, PHCompositeNode* from);
  static
  PHIODataNode<T>* getObjectNode(const char* objectName, PHCompositeNode* from);

  static
  bool copyTable(const char* tableName, PHCompositeNode* from, PHCompositeNode* to);
  static
  bool copyObject(const char* objectName, PHCompositeNode* from, PHCompositeNode* to);

};

//_____________________________________________________________________________
template<class T>
void
PHNodeHelper<T>::createTable(const char* tableName, int tableSize,
			     PHCompositeNode* attachTo,
			     const char* tableNodeName,
			     const char* className)
{
  std::string nodename = tableNodeName;

  if ( nodename.empty() ) 
    {
      nodename = tableName;
    }

  T* table = new T(tableName,tableSize);

  std::string classN = className;
  PHIODataNode<T>* tableNode;

  if (classN.empty()) 
    {
      tableNode = 
	new PHIODataNode<T>(table,nodename.c_str());
    }
  else
    {
      tableNode = 
	new PHIODataNode<T>(table,nodename.c_str(),classN.c_str());      
    }
  
  assert(attachTo!=0);
  attachTo->addNode(tableNode);
}

//_____________________________________________________________________________
template<class T>
void
PHNodeHelper<T>::createObject(const char* objectName, 
			      PHCompositeNode* attachTo,
			      const char* objectNodeName,
			      const char* className)
{
  std::string nodename = objectNodeName;

  if ( nodename.empty() ) 
    {
      nodename = objectName;
    }

  T* object = new T();

  std::string classN = className;
  PHIODataNode<T>* objectNode = NULL;

  if (classN.empty() ) 
    {
      objectNode = 
	new PHIODataNode<T>(object,nodename.c_str(),"PHObject");
    }
  else
    {
      objectNode = 
	new PHIODataNode<T>(object,nodename.c_str(),"PHObject");      
    }
  
  assert(attachTo!=0);
  attachTo->addNode(objectNode);
}

//_____________________________________________________________________________
template<class T>
bool
PHNodeHelper<T>::createTableNode(const char* tableNodeName, 
				 PHCompositeNode* attachTo)
{
  PHIODataNode<PHTable>* tableNode = 
    new PHIODataNode<PHTable>(0,tableNodeName);

  assert(attachTo!=0);
  attachTo->addNode(tableNode);

  return true;
}

//_____________________________________________________________________________
template<class T>
bool
PHNodeHelper<T>::createObjectNode(const char* objectNodeName, 
				  PHCompositeNode* attachTo)
{
  PHIODataNode<PHObject>* objectNode = 
    new PHIODataNode<PHObject>(0,objectNodeName);

  assert(attachTo!=0);
  attachTo->addNode(objectNode);
  return true;
}

//_____________________________________________________________________________
template<class T>
PHIODataNode<T>* 
PHNodeHelper<T>::getTableNode(const char* tableName, PHCompositeNode* from)
{
  assert(from!=0);
  PHNodeIterator iter(from);

  PHIODataNode<PHTable>* node = 
    static_cast<PHIODataNode<PHTable>*>(iter.findFirst("PHIODataNode",
						       tableName));
  return node;
}

//_____________________________________________________________________________
template<class T>
PHIODataNode<T>* 
PHNodeHelper<T>::getObjectNode(const char* objectName, PHCompositeNode* from)
{
  assert(from!=0);
  PHNodeIterator iter(from);

  PHIODataNode<PHObject>* node = 
    static_cast<PHIODataNode<PHObject>*>(iter.findFirst("PHIODataNode",
							objectName));
  return node;
}

//_____________________________________________________________________________
template<class T>
T*
PHNodeHelper<T>::getTable(const char* tableName, PHCompositeNode* from)
{
  assert(from!=0);
  PHNodeIterator iter(from);

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

//_____________________________________________________________________________
template<class T>
T*
PHNodeHelper<T>::getObject(const char* objectName, PHCompositeNode* from)
{
  assert(from!=0);
  PHNodeIterator iter(from);

  PHIODataNode<PHObject>* node = 
    static_cast<PHIODataNode<PHObject>*>(iter.findFirst("PHIODataNode",
							objectName));

  T* rv=0;

  if ( node ) 
    {
      rv = static_cast<T*>(node->getData());
    }
  return rv;
}

//_____________________________________________________________________________
template<class T>
bool
PHNodeHelper<T>::copyObject(const char* objectName,
			    PHCompositeNode* from, PHCompositeNode* to)
{
 
  T* fromobject = getObject(objectName,from);
  assert(fromobject!=0);

  PHIODataNode<PHObject>* tonode = 
    static_cast<PHIODataNode<PHObject>*>(getObjectNode(objectName,to));

  if ( !tonode ) 
    {
      printf("Creating tonode: %s\n",objectName);
      createObjectNode(objectName,to);
    }
  else
    {
      T* toobject = getObject(objectName,to);
      if ( !toobject ) 
	{
	  printf("Creating toobject: %s\n",objectName);
	  toobject = new T();
	}
      tonode->setData(toobject);
      to->addNode(tonode);
      *toobject = *fromobject;
    }

  return true;
}

#endif
