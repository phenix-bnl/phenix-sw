#ifndef __PHNodeHelper_h__
#define __PHNodeHelper_h__

#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "PHIODataNode.h"
#include "PHTable.hh"

template<class T>
class PHNodeHelper
{
public:

  static
  T* getTable(const char* tableName, PHCompositeNode* from);

  static
  T* getObject(const char* objectName, PHCompositeNode* from);

};


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

template<class T>
T*
PHNodeHelper<T>::getObject(const char* objectName, PHCompositeNode* from)
{
  assert(from!=0);
  PHNodeIterator iter(from);

  PHIODataNode<PHObject>* node = 
    static_cast<PHIODataNode<PHObject>*>(iter.findFirst("PHIODataNode",
							objectName));

  iter.print();

  T* rv=0;

  if ( node ) 
    {
      rv = static_cast<T*>(node->getData());
    }
  return rv;
}

#endif
