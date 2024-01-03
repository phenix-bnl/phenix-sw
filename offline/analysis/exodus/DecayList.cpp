//-----------------------------------------------------------------------------
//
//  Implementation of the class DecayList
//
//-----------------------------------------------------------------------------

#include <algorithm>
#include "DecayList.h"

DLInternalNode::DLInternalNode(Decay *thePart, DLNode *theNext)
{
  myNext = theNext;
  myPart = thePart;
}

DLNode * DLInternalNode::Insert(Decay *thePart)
{
  myNext = myNext->Insert(thePart);
  return this;
}

Decay *DLInternalNode::Get(const int n) const
{
  if ( n==0 )
    return myPart;
  else
    return myNext->Get(n-1);
}

Decay *DLInternalNode::GetByID(const int n) const
{
  if ( n==myPart->GetID() )
    return myPart;
  else
    return myNext->GetByID(n);
}

DLNode * DLTailNode::Insert(Decay *part)
{
  DLInternalNode * dNode = new DLInternalNode(part,this);
  return dNode;
}

Decay *DLTailNode::Get(const int) const
{
  Decay * zero = new Decay;
  return zero;
}

Decay *DLTailNode::GetByID(const int) const
{
  Decay * zero = new Decay;
  return zero;
}

DLHeadNode::DLHeadNode()
{
  myNext = new DLTailNode;
}

DLNode * DLHeadNode::Insert(Decay *thePart)
{
  myNext = myNext->Insert(thePart);
  return this;
}

DecayList::DecayList()
{
  myHead = new DLHeadNode;
  mySize = 0;
}

DecayList::~DecayList()
{
  delete myHead;
  myHead = 0;
}

void DecayList::Insert(Decay *pPart)
{
  myHead->Insert(pPart);
  mySize++;
}

Decay::Decay() :
    itsID(0),
    itsNBody(0),
    itsBranchingRatio(0),
    itsBRSum(0),
    itsParentID(0),
    itsChildrenStable(false),
    itsHistogram(0)
{
  std::fill_n(itsChildID, sizeof(itsChildID)/sizeof(itsChildID[0]), 0);
}
