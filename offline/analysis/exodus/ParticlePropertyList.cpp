//-----------------------------------------------------------------------------
//
//  Implementation of the class ParticlePropertyList
//
//-----------------------------------------------------------------------------

#include "Particle.h"
#include "ParticleProperty.h"
#include "ParticlePropertyList.h"

PPLInternalNode::PPLInternalNode(ParticleProperty *thePart, PPLNode *theNext)
{
  Next     = theNext;
  Particle = thePart;
}

PPLNode * PPLInternalNode::Insert(ParticleProperty *thePart)
{
  Next = Next->Insert(thePart);
  return this;
}

ParticleProperty * PPLInternalNode::Get(const int n) const
{
  if ( n==0 )
    return Particle;
  else
    return Next->Get(n-1);
}

ParticleProperty * PPLInternalNode::GetByID(const int n) const
{
  if (n == Particle->GetID())
    return Particle;
  else
    return Next->GetByID(n);
}

PPLNode * PPLTailNode::Insert(ParticleProperty *part)
{
  PPLInternalNode * dNode = new PPLInternalNode(part,this);
  return dNode;
}

ParticleProperty* PPLTailNode::Get(const int) const
{
  ParticleProperty * zero = new ParticleProperty;
  zero->Set(0,0,0,0);
  return zero;
}

ParticleProperty* PPLTailNode::GetByID(const int) const
{
  ParticleProperty * zero = new ParticleProperty;
  zero->Set(0,0,0,0);
  return zero;
}

PPLHeadNode::PPLHeadNode()
{
  Next = new PPLTailNode;
}

PPLNode * PPLHeadNode::Insert(ParticleProperty *thePart)
{
  Next = Next->Insert(thePart);
  return this;
}

ParticlePropertyList::ParticlePropertyList()
{
  Head = new PPLHeadNode;
}

ParticlePropertyList::~ParticlePropertyList()
{
  delete Head;
  Head = 0;
}

void ParticlePropertyList::Insert(ParticleProperty *pPart)
{
  Head->Insert(pPart);
}
