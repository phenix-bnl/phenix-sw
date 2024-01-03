//-----------------------------------------------------------------------------
//
//  Implementation of the class ParticleGeneratorList
//
//-----------------------------------------------------------------------------

#include "Particle.h"
#include "ParticleGenerator.h"
#include "ParticleGeneratorList.h"

PGLInternalNode::PGLInternalNode(ParticleGenerator *theGen, PGLNode *theNext)
{
  Next      = theNext;
  Generator = theGen;
}

PGLNode * PGLInternalNode::Insert(ParticleGenerator *theGen)
{
  Next = Next->Insert(theGen);
  return this;
}

ParticleGenerator * PGLInternalNode::Get(const int n)
{
  if ( n==0 )
    return Generator;
  else
    return Next->Get(n-1);
}

ParticleGenerator * PGLInternalNode::GetByID(const int n)
{
  if (n == Generator->GetID())
    return Generator;
  else
    return Next->GetByID(n);
}

PGLNode * PGLTailNode::Insert(ParticleGenerator *gen)
{
  PGLInternalNode * gNode = new PGLInternalNode(gen,this);
  return gNode;
}

ParticleGenerator* PGLTailNode::Get(const int)
{
  return 0;
}

ParticleGenerator * PGLTailNode::GetByID(const int n)
{
  return Get(n);
}

PGLHeadNode::PGLHeadNode()
{
  Next = new PGLTailNode;
}

PGLNode * PGLHeadNode::Insert(ParticleGenerator *theGen)
{
  Next = Next->Insert(theGen);
  return this;
}

ParticleGeneratorList::ParticleGeneratorList()
{
  Head = new PGLHeadNode;
}

ParticleGeneratorList::~ParticleGeneratorList()
{
  delete Head;
  Head = 0;
}

void ParticleGeneratorList::Insert(ParticleGenerator *pGen)
{
  Head->Insert(pGen);
}

ParticleGeneratorList::operator std::vector<ParticleGenerator*>() const {
  std::vector<ParticleGenerator*> generators;

  int i = 1;
  ParticleGenerator* generator = 0;
  while ((generator = Get(i++)))
    generators.push_back(generator);

  return generators;
}
