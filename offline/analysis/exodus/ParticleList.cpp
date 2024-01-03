//-----------------------------------------------------------------------------
//
//  Implementation of the class ParticleList
//
//-----------------------------------------------------------------------------

#include <iostream>
#include <string>
#include "Particle.h"
#include "ParticleList.h"

PLInternalNode::PLInternalNode(Particle *theParticle, PLNode *next)
{
  ParticleInList = theParticle;
  Next           = next;
}

PLNode * PLInternalNode::Insert(Particle *theParticle)
{
  Next = Next->Insert(theParticle);
  return this;
}

void PLInternalNode::InsertAfter(Particle *theParticle)
{
  PLInternalNode * dataNode = new PLInternalNode(theParticle,Next);
  Next                      = dataNode;
}

void PLInternalNode::ShowOne(int n) const
{
  if ( n==0 )
    ParticleInList->Show();
  else
    Next->ShowOne(n-1);
}

Particle * PLInternalNode::Get(const int n) const
{
  if ( n==0 )
    return ParticleInList;
  else
    return Next->Get(n-1);
}

PLNode * PLTailNode::Insert(Particle *theParticle)
{
  PLInternalNode * dataNode = new PLInternalNode(theParticle,this);
  return dataNode;
}

void PLTailNode::InsertAfter(Particle*)
{
  std::cerr << "I don't know to insert something after the tailnode of the list!";
  std::cerr << std::endl;
}

PLHeadNode::PLHeadNode()
{
  Next = new PLTailNode;
}

PLNode * PLHeadNode::Insert(Particle *theParticle)
{
  Next = Next->Insert(theParticle);
  return this;
}

void PLHeadNode::InsertAfter(Particle *theParticle)
{
  PLInternalNode * dataNode = new PLInternalNode(theParticle,Next);
  Next                      = dataNode;
}

ParticleList::ParticleList()
{
  Head   = new PLHeadNode;
  Length = 0;
}

void ParticleList::Insert(Particle *pParticle)
{
  Head->Insert(pParticle);
  Length++;
}

void ParticleList::InsertAfter(PLNode *theCurrent, Particle *theParticle)
{
  theCurrent->InsertAfter(theParticle);
  Length++;
}

ParticleList::operator std::vector<Particle*>() const {
  std::vector<Particle*> particles;

  PLNode* node = GetHeadNode();

  for (int i = 0; i<GetLength(); i++) {
    node = node->GetNextNode();
    particles.push_back(node->Get(0));
  }

  return particles;
}
