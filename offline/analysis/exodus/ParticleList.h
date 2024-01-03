#ifndef PARTICLELIST_H
#define PARTICLELIST_H

//-----------------------------------------------------------------------------
//
//  Declaration of the class ParticleList
//
//-----------------------------------------------------------------------------

#include <vector>
#include <boost/utility.hpp>
#include "Particle.h"

class PLNode
{
  public:
    virtual ~PLNode(){}
    virtual PLNode * Insert(Particle *theParticle)=0;
    virtual void InsertAfter(Particle *theParticle)=0;
    virtual void Show() const =0;
    virtual void ShowOne(const int) const =0;
    virtual Particle * Get(const int) const =0;
    virtual PLNode * GetNextNode()=0;
  private:
};

class PLInternalNode : public PLNode
{
  public:
    PLInternalNode(Particle *theParticle, PLNode *next);
    ~PLInternalNode() {delete Next; Next=0; delete ParticleInList; ParticleInList=0;}
    virtual PLNode * Insert(Particle *theParticle);
    virtual void InsertAfter(Particle *theParticle);
    virtual void Show() const {ParticleInList->Show(); Next->Show();}
    virtual void ShowOne(const int n) const;
    PLNode * GetNextNode(){return Next;}
    Particle * Get(const int n) const;
  private:
    Particle * ParticleInList;
    PLNode   * Next;
};

class PLTailNode : public PLNode
{
  public:
    virtual PLNode * Insert(Particle *theParticle);
    virtual void InsertAfter(Particle *theParticle);
    virtual void Show() const {}
    virtual void ShowOne(const int) const {}

    PLNode * GetNextNode(){return this;}
    virtual Particle * Get(const int) const
    {
      Particle *p = new Particle; return p;
    }

  private:
};

class PLHeadNode : public PLNode
{
  public:
    PLHeadNode();
    ~PLHeadNode(){delete Next;}
    virtual PLNode * Insert(Particle *theParticle);
    virtual void InsertAfter(Particle *theParticle);
    virtual void Show() const {Next->Show();}
    virtual void ShowOne(const int n) const {Next->ShowOne(n-1);}
    PLNode * GetNextNode(){return Next;}
    virtual Particle * Get(const int n) const {return Next->Get(n-1);}
  private:
    PLNode * Next;
};

class ParticleList : public boost::noncopyable
{
  public:
    ParticleList();
    ~ParticleList(){delete Head; Head=0;}
    void Insert(Particle *theParticle);
    void InsertAfter(PLNode *theCurrent, Particle *theParticle);
    void ShowOne(const int n) const {Head->ShowOne(n);}
    Particle * Get(const int n) const {return Head->Get(n);}
    PLNode * GetHeadNode() const {return Head;}
    int GetLength() const {return Length;}

    operator std::vector<Particle*>() const;
  private:
    PLHeadNode * Head;
    int          Length;
};


#endif /* PARTICLELIST_H */
