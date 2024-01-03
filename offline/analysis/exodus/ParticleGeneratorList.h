#ifndef PARTICLEGENERATORLIST_H
#define PARTICLEGENERATORLIST_H

//-----------------------------------------------------------------------------
//
//  Declaration of the class ParticleGeneratorList
//
//-----------------------------------------------------------------------------

#include <vector>
#include <boost/utility.hpp>

class ParticleGenerator;

class PGLNode
{
  public:
    virtual ~PGLNode(){}
    virtual PGLNode * Insert(ParticleGenerator *pGen)=0;
    virtual ParticleGenerator* Get(const int)=0;
    virtual ParticleGenerator* GetByID(const int)=0;
  private:
};

class PGLInternalNode: public PGLNode
{
  public:
    PGLInternalNode(ParticleGenerator *theGen, PGLNode *theNext);
    ~PGLInternalNode(){delete Next;Next=0;}
    virtual PGLNode * Insert(ParticleGenerator *pGen);
    ParticleGenerator * Get(const int n);
    ParticleGenerator * GetByID(const int n);
  private:
    PGLNode           * Next;
    ParticleGenerator * Generator;
};

class PGLTailNode: public PGLNode
{
  public:
    virtual PGLNode * Insert(ParticleGenerator *gen);
    virtual ParticleGenerator * Get(const int n);
    virtual ParticleGenerator * GetByID(const int n);
  private:
};

class PGLHeadNode: public PGLNode
{
  public:
    PGLHeadNode();
    ~PGLHeadNode(){delete Next; Next=0;}
    virtual PGLNode * Insert(ParticleGenerator *pGen);
    ParticleGenerator * Get(const int n){return Next->Get(n-1);}
    ParticleGenerator * GetByID(const int n){return Next->GetByID(n);}
  private:
    PGLNode * Next;
};

class ParticleGeneratorList : public boost::noncopyable
{
  public:
    ParticleGeneratorList();
    ~ParticleGeneratorList();
    void Insert(ParticleGenerator *pGen);
    ParticleGenerator* Get(const int n) const {return Head->Get(n);}
    ParticleGenerator* GetByID(const int n) const {return Head->GetByID(n);}

    operator std::vector<ParticleGenerator*>() const;
  private:
    PGLHeadNode * Head;
};

#endif /* PARTICLEGENERATORLIST_H */
