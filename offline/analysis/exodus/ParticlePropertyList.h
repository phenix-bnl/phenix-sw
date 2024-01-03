#ifndef PARTICLEPROPERTYLIST_H
#define PARTICLEPROPERTYLIST_H

//-----------------------------------------------------------------------------
//
//  Declaration of the class ParticlePropertyList
//
//-----------------------------------------------------------------------------
#include <boost/utility.hpp>

class ParticleProperty;

class PPLNode
{
  public:
    virtual ~PPLNode(){}
    virtual PPLNode * Insert(ParticleProperty *pDec)=0;
    virtual ParticleProperty * Get(const int) const =0;
    virtual ParticleProperty * GetByID(const int) const =0;
  private:
};

class PPLInternalNode: public PPLNode
{
  public:
    PPLInternalNode(ParticleProperty *thePart, PPLNode *theNext);
    ~PPLInternalNode(){delete Next;Next=0;}
    virtual PPLNode * Insert(ParticleProperty *pDec);
    ParticleProperty * Get(const int n) const ;
    ParticleProperty * GetByID(const int n) const;
  private:
    PPLNode          * Next;
    ParticleProperty * Particle;
};

class PPLTailNode: public PPLNode
{
  public:
    virtual PPLNode * Insert(ParticleProperty *part);
    virtual ParticleProperty * Get(const int n) const;
    virtual ParticleProperty * GetByID(const int n) const;
  private:
};

class PPLHeadNode: public PPLNode
{
  public:
    PPLHeadNode();
    ~PPLHeadNode(){delete Next; Next=0;}
    virtual PPLNode * Insert(ParticleProperty *pDec);
    ParticleProperty * Get(const int n) const {return Next->Get(n-1);}
    ParticleProperty * GetByID(const int n) const {return Next->GetByID(n);}
  private:
    PPLNode * Next;
};

class ParticlePropertyList : public boost::noncopyable
{
  public:
    ParticlePropertyList();
    ~ParticlePropertyList();
    void Insert(ParticleProperty *pDec);
    ParticleProperty * Get(const int n) const {return Head->Get(n);}
    ParticleProperty * GetByID(const int n) const {return Head->GetByID(n);}
  private:
    PPLHeadNode * Head;
};

#endif /* PARTICLEPROPERTYLIST_H */
