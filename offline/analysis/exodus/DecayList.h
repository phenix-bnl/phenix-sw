#ifndef DECAYLIST_H
#define DECAYLIST_H

//-----------------------------------------------------------------------------
//
//  Declaration of the classes Decay and DecayList
//
//-----------------------------------------------------------------------------

#include <boost/utility.hpp>

class TH1F;

class Decay
{
  public:
    Decay();
    void SetID(const int id) {itsID=id;}
    void SetNBody(const int nb) {itsNBody=nb;}
    void SetBranchingRatio(const double br) {itsBranchingRatio=br;}
    void SetBRSum(const double brsum) {itsBRSum=brsum;}
    void SetParentID(const int pID) {itsParentID=pID;}
    void SetChildID(const int i, const int cID) {itsChildID[i]=cID;}
    void SetChildrenStable(const bool stable) {itsChildrenStable=stable;}
    void SetHistogram(TH1F *histogram){itsHistogram=histogram;}
    int GetID() const {return itsID;}
    int GetNBody() const {return itsNBody;}
    double GetBranchingRatio() const {return itsBranchingRatio;}
    double GetBRSum() const {return itsBRSum;}
    int GetParentID() const {return itsParentID;}
    int GetChildID(const int i) const {return itsChildID[i];}
    bool GetChildrenStable() const {return itsChildrenStable;}
    TH1F* GetHistogram() const {return itsHistogram;}
  private:
    int    itsID;
    int    itsNBody;
    double itsBranchingRatio;
    double itsBRSum;
    int    itsParentID;
    int    itsChildID[10];
    bool   itsChildrenStable;
    TH1F*  itsHistogram;
};

class DLNode
{
  public:
    virtual ~DLNode(){}
    virtual DLNode * Insert(Decay *pDec)=0;
    virtual Decay * Get(const int) const =0;
    virtual Decay * GetByID(const int) const =0;
  private:
};

class DLInternalNode: public DLNode
{
  public:
    DLInternalNode(Decay *thePart, DLNode *theNext);
    ~DLInternalNode(){delete myNext; myNext=0;}
    virtual DLNode * Insert(Decay * pDec);
    Decay * Get(const int n) const;
    Decay * GetByID(const int n) const;
  private:
    DLNode * myNext;
    Decay  * myPart;
};

class DLTailNode: public DLNode
{
  public:
    virtual DLNode * Insert(Decay *part);
    virtual Decay * Get(const int n) const;
    virtual Decay * GetByID(const int n) const;
  private:

};

class DLHeadNode: public DLNode
{
  public:
    DLHeadNode();
    ~DLHeadNode(){delete myNext; myNext=0;}
    virtual DLNode * Insert(Decay *pDec);
    Decay * Get(const int n) const {return myNext->Get(n-1);}
    Decay * GetByID(const int n) const {return myNext->GetByID(n);}
  private:
    DLNode * myNext;
};

class DecayList : public boost::noncopyable
{
  public:
    DecayList();
    ~DecayList();
    void Insert(Decay *pDec);
    Decay * Get(const int n) const {return myHead->Get(n);}
    Decay * GetByID(const int n) const {return myHead->GetByID(n);}
    DLNode * GetHeadNode(){return myHead;}
    int GetSize() const {return mySize;}
  private:
    DLHeadNode * myHead;
    int          mySize;
};

#endif /* DECAYLIST_H */
