#ifndef __SVXFILLHISTO__
#define __SVXFILLHISTO__

#include <string>

class TFile;

class BbcOut;
class SvxRawhitList;
class SvxClusterList;
class SvxRawhitClusterList;
class SvxSegmentList;
class VtxOut;

class svxAddress;

struct SvxEventContainer {
  public:
    BbcOut               *m_bbc;
    SvxRawhitList        *m_rawlist;
    SvxClusterList       *m_cluslist;
    SvxRawhitClusterList *m_rawcluslist;
    SvxSegmentList       *m_seglist;
    VtxOut               *m_vtxout;

  public:
    virtual void setPointer(BbcOut *bbc, 
                            SvxRawhitList *rawlist, SvxClusterList *cluslist,
                            SvxRawhitClusterList *rawcluslist, SvxSegmentList *seglist,VtxOut *vtxout){
      m_bbc        = bbc;
      m_rawlist    = rawlist;
      m_cluslist   = cluslist;
      m_rawcluslist= rawcluslist;
      m_seglist    = seglist;
      m_vtxout     = vtxout;
    };


};

class SvxFillHisto {
  public:
    SvxFillHisto(std::string name=""){ m_name = name; }
    virtual ~SvxFillHisto(){}

    virtual std::string GetName(){ return m_name;}; // if NULL, not chagne gDirectory

    virtual void initialize(TFile *outfile=NULL){}; // if NULL, not chagne gDirectory
    virtual void write(TFile *outfile=NULL){};

    virtual void fillInitRun(int runnumber=0){};
//    virtual void fill(SvxRawhitList *rawlist, BbcOut* bbc){}; // called event by event
    virtual void fill(SvxEventContainer *cnt=NULL){}; // called event by event
    virtual void set_svxAddress(svxAddress* address) { m_addr = address; }

  private:
    std::string m_name;

  protected:
    svxAddress* m_addr;
};
#endif
