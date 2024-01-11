// ==================
// FILE: SvxCentralTrackv1.h
// ==================

#ifndef __SVXCENTRALTRACKV1_H_
#define __SVXCENTRALTRACKV1_H_

#include <SvxCentralTrack.h>
#include <SvxClusterInfov1.h>

#include <iostream>

/**
 * @brief  The implementation v1 of SvxCentralTrack.
 *
 * Created on 11/07/2011 by Takashi Hachiya.
 * vector is changed to a fixed array 2/10/2012 by Takashi Hachiya
 */


class SvxCentralTrackv1 : public SvxCentralTrack
{
  enum {MAXCLUSTERS = 6};

  public:
    SvxCentralTrackv1();
    virtual ~SvxCentralTrackv1() {}

    virtual void setDchIndex(int ind){ m_dchindex = ind;}
    virtual int  getDchIndex()       { return m_dchindex;}
  
    virtual void            addClusterInfo(SvxClusterInfo* cinfo);
    virtual SvxClusterInfo* getClusterInfo(int hit=0);
  
    virtual short getNhits();
    
    // Chi2-Probability of Drack association
    virtual void  setQuality(float q){ m_quality = q; }
    virtual float getQuality()       { return m_quality;}
    
    // DCA2D
    virtual void  setDCA2D(float d){ m_DCA2D = d;}
    virtual float getDCA2D()       { return m_DCA2D; }
    virtual void  setDCA2DError(float d){ m_DCA2DErr = d;}
    virtual float getDCA2DError()       { return m_DCA2DErr; }

    // Primary
    virtual void setIsPrimary(bool d){ m_primary = d;}
    virtual bool isPrimary()       { return m_primary; }

    // DCA position
    virtual void  setClosestApproach(float cax, float cay, float caz)
    {
      m_closestApproach[0] = cax;
      m_closestApproach[1] = cay;
      m_closestApproach[2] = caz;
    }
    virtual float getClosestApproach(int coor);
  
    // refined momentum at DCA
    virtual void  set3MomentumAtPrimaryVertex(float px, float py, float pz){ // Mom at DCA
      m_mom3AtDCA[0] = px;
      m_mom3AtDCA[1] = py;
      m_mom3AtDCA[2] = pz;
    }
    virtual float get3MomentumAtPrimaryVertex(int coor);

    virtual void  set3MomErrorAtPrimaryVertex(float pex, float pey, float pez){ // Mom Error at DCA
      m_mom3ErrAtDCA[0] = pex;
      m_mom3ErrAtDCA[1] = pey;
      m_mom3ErrAtDCA[2] = pez;
    }
    virtual float get3MomErrorAtPrimaryVertex(int coor);
   
    // dE/dx  
    virtual void set_dEdX1(float val) { m_dEdX[0] = val;}
    virtual void set_dEdX2(float val) { m_dEdX[1] = val;}
    virtual float get_dEdX1() { return m_dEdX[0];}
    virtual float get_dEdX2() { return m_dEdX[1];}

    // VtxIndex
    virtual void  setVtxID(int id) { m_vtxId = id;}
    virtual int   getVtxID()       { return m_vtxId;}
  
    // Chi-Square
    virtual void  setChiSquare(float chi2) { m_chisquare = chi2;}
    virtual float getChiSquare()           { return m_chisquare;}
    virtual void  setNDF(int ndf)          { m_ndf = ndf;}
    virtual float getNDF()                 { return m_ndf;}
  
    // phool object
    virtual void Reset();
  
    virtual int isValid() const { return 1; }
  
    virtual void identify(std::ostream &os=std::cout) const {
      os << "identify yourself: SvxCentralTrackv1 object" << std::endl;
    }

    void print();

  protected:
    bool isValidHit(int hit) {return (0<=hit&&hit<m_ninfo); }
    bool isValidCoordinate(int coor) {return (0<=coor&&coor<3); }
  
  protected:
    // Data member definition
    int  m_dchindex;

    int              m_ninfo; // Ncluster associated
    SvxClusterInfov1 m_vinfo[MAXCLUSTERS];

    float m_quality;
    
    float m_DCA2D;
    float m_DCA2DErr;
    bool  m_primary;
    float m_mom3AtDCA[3];
    float m_mom3ErrAtDCA[3];
    float m_closestApproach[3];

    float m_dEdX[2];
    int   m_vtxId;

    int   m_ndf;
    float m_chisquare;

  ClassDef(SvxCentralTrackv1,1)
};
#endif
