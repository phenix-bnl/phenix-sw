// ==================
// FILE: SvxCentralTrackv5.h
// ==================

#ifndef __SVXCENTRALTRACKV5_H_
#define __SVXCENTRALTRACKV5_H_

#include <SvxCentralTrack.h>
#include <SvxClusterInfov3.h>
#include <SvxResidualInfov1.h>

#include <iostream>

/**
 * @brief  The implementation v5 of SvxCentralTrack.
 *
 * Created on 04/28/2012 by Takashi Hachiya.
 */


class SvxCentralTrackv5 : public SvxCentralTrack
{
  public:
  enum {MAXCLUSTERS = 6, MAXRESIDUALS=3, MAXRESIDUALLAYER=2};

  public:
    SvxCentralTrackv5();
    virtual ~SvxCentralTrackv5() {}

    virtual void setDchIndex(int ind){ m_dchindex = ind;}
    virtual int  getDchIndex()       { return m_dchindex;}
  
    virtual void            addClusterInfo(SvxClusterInfo* cinfo);
    virtual SvxClusterInfo* getClusterInfo(int hit=0);
  
    virtual short getNhits();
    virtual short getNhits(int layer);
    virtual char  getHitPattern() { return m_hitpattern; }
    
    // Chi2-Probability of Drack association
    virtual void  setQuality(float q){ m_quality = q; }
    virtual float getQuality()       { return m_quality;}
    
    // DCA2D
    virtual void  setDCA2D(float d){ m_DCA2D = d;}
    virtual float getDCA2D()       { return m_DCA2D; }
    virtual void  setDCA2DError(float d){ m_DCA2DErr = d;}
    virtual float getDCA2DError()       { return m_DCA2DErr; }


    virtual void  setDCAZ(float d){ m_DCAZ = d;}
    virtual float getDCAZ()       { return m_DCAZ; }

   // for quick DCA estimation
   virtual void  setD2DCA0(float d){ m_DCA2D0=d;}
   virtual float getD2DCA0(){return m_DCA2D0;}

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
    virtual int   getNDOF()                { return m_ndf;}


   virtual void  setChiSquareDPHI(float chi2) { m_chisquare_dphi = chi2;}
   virtual float getChiSquareDPHI()           { return m_chisquare_dphi;}
   virtual void  setNDFDPHI(int ndf)          { m_ndf_dphi = ndf;}
   virtual int   getNDFDPHI()                 { return m_ndf_dphi;}

   virtual void  setChiSquareDZ(float chi2) { m_chisquare_dz = chi2;}
   virtual float getChiSquareDZ()           { return m_chisquare_dz;}
   virtual void  setNDFDZ(int ndf)          { m_ndf_dz = ndf;}
   virtual int   getNDFDZ()                 { return m_ndf_dz;}


    virtual void  setChiSquare2(float chi2) { m_chisquare2 = chi2;}
    virtual float getChiSquare2()           { return m_chisquare2;}

   // Unique 0b_XXXX 4 bit word (each bit show the if the hit is unique)
   // MSB-LSB(4,3,2,1bit) = (B3,B2,B1,B0)
   virtual void setUnique(int unique){ m_unique = unique;}
   virtual int  getUnique(){ return m_unique;}


   // i=0:phi0, i=1':the0
   virtual void  setRotatedAngle(int idx, float angle) { m_rotatedAngle[idx] = angle;} 
   virtual float getRotatedAngle(int idx)              { return m_rotatedAngle[idx];} 


   // second closest hits
   virtual void addSecondHit(int layer, SvxResidualInfo* cinfo);
   virtual int              getNSecondHits(int layer) { return (isValidResidualLayer(layer)) ? m_nresidual[layer] : -1;}
   virtual SvxResidualInfo* getSecondHit(int layer, int hit);


  
    // phool object
    virtual void Reset();
  
    virtual int isValid() const { return 1; }
  
    virtual void identify(std::ostream &os=std::cout) const {
      os << "identify yourself: SvxCentralTrackv5 object" << std::endl;
    }

    void print();

  protected:
    bool isValidHit(int hit) {return (0<=hit&&hit<m_ninfo); }
    bool isValidCoordinate(int coor) {return (0<=coor&&coor<3); }
    bool isValidResidualLayer(int layer) {return (0<=layer&&layer<MAXRESIDUALLAYER); }
  
  protected:
    // Data member definition
    int  m_dchindex;

    int              m_ninfo;       // Ncluster associated
    SvxClusterInfov3 m_vinfo[MAXCLUSTERS];
    char             m_hitpattern; // bit[7:0] = bit pattern showing the hit at each sublayer LSB=0, MSB=7.

    float m_quality;
    
    float m_DCA2D;
    float m_DCA2DErr;
    float m_DCAZ;
    float m_DCA2D0;
    bool  m_primary;
    float m_mom3AtDCA[3];
    float m_mom3ErrAtDCA[3];
    float m_closestApproach[3];

    float m_dEdX[2];
    int   m_vtxId;

    int   m_ndf;
    float m_chisquare;
    float m_chisquare2;

    int   m_ndf_dphi;
    float m_chisquare_dphi;
    int   m_ndf_dz;
    float m_chisquare_dz;

    int   m_unique;

    float m_rotatedAngle[2];

    int               m_nresidual[MAXRESIDUALLAYER]; // Nsecond associated clusters
    SvxResidualInfov1 m_vresidual[MAXRESIDUALLAYER][MAXRESIDUALS];

  ClassDef(SvxCentralTrackv5,1)
};
#endif
