// ==================
// FILE: SvxCentralTrack.h
// ==================

#ifndef __SVXCENTRALTRACK_H_
#define __SVXCENTRALTRACK_H_

#include <PHObject.h>
#include <phool.h>

#include <iostream>

#include <SvxClusterInfo.h>

/**
 * @brief  The abstract class for a reconstructed DC based tracking with associated VTX clusters.
 *
 * Created on 11/07/2011 by Takashi Hachiya.
 */

class SvxClusterInfo;
class SvxResidualInfo;

class SvxCentralTrack : public PHObject
{

 public:
   virtual ~SvxCentralTrack() {}

   virtual void setDchIndex(int ind){PHOOL_VIRTUAL_WARN("DchIndex ");}
   virtual int  getDchIndex()       {PHOOL_VIRTUAL_WARN("DchIndex "); return -9999;}

   virtual void  addClusterInfo(SvxClusterInfo* cinfo){ PHOOL_VIRTUAL_WARN("ClusterPosition");}
   virtual SvxClusterInfo* getClusterInfo(int hit=0)  { PHOOL_VIRTUAL_WARN("ClusterPosition"); return NULL;}

   virtual short getNhits()       {PHOOL_VIRTUAL_WARN("Nhits "); return -9999;}
   virtual short getNhits(int layer) {PHOOL_VIRTUAL_WARN("Nhits "); return -9999;}
   virtual char  getHitPattern()  {PHOOL_VIRTUAL_WARN("HitPattern "); return 0;}
   
   // Chi2-Probability of track association
   virtual void  setQuality(float q){PHOOL_VIRTUAL_WARN("Quality ");}
   virtual float getQuality()       {PHOOL_VIRTUAL_WARN("Quality "); return -9999.;}
    
   // Quality of VTX cluster associations (links)
   virtual void  setLinkQuality(float q){PHOOL_VIRTUAL_WARN("LinkQuality ");}
   virtual float getLinkQuality()       {PHOOL_VIRTUAL_WARN("LinkQuality "); return -9999.;}

   // Score of VTX links
   virtual void  setLinkScore(float q){PHOOL_VIRTUAL_WARN("LinkScore ");}
   virtual float getLinkScore()       {PHOOL_VIRTUAL_WARN("LinkScore "); return -9999.;}

   // DCA2D (previously default, but now save DCA from beam center)
   virtual void  setDCA2D(float d){PHOOL_VIRTUAL_WARN("DCA2D ");}
   virtual float getDCA2D(){PHOOL_VIRTUAL_WARN("DCA2D ");return -9999.;}

   virtual void  setDCAZ(float d){PHOOL_VIRTUAL_WARN("DCAZ ");}
   virtual float getDCAZ(){PHOOL_VIRTUAL_WARN("DCAZ ");return -9999.;}
   //
   // DCA from primary (event by event vertex) 
   virtual void  setDCA2Dprimary(float d){PHOOL_VIRTUAL_WARN("DCA2Dprimary ");}
   virtual float getDCA2Dprimary(){PHOOL_VIRTUAL_WARN("DCA2Dprimary ");return -9999.;}

   virtual void  setDCAZprimary(float d){PHOOL_VIRTUAL_WARN("DCAZprimary ");}
   virtual float getDCAZprimary(){PHOOL_VIRTUAL_WARN("DCAZprimary ");return -9999.;}

   // DCA position
   virtual float getClosestApproach(int coor)
   {
     PHOOL_VIRTUAL_WARN("getClosestApproachToPrimaryVirtex");
     return -9999.9;
   }
   virtual void setClosestApproach(float cax, float cay, float caz)
   {
     PHOOL_VIRTUAL_WARN("setClosestApproachToPrimaryVirtex");
     return;
   }

   // Live fraction percentage in area surrounding expected hit position in each layer
   virtual void  setLivePercentage(int layer, float livepercent) {PHOOL_VIRTUAL_WARN("m_live_percentage[] ");}
   virtual float getLivePercentage(int layer) {PHOOL_VIRTUAL_WARN("m_live_percentage[] "); return -9999;}
   
   // for quick DCA estimation
   virtual void  setD2DCA0(float d){PHOOL_VIRTUAL_WARN("DCA2D ");}
   virtual float getD2DCA0(){PHOOL_VIRTUAL_WARN("DCA2D ");return -9999.;}

   // IsPrimary
   virtual void setIsPrimary(bool d){PHOOL_VIRTUAL_WARN("Primary ");}
   virtual bool isPrimary(){PHOOL_VIRTUAL_WARN("Primary ");return false;}


   // refined momentum at DCA
   virtual void  set3MomentumAtPrimaryVertex(float px, float py, float pz){PHOOL_VIRTUAL_WARN("Mom3AtDCA ");} // Mom at DCA
   virtual float get3MomentumAtPrimaryVertex(int coor) {PHOOL_VIRTUAL_WARN("Mom3AtDCA "); return -9999.;} // Mom at DCA
   virtual void  set3MomErrorAtPrimaryVertex(float pex, float pey, float pez){PHOOL_VIRTUAL_WARN("Mom3ErrAtDCA ");}
   virtual float get3MomErrorAtPrimaryVertex(int coor){PHOOL_VIRTUAL_WARN("Mom3ErrAtDCA "); return -9999.;}

  
   // dE/dx  
   virtual void  set_dEdX1(float val) {PHOOL_VIRTUAL_WARN("dEdX1 ");}
   virtual void  set_dEdX2(float val) {PHOOL_VIRTUAL_WARN("dEdX2 ");}
   virtual float get_dEdX1() {PHOOL_VIRTUAL_WARN("dEdX1 "); return -9999;}
   virtual float get_dEdX2() {PHOOL_VIRTUAL_WARN("dEdX2 "); return -9999;}

   // VtxIndex
   virtual void  setVtxID(int id) {PHOOL_VIRTUAL_WARN("VtxID ");}
   virtual int   getVtxID()       {PHOOL_VIRTUAL_WARN("VtxID "); return -9999.;}

   // Chi-Square
   virtual void  setChiSquare(float chi2) {PHOOL_VIRTUAL_WARN("ChiSquare ");}
   virtual float getChiSquare()           {PHOOL_VIRTUAL_WARN("ChiSquare "); return -9999.;}
   virtual void  setNDF(int ndf)          {PHOOL_VIRTUAL_WARN("NDF ");}
   virtual float getNDF()                 {PHOOL_VIRTUAL_WARN("NDF "); return -9999;} // this is a bug. return type (float -> int) should be int.
   virtual int   getNDOF()                {PHOOL_VIRTUAL_WARN("NDOF "); return -9999;}

   virtual void  setChiSquareDPHI(float chi2) {PHOOL_VIRTUAL_WARN("ChiSquareDPHI ");}
   virtual float getChiSquareDPHI()           {PHOOL_VIRTUAL_WARN("ChiSquareDPHI "); return -9999.;}
   virtual void  setNDFDPHI(int ndf)          {PHOOL_VIRTUAL_WARN("NDFDPHI ");}
   virtual int   getNDFDPHI()                 {PHOOL_VIRTUAL_WARN("NDFDPHI "); return -9999;}

   virtual void  setChiSquareDZ(float chi2) {PHOOL_VIRTUAL_WARN("ChiSquareDZ ");}
   virtual float getChiSquareDZ()           {PHOOL_VIRTUAL_WARN("ChiSquareDZ "); return -9999.;}
   virtual void  setNDFDZ(int ndf)          {PHOOL_VIRTUAL_WARN("NDFDZ ");}
   virtual int   getNDFDZ()                 {PHOOL_VIRTUAL_WARN("NDFDZ "); return -9999;}

   virtual void  setChiSquare2(float chi2) {PHOOL_VIRTUAL_WARN("ChiSquare2 ");} // chi2 from 1:2 method
   virtual float getChiSquare2()           {PHOOL_VIRTUAL_WARN("ChiSquare2 "); return -9999.;}

   virtual void  setChiSquareNoCNT(float chi2) {PHOOL_VIRTUAL_WARN("ChiSquare2 ");} // chi2 from 1:2 method
   virtual float getChiSquareNoCNT()           {PHOOL_VIRTUAL_WARN("ChiSquare2 "); return -9999.;}
   virtual void  setNDFNoCNT(int ndf)          { PHOOL_VIRTUAL_WARN("NDF2 ");}
   virtual float getNDFNoCNT()                 { PHOOL_VIRTUAL_WARN("NDF2 ");  return -9999.;}
   virtual int   getNDOFNoCNT()                { PHOOL_VIRTUAL_WARN("NDOF2 "); return -9999;}
   virtual void  setDCA2DNoCNT(float d)        { PHOOL_VIRTUAL_WARN("DCA2D2 ");}
   virtual float getDCA2DNoCNT()               { PHOOL_VIRTUAL_WARN("DCA2D2 ");return -9999.; }
   virtual void  setDCAZNoCNT(float d)         { PHOOL_VIRTUAL_WARN("DCAZ2 ");}
   virtual float getDCAZNoCNT()                { PHOOL_VIRTUAL_WARN("DCAZ2 "); return -9999.; }


   // Unique 0b_XXXX 4 bit word (each bit show the if the hit is unique)
   // MSB-LSB(4,3,2,1bit) = (B3,B2,B1,B0)
   virtual void setUnique(int unique){PHOOL_VIRTUAL_WARN("Unique ");}
   virtual int  getUnique(){PHOOL_VIRTUAL_WARN("Unique "); return -9999;}

   // ratiotion angle (radian) for the background track
   //   idx : 0= phi, 1=theta
   virtual void  setRotationAngle(int idx, float val) {PHOOL_VIRTUAL_WARN("RotationAngle ");}                // no longer use
   virtual float getRotationAngle(int idx)            {PHOOL_VIRTUAL_WARN("RotationAngle "); return -9999.;} // no longer use
   virtual void  setRotatedAngle(int idx, float val) {PHOOL_VIRTUAL_WARN("RotatedAngle ");} 
   virtual float getRotatedAngle(int idx)            {PHOOL_VIRTUAL_WARN("RotatedAngle "); return -9999.;}

   // second closest hits at B0 and B1
   virtual void addSecondHit(int layer, SvxResidualInfo* cinfo){ PHOOL_VIRTUAL_WARN("addSecondHit");}
   virtual int              getNSecondHits(int layer)          { PHOOL_VIRTUAL_WARN("getNSecondHit"); return -9999;}
   virtual SvxResidualInfo* getSecondHit(int layer, int hit=0) { PHOOL_VIRTUAL_WARN("getSecondHit"); return NULL;}

 
   virtual void Reset() {
     std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
     return;
   }

   virtual int isValid() const {
     std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
     return 0;
   }

   virtual void identify(std::ostream &os=std::cout) const {
     os << "identify yourself: virtual SvxCentralTrack object" << std::endl;
     return;
   }

   virtual void print(){PHOOL_VIRTUAL_WARN("print");}

   ClassDef(SvxCentralTrack, 1);
};
#endif
