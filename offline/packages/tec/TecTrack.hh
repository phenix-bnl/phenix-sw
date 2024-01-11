#ifndef TECTRACK_H
#define TECTRACK_H 

#include "PHTrack.h"                                                            
#include "PHNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHObject.h"

//parameters for errors with 3 planes
const float TECSPRESP0_3p = 0.071;
const float TECSPRESP1_3p = 0.085; 
const float TECSPRESP2_3p = 128.3;

//parameters for errors with 4 planes
const float TECSPRESP0_4p = 0.0546;
const float TECSPRESP1_4p = 0.065;
const float TECSPRESP2_4p = 138.7;

//parameters for errors with 5 planes
const float TECSPRESP0_5p = 0.0456;
const float TECSPRESP1_5p = 0.051;
const float TECSPRESP2_5p = 139.3;

//parameters for errors with 6 planes
const float TECSPRESP0_6p = 0.0402;
const float TECSPRESP1_6p = 0.042;
const float TECSPRESP2_6p = 170.1;

#include "TecHit.hh"
                                                        
#include "PHGeometry.h"

#include <iostream>

/** Represents a single Tec track */
class TecTrack : public TObject {
 
 public:
 
///
  void identify(std::ostream& os = std::cout) const; 

/// Default constructor
  TecTrack(){}                                            
/// Constructor
  TecTrack(float xyzin[3], float xyzout[3]);
/// Constructor
  TecTrack(float xyzin[3], float xyzout[3], int nhits, 
           int sector, int side, int nwires[6], int nh[6]);
/// Copy constructor
  TecTrack(const TecTrack &source);

///
  void Initialize(float xyzin[3], float xyzout[3]);

/// Destructor
  virtual ~TecTrack() { }

///
  TecTrack& operator=(const TecTrack &);

// Accessors
/// 
  float* getXYZin() {return &XYZin[0];}
/// 
  float getXin() {return XYZin[0];}
/// 
  float getXinError();
/// 
  float getYinError();
/// 
  float getZinError() {return 0.;}
/// 
  float getXoutError();
/// 
  float getYoutError();
/// 
  float getZoutError() {return 0.;}
/// 
  float getYin() {return XYZin[1];}
/// 
  float getZin() {return XYZin[2];}
/// 
  void setXin(float a) {XYZin[0] = a;}
/// 
  void setYin(float a) {XYZin[1] = a;}
/// 
  void setZin(float a) {XYZin[2] = a;}
/// 
  float* getXYZout() {return &XYZout[0];}
/// 
  float getXout() {return XYZout[0];}
/// 
  float getYout() {return XYZout[1];}
/// 
  float getZout() {return XYZout[2];}
/// 
  void setXout(float a) {XYZout[0] = a;}
/// 
  void setYout(float a) {XYZout[1] = a;}
/// 
  void setZout(float a) {XYZout[2] = a;}
///
  float getPhi() {return Phi;}
/// 
  float getAlpha() {return Alpha;}
///
  float getPhiAtDch() {return PhiAtDch;}
/// 
  float getAlphaAtDch() {return AlphaAtDch;}
///
  float getSlope() {return Slope;}
///
  float getIntercept() {return Intercept;}
///
  int getNhits() { return Nhits; }
///
  void setNhits(int nh) { Nhits = nh; }
///
  float gettecMomentum() {return tecMomentum; }
///
  int gettecCharge() { return tecCharge; }
///
  int getIndex() { return Index; }
///
  void setIndex(int is) { Index = is; }
///
  int getSector() { return Index/TECMAXSIDE; }
///
  void setSector(int is) { Sector = is; }
///
  int getSide() { return Index%TECMAXSIDE; }
///
  void setSide(int is) { Side = is; }
///
  int getNwires(int i) {return Nwires[i];}
///
  void setNwires(int i, int nw) {Nwires[i]=nw;}
///
  int getNHITS(int i) {return NHITS[i];}
///
  void setNHITS(int i, int nh) {NHITS[i]=nh;}
///
  int getNFplanes();
///
  int getNFwires();
///
  int getPc3pointer(int i) { if(i>-1 && i<3) {return Pc3pointer[i];} else {return -1;} }
///
  int getPc1pointer(int i) { if(i>-1 && i<3) {return Pc1pointer[i];} else {return -1;} }
///
  float getPc1distance(int i) { if(i>-1 && i<3) {return Pc1distance[i];} else {return 999.;} }
///
  float getPc3distance(int i) { if(i>-1 && i<3) {return Pc3distance[i];} else {return 999.;} }
///
  int getPc3pointer() {return Pc3pointer[0];}
///
  int getPc1pointer() {return Pc1pointer[0];}
///
  float getPc3distance() {return Pc3distance[0];}
///
  float getPc1distance() {return Pc1distance[0];}
///
  float getTOFdistance() {return TOFdistance[0];}
///
  float getTOFdistance(int i) { if(i>-1 && i<3) {return TOFdistance[i];} else {return 999.;} }
///
  int getTOFpointer(int i) { if(i>-1 && i<3) {return TOFpointer[i];} else {return -1;} }
///
  int getTOFpointer() {return TOFpointer[0];}
///
  float getEMCdistance() {return EMCdistance[0];}
///
  float getEMCdistance(int i) { if(i>-1 && i<3) {return EMCdistance[i];} else {return 999.;} }
///
  int getEMCpointer() {return EMCpointer[0];}
///
  int getEMCpointer(int i) { if(i>-1 && i<3) {return EMCpointer[i];} else {return -1;} }
///
  float getEemcMin() {return EemcMin;}
///
  float getPc3Cut() {return Pc3Cut;}
///
  float getPc3EmcZCut() {return Pc3EmcZCut;}
///
  float getdEdX() {return dEdX;}
///
  void setdEdX(float a) {dEdX=a;}
///
  float getTrackLength() {return TrackLength;}
///
  void setTrackLength(float tl) {TrackLength=tl;}
///
  int getNdEdXbins() {return NdEdXbins;}
///
  void setNdEdXbins(int nb) {NdEdXbins=nb;}

///
  void setEemcMin(float e) {EemcMin=e;}
///
  void setPc3EmcZCut(float e) {Pc3EmcZCut=e;}
///
  void setPc3Cut(float e) {Pc3Cut=e;}

/// 
  PHBoolean project2TOF(PHCompositeNode*);
/// 
  PHBoolean project2YinvertedTOF(PHCompositeNode*);
/// 
  PHBoolean project2ZinvertedTOF(PHCompositeNode*);
/// 
  PHBoolean project2PC(PHCompositeNode*);
/// 
  PHBoolean project2YinvertedPC(PHCompositeNode*);
/// 
  PHBoolean project2ZinvertedPC(PHCompositeNode*);
///
  PHBoolean project2EMC(PHCompositeNode*);
///
  PHBoolean project2ZinvertedEMC(PHCompositeNode*);
///
  PHBoolean project2Pc3EMC(PHCompositeNode*);
///
  PHBoolean project2Pc3ZinvertedEMC(PHCompositeNode*);

 public:
// protected:
///
  float XYZin[3];
///
  float XYZout[3];
///
  float Phi;
///
  float Alpha;
///
  float PhiAtDch;
///
  float AlphaAtDch;
///
  float Slope;
///
  float Intercept;
///
  int Nhits;
/// 
  float tecMomentum;
///
  int tecCharge;
///
  int Index;
///
  int Sector;
///
  int Side;
/// 
  int Nwires[6];
///
  int NHITS[6];
///
  int Pc3pointer[3];
///
  float Pc3distance[3];
///
  int Pc1pointer[3];
///
  float Pc1distance[3];
///
  int TOFpointer[3];
///
  float TOFdistance[3];
///
  int EMCpointer[3];
///
  float EMCdistance[3];
///
  float EemcMin;
///
  float Pc3Cut;
///
  float Pc3EmcZCut;
///
  float dEdX;
///
  float TrackLength;
///
  int NdEdXbins; 

  ClassDef(TecTrack,1)

};

#endif /* TECTRACK_H */                                                         

