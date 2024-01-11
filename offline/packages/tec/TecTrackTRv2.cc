#include "TecTrackTRv2.hh"
#include "PadCluster.h"
#include "PHGeometry.h"
#include <cmath>
#include <iostream>

ClassImp(TecTrackTRv2)

using namespace std;

//---------------------------------------------------------

TecTrackTRv2::TecTrackTRv2() { 
//  cout << "Use TecTrackTR::TecTrackTR(float in[3], float out[3]) constructor." << endl;
}

//---------------------------------------------------------

void TecTrackTRv2::Initialize(float in[3], float out[3]) {

using namespace PHGeometry;

 Nhits=0;
 XYZin[0]=in[0];
 XYZin[1]=in[1];
 XYZin[2]=in[2];
 XYZout[0]=out[0];
 XYZout[1]=out[1];
 XYZout[2]=out[2];
 Pc3Pointer=-1;
 Pc3Distance=999.;
 Pc3sPointer=-1;
 Pc3sDistance=999.;
 Sector = -1;
 Side = -1;
 if(in[2]<0. && out[2]<0.) {Side=0;}
 if(in[2]>0. && out[2]>0.) {Side=1;}
 for(int k=0; k<6; k++) Nwires[k] = 0;
 for(int k=0; k<6; k++) NHITS[k] = 0;
 for(int k=0; k<6; k++) NTR[k] = 0;
 for(int k=0; k<6; k++) DE[k] = 0.;
 for(int k=0; k<6; k++) TR[k] = 0.;
 dEdX = 0.;
 NdEdXbins = 0;
 dEdX06 = 0.;
 TrackLength = 0.;
 Nhits200 = 0;
 Nhits100 = 0;
 Nhits50 = 0;
 Ntr = 0;
 Tr = 0.;
 WeightedTimeBin=0.;
 WeightedTimeBinSq=0.;

 calculateXinError();
 calculateYinError();
 XYZinError[2]=999.;
 XYZoutError[0]=XYZinError[0];
 XYZoutError[1]=XYZinError[1];
 XYZoutError[2]=XYZinError[2];

    float xin = in[0];
    float yin = in[1];
    float xout = out[0];
    float yout = out[1];

    PHPoint pnt1 = PHPoint(xin,yin,0.);
    PHVector vct1 = PHVector(xout-xin,yout-yin,0.);
    PHPoint pnt2 = PHPoint(xout,yout,0.);
    PHLine ln1 = PHLine(pnt1,vct1);
    PHLine ln2 = PHLine(pnt2,vct1);
    PHPoint vtx = PHPoint(0.,0.,0.);
    PHVector beam = PHVector(0.,0.,1.);
// calculate Phi and Alpha at intersection with Tec reference circle
      PHCylinder reference = PHCylinder(vtx, TECREFERENCERADIUS, beam);
      PHPoint int1;
      PHPoint int2;
      intersectionLineCylinder(ln1, reference, int1, int2);
// Tec intersection coordinates
        float tecX = int1.getX();
        float tecY = int1.getY();
        if(tecX!=0 && (xin-xout)!=0) {
          Phi = -atan(tecY/tecX);
          Slope = (yin-yout)/(xin-xout);
          Intercept = yin - Slope*xin;
          Alpha = -(Phi+atan(Slope));
          Phi = Pi - Phi;
          if(fabs(Alpha)<0.002) { tecMomentum=999.; } else {
            tecMomentum = 1./(21.5*fabs(Alpha));
          }
          if(Alpha>0) {tecCharge=1;} else {tecCharge = -1;}
        }
        else {
          Alpha = Pi/2.; Phi = 2.*Pi; Slope = 999.; Intercept = 999.;
          tecMomentum = 0.; tecCharge=0;
        }
// calculate Phi and Alpha at intersection with Dch reference radius (220cm)
      PHCylinder referencedch = PHCylinder(vtx, 220, beam);
      intersectionLineCylinder(ln1, referencedch, int1, int2);
// Tec intersection coordinates
        tecX = int1.getX();
        tecY = int1.getY();
        if(tecX!=0) {
          PhiAtDch = -atan(tecY/tecX);
          AlphaAtDch = -(PhiAtDch+atan(Slope));
          PhiAtDch = Pi - PhiAtDch;
        }
        else {
          AlphaAtDch = Pi/2.; PhiAtDch = 2.*Pi;
        }

  return;
}

//---------------------------------------------------------

TecTrackTRv2::TecTrackTRv2(float in[3], float out[3]) {
 Initialize(in, out);
}

//---------------------------------------------------------

TecTrackTRv2::TecTrackTRv2(const TecTrack &source) {
  XYZin[0] = source.XYZin[0];
  XYZin[1] = source.XYZin[1];
  XYZin[2] = source.XYZin[2];
  XYZout[0] = source.XYZout[0];
  XYZout[1] = source.XYZout[1];
  XYZout[2] = source.XYZout[2];
  Phi = source.Phi;
  Alpha = source.Alpha;
  PhiAtDch = source.PhiAtDch;
  AlphaAtDch = source.AlphaAtDch;
  Slope = source.Slope;
  Intercept = source.Intercept;
  Nhits = source.Nhits;
  tecMomentum = source.tecMomentum;
  tecCharge = source.tecCharge;
  Pc3Pointer = source.Pc3pointer[0];
  Pc3Distance = source.Pc3distance[0];
  Pc3sPointer = -1;
  Pc3sDistance = 999.;
  Sector = source.Sector;
  Side = source.Side;
  dEdX = source.dEdX;
  NdEdXbins = source.NdEdXbins;
  TrackLength = source.TrackLength;
  for(int k=0; k<6; k++) Nwires[k] = source.Nwires[k];
  for(int k=0; k<6; k++) NHITS[k] = source.NHITS[k];
  for(int k=0; k<6; k++) NTR[k] = 0;
    dEdX06=0.;
    WeightedTimeBin=0.;
    WeightedTimeBinSq=0.;
    Nhits100=0;
    Nhits200=0;
    Nhits50=0;
    Ntr=0;
    Tr=0.;
  calculateXinError();
  calculateYinError();
  XYZinError[2]=999.;
  XYZoutError[0]=XYZinError[0];
  XYZoutError[1]=XYZinError[1];
  XYZoutError[2]=XYZinError[2];
}

TecTrackTRv2& TecTrackTRv2::operator=(const TecTrackTRv2 &source)
{
  XYZin[0]=source.getXin();
  XYZin[1]=source.getYin();
  XYZin[2]=source.getZin();
  XYZout[0]=getXout();
  XYZout[1]=getYout();
  XYZout[2]=getZout();
  Nhits = source.getNhits();
  Phi = source.getPhi();
  Alpha = source.getAlpha();
  tecMomentum = source.gettecMomentum();
  Sector = source.getSector();
  Side = source.getSide();
  TrackLength = source.getTrackLength();  
  dEdX06 = source.getdEdX06();
  WeightedTimeBin = source.getWeightedTimeBin();
  WeightedTimeBinSq = source.getWeightedTimeBinSq();
  Nhits50 = source.getNhits50();
  Nhits100 = source.getNhits100();
  Nhits200 = source.getNhits200();
  NdEdXbins = source.getNdEdXbins();
  Ntr = 0;
  Tr = 0;
  dEdX = 0;
  for (int k=0; k<6; k++)
    {
      DE[k] = source.getDE(k);
      dEdX += DE[k];
      NTR[k] = source.getNTR(k);
      Ntr += NTR[k];
      TR[k] = source.getTR(k);
      Tr += TR[k];
      Nwires[k] = source.getNwires(k);
      NHITS[k] = source.getNHITS(k);
    }
  PhiAtDch = source.getPhiAtDch();
  AlphaAtDch = source.getAlphaAtDch();
  Slope = source.getSlope();
  Intercept = source.getIntercept();
  XYZinError[0] = source.getXinError();
  XYZinError[1] = source.getYinError();
  XYZinError[2] = source.getZinError();
  XYZoutError[0] = source.getXoutError();
  XYZoutError[1] = source.getYoutError();
  XYZoutError[2] = source.getZoutError();
  Pc3Pointer=-1;
  Pc3Distance=999.;
  Pc3sPointer=-1;
  Pc3sDistance=999.;
  return *this;
 }

//---------------------------------------------------------

// Methods for track coordinate error determination.
// Functional form comes from a fit to space resolution as a function of 
// incidence angle for momenta from 0.5 to 4.0 GeV/c in simulation.
// 1.5 comes from comparison of simulation with gas gain 3000 and the data (run00)

void TecTrackTRv2::calculateXinError() {

  int sector = Sector;
  if(sector<0 || sector>3) sector=1;
  float SectorCenter=(TECPHIBOTE-(2*sector+1)*(TECPHIBOTE-TECPHITOPE)/8.)/180.*Pi;

    float angle=Alpha-(Phi-SectorCenter);
    float spres=0;
    int nfp=getNFplanes();
    if(nfp<=3)
     {
     spres = TECSPRESP0_3p + TECSPRESP1_3p*exp(-angle*angle*TECSPRESP2_3p);
     }
    if(nfp==4)
     {
     spres = TECSPRESP0_4p + TECSPRESP1_4p*exp(-angle*angle*TECSPRESP2_4p);
     }
    if(nfp==5)
     {
     spres = TECSPRESP0_5p + TECSPRESP1_5p*exp(-angle*angle*TECSPRESP2_5p);
     }
    if(nfp==6)
     {
     spres = TECSPRESP0_6p + TECSPRESP1_6p*exp(-angle*angle*TECSPRESP2_6p);
     }

    float erx = fabs(spres*sin(Phi));

  XYZinError[0] = erx;

}

//---------------------------------------------------------

void TecTrackTRv2::calculateYinError() {

  int sector=Sector;
  if(sector<0 || sector>3) sector=1;
  float SectorCenter=(TECPHIBOTE-(2*sector+1)*(TECPHIBOTE-TECPHITOPE)/8.)/180.*Pi;

    float angle=Alpha-(Phi-SectorCenter);
    float spres=0;
    int nfp=getNFplanes();
    if(nfp<=3)
     {
     spres = TECSPRESP0_3p + TECSPRESP1_3p*exp(-angle*angle*TECSPRESP2_3p);
     }
    if(nfp==4)
     {
     spres = TECSPRESP0_4p + TECSPRESP1_4p*exp(-angle*angle*TECSPRESP2_4p);
     }
    if(nfp==5)
     {
     spres = TECSPRESP0_5p + TECSPRESP1_5p*exp(-angle*angle*TECSPRESP2_5p);
     }
    if(nfp==6)
     {
     spres = TECSPRESP0_6p + TECSPRESP1_6p*exp(-angle*angle*TECSPRESP2_6p);
     }

    float ery = fabs(spres*cos(Phi));
    
  XYZinError[1] = ery;

}

//---------------------------------------------------------

void TecTrackTRv2::project2PC(PHCompositeNode* topNode) {

  PadCluster* pc3 = 0;
  PHTypedNodeIterator<PadCluster> paditer(topNode);
  PHIODataNode<PadCluster> *Pc3Node = paditer.find("Pc3Cluster");
  if(Pc3Node) { pc3 = (PadCluster*)Pc3Node->getData(); }
    else {cerr << "TecTrackTRv2::project2PC: Can't find Pc3Cluster." << endl;}

// Loop over Pc3 clusters and find the closest one
  float dist,x0,y0;
  float mindist=999.;
  int closestCl=-1;
  float a=Slope; 
  float b=Intercept; 

  for(unsigned int i=0; i<pc3->get_PadNCluster(); i++) {

    float Xpc = pc3->get_xyz(i,0);
    float Ypc = pc3->get_xyz(i,1);
    float Zpc = pc3->get_xyz(i,2);
// look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
        x0=Xpc; y0=Ypc;
        dist = (y0-a*x0-b)/(sqrt(a*a+1.0));
        if(fabs(dist)<fabs(mindist)) { 
           mindist = dist; 
           closestCl=i; 
        }
      }

  } // end i loop

  Pc3Pointer = closestCl;
  Pc3Distance = mindist;

  return;
}

//---------------------------------------------------------

void TecTrackTRv2::project2InvertedPC(PHCompositeNode* topNode) {

  PadCluster* pc3 = 0;
  PHTypedNodeIterator<PadCluster> paditer(topNode);
  PHIODataNode<PadCluster> *Pc3Node = paditer.find("Pc3Cluster");
  if(Pc3Node) { pc3 = (PadCluster*)Pc3Node->getData(); }
    else {cerr << "TecTrackTRv2::project2PC: Can't find Pc3Cluster." << endl;}

// Loop over Pc3 clusters and find the closest one
  float dist,x0,y0;
  float mindist=999.;
  int closestCl=-1;
  float a=Slope;
  float b=Intercept;

  for(unsigned int i=0; i<pc3->get_PadNCluster(); i++) {

    float Xpc = pc3->get_xyz(i,0);
    float Ypc = pc3->get_xyz(i,1);
    float Zpc = pc3->get_xyz(i,2);
// look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]>0) || (Zpc>0 && XYZin[2]<0))) { // invert Tec
        x0=Xpc; y0=Ypc;
        dist = (y0-a*x0-b)/(sqrt(a*a+1.0));
        if(fabs(dist)<fabs(mindist)) {
           mindist = dist;
           closestCl=i;
        }
      }

  } // end i loop

  Pc3sPointer = closestCl;
  Pc3sDistance = mindist;

  return;
}

//---------------------------------------------------------

int TecTrackTRv2::getNFwires() const {
    int nfwires=0;
    for(int i=0; i<6; i++) { nfwires+=Nwires[i]; }
  return nfwires;
}

//---------------------------------------------------------

int TecTrackTRv2::getNFplanes() const {
    int nfplanes=0;
    for(int i=0; i<6; i++) { if(Nwires[i]>0) { nfplanes++; } }
  return nfplanes;
}


