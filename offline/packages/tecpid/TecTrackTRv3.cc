#include "TecTrackTRv3.hh"
#include "TecTrackV2.hh"
#include "PadCluster.h"
#include "PHGeometry.h"
#include <cmath>
#include <iostream>

ClassImp(TecTrackTRv3)

using namespace std;

//---------------------------------------------------------

TecTrackTRv3::TecTrackTRv3() { 
//  cout << "Use TecTrackTR::TecTrackTR(float in[3], float out[3]) constructor." << endl;
}

//---------------------------------------------------------

void TecTrackTRv3::Initialize(float in[3], float out[3]) {

using namespace PHGeometry;
 Sector = -1;
 Side = -1;
 if(in[2]<0. && out[2]<0.) {Side=0;}
 if(in[2]>0. && out[2]>0.) {Side=1;}
 memset(Nhits20, 0, sizeof(Nhits20));
 memset(Nhits100, 0, sizeof(Nhits100));
 memset(Nwires, 0, sizeof(Nwires));
 memset(NTR, 0, sizeof(NTR));
 memset(DE, 0, sizeof(DE));
 memset(TR, 0, sizeof(TR));
 memset(NdEdXbins, 0, sizeof(NdEdXbins));
 memset(dEdX06, 0, sizeof(dEdX06));
 memset(WeightedTimeBin, 0, sizeof(WeightedTimeBin));
 memset(dEdX06, 0, sizeof(dEdX06));
 memset(WeightedTimeBin, 0, sizeof(WeightedTimeBin));
 TrackLength = 0.;
 hitLikelihood = -9999.;
 
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
   float Slope = (yin-yout)/(xin-xout);
   Alpha = -(Phi+atan(Slope));
   Phi = Pi - Phi;
   if(fabs(Alpha)<0.002) { tecMomentum=999.; } else {
     tecMomentum = 1./(21.5*fabs(Alpha));
   }
 }
 else {
   Alpha = Pi/2.; Phi = 2.*Pi; 
   tecMomentum = 0.;
 }
 
  return;
}

//---------------------------------------------------------

TecTrackTRv3::TecTrackTRv3(float in[3], float out[3]) {
 Initialize(in, out);
}

//---------------------------------------------------------

TecTrackTRv3::TecTrackTRv3(const TecTrack &source) {
  hitLikelihood = 0.;
  Phi = source.Phi;
  Alpha = source.Alpha;
  memset(dEdX06, 0, sizeof(dEdX06));
  memset(DE, 0, sizeof(DE));
  memset(WeightedTimeBin, 0, sizeof(WeightedTimeBin));
  memset(Nhits20, 0, sizeof(Nhits20));
  memset(Nhits100, 0, sizeof(Nhits100));
  memset(NTR, 0, sizeof(NTR));
  memset(TR, 0, sizeof(TR));
  memset(NdEdXbins, 0, sizeof(NdEdXbins));
  for (int k=0; k<6; k++)
    NHITS[k] = source.NHITS[k];
  tecMomentum = source.tecMomentum;
  Sector = source.Sector;
  Side = source.Side;
  TrackLength = source.TrackLength;
}

TecTrackTRv3& TecTrackTRv3::operator=(const TecTrackV2 &source) {
   hitLikelihood = source.getLikelihood();
   Phi = source.getPhi();
   Alpha = source.getAlpha();
   tecMomentum = source.gettecMomentum();
   Sector = source.getSector();
   Side = source.getSide();
   TrackLength = source.getTrackLength();  
   for (int k=0; k<6; k++)
     {
       dEdX06[k] = source.getdEdX06(k);
       DE[k] = source.getDE(k);
       WeightedTimeBin[k] = source.getWeightedTimeBin(k);
       Nhits20[k] = source.getNhits20(k);
       Nhits100[k] = source.getNhits100(k);
       NdEdXbins[k] = source.getNdEdXbins(k);
       NTR[k] = source.getNTR(k);
       TR[k] = source.getTR(k);
       Nwires[k] = source.getNwires(k);
       NHITS[k] = source.getNHITS(k);
     }
   return *this;
 }

TecTrackTRv3& TecTrackTRv3::operator=(const TecTrackTRv3 &source) {
   hitLikelihood = source.getLikelihood();
   Phi = source.getPhi();
   Alpha = source.getAlpha();
   tecMomentum = source.gettecMomentum();
   Sector = source.getSector();
   Side = source.getSide();
   TrackLength = source.getTrackLength();  
   for (int k=0; k<6; k++)
     {
       dEdX06[k] = source.getdEdX06(k);
       DE[k] = source.getDE(k);
       WeightedTimeBin[k] = source.getWeightedTimeBin(k);
       Nhits20[k] = source.getNhits20(k);
       Nhits100[k] = source.getNhits100(k);
       NdEdXbins[k] = source.getNdEdXbins(k);
       NTR[k] = source.getNTR(k);
       TR[k] = source.getTR(k);
       Nwires[k] = source.getNwires(k);
       NHITS[k] = source.getNHITS(k);
     }
   return *this;
 }


//---------------------------------------------------------

// Methods for track coordinate error determination.
// Functional form comes from a fit to space resolution as a function of 
// incidence angle for momenta from 0.5 to 4.0 GeV/c in simulation.
// 1.5 comes from comparison of simulation with gas gain 3000 and the data (run00)

// void TecTrackTRv3::calculateXinError() {

//   int sector = Sector;
//   if(sector<0 || sector>3) sector=1;
//   float SectorCenter=(TECPHIBOTE-(2*sector+1)*(TECPHIBOTE-TECPHITOPE)/8.)/180.*Pi;

//     float angle=Alpha-(Phi-SectorCenter);
//     float spres=0;
//     int nfp=getNFplanes();
//     if(nfp<=3)
//      {
//      spres = TECSPRESP0_3p + TECSPRESP1_3p*exp(-angle*angle*TECSPRESP2_3p);
//      }
//     if(nfp==4)
//      {
//      spres = TECSPRESP0_4p + TECSPRESP1_4p*exp(-angle*angle*TECSPRESP2_4p);
//      }
//     if(nfp==5)
//      {
//      spres = TECSPRESP0_5p + TECSPRESP1_5p*exp(-angle*angle*TECSPRESP2_5p);
//      }
//     if(nfp==6)
//      {
//      spres = TECSPRESP0_6p + TECSPRESP1_6p*exp(-angle*angle*TECSPRESP2_6p);
//      }

//     float erx = fabs(spres*sin(Phi));

//   XYZinError[0] = erx;

// }

// //---------------------------------------------------------

// void TecTrackTRv3::calculateYinError() {

//   int sector=Sector;
//   if(sector<0 || sector>3) sector=1;
//   float SectorCenter=(TECPHIBOTE-(2*sector+1)*(TECPHIBOTE-TECPHITOPE)/8.)/180.*Pi;

//     float angle=Alpha-(Phi-SectorCenter);
//     float spres=0;
//     int nfp=getNFplanes();
//     if(nfp<=3)
//      {
//      spres = TECSPRESP0_3p + TECSPRESP1_3p*exp(-angle*angle*TECSPRESP2_3p);
//      }
//     if(nfp==4)
//      {
//      spres = TECSPRESP0_4p + TECSPRESP1_4p*exp(-angle*angle*TECSPRESP2_4p);
//      }
//     if(nfp==5)
//      {
//      spres = TECSPRESP0_5p + TECSPRESP1_5p*exp(-angle*angle*TECSPRESP2_5p);
//      }
//     if(nfp==6)
//      {
//      spres = TECSPRESP0_6p + TECSPRESP1_6p*exp(-angle*angle*TECSPRESP2_6p);
//      }

//     float ery = fabs(spres*cos(Phi));
    
//   XYZinError[1] = ery;

// }

// //---------------------------------------------------------

// void TecTrackTRv3::project2PC(PHCompositeNode* topNode) {

//   PadCluster* pc3 = 0;
//   PHTypedNodeIterator<PadCluster> paditer(topNode);
//   PHIODataNode<PadCluster> *Pc3Node = paditer.find("Pc3Cluster");
//   if(Pc3Node) { pc3 = (PadCluster*)Pc3Node->getData(); }
//     else {cerr << "TecTrackTRv3::project2PC: Can't find Pc3Cluster." << endl;}

// // Loop over Pc3 clusters and find the closest one
//   float dist,x0,y0;
//   float mindist=999.;
//   int closestCl=-1;
//   float a=Slope; 
//   float b=Intercept; 

//   for(unsigned int i=0; i<pc3->get_PadNCluster(); i++) {

//     float Xpc = pc3->get_xyz(i,0);
//     float Ypc = pc3->get_xyz(i,1);
//     float Zpc = pc3->get_xyz(i,2);
// // look at east arm only
//       if(Xpc<0 &&
//         ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
//         x0=Xpc; y0=Ypc;
//         dist = (y0-a*x0-b)/(sqrt(a*a+1.0));
//         if(fabs(dist)<fabs(mindist)) { 
//            mindist = dist; 
//            closestCl=i; 
//         }
//       }

//   } // end i loop

//   Pc3Pointer = closestCl;
//   Pc3Distance = mindist;

//   return;
// }

// //---------------------------------------------------------

// void TecTrackTRv3::project2InvertedPC(PHCompositeNode* topNode) {

//   PadCluster* pc3 = 0;
//   PHTypedNodeIterator<PadCluster> paditer(topNode);
//   PHIODataNode<PadCluster> *Pc3Node = paditer.find("Pc3Cluster");
//   if(Pc3Node) { pc3 = (PadCluster*)Pc3Node->getData(); }
//     else {cerr << "TecTrackTRv3::project2PC: Can't find Pc3Cluster." << endl;}

// // Loop over Pc3 clusters and find the closest one
//   float dist,x0,y0;
//   float mindist=999.;
//   int closestCl=-1;
//   float a=Slope;
//   float b=Intercept;

//   for(unsigned int i=0; i<pc3->get_PadNCluster(); i++) {

//     float Xpc = pc3->get_xyz(i,0);
//     float Ypc = pc3->get_xyz(i,1);
//     float Zpc = pc3->get_xyz(i,2);
// // look at east arm only
//       if(Xpc<0 &&
//         ((Zpc<0 && XYZin[2]>0) || (Zpc>0 && XYZin[2]<0))) { // invert Tec
//         x0=Xpc; y0=Ypc;
//         dist = (y0-a*x0-b)/(sqrt(a*a+1.0));
//         if(fabs(dist)<fabs(mindist)) {
//            mindist = dist;
//            closestCl=i;
//         }
//       }

//   } // end i loop

//   Pc3sPointer = closestCl;
//   Pc3sDistance = mindist;

//   return;
// }

//---------------------------------------------------------

int TecTrackTRv3::getNFwires() const {
    int nfwires=0;
    for(int i=0; i<6; i++) { nfwires+=Nwires[i]; }
  return nfwires;
}

//---------------------------------------------------------

int TecTrackTRv3::getNFplanes() const {
    int nfplanes=0;
    for(int i=0; i<6; i++) { if(Nwires[i]>0) { nfplanes++; } }
  return nfplanes;
}

int TecTrackTRv3::getNhits() const {
  int totnhits=0;
  for(int k=0; k<6; k++)
    totnhits += NHITS[k];
  return totnhits;
}

int TecTrackTRv3::getNhits20() const {
  int totnhits=0;
  for(int k=0; k<6; k++)
    totnhits += Nhits20[k];
  return totnhits;
}

int TecTrackTRv3::getNhits100() const {
  int totnhits=0;
  for(int k=0; k<6; k++)
    totnhits += Nhits100[k];
  return totnhits;
}

int TecTrackTRv3::getNTR() const {
  int totnhits=0;
  for(int k=0; k<6; k++)
    totnhits += NTR[k];
  return totnhits;
}

float TecTrackTRv3::getTR() const {
  float tot=0;
  for(int k=0; k<6; k++)
    tot += TR[k];
  return tot;
}

float TecTrackTRv3::getDE() const {
  float tot=0;
  for(int k=0; k<6; k++)
    tot += DE[k];
  return tot;
}

float TecTrackTRv3::getdEdX() const {
  return getDE();
}

float TecTrackTRv3::getWeightedTimeBin() const {
  float tot=0;
  for(int k=0; k<6; k++)
    tot += WeightedTimeBin[k];
  return tot;
}

float TecTrackTRv3::getdEdX06() const {
  float tot=0;
  for(int k=0; k<6; k++)
    tot += dEdX06[k];
  return tot;
}

void TecTrackTRv3::setAlpha(float a)
  {
    Alpha = a;
    if (fabs(Alpha) < 0.002)
      {
        tecMomentum = 999.;
      }
    else
      {
        tecMomentum = 1. / (21.5 * fabs(Alpha));
      }
    return ;
  }
