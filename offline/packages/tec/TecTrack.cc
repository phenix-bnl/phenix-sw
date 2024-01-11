#include <TecTrack.hh>
#include <TecBasicObject.hh>

#include <dPadClusterWrapper.h>
#include <dTofReconstructedWrapper.h>
#include <dEmcClusterLocalExtWrapper.h>
#include <iostream>

ClassImp(TecTrack)

using namespace std;
using namespace PHGeometry;

typedef PHIODataNode<dPadClusterWrapper> dPadClusterNode_t;
typedef PHIODataNode<dTofReconstructedWrapper> dTofReconstructedNode_t;
typedef PHIODataNode<dEmcClusterLocalExtWrapper> dEmcClusterLocalExtNode_t;


//---------------------------------------------------------

TecTrack::TecTrack(const TecTrack &source) { 

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
  Pc1pointer[0] = source.Pc1pointer[0];
  Pc1pointer[1] = source.Pc1pointer[1];
  Pc1pointer[2] = source.Pc1pointer[2];
  Pc3pointer[0] = source.Pc3pointer[0];
  Pc3pointer[1] = source.Pc3pointer[1];
  Pc3pointer[2] = source.Pc3pointer[2];
  Pc3distance[0] = source.Pc3distance[0];
  Pc3distance[1] = source.Pc3distance[1];
  Pc3distance[2] = source.Pc3distance[2];
  Pc1distance[0] = source.Pc1distance[0];
  Pc1distance[1] = source.Pc1distance[1];
  Pc1distance[2] = source.Pc1distance[2];
  TOFpointer[0] = source.TOFpointer[0];
  TOFpointer[1] = source.TOFpointer[1];
  TOFpointer[2] = source.TOFpointer[2];
  TOFdistance[0] = source.TOFdistance[0];
  TOFdistance[1] = source.TOFdistance[1];
  TOFdistance[2] = source.TOFdistance[2];
  EMCpointer[0] = source.EMCpointer[0];
  EMCpointer[1] = source.EMCpointer[1];
  EMCpointer[2] = source.EMCpointer[2];
  EMCdistance[0] = source.EMCdistance[0];
  EMCdistance[1] = source.EMCdistance[1];
  EMCdistance[2] = source.EMCdistance[2];
  EemcMin = source.EemcMin;
  Pc3Cut = source.Pc3Cut;
  Pc3EmcZCut = source.Pc3EmcZCut;
  Index = source.Index;
  Sector = source.Sector;
  Side = source.Side;
  dEdX = source.dEdX;
  TrackLength = source.TrackLength;
  NdEdXbins = source.NdEdXbins;
  for(int k=0; k<6; k++) Nwires[k] = source.Nwires[k];
  for(int k=0; k<6; k++) NHITS[k] = source.NHITS[k];
}

TecTrack& TecTrack::operator=(const TecTrack &source) {

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
  Pc1pointer[0] = source.Pc1pointer[0];
  Pc1pointer[1] = source.Pc1pointer[1];
  Pc1pointer[2] = source.Pc1pointer[2];
  Pc3pointer[0] = source.Pc3pointer[0];
  Pc3pointer[1] = source.Pc3pointer[1];
  Pc3pointer[2] = source.Pc3pointer[2];
  Pc3distance[0] = source.Pc3distance[0];
  Pc3distance[1] = source.Pc3distance[1];
  Pc3distance[2] = source.Pc3distance[2];
  Pc1distance[0] = source.Pc1distance[0];
  Pc1distance[1] = source.Pc1distance[1];
  Pc1distance[2] = source.Pc1distance[2];
  TOFpointer[0] = source.TOFpointer[0];
  TOFpointer[1] = source.TOFpointer[1];
  TOFpointer[2] = source.TOFpointer[2];
  TOFdistance[0] = source.TOFdistance[0];
  TOFdistance[1] = source.TOFdistance[1];
  TOFdistance[2] = source.TOFdistance[2];
  EMCpointer[0] = source.EMCpointer[0];
  EMCpointer[1] = source.EMCpointer[1];
  EMCpointer[2] = source.EMCpointer[2];
  EMCdistance[0] = source.EMCdistance[0];
  EMCdistance[1] = source.EMCdistance[1];
  EMCdistance[2] = source.EMCdistance[2];
  EemcMin = source.EemcMin;
  Pc3Cut = source.Pc3Cut;
  Pc3EmcZCut = source.Pc3EmcZCut;
  Index = source.Index;
  Sector = source.Sector;
  Side = source.Side;
  dEdX = source.dEdX;
  TrackLength = source.TrackLength;
  NdEdXbins = source.NdEdXbins;
  for(int k=0; k<6; k++) Nwires[k] = source.Nwires[k];
  for(int k=0; k<6; k++) NHITS[k] = source.NHITS[k];

  return *this;

}

void TecTrack::Initialize(float in[3], float out[3]) {

 Nhits=-1;
 XYZin[0]=in[0];
 XYZin[1]=in[1];
 XYZin[2]=in[2];
 XYZout[0]=out[0];
 XYZout[1]=out[1];
 XYZout[2]=out[2];
 Pc1pointer[0]=-1;
 Pc1pointer[1]=-1;
 Pc1pointer[2]=-1;
 Pc3pointer[0]=-1;
 Pc3pointer[1]=-1;
 Pc3pointer[2]=-1;
 Pc1distance[0]=999.;
 Pc1distance[1]=999.;
 Pc1distance[2]=999.;
 Pc3distance[0]=999.;
 Pc3distance[1]=999.;
 Pc3distance[2]=999.;
 TOFpointer[0]=-1;
 TOFpointer[1]=-1;
 TOFpointer[2]=-1;
 TOFdistance[0]=999.;
 TOFdistance[1]=999.;
 TOFdistance[2]=999.;
 EMCpointer[0]=-1;
 EMCpointer[1]=-1;
 EMCpointer[2]=-1;
 EMCdistance[0]=999.;
 EMCdistance[1]=999.;
 EMCdistance[2]=999.;
 EemcMin=0.1;
 Pc3Cut=3.0;
 Pc3EmcZCut=10.0;
 Index = -1;
 Sector = -1;
 Side = -1;
 for(int k=0; k<6; k++) Nwires[k] = -1;
 for(int k=0; k<6; k++) NHITS[k] = -1;
 dEdX = 0.;
 TrackLength = 0.;
 NdEdXbins = 0;

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

}

TecTrack::TecTrack(float in[3], float out[3]) {

 Initialize(in, out);

}

TecTrack::TecTrack(float in[3], float out[3], int nhits,
                   int sector, int side, 
                   int nwires[6], int nh[6]) {

 Initialize(in, out);

 Nhits=nhits;
 Sector = sector;
 Side = side;
 Index = sector*TECMAXSIDE+side;
 for(int k=0; k<6; k++) Nwires[k] = nwires[k];
 for(int k=0; k<6; k++) NHITS[k] = nh[k];

}

void TecTrack::identify(ostream& out) const {
  out << "I am a TecTrack object." << endl;
}

//========================================================================

// Methods for track coordinate error determination.
// Functional form comes from a fit to space resolution as a function of 
// incidence angle for momenta from 0.5 to 4.0 GeV/c in simulation.
// 1.5 comes from comparison of simulation with gas gain 3000 and the data (run00)

float TecTrack::getXinError() {

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

  return erx;

}

float TecTrack::getYinError() {

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
  return ery;

}

float TecTrack::getXoutError() {
  return getXinError();
}

float TecTrack::getYoutError() {
  return getYinError();
}

//========================================================================

PHBoolean TecTrack::project2PC(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dPadClusterNode_t* PC3 = static_cast<dPadClusterNode_t*>(iii.findFirst("PHIODataNode", "dPc3Cluster"));
  if (!PC3) {
    cerr << "project2PC -> ERROR: dPc3Cluster table not found." << endl;
    return False;
  }
  dPadClusterWrapper* dPc3Cluster = PC3->getData();

  dPadClusterNode_t* PC1 = static_cast<dPadClusterNode_t*>(iii.findFirst("PHIODataNode", "dPc1Cluster"));
  if (!PC1) {
    cerr << "project2PC -> ERROR: dPc1Cluster table not found." << endl;
    return False;
  }
  dPadClusterWrapper* dPc1Cluster = PC1->getData();


// Loop over Pc3 clusters and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.; 
  mindist0=999.; mindist1=999.; mindist2=999.; 
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dPc3Cluster->RowCount(); i++) {

    float Xpc = dPc3Cluster->get_xyz(0,i);
    float Ypc = dPc3Cluster->get_xyz(1,i);
    float Zpc = dPc3Cluster->get_xyz(2,i);
// look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
	a=Slope; b=Intercept; x0=Xpc; y0=Ypc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist; 
           mindist2 = mindist1; closestCl2=closestCl1; 
           mindist1 = mindist0; closestCl1=closestCl0; 
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i; 
        }
      }

  } // end i loop

  Pc3pointer[0] = closestCl0;
  Pc3pointer[1] = closestCl1;
  Pc3pointer[2] = closestCl2;
  Pc3distance[0] = mindist0;
  Pc3distance[1] = mindist1;
  Pc3distance[2] = mindist2;

// Loop over Pc1 clusters and find the closest one

  mindist=999.; 
  mindist0=999.; mindist1=999.; mindist2=999.; 
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dPc1Cluster->RowCount(); i++) {

    float Xpc = dPc1Cluster->get_xyz(0,i);
    float Ypc = dPc1Cluster->get_xyz(1,i);
    float Zpc = dPc1Cluster->get_xyz(2,i);
// Pc1 has 8 "sectors"
// Look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xpc; y0=Ypc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist; 
           mindist2 = mindist1; closestCl2=closestCl1; 
           mindist1 = mindist0; closestCl1=closestCl0; 
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i; 
        }
      }

  } // end i loop

  Pc1pointer[0] = closestCl0;
  Pc1pointer[1] = closestCl1;
  Pc1pointer[2] = closestCl2;
  Pc1distance[0] = mindist0;
  Pc1distance[1] = mindist1;
  Pc1distance[2] = mindist2;

  return True;
}

PHBoolean TecTrack::project2ZinvertedPC(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dPadClusterNode_t* PC3 = static_cast<dPadClusterNode_t*>(iii.findFirst("PHIODataNode", "dPc3Cluster"));
  if (!PC3) {
    cerr << "project2PC -> ERROR: dPc3Cluster table not found." << endl;
    return False;
  }
  dPadClusterWrapper* dPc3Cluster = PC3->getData();

  dPadClusterNode_t* PC1 = static_cast<dPadClusterNode_t*>(iii.findFirst("PHIODataNode", "dPc1Cluster"));
  if (!PC1) {
    cerr << "project2PC -> ERROR: dPc1Cluster table not found." << endl;
    return False;
  }
  dPadClusterWrapper* dPc1Cluster = PC1->getData();


// Loop over Pc3 clusters and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dPc3Cluster->RowCount(); i++) {

    float Xpc = dPc3Cluster->get_xyz(0,i);
    float Ypc = dPc3Cluster->get_xyz(1,i);
    float Zpc = -dPc3Cluster->get_xyz(2,i);
// look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xpc; y0=Ypc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  Pc3pointer[0] = closestCl0;
  Pc3pointer[1] = closestCl1;
  Pc3pointer[2] = closestCl2;
  Pc3distance[0] = mindist0;
  Pc3distance[1] = mindist1;
  Pc3distance[2] = mindist2;

// Loop over Pc1 clusters and find the closest one

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dPc1Cluster->RowCount(); i++) {

    float Xpc = dPc1Cluster->get_xyz(0,i);
    float Ypc = dPc1Cluster->get_xyz(1,i);
    float Zpc = -dPc1Cluster->get_xyz(2,i);
// Pc1 has 8 "sectors"
// Look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xpc; y0=Ypc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  Pc1pointer[0] = closestCl0;
  Pc1pointer[1] = closestCl1;
  Pc1pointer[2] = closestCl2;
  Pc1distance[0] = mindist0;
  Pc1distance[1] = mindist1;
  Pc1distance[2] = mindist2;

  return True;

}

PHBoolean TecTrack::project2YinvertedPC(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dPadClusterNode_t* PC3 = static_cast<dPadClusterNode_t*>(iii.findFirst("PHIODataNode", "dPc3Cluster"));
  if (!PC3) {
    cerr << "project2PC -> ERROR: dPc3Cluster table not found." << endl;
    return False;
  }
  dPadClusterWrapper* dPc3Cluster = PC3->getData();

  dPadClusterNode_t* PC1 = static_cast<dPadClusterNode_t*>(iii.findFirst("PHIODataNode", "dPc1Cluster"));
  if (!PC1) {
    cerr << "project2PC -> ERROR: dPc1Cluster table not found." << endl;
    return False;
  }
  dPadClusterWrapper* dPc1Cluster = PC1->getData();


// Loop over Pc3 clusters and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dPc3Cluster->RowCount(); i++) {

    float Xpc = dPc3Cluster->get_xyz(0,i);
    float Ypc = -dPc3Cluster->get_xyz(1,i);
    float Zpc = dPc3Cluster->get_xyz(2,i);
// look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xpc; y0=Ypc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  Pc3pointer[0] = closestCl0;
  Pc3pointer[1] = closestCl1;
  Pc3pointer[2] = closestCl2;
  Pc3distance[0] = mindist0;
  Pc3distance[1] = mindist1;
  Pc3distance[2] = mindist2;

// Loop over Pc1 clusters and find the closest one

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dPc1Cluster->RowCount(); i++) {

    float Xpc = dPc1Cluster->get_xyz(0,i);
    float Ypc = -dPc1Cluster->get_xyz(1,i);
    float Zpc = dPc1Cluster->get_xyz(2,i);
// Pc1 has 8 "sectors"
// Look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xpc; y0=Ypc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  Pc1pointer[0] = closestCl0;
  Pc1pointer[1] = closestCl1;
  Pc1pointer[2] = closestCl2;
  Pc1distance[0] = mindist0;
  Pc1distance[1] = mindist1;
  Pc1distance[2] = mindist2;

  return True;

}

PHBoolean TecTrack::project2EMC(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dEmcClusterLocalExtNode_t* EMC = static_cast<dEmcClusterLocalExtNode_t*>(iii.findFirst("PHIODataNode", "dEmcClusterLocalExt"));
  if (!EMC) {
    cerr << "project2EMC -> ERROR: dEmcClusterLocalExt table not found." << endl;
    return False;
  }
  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = EMC->getData();

// Loop over EMC clusters and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dEmcClusterLocalExt->RowCount(); i++) {

    float Xemc = dEmcClusterLocalExt->get_xyz(0,i);
    float Yemc = dEmcClusterLocalExt->get_xyz(1,i);
    float Zemc = dEmcClusterLocalExt->get_xyz(2,i);
    float Eemc = dEmcClusterLocalExt->get_e(i);
    int emcsector = dEmcClusterLocalExt->get_sector(i);
    int emcarm = dEmcClusterLocalExt->get_arm(i);
// PbGl sector 1 only
      if(emcarm==1 && emcsector==1 && Eemc > EemcMin) {
      if(((Zemc<0 && XYZin[2]<0) || (Zemc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xemc; y0=Yemc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }
      }

  } // end i loop

  EMCpointer[0] = closestCl0;
  EMCpointer[1] = closestCl1;
  EMCpointer[2] = closestCl2;
  EMCdistance[0] = mindist0;
  EMCdistance[1] = mindist1;
  EMCdistance[2] = mindist2;

  return True;
}

PHBoolean TecTrack::project2Pc3EMC(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dEmcClusterLocalExtNode_t* EMC = static_cast<dEmcClusterLocalExtNode_t*>(iii.findFirst("PHIODataNode", "dEmcClusterLocalExt"));
  if (!EMC) {
    cerr << "project2EMC -> ERROR: dEmcClusterLocalExt table not found." << endl;
    return False;
  }
  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = EMC->getData();

  dPadClusterNode_t* PC3 = static_cast<dPadClusterNode_t*>(iii.findFirst("PHIODataNode", "dPc3Cluster"));
  if (!PC3) {
    cerr << "project2PC -> ERROR: dPc3Cluster table not found." << endl;
    return False;
  }
  dPadClusterWrapper* dPc3Cluster = PC3->getData();

// Loop over Pc3 clusters and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dPc3Cluster->RowCount(); i++) {

    float Xpc = dPc3Cluster->get_xyz(0,i);
    float Ypc = dPc3Cluster->get_xyz(1,i);
    float Zpc = dPc3Cluster->get_xyz(2,i);
// look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xpc; y0=Ypc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  float trackZ;
  if(mindist0>-Pc3Cut && mindist0<Pc3Cut && closestCl0>-1) {
    trackZ = dPc3Cluster->get_xyz(2,closestCl0); 
  } 
  else {
    trackZ = -999.;
  }

// Loop over EMC clusters and find the closest one

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dEmcClusterLocalExt->RowCount(); i++) {

    float Xemc = dEmcClusterLocalExt->get_xyz(0,i);
    float Yemc = dEmcClusterLocalExt->get_xyz(1,i);
    float Zemc = dEmcClusterLocalExt->get_xyz(2,i);
    float Eemc = dEmcClusterLocalExt->get_e(i);
    int emcsector = dEmcClusterLocalExt->get_sector(i);
    int emcarm = dEmcClusterLocalExt->get_arm(i);
// PbGl sector 1 only
      if(emcarm==1 && emcsector==1 && Eemc > EemcMin) {
      if(((Zemc<0 && XYZin[2]<0) || (Zemc>0 && XYZin[2]>0))) {
      if(trackZ>-999. && fabs(trackZ-Zemc)<Pc3EmcZCut) {
        a=Slope; b=Intercept; x0=Xemc; y0=Yemc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }
      }
      }

  } // end i loop

  EMCpointer[0] = closestCl0;
  EMCpointer[1] = closestCl1;
  EMCpointer[2] = closestCl2;
  EMCdistance[0] = mindist0;
  EMCdistance[1] = mindist1;
  EMCdistance[2] = mindist2;

  return True;
}

PHBoolean TecTrack::project2Pc3ZinvertedEMC(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dEmcClusterLocalExtNode_t* EMC = static_cast<dEmcClusterLocalExtNode_t*>(iii.findFirst("PHIODataNode", "dEmcClusterLocalExt"));
  if (!EMC) {
    cerr << "project2EMC -> ERROR: dEmcClusterLocalExt table not found." << endl;
    return False;
  }
  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = EMC->getData();

  dPadClusterNode_t* PC3 = static_cast<dPadClusterNode_t*>(iii.findFirst("PHIODataNode", "dPc3Cluster"));
  if (!PC3) {
    cerr << "project2PC -> ERROR: dPc3Cluster table not found." << endl;
    return False;
  }
  dPadClusterWrapper* dPc3Cluster = PC3->getData();

// Loop over Pc3 clusters and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dPc3Cluster->RowCount(); i++) {

    float Xpc = dPc3Cluster->get_xyz(0,i);
    float Ypc = dPc3Cluster->get_xyz(1,i);
    float Zpc = dPc3Cluster->get_xyz(2,i);
// look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xpc; y0=Ypc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  float trackZ;
  if(mindist0>-Pc3Cut && mindist0<Pc3Cut && closestCl0>-1) {
    trackZ = dPc3Cluster->get_xyz(2,closestCl0);
  }
  else {
    trackZ = -999.;
  }

// Loop over EMC clusters and find the closest one

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dEmcClusterLocalExt->RowCount(); i++) {

    float Xemc = dEmcClusterLocalExt->get_xyz(0,i);
    float Yemc = dEmcClusterLocalExt->get_xyz(1,i);
    float Zemc = -dEmcClusterLocalExt->get_xyz(2,i);	// invert EMCal Z
    float Eemc = dEmcClusterLocalExt->get_e(i);
    int emcsector = dEmcClusterLocalExt->get_sector(i);
    int emcarm = dEmcClusterLocalExt->get_arm(i);
// PbGl sector 1 only
      if(emcarm==1 && emcsector==1 && Eemc > EemcMin) {
      if(((Zemc<0 && XYZin[2]<0) || (Zemc>0 && XYZin[2]>0))) {
      if(trackZ>-999. && fabs(trackZ-Zemc)<Pc3EmcZCut) {
        a=Slope; b=Intercept; x0=Xemc; y0=Yemc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }
      }
      }

  } // end i loop

  EMCpointer[0] = closestCl0;
  EMCpointer[1] = closestCl1;
  EMCpointer[2] = closestCl2;
  EMCdistance[0] = mindist0;
  EMCdistance[1] = mindist1;
  EMCdistance[2] = mindist2;

  return True;
}

PHBoolean TecTrack::project2ZinvertedEMC(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dEmcClusterLocalExtNode_t* EMC = static_cast<dEmcClusterLocalExtNode_t*>(iii.findFirst("PHIODataNode", "dEmcClusterLocalExt"));
  if (!EMC) {
    cerr << "project2EMC -> ERROR: dEmcClusterLocalExt table not found." << endl;
    return False;
  }
  dEmcClusterLocalExtWrapper* dEmcClusterLocalExt = EMC->getData();

// Loop over EMC clusters and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dEmcClusterLocalExt->RowCount(); i++) {

    float Xemc = dEmcClusterLocalExt->get_xyz(0,i);
    float Yemc = dEmcClusterLocalExt->get_xyz(1,i);
    float Zemc = -dEmcClusterLocalExt->get_xyz(2,i); // invert Z EMC
    float Eemc = dEmcClusterLocalExt->get_e(i);
    int emcsector = dEmcClusterLocalExt->get_sector(i);
    int emcarm = dEmcClusterLocalExt->get_arm(i);
// PbGl sector 1 only
      if(emcarm==1 && emcsector==1 && Eemc > EemcMin) {
      if(((Zemc<0 && XYZin[2]<0) || (Zemc>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xemc; y0=Yemc;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }
      }

  } // end i loop

  EMCpointer[0] = closestCl0;
  EMCpointer[1] = closestCl1;
  EMCpointer[2] = closestCl2;
  EMCdistance[0] = mindist0;
  EMCdistance[1] = mindist1;
  EMCdistance[2] = mindist2;

  return True;
}

PHBoolean TecTrack::project2TOF(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dTofReconstructedNode_t* TOF = static_cast<dTofReconstructedNode_t*>(iii.findFirst("PHIODataNode", "dTofReconstructed"));
  if (!TOF) {
    cerr << "project2TOF -> ERROR: dTofReconstructed table not found." << endl;
    return False;
  }
  dTofReconstructedWrapper* dTofReconstructed = TOF->getData();

// Loop over Tof hits and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dTofReconstructed->RowCount(); i++) {

    float Xtof = dTofReconstructed->get_xtof(0,i);
    float Ytof = dTofReconstructed->get_xtof(1,i);
    float Ztof = dTofReconstructed->get_xtof(2,i);
      if(((Ztof<0 && XYZin[2]<0) || (Ztof>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xtof; y0=Ytof;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  TOFpointer[0] = closestCl0;
  TOFpointer[1] = closestCl1;
  TOFpointer[2] = closestCl2;
  TOFdistance[0] = mindist0;
  TOFdistance[1] = mindist1;
  TOFdistance[2] = mindist2;

  return True;
}

PHBoolean TecTrack::project2YinvertedTOF(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dTofReconstructedNode_t* TOF = static_cast<dTofReconstructedNode_t*>(iii.findFirst("PHIODataNode", "dTofReconstructed"));
  if (!TOF) {
    cerr << "project2TOF -> ERROR: dTofReconstructed table not found." << endl;
    return False;
  }
  dTofReconstructedWrapper* dTofReconstructed = TOF->getData();

// Loop over Tof hits and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dTofReconstructed->RowCount(); i++) {

    float Xtof = dTofReconstructed->get_xtof(0,i);
    float Ytof = -dTofReconstructed->get_xtof(1,i);
    float Ztof = dTofReconstructed->get_xtof(2,i);
      if(((Ztof<0 && XYZin[2]<0) || (Ztof>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xtof; y0=Ytof;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  TOFpointer[0] = closestCl0;
  TOFpointer[1] = closestCl1;
  TOFpointer[2] = closestCl2;
  TOFdistance[0] = mindist0;
  TOFdistance[1] = mindist1;
  TOFdistance[2] = mindist2;

  return True;
}

PHBoolean TecTrack::project2ZinvertedTOF(PHCompositeNode* topNode) {

  PHNodeIterator iii(topNode);

  dTofReconstructedNode_t* TOF = static_cast<dTofReconstructedNode_t*>(iii.findFirst("PHIODataNode", "dTofReconstructed"));
  if (!TOF) {
    cerr << "project2TOF -> ERROR: dTofReconstructed table not found." << endl;
    return False;
  }
  dTofReconstructedWrapper* dTofReconstructed = TOF->getData();

// Loop over Tof hits and find the closest one
  float dist,a,b,x0,y0,mindist,mindist0,mindist1,mindist2;
  int closestCl0,closestCl1,closestCl2;

  mindist=999.;
  mindist0=999.; mindist1=999.; mindist2=999.;
  closestCl0=-1; closestCl1=-1; closestCl2=-1;
  for(unsigned int i=0; i<dTofReconstructed->RowCount(); i++) {

    float Xtof = dTofReconstructed->get_xtof(0,i);
    float Ytof = dTofReconstructed->get_xtof(1,i);
    float Ztof = -dTofReconstructed->get_xtof(2,i);
      if(((Ztof<0 && XYZin[2]<0) || (Ztof>0 && XYZin[2]>0))) {
        a=Slope; b=Intercept; x0=Xtof; y0=Ytof;
        dist = (fabs)((y0-a*x0-b)/(sqrt(a*a+1.0)));
        if(dist<mindist) { mindist = dist;
           mindist2 = mindist1; closestCl2=closestCl1;
           mindist1 = mindist0; closestCl1=closestCl0;
           mindist0 = (y0-a*x0-b)/(sqrt(a*a+1.0)); closestCl0=i;
        }
      }

  } // end i loop

  TOFpointer[0] = closestCl0;
  TOFpointer[1] = closestCl1;
  TOFpointer[2] = closestCl2;
  TOFdistance[0] = mindist0;
  TOFdistance[1] = mindist1;
  TOFdistance[2] = mindist2;

  return True;
}

int TecTrack::getNFwires() {

    int nfwires=0;
    for(int i=0; i<6; i++) { nfwires+=Nwires[i]; }

      return nfwires;
}

int TecTrack::getNFplanes() {

    int nfplanes=0;
    for(int i=0; i<6; i++) { if(Nwires[i]>0) { nfplanes++; } }

      return nfplanes;
}


