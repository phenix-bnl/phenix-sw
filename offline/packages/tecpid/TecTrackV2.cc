#include "TecTrackV2.hh"
#include "PadCluster.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "CrkGeometryObject.hh"
#include "dCrkHitWrapper.h"
#include "BbcOut.h"
#include "PHGeometry.h"
#include "getClass.h"
#include <cmath>
#include <iostream>

ClassImp(TecTrackV2)

using namespace std;
//---------------------------------------------------------

TecTrackV2::TecTrackV2() { 
//  cout << "Use TecTrackTR::TecTrackTR(float in[3], float out[3]) constructor." << endl;
}

//---------------------------------------------------------

void TecTrackV2::Initialize(float in[3], float out[3]) {

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
 Pc3Z = -999.;
 Pc3sPointer=-1;
 Pc3sDistance=999.;
 EmcPointer = -1;
 EmcDistance = -999.;
 EmcEcore = -999.;
 EmcZ = -999.;
 EmcsPointer = -1;
 EmcsDistance = -999.;
 EmcsEcore = -999;
 Chi2 = 0;
 CrkNpe0 = -1.;
 CrkN0 = -1;
 CrkChi2 = -1.;
 CrksNpe0 = -1.;
 CrksN0 = -1;
 CrksChi2 = -1.;
 CrkDist = 999.;
 CrksDist = 999.;
 Sector = -1;
 Side = -1;
 if(in[2]<0. && out[2]<0.) {Side=0;}
 if(in[2]>0. && out[2]>0.) {Side=1;}
 memset(Nhits20, 0, sizeof(Nhits20));
 memset(Nhits100, 0, sizeof(Nhits100));
 memset(Nwires, 0, sizeof(Nwires));
 memset(NTR, 0, sizeof(NTR));
 memset(NHITS, 0, sizeof(NHITS));
 memset(DE, 0, sizeof(DE));
 memset(TR, 0, sizeof(TR));
 memset(NdEdXbins, 0, sizeof(NdEdXbins));
 memset(dEdX06, 0, sizeof(dEdX06));
 memset(WeightedTimeBin, 0, sizeof(WeightedTimeBin));
 TrackLength = 0.;
 hitLikelihood = 0.;
 
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
      PHCylinder referencetec = PHCylinder(vtx, TECREFERENCERADIUS, beam);
      PHPoint int1;
      PHPoint int2;
      intersectionLineCylinder(ln1, referencetec, int1, int2);
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

TecTrackV2::TecTrackV2(float in[3], float out[3]) {
 Initialize(in, out);
}

//---------------------------------------------------------

TecTrackV2::TecTrackV2(const TecTrack &source) {
  XYZin[0] = source.XYZin[0];
  XYZin[1] = source.XYZin[1];
  XYZin[2] = source.XYZin[2];
  XYZout[0] = source.XYZout[0];
  XYZout[1] = source.XYZout[1];
  XYZout[2] = source.XYZout[2];
  hitLikelihood = 0.;
  Nhits = source.Nhits;
  Phi = source.Phi;
  Alpha = source.Alpha;
  PhiAtDch = source.PhiAtDch;
  AlphaAtDch = source.AlphaAtDch;
  Slope = source.Slope;
  Chi2 = source.EemcMin;  // I'm using this variable like a tmp variable in TecOutV1 
  Intercept = source.Intercept;
  memset(dEdX06, 0, sizeof(dEdX06));
  memset(NdEdXbins, 0, sizeof(NdEdXbins));
  memset(DE, 0, sizeof(DE));
  memset(WeightedTimeBin, 0, sizeof(WeightedTimeBin));
  memset(Nhits20, 0, sizeof(Nhits20));
  memset(Nhits100, 0, sizeof(Nhits100));
  memset(NTR, 0, sizeof(NTR));
  memset(TR, 0, sizeof(TR));
  tecMomentum = source.tecMomentum;
  tecCharge = source.tecCharge;
  Pc3Pointer = source.Pc3pointer[0];
  Pc3Distance = source.Pc3distance[0];
  Pc3Z = -999.;
  Pc3sPointer = -1;
  Pc3sDistance = 999.;
  Pc1Pointer = -1;
  Pc1Distance = -999.;
  Pc1Z = -999.;
  Pc1sPointer = -1;
  Pc1sDistance = 999.;
  EmcPointer = -1;
  EmcDistance = -999.;
  EmcEcore = -999.;
  EmcZ = -999.;
  EmcsPointer = -1;
  EmcsDistance = -999.;
  EmcsEcore = -999;
  CrkNpe0 = -1.;
  CrkN0 = -1;
  CrkChi2 = -1.;
  CrksNpe0 = -1.;
  CrksN0 = -1;
  CrksChi2 = -1.;
  CrkDist = 999.;
  CrksDist = 999.;
  Sector = source.Sector;
  Side = source.Side;
  TrackLength = source.TrackLength;
  for(int k=0; k<6; k++) Nwires[k] = source.Nwires[k];
  for(int k=0; k<6; k++) NHITS[k] = source.NHITS[k];
  calculateXinError();
  calculateYinError();
  XYZinError[2]=999.;
  XYZoutError[0]=XYZinError[0];
  XYZoutError[1]=XYZinError[1];
  XYZoutError[2]=XYZinError[2];
}

TecTrackV2::TecTrackV2(const TecTrackV2 &source) {
  XYZin[0] = source.XYZin[0];
  XYZin[1] = source.XYZin[1];
  XYZin[2] = source.XYZin[2];
  XYZout[0] = source.XYZout[0];
  XYZout[1] = source.XYZout[1];
  XYZout[2] = source.XYZout[2];
  hitLikelihood = source.hitLikelihood;
  for(int k=0; k<6; k++) Nwires[k] = source.Nwires[k];
  for(int k=0; k<6; k++) NHITS[k] = source.NHITS[k];
  for(int k=0; k<6; k++) dEdX06[k] = source.dEdX06[k];
  for(int k=0; k<6; k++) NdEdXbins[k] = source.NdEdXbins[k];
  for(int k=0; k<6; k++) DE[k] = source.DE[k];
  for(int k=0; k<6; k++) WeightedTimeBin[k] = source.WeightedTimeBin[k];
  for(int k=0; k<6; k++) Nhits20[k] = source.Nhits20[k];
  for(int k=0; k<6; k++) Nhits100[k] = source.Nhits100[k];
  for(int k=0; k<6; k++) NTR[k] = source.NTR[k];
  for(int k=0; k<6; k++) TR[k] = source.TR[k];
  Nhits = source.Nhits;
  Phi = source.Phi;
  Alpha = source.Alpha;
  PhiAtDch = source.PhiAtDch;
  AlphaAtDch = source.AlphaAtDch;
  Slope = source.Slope;
  Chi2 = source.Chi2;
  Intercept = source.Intercept;
  tecMomentum = source.tecMomentum;
  tecCharge = source.tecCharge;
  Pc3Pointer = source.Pc3Pointer;
  Pc3Distance = source.Pc3Distance;
  Pc3Z = source.Pc3Z;
  Pc3sPointer = source.Pc3sPointer;
  Pc3sDistance = source.Pc3sDistance;
  Pc1Pointer = source.Pc1Pointer;
  Pc1Distance = source.Pc1Distance;
  Pc1Z = source.Pc1Z;
  Pc1sPointer = source.Pc1sPointer;
  Pc1sDistance = source.Pc1sDistance;
  EmcPointer = source.EmcPointer;
  EmcDistance = source.EmcDistance;
  EmcEcore = source.EmcEcore;
  EmcZ = source.EmcZ;
  EmcsPointer = source.EmcsPointer;
  EmcsDistance = source.EmcsDistance;
  EmcsEcore = source.EmcsEcore;
  CrkNpe0 = source.CrkNpe0;
  CrkN0 = source.CrkN0;
  CrkChi2 = source.CrkChi2;
  CrksNpe0 = source.CrksNpe0;
  CrksN0 = source.CrksN0;
  CrksChi2 = source.CrksChi2;
  CrkDist = source.CrkDist;
  CrksDist = source.CrksDist;
  Sector = source.Sector;
  Side = source.Side;
  TrackLength = source.TrackLength;
  XYZinError[0] = source.XYZinError[0];
  XYZinError[1] = source.XYZinError[1];
  XYZinError[2] = source.XYZinError[2];
  XYZoutError[0]=XYZinError[0];
  XYZoutError[1]=XYZinError[1];
  XYZoutError[2]=XYZinError[2];
}

//---------------------------------------------------------
TecTrackV2& TecTrackV2::operator=(const TecTrack &source) {
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
  Pc1Pointer = -1;
  Pc1Distance = -999.;
  Pc1Z = -999.;
  Pc1sPointer = -1;
  Pc1sDistance = -999.;
  Pc3Pointer = -1;
  Pc3Distance = -999.;
  Pc3Z = -999.;
  Pc3sPointer = -1;
  Pc3sDistance = -999.;
  EmcPointer = -1;
  EmcDistance = -999.;
  EmcEcore = -999.;
  EmcZ = -999.;
  EmcsPointer = -1;
  EmcsDistance = -999.;
  EmcsEcore = -999;
  CrkNpe0 = -1.;
  CrkN0 = -1;
  CrkChi2 = -1.;
  CrksNpe0 = -1.;
  CrksN0 = -1;
  CrksChi2 = -1.;
  CrkDist = 999.;
  CrksDist = 999.;
  Sector = source.Sector;
  Side = source.Side;
  TrackLength = source.TrackLength;
  for(int k=0; k<6; k++) Nwires[k] = source.Nwires[k];
  for(int k=0; k<6; k++) NHITS[k] = source.NHITS[k];
  memset(dEdX06, 0, sizeof(dEdX06));
  memset(DE, 0, sizeof(DE));
  memset(WeightedTimeBin, 0, sizeof(WeightedTimeBin));
  memset(Nhits20, 0, sizeof(Nhits20));
  memset(Nhits100, 0, sizeof(Nhits100));
  memset(NTR, 0, sizeof(NTR));
  memset(TR, 0, sizeof(TR));
  memset(NdEdXbins, 0, sizeof(NdEdXbins));

  return *this;

}
//-----------------------------------------------------------------------------
// Methods for track coordinate error determination.
// Functional form comes from a fit to space resolution as a function of 
// incidence angle for momenta from 0.5 to 4.0 GeV/c in simulation.
// 1.5 comes from comparison of simulation with gas gain 3000 and the data (run00)

void TecTrackV2::calculateXinError() {

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

void TecTrackV2::calculateYinError() {

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

void TecTrackV2::project2PC(PHCompositeNode* topNode)
{
  using namespace PHGeometry;
  PadCluster* pc3 =
    findNode::getClass<PadCluster>(topNode, "Pc3Cluster");
  if (!pc3)
    {
      cerr << PHWHERE << ": project2PC: Can't find Pc3Cluster." << endl;
      return;
    }

  PadCluster* pc1 =
    findNode::getClass<PadCluster>(topNode, "Pc1Cluster");
  if (!pc1)
    {
      cerr << PHWHERE << ": project2PC: Can't find Pc1Cluster." << endl;
      return;
    }  

// Loop over Pc3 clusters and find the closest one
  float dist = -999.;
  float x0 = -999.;
  float y0 = -999.;
  float mindist=999.;
  float a=Slope; 
  float b=Intercept; 
  for(unsigned int i=0; i<pc3->get_PadNCluster(); i++)
    {
      float Xpc = pc3->get_xyz(i,0);
      float Ypc = pc3->get_xyz(i,1);
      float Zpc = pc3->get_xyz(i,2);
      // look at east arm only
      if(Xpc<0 &&
	 ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0)))
	{
	  x0=Xpc; y0=Ypc;
	  dist = (y0-a*x0-b)/(sqrt(a*a+1.0));
	  if(fabs(dist)<fabs(mindist))
	    { 
	      mindist = dist; 
	      Pc3Pointer = i;
	      Pc3Distance = mindist;
	      Pc3Z = Zpc;
	    }
	}
    } // end i loop
  
// Loop over Pc1 clusters and find the closest one
// Here I match by using phi angle
  mindist=999.; Pc1Z = -999.; 
  for(unsigned int i=0; i<pc1->get_PadNCluster(); i++)
    {
	float Xpc = pc1->get_xyz(i,0);
	float Ypc = pc1->get_xyz(i,1);
	float Zpc = pc1->get_xyz(i,2);
// Pc1 has 8 "sectors"
// Look at east arm only
	if(Xpc<0 &&
	   ((Zpc<0 && XYZin[2]<0) || (Zpc>0 && XYZin[2]>0)))
	  {
	    x0=Xpc; y0=Ypc;
	    dist = (y0-a*x0-b)/(sqrt(a*a+1.0));
	    if(fabs(dist)<fabs(mindist))
	      {
		mindist = dist; 
		Pc1Distance = mindist;
		Pc1Pointer = i;
		Pc1Z = Zpc;
	      }
	  }
    } // end i loop
  return;
}

//---------------------------------------------------------

void TecTrackV2::project2ZinvertedPC(PHCompositeNode* topNode)
{
  using namespace PHGeometry;
  PadCluster* pc3 =
    findNode::getClass<PadCluster>(topNode, "Pc3Cluster");
  if (!pc3)
    {
      cerr << PHWHERE << ": project2PC: Can't find Pc3Cluster." << endl;
      return;
    }

  PadCluster* pc1 =
    findNode::getClass<PadCluster>(topNode, "Pc1Cluster");
  if (!pc1)
    {
      cerr << PHWHERE << ": project2PC: Can't find Pc1Cluster." << endl;
      return;
    }  

// Loop over Pc3 clusters and find the closest one
  float dist,x0,y0;
  float mindist=999.;
  float a=Slope;
  float b=Intercept;

  for(unsigned int i=0; i<pc3->get_PadNCluster(); i++) {

    float Xpc = pc3->get_xyz(i,0);
    float Ypc = pc3->get_xyz(i,1);
    float Zpc = pc3->get_xyz(i,2);
// look at east arm only
      if(Xpc<0 &&
        ((Zpc<0 && XYZin[2]>0) || (Zpc>0 && XYZin[2]<0)))
	{ // invert Tec
	  x0=Xpc; y0=Ypc;
	  dist = ((y0-a*x0-b)/(sqrt(a*a+1.0)));
	  if(fabs(dist)<fabs(mindist))
	    {
	      mindist = dist;
	      Pc3sPointer = i;
	      Pc3sDistance = mindist;
	    }
	}
  } // end i loop

// Loop over Pc1 clusters and find the closest one
  // Here I match by using phi angle projection
  mindist=999.;
  for(unsigned int i=0; i<pc1->get_PadNCluster(); i++)
    {
	float Xpc = pc1->get_xyz(i,0);
	float Ypc = pc1->get_xyz(i,1);
	float Zpc = pc1->get_xyz(i,2);
// Pc1 has 8 "sectors"
// Look at east arm only
	if(Xpc<0 &&
	   ((Zpc<0 && XYZin[2]>0) || (Zpc>0 && XYZin[2]<0)))
	  {
	    x0=Xpc; y0=Ypc;
	    dist = ((y0-a*x0-b)/(sqrt(a*a+1.0)));	    
	    if(fabs(dist)<fabs(mindist))
	      {
		mindist = dist; 
		Pc1sDistance = dist;
		Pc1sPointer = i;
	      }
	  }
    } // end i loop
  return;
}

//---------------------------------------------------------

void TecTrackV2::project2EMC(PHCompositeNode* topNode)
{
  using namespace PHGeometry;
  emcClusterContainer* clusters =
    findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");

  if (!clusters)
    {
      cerr << PHWHERE
	   << ": cannot find emcClusterContainer object." << endl;
      return;
    }

// Loop over EMC clusters and find the closest one
  float dist,mindist;

  mindist=999.;
  EmcPointer = -1; EmcDistance = 999.0; EmcEcore = -999.;
  
  for (size_t iemc = 0; iemc < clusters->size(); ++iemc)
    {
      emcClusterContent* clus = clusters->getCluster(iemc);
      float Xemc = clus->x();
      float Yemc = clus->y();
      float Zemc = clus->z();
      if (Xemc>0) continue;
      if (Zemc>0 && XYZin[2]<0) continue; // inverted TEC
      if (Zemc<0 && XYZin[2]>0) continue;
      float a=Slope; float b=Intercept; float x0=Xemc; float y0=Yemc;
      dist = ((y0-a*x0-b)/(sqrt(a*a+1.0)));
      if(fabs(dist)<fabs(mindist))
	{
	  mindist = dist;
	  EmcPointer=iemc;
	  EmcEcore = clus->ecore();
	  EmcDistance = mindist;
	  EmcZ = Zemc;
	}
    }  
  return;
}

//---------------------------------------------------------

void TecTrackV2::project2ZinvertedEMC(PHCompositeNode* topNode)
{
  using namespace PHGeometry;
  emcClusterContainer* clusters =
    findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");

  if (!clusters)
    {
      cerr << PHWHERE
	   << ": cannot find emcClusterContainer object." << endl;
      return;
    }

// Loop over EMC clusters and find the closest one
  float dist,mindist;

  mindist=999.;
  EmcsPointer = -1; EmcsDistance = 999.0; EmcsEcore = -999.;
  
  for (size_t iemc = 0; iemc < clusters->size(); ++iemc)
    {
      emcClusterContent* clus = clusters->getCluster(iemc);
      float Xemc = clus->x();
      float Yemc = clus->y();
      float Zemc = clus->z();
      if (Xemc>0) continue;
      if (Zemc>0 && XYZin[2]>0) continue; // inverted TEC
      if (Zemc<0 && XYZin[2]<0) continue;
      float a=Slope; float b=Intercept; float x0=Xemc; float y0=Yemc;
      dist = ((y0-a*x0-b)/(sqrt(a*a+1.0)));
      if(fabs(dist)<fabs(mindist))
	{
	  mindist = dist;
	  EmcsPointer=iemc;
	  EmcsEcore = clus->ecore();
	  EmcsDistance = mindist;
	}
    }  
  return;
}

//---------------------------------------------------------

// this function project TEC track in RICH pmts and checks pmts fired into pmt_dist_cut distance from track
void TecTrackV2::project2Crk(PHCompositeNode* topNode, CrkGeometryObject* cgo, float pmt_dist_cut)
{
  using namespace PHGeometry;

  dCrkHitWrapper* crkhit = findNode::getClass<dCrkHitWrapper>(topNode, "dCrkHit");
  if (!crkhit)
    {
     cerr << PHWHERE
	   << ": cannot find dCrkHit object." << endl;
      return;
    }
  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (!bbcout)
    {
      cerr << PHWHERE
	   << ": cannot find BbcOut object." << endl;
      return;
    }
  
  float zvtx = bbcout->get_VertexPoint();
  float Z1, R;
  if (abs(Pc3Distance)<10 && Pc3Pointer>-1)
    {
      PadCluster* pc3 =
	findNode::getClass<PadCluster>(topNode, "Pc3Cluster");
      R = pc3->get_xyz(Pc3Pointer,0) * pc3->get_xyz(Pc3Pointer,0);
      R += pc3->get_xyz(Pc3Pointer,1) * pc3->get_xyz(Pc3Pointer,1);
      R = sqrt(R);
      Z1 = Pc3Z;
    }
  else
    {
      if (EmcPointer>-1)
	{
	  emcClusterContainer* clusters =
	    findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
	  emcClusterContent* clus = clusters->getCluster(EmcPointer);
	  R = sqrt(clus->x()*clus->x() + clus->y()*clus->y());
	  Z1 = EmcZ;
	}
      else
	return;
    }
  if (R==0) return;
  float Rin = sqrt(XYZin[0]*XYZin[0] + XYZin[1]*XYZin[1]);
  float Rout = sqrt(XYZout[0]*XYZout[0] + XYZout[1]*XYZout[1]);
  float dz = (Z1 - zvtx) / R;
  float Zin = zvtx + dz*Rin;
  float Zout = zvtx + dz*Rout;
  PHPoint pnt1 = PHPoint(XYZin[0],XYZin[1],Zin);
  PHVector vct1 = PHVector(XYZout[0]-XYZin[0],XYZout[1]-XYZin[1],Zout-Zin);
  CrkNpe0 = 0.;
  CrkN0 = 0;
  int nhits = crkhit->RowCount();
  CrkDist = 999.;
  int arm = 1;
  int side, panel;
  double path;
  PHLine ref = cgo->Reflect (arm,PHLine(pnt1,vct1) , side, panel, path);
  for (int icrk = 0; icrk < nhits; ++icrk)
    {
      int ipmt = crkhit->get_pmt(icrk);
      if (ipmt<0) continue;
      arm = cgo->IdToArm(ipmt);
      if (arm==0) continue;  // east arm
      PHPoint pmt_pos = 
	cgo->GetPmtPosition (arm,
			     cgo->IdToSide(ipmt),
			     cgo->IdToSm(ipmt),
			     cgo->IdToPmt(ipmt));
      if (pmt_pos.getZ()>0 && XYZin[2]<0) continue;
      if (pmt_pos.getZ()<0 && XYZin[2]>0) continue;
      double pmt_dist = distanceLinePoint (ref, pmt_pos);
      if (pmt_dist<CrkDist)
	CrkDist = pmt_dist;
      if (pmt_dist < pmt_dist_cut)
	{
	  CrkNpe0 += crkhit->get_npe(icrk);
	  CrkN0   ++;
	  CrkChi2 += pmt_dist*pmt_dist;
	}
    }
}

// this function project swapped TEC track in RICH pmts and checks pmts fired into pmt_dist_cut distance from track
void TecTrackV2::project2ZinvertedCrk(PHCompositeNode* topNode, CrkGeometryObject* cgo, float pmt_dist_cut)
{
  using namespace PHGeometry;

  dCrkHitWrapper* crkhit = findNode::getClass<dCrkHitWrapper>(topNode, "dCrkHit");
  if (!crkhit)
    {
     cerr << PHWHERE
	   << ": cannot find dCrkHit object." << endl;
      return;
    }
  BbcOut * bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (!bbcout)
    {
      cerr << PHWHERE
	   << ": cannot find BbcOut object." << endl;
      return;
    }
  
  float zvtx = bbcout->get_VertexPoint();
  float Z1, R;
  if (abs(Pc3sDistance)<10 && Pc3sPointer>-1)
    {
      PadCluster* pc3 =
	findNode::getClass<PadCluster>(topNode, "Pc3Cluster");
      R = pc3->get_xyz(Pc3sPointer,0) * pc3->get_xyz(Pc3sPointer,0);
      R += pc3->get_xyz(Pc3sPointer,1) * pc3->get_xyz(Pc3sPointer,1);
      R = sqrt(R);
      Z1 = pc3->get_xyz(Pc3sPointer,2);
    }
  else
    {
	if (EmcsPointer>-1)
	  {
	    emcClusterContainer* clusters =
	      findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
	    emcClusterContent* clus = clusters->getCluster(EmcsPointer);
	    R = sqrt(clus->x()*clus->x() + clus->y()*clus->y());
	    Z1 = clus->z();
	  }
	else
	  return;
      }
  if (R==0) return;
  float Rin = sqrt(XYZin[0]*XYZin[0] + XYZin[1]*XYZin[1]);
  float Rout = sqrt(XYZout[0]*XYZout[0] + XYZout[1]*XYZout[1]);
  float dz = (Z1 - zvtx) / R;
  float Zin = zvtx + dz*Rin;
  float Zout = zvtx + dz*Rout;
  PHPoint pnt1 = PHPoint(XYZin[0],XYZin[1],Zin);
  PHVector vct1 = PHVector(XYZout[0]-XYZin[0],XYZout[1]-XYZin[1],Zout-Zin);
  CrksNpe0 = 0.;
  CrksN0 = 0;
  int nhits = crkhit->RowCount();
  CrksDist = 999.;
  int arm = 1;
  int side, panel;
  double path;
  PHLine ref = cgo->Reflect (arm,PHLine(pnt1,vct1) , side, panel, path);
  for (int icrk = 0; icrk < nhits; ++icrk)
    {
      int ipmt = crkhit->get_pmt(icrk);
      if (ipmt<0) continue;
      arm = cgo->IdToArm(ipmt);
      if (arm==0) continue;  // east arm
      PHPoint pmt_pos = 
	cgo->GetPmtPosition (arm,
			     cgo->IdToSide(ipmt),
			     cgo->IdToSm(ipmt),
			     cgo->IdToPmt(ipmt));
      if (pmt_pos.getZ()>0 && XYZin[2]>0) continue;
      if (pmt_pos.getZ()<0 && XYZin[2]<0) continue;
      double pmt_dist = distanceLinePoint (ref, pmt_pos);
      if (pmt_dist<CrksDist)
	CrksDist = pmt_dist;
      if (pmt_dist < pmt_dist_cut)
	{
	  CrksNpe0 += crkhit->get_npe(icrk);
	  CrksN0   ++;
	  CrksChi2 += pmt_dist*pmt_dist;
	}
    }
}


int TecTrackV2::getNFwires() const {
    int nfwires=0;
    for(int i=0; i<6; i++) { nfwires+=Nwires[i]; }
  return nfwires;
}

//---------------------------------------------------------

int TecTrackV2::getNFplanes() const {
    int nfplanes=0;
    for(int i=0; i<6; i++) { if(Nwires[i]>0) { nfplanes++; } }
  return nfplanes;
}

