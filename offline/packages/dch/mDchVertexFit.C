#include <iostream>

#include "mDchVertexFit.hh"

//INCLUDECHECKER: Removed this line: #include "PHNodeIterator.h"
#include "PHIODataNode.h"
#include "PHTable.hh"
//#include "PHDchHistogrammer.hh"

#include "dDchTracksWrapper.h"

#include "TMinuit.h"

typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;
typedef PHIODataNode<PHTable> TableNode_t;

mDchVertexFit::mDchVertexFit() {}

mDchVertexFit::~mDchVertexFit() {}

PHBoolean mDchVertexFit::event(PHCompositeNode *root) 
{
 PHPointerList<PHNode> nodes;
 PHNodeIterator i(root);
 TableNode_t *d;
 PHTable *w;
 PHCompositeNode *dchNode;

 dchNode = static_cast<PHCompositeNode*>(i.findFirst("PHCompositeNode", "DCH"));
 if (!dchNode) {
   dchNode = new PHCompositeNode("DCH");
   root->addNode(dchNode);
 }

// Insert code here to navigate node hierarchy and find
// or create specific nodes to pass to physics module...

  if (!(d = static_cast<TableNode_t*>(i.findFirst("PHIODataNode","dDchTracks")))) {
    w = new dDchTracksWrapper("dDchTracks", 60000);
     if (!w) {
       return 1;
     }
     d = new TableNode_t(w,"dDchTracks");
  }
  nodes.append(d);
     
  return callPAM(nodes);

}

PHBoolean mDchVertexFit::callPAM(PHPointerList<PHNode> &nl)
{
  dDchTracksNode_t* trackNode = static_cast<dDchTracksNode_t*>(nl[0]);

  TABLE_HEAD_ST dDchTracks_h;
  DDCHTRACKS_ST *dDchTracks;

  dDchTracks_h = trackNode->getData()->TableHeader();
  dDchTracks   = trackNode->getData()->TableData();

  gMinuit = new TMinuit(5);  // initialize TMinuit with a maximum of 5 params
  this->setMinuit(gMinuit);
  gMinuit->mninit(5,6,7);             // setup input/output
  gMinuit->SetFCN(MinimizationFCN);   // choose minimization function
  gMinuit->SetObjectFit(this);
  gMinuit->Command("SET PRINT -1");   // set output information level

  mDchVertexFit *DchVertexFit = (mDchVertexFit*)gMinuit->GetObjectFit();
  double xprelim, yprelim, zprelim;

  // initialize the vertex coordinates and some counters

  xini =   0.0;
  yini =   0.0;
  zini =   0.0;
  ngood = 0; 
  nbad = 0;
  ngeogood = 0; 
  ngeobad = 0;
  nvtx = 0;
  
  // read data from the track table

  int hitmul = 0;
  int ipart  = 0;
  npart = dDchTracks_h.nok;    

  if ( npart<1 ) {delete gMinuit; return True;}

  for (ipart=0; ipart < npart; ipart++) 
  {                     
    // a) read intersection point and direction of track on reference cylinder 
    sarm[ipart] = (double) dDchTracks[ipart].arm;
    sx[ipart] = (double) dDchTracks[ipart].point[0];
    sy[ipart] = (double) dDchTracks[ipart].point[1];
    sz[ipart] = (double) dDchTracks[ipart].point[2];
    ax[ipart] = (double) dDchTracks[ipart].direction[0];
    ay[ipart] = (double) dDchTracks[ipart].direction[1];
    az[ipart] = (double) dDchTracks[ipart].direction[2];
    // b) read number of hits per track
    nhits[ipart] = 0;
    for(int j=0; j<40; j++) 
    {
      hitmul = (int) dDchTracks[ipart].hits[j];
      if(hitmul != -1) nhits[ipart]++;
    }
  }

  //########################################################################
  //use MINUIT
  //########################################################################

  gMinuit->Command("CALL FCN 1");     // access data with z forced to 0
  gMinuit->Command("CALL FCN 10");    // find distances of all tracks  
                                      // to coordinate origin

  // initialize the vertex coordinates
  
  (*DchVertexFit).vx = xini;
  (*DchVertexFit).vy = yini;
  (*DchVertexFit).vz = zini;

  // find distances of ALL tracks to this preliminary vertex

  for(ipart=0; ipart<npart; ipart++)  
  {
      track_ok[ipart] = 1;         
  }
  gMinuit->Command("CALL FCN 11");    

  // mark good tracks for final vertex fit in xy: 
  // cut on radial distance to preliminary vertex;
  // cut on number of hits/track

  for(ipart=0; ipart<npart; ipart++)
  {
    if( fabs(accdistr[ipart])<=DISTPRIMR && 
        fabs(accdistz[ipart])<=DISTPRIMZ &&
        nhits[ipart]>=HITSMIN && nhits[ipart]<=HITSMAX && sarm[ipart]==1. ) 
    {
      ngood++;
      track_ok[ipart] = 1;         
    }
    else 
    {
      nbad++;
      track_ok[ipart] = 0;         
    }

    if ( fabs(distz[ipart]) > 50.0 ) track_ok[ipart] = 0;

     if( fabs(accdistr[ipart])<=DISTPRIMR && 
         fabs(accdistz[ipart])<=DISTPRIMZ ) 
     {
       ngeogood++;
     }
     else
     {
       ngeobad++;
     }
  }

  // perform the vertex fit in xy

  gMinuit->DefineParameter(0, "X1", xini,  0.001, 0,0);
  gMinuit->DefineParameter(1, "X2", yini,  0.001, 0,0);
  gMinuit->DefineParameter(2, "X3", zini,  0.001, 0,0);    
  gMinuit->FixParameter(2);           // fix z parameter
  gMinuit->Migrad();                  // minimization strategy I
  gMinuit->Command("MINOS");          // minimization strategy II
  gMinuit->Release(2);                // release z parameter
  gMinuit->Command("CALL FCN 3");     // output of vertex fit

  // mark good tracks for final vertex fit in xyz: 
  // cut on radial distance and distance in z to preliminary vertex;
  // cut on number of hits/track

  gMinuit->Command("CALL FCN 15");    // access data with correct z
  gMinuit->Command("CALL FCN 11");    // find distances of accepted tracks 
                                      // to reconstructed vertex
  ngood = 0;
  nbad  = 0;
  for(ipart=0; ipart<npart; ipart++)  
  {
      track_ok[ipart] = 1;         
  }
  for(ipart=0; ipart<npart; ipart++)
  {
    if( fabs(accdistr[ipart])<=DISTPRIMR && 
        fabs(accdistz[ipart])<=DISTPRIMZ &&
        nhits[ipart]>=HITSMIN && nhits[ipart]<=HITSMAX ) 
    {
      ngood++;
      track_ok[ipart] = 1;         
    }
    else 
    {
      nbad++;
      track_ok[ipart] = 0;         
    }

    if ( fabs(distz[ipart]) > 50.0 ) track_ok[ipart] = 0;

     if( fabs(accdistr[ipart])<=DISTPRIMR && 
         fabs(accdistz[ipart])<=DISTPRIMZ ) 
     {
       ngeogood++;
     }
     else
     {
       ngeobad++;
     }
  }

  // start vertex fit with coordinates of preliminary vertex

  xprelim = (*DchVertexFit).vx;
  yprelim = (*DchVertexFit).vy;
  zprelim = (*DchVertexFit).vz;
  gMinuit->DefineParameter(0, "X1",    xprelim,  0.001, 0,0);
  gMinuit->DefineParameter(1, "X2",    yprelim,  0.001, 0,0);
  gMinuit->DefineParameter(2, "X3",    zprelim,  0.001, 0,0);    
  gMinuit->FixParameter(0);           // fix x parameter
  gMinuit->FixParameter(1);           // fix y parameter
  gMinuit->Migrad();                  // minimization strategy I
  gMinuit->Command("MINOS");          // minimization strategy II
  gMinuit->Release(0);                // release x parameter
  gMinuit->Release(1);                // release y parameter
  gMinuit->Command("CALL FCN 10");    // find distances of all tracks 
                                      // to reconstructed vertex
  gMinuit->Command("CALL FCN 11");    // find distances of accepted tracks 
                                      // to reconstructed vertex
  gMinuit->Command("CALL FCN 3");     // output of vertex fit

  // fill some histograms

  if ( vz!=0.0 ){
  for (ipart=0; ipart<npart; ipart++)
  {
    if(track_ok[ipart] == 1) 
    {
      if(  fabs(accdistr[ipart]) <= DISTRECR  &&
           fabs(accdistz[ipart]) <= DISTRECZ  ) 
	nvtx++;
    }
  }
  vtxPoint.setX(vx);
  vtxPoint.setY(vy);
  vtxPoint.setZ(vz);

  }

  delete gMinuit;
  return True;

}

void mDchVertexFit::initialize()
{
  DISTPRIMR=15.;
  DISTPRIMX=10.;
  DISTPRIMY=10.;
  DISTPRIMZ=100.;
  DISTRECR=1.;
  DISTRECX=0.5;
  DISTRECY=1.;
  DISTRECZ=5.;
  HITSMIN=6, HITSMAX=40;
}

void MinimizationFCN(int &, double *, double &f, double *x, int iflag)
{
   mDchVertexFit *DchVertexFit = (mDchVertexFit*)gMinuit->GetObjectFit();

   static double s1[MAXDIM],s2[MAXDIM],s3[MAXDIM];
   static double a1[MAXDIM],a2[MAXDIM],a3[MAXDIM];
   double x1,x2,x3,chisq;
   double u1,u2,b1,b2,b3,c1,c2,c3,sigma;
   double u1p,u2p,l,r1,r2,r3,v1,v2,v3;
   double a,b,d;
   int    n,i;

   n = (*DchVertexFit).npart;

   x1 = x[0];
   x2 = x[1];
   x3 = x[2];

   if (iflag == 1) {
     //reset arrays
     for(i=0; i<MAXDIM; i++){
       s1[i] = 0.;
       s2[i] = 0.;
       s3[i] = 0.;
       a1[i] = 0.;
       a2[i] = 0.;
       a3[i] = 0.;
       (*DchVertexFit).track_ok[i] = 1;
       (*DchVertexFit).distr[i] = 11111.;
       (*DchVertexFit).distx[i] = 11111.;
       (*DchVertexFit).disty[i] = 11111.;
       (*DchVertexFit).distz[i] = 11111.;
       (*DchVertexFit).accdistr[i] = 11111.;
       (*DchVertexFit).accdistx[i] = 11111.;
       (*DchVertexFit).accdisty[i] = 11111.;
       (*DchVertexFit).accdistz[i] = 11111.;
     }     

     (*DchVertexFit).vx = 0.; 
     (*DchVertexFit).vy = 0.; 
     (*DchVertexFit).vz = 0.;

     //copy array
     for(i=0; i<n; i++){
       s1[i] = (*DchVertexFit).sx[i];
       s2[i] = (*DchVertexFit).sy[i];
       s3[i] = (*DchVertexFit).sz[i];
       s3[i] = 0.0;
       a1[i] = (*DchVertexFit).ax[i];
       a2[i] = (*DchVertexFit).ay[i];
       a3[i] = (*DchVertexFit).az[i];
       a3[i] = 0.0;
     }     
   }

   if (iflag == 15) {
     //reset arrays
     for(i=0; i<MAXDIM; i++){
       s1[i] = 0.;
       s2[i] = 0.;
       s3[i] = 0.;
       a1[i] = 0.;
       a2[i] = 0.;
       a3[i] = 0.;
     }     

     //copy array
     for(i=0; i<n; i++){
       s1[i] = (*DchVertexFit).sx[i];
       s2[i] = (*DchVertexFit).sy[i];
       s3[i] = (*DchVertexFit).sz[i];
       a1[i] = (*DchVertexFit).ax[i];
       a2[i] = (*DchVertexFit).ay[i];
       a3[i] = (*DchVertexFit).az[i];
     }     
   }

   // #################################################################
   // chisquare function, called multiple times during the fit process. 
   // Take into account only tracks that pass in a certain distance 
   // from the primary vertex !!
   // #################################################################
   chisq = 0.;
   sigma = 0.01;
   for(i=0; i<n; i++){
     if((*DchVertexFit).track_ok[i]==1 ) {
        b1 = x1 - s1[i]; 
        b2 = x2 - s2[i];
        b3 = x3 - s3[i];

        c1 = a2[i]*b3-a3[i]*b2;
        c2 = a3[i]*b1-a1[i]*b3;
        c3 = a1[i]*b2-a2[i]*b1;
          
        d = sqrt(c1*c1+c2*c2+c3*c3);
        chisq = chisq + d*d/(sigma*sigma);
      }
   }
   f = chisq;

   // output of vertex fit
   if (iflag == 3) {
     printf("\n");
     (*DchVertexFit).chisquare=f;
     (*DchVertexFit).vx=x1;
     (*DchVertexFit).vy=x2;
     (*DchVertexFit).vz=x3;
   }
// xy-distance of all tracks to primary vertex

   if (iflag == 10) {
      for(i=0; i<n; i++){
           
         // projection of track in xy-plane at z=vz.
         // remember: b3 = 0. and u3 = 0. in this projection !!
         v1 = (*DchVertexFit).xini;
         v2 = (*DchVertexFit).yini;
         v3 = (*DchVertexFit).zini;

         b1 = v1 - s1[i]; 
         b2 = v2 - s2[i];
         b3 = 0.;
    
         u1 = a1[i];
         u2 = a2[i];


         u1p = -a2[i];
         u2p = a1[i];

         l = (b2/u2 - b1/u1)/(u1p/u1-u2p/u2);
        
         r1 = v1 + l*u1p;
         r2 = v2 + l*u2p;

         (*DchVertexFit).distx[i] = r1-v1;
         (*DchVertexFit).disty[i] = r2-v2;
         (*DchVertexFit).distr[i] = sqrt((*DchVertexFit).distx[i]
					 *(*DchVertexFit).distx[i] 
					 +(*DchVertexFit).disty[i]
					 *(*DchVertexFit).disty[i]);
      }
   }

// 3-dim z-distance of all tracks to primary vertex

   if (iflag == 10) {
      for(i=0; i<n; i++){

         v1 = (*DchVertexFit).xini;
         v2 = (*DchVertexFit).yini;
         v3 = (*DchVertexFit).zini;

         b1 = v1 - s1[i]; 
         b2 = v2 - s2[i];
         b3 = v3 - s3[i];

         c1 = a2[i]*b3-a3[i]*b2;
         c2 = a3[i]*b1-a1[i]*b3;
         c3 = a1[i]*b2-a2[i]*b1;
 

         a = a1[i]*a1[i] + a2[i]*a2[i] + a3[i]*a3[i]; 
         b = 2*( a1[i]*(s1[i]-v1) + a2[i]*(s2[i]-v2) + a3[i]*(s3[i]-v3) );
	 // c = s1[i]*s1[i] + s2[i]*s2[i] + s3[i]*s3[i] - 2*(v1*s1[i] + v2*s2[i] + v3*s3[i]) - d*d; 

         l = -b/(2*a);

         r3 = s3[i] + l*a3[i];
   
         (*DchVertexFit).distz[i] = r3-v3;
      }
   }

// xy-distance of accepted tracks to reconstructed vertex

   if (iflag == 11) {
      for(i=0; i<n; i++){

	 if((*DchVertexFit).track_ok[i] ==1) {
           // projection of track in xy-plane at z=vz.
           // remember: b3 = 0. and u3 = 0. in this projection !!
           v1 = (*DchVertexFit).vx;
           v2 = (*DchVertexFit).vy;
           v3 = 0.;

           b1 = v1 - s1[i]; 
           b2 = v2 - s2[i];
           b3 = 0.;
    
           u1 = a1[i];
           u2 = a2[i];

           u1p = -a2[i];
           u2p = a1[i];

           l = (b2/u2 - b1/u1)/(u1p/u1-u2p/u2);
        
           r1 = v1 + l*u1p;
           r2 = v2 + l*u2p;

           (*DchVertexFit).accdistx[i] = r1-v1;
           (*DchVertexFit).accdisty[i] = r2-v2;
           (*DchVertexFit).accdistr[i] = sqrt((*DchVertexFit).accdistx[i]
					      *(*DchVertexFit).accdistx[i] 
					      +(*DchVertexFit).accdisty[i]
					      *(*DchVertexFit).accdisty[i]);
	 }
      }
   }

// 3-dim z-distance of all tracks to reconstructed vertex

   if (iflag == 11) {
      for(i=0; i<n; i++){

         v1 = (*DchVertexFit).vx;
         v2 = (*DchVertexFit).vy;
         v3 = (*DchVertexFit).vz;

         b1 = v1 - s1[i]; 
         b2 = v2 - s2[i];
         b3 = v3 - s3[i];

         c1 = a2[i]*b3-a3[i]*b2;
         c2 = a3[i]*b1-a1[i]*b3;
         c3 = a1[i]*b2-a2[i]*b1;


         a = a1[i]*a1[i] + a2[i]*a2[i] + a3[i]*a3[i]; 
         b = 2*( a1[i]*(s1[i]-v1) + a2[i]*(s2[i]-v2) + a3[i]*(s3[i]-v3) );
	 // c = s1[i]*s1[i] + s2[i]*s2[i] + s3[i]*s3[i] - 2*(v1*s1[i] + v2*s2[i] + v3*s3[i]) - d*d; 

         l = -b/(2*a);

         r3 = s3[i] + l*a3[i];
   
         (*DchVertexFit).accdistz[i] = r3-v3;
      }
   }

}
