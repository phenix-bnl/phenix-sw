#include "SvxCglReco.h"
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include "SvxSegmentListv2.h"
#include "SvxClusterListv4.h"
#include "CglTrack.h"
#include "DchTrack.h"


using namespace std;


static float twopi = 6.28318530717958623;
static float twopi_inv = 1.59154943091895346e-01;

static inline float phiDiff(float phi1, float phi2)
{
  float pd = (phi1 - phi2);
  float pi = 3.14159265358979312;
  
  int a = (pd>pi);
  int b = (pd<(-pi));
  
  pd -= a*twopi;
  pd += b*twopi;
  
  return pd;
}

static inline int angleBin(float phi)
{
  int a = (phi<0.);
  phi += a*twopi;
  
  return ((int)(floor(((float)64)*phi*twopi_inv)));
}


int SvxCglReco::process_event(PHCompositeNode *topNode)
{
  CglTrack* cgl = 0;
  PHTypedNodeIterator<CglTrack> cgliter(topNode);
  PHIODataNode<CglTrack> *CglNode = cgliter.find("CglTrack");
  if(CglNode) { cgl = (CglTrack*)CglNode->getData(); }
  else {cerr << "ERROR: Can't find CglTrack." << endl; return EVENT_OK;}
  
  DchTrack *dch=0;
  PHTypedNodeIterator<DchTrack> dchiter(topNode);
  PHIODataNode<DchTrack> *DchTrackNode = dchiter.find("DchTrack");
  if (!DchTrackNode) { cerr << PHWHERE << " ERROR: Can't find DchTrack." << endl;  return EVENT_OK; }
  else { dch = (DchTrack*)DchTrackNode->getData(); }
  
  SvxSegmentList *d_segment=0;
  PHTypedNodeIterator<SvxSegmentList> segmentiter(topNode);
  PHIODataNode<SvxSegmentList> *SvxSegmentListNode = segmentiter.find("SvxSegmentList");
  if (!SvxSegmentListNode) { cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;  return EVENT_OK; }
  else { d_segment = (SvxSegmentList*)SvxSegmentListNode->getData(); }
  
  SvxClusterList       *d_cluster=0;
  PHTypedNodeIterator<SvxClusterList> clusteriter(topNode);
  PHIODataNode<SvxClusterList> *SvxClusterListNode = clusteriter.find("SvxClusterList");
  if (!SvxClusterListNode) { cerr << PHWHERE << " ERROR: Can't find SvxClusterList." << endl;  return EVENT_OK; }
  else { d_cluster = (SvxClusterList*)SvxClusterListNode->getData(); }

// vtx cluster to cgl track association
  for(int i=0; i<(int)cgl->get_CglNTrack(); i++) {
    for(int j=0; j<4; j++) {
      int isvx = cgl->get_svxclusid(i, j);
      if(isvx>-1 && isvx<d_cluster->get_nClusters()) {
        //cout << "associatedcgl: " << isvx << endl;
        (d_cluster->get_Cluster(isvx))->set_AssociatedCGL(i);
      }
    }
  }



  segpt.clear();
  segphi.clear();
  segtheta.clear();
  for(int i=0;i<64;i++)
  {
    for(int j=0;j<64;j++)
    {
      org[i][j].clear();
    }
  }
  
  float px,py,pz;
  float pt;
  float phi, theta;
  
  segpt.assign(d_segment->get_nSegments(), 0.);
  segphi.assign(d_segment->get_nSegments(), 0.);
  segtheta.assign(d_segment->get_nSegments(), 0.);
  for(int n=0;n<d_segment->get_nSegments();n++)
  {
    px = d_segment->get_segment(n)->get3Momentum(0);
    py = d_segment->get_segment(n)->get3Momentum(1);
    pz = d_segment->get_segment(n)->get3Momentum(2);
    pt = sqrt(px*px + py*py);
    segpt[n] = pt;
    
    phi = atan2(py, px);
    segphi[n] = phi;
    theta = atan2(pz, pt);
    segtheta[n] = theta;
    
    int phibin = angleBin(phi);
    int thetabin = angleBin(theta);
    
    org[phibin][thetabin].push_back(n);
  }
  
  unsigned int idctrk;
  
  float temp, cur;
  
  int best;
  
  for(unsigned int j=0;j<cgl->get_CglNTrack();j++)
  {
    idctrk = cgl->get_dctracksid(j);
    
    pt=dch->get_momentum(idctrk)*sin(dch->get_theta0(idctrk));
    px=pt*cos(dch->get_phi0(idctrk));
    py=pt*sin(dch->get_phi0(idctrk));
    pz=dch->get_momentum(idctrk)*cos(dch->get_theta0(idctrk));
    phi=atan2(py, px);
    theta=atan2(pz, pt);
    
    cur=100.;
    best=-1;
    
    int phibin = angleBin(phi);
    int thetabin = angleBin(theta);
    int width=1;
    
    for(int w=-width;w<=width;w++)
    {
      int phibin2=(64 + phibin + w)%64;
      for(int t=-width;t<=width;t++)
      {
        int thetabin2=(64 + thetabin + w)%64;
        
        for(unsigned int i=0;i<org[phibin2][thetabin2].size();i++)
        {
          int n = org[phibin2][thetabin2][i];
          
          float phid = phiDiff(segphi[n],phi);
          float thetad = phiDiff(segtheta[n],theta);
          if(fabs(phid)<0.05 && fabs(thetad)<0.05 && fabs(segpt[n]-pt)<(0.5*pt))
          {
            
            temp = phid*phid + thetad*thetad;
            
            if(temp<cur)
            {
              best=n;
              cur=temp;
            }
          }
        }
      }
    }
    
    if(best>=0)
    {
      d_segment->get_segment(best)->setDchIndex(idctrk);
      
      for(int l=0;l<4;l++)
      {
        cgl->set_svxclusid(j, l, d_segment->get_segment(best)->getClusterID(l+1));
      }
    }
  }

  
// match clusters to standalone tracks
  for(int i=0; i<d_segment->get_nSegments(); i++) {
    for(int j=0; j<4; j++) {
      int isvx = (d_segment->get_segment(i))->getClusterID(j, i);
      if(isvx>-1 && isvx<d_cluster->get_nClusters()) {
        //cout << "associatedstandalone: " << isvx << endl;
        (d_cluster->get_Cluster(isvx))->set_AssociatedStandalone(i);
      }
    }
  }


  return 0;
}


