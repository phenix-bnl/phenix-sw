#include <cmath>
#include "SvxClusterContainer.h"
#include "SvxClusterList.h"
#include "SvxCluster.h"

using namespace std;

const float SvxClusterContainer::Zmax0=11.0;  //max Z value for layer 0 (pixel)
const float SvxClusterContainer::Zmax1=11.0;  //max Z value for layer 1 (pixel)
const float SvxClusterContainer::Zmax2=16.0;  //max Z value for layer 2 (strip)
const float SvxClusterContainer::Zmax3=19.5;  //max Z value for layer 3 (strip)

SvxClusterContainer::SvxClusterContainer() {
  d_xcenter = 0.;
  d_ycenter = 0.;

  d_initialized = false;

  m_verbosity = 0;
}

void SvxClusterContainer::load_clusters(SvxClusterList *clusterlist) {
  //
  // store all clusters in clusterlist into this container.
  // the clusters are stored sub-layers in the following three arrays for
  // quicker access.
  // Sub-layers    0 <--> Layer 0 (pixel)
  //               1 <--> Layer 1 (pixel)
  //           2,3,4 <--> Layer 2 (strip) 
  //           5,6,7 <--> Layer 3 (strip) 
  // (2/3 of bins in strip sub-layers are always empty. But memory is cheap)
  //
  // dv_cluster0[SVXMAXSUBLAYER+1]            sorted by sub layers
  // dv_cluster1[SVXMAXSUBLAYER+1][NPHI]      sorted by layer and phi
  // dv_cluster2[SVXMAXSUBLAYER+1][NPHI][NZ]  sorted by layer, phi, and z
  //
  // dv_used1 and dv_used2 are to record which bin in dv_cluster1 and
  // dv_cluster2 are used. They are used in clear() method to clear
  // dv_cluser1 and dv_cluser2 quickly.
  //
  int n = clusterlist->get_nClusters();
  for(int i=0;i<n;i++) {
    SvxCluster *cluster;
    cluster = clusterlist->get_Cluster(i);
    float x   = cluster->get_xyz_global(0);
    float y   = cluster->get_xyz_global(1);
    float z   = cluster->get_xyz_global(2);

    int layer = cluster->get_layer();
    int ladder = cluster->get_ladder();
    int sublayer = get_sublayer(layer,ladder);
    int iphi = calc_iphi(calc_phi(x,y));
    int iz = calc_iz(sublayer,z);

    // store the cluster in the sorted vectors based on
    // (iphi, iz).
    //
    dv_cluster0[sublayer].push_back(cluster);
    dv_cluster1[sublayer][iphi].push_back(cluster);
    dv_cluster2[sublayer][iphi][iz].push_back(cluster);

    // record which bins in the vector array are used    
    //
    if(dv_cluster1[sublayer][iphi].size()==1) { //this bin is used for the first time. record it
      dv_used1.push_back(&(dv_cluster1[sublayer][iphi]));
    }
    if(dv_cluster2[sublayer][iphi][iz].size()==1) { //this bin is used for the first time. record it
      dv_used2.push_back(&(dv_cluster2[sublayer][iphi][iz]));
    }
  }

  d_initialized = true;
}

void SvxClusterContainer::load_clusters(vector<SvxCluster*> &clusterlist) {
  //
  // store all clusters in clusterlist into this container.
  // the clusters are stored sub-layers in the following three arrays for
  // quicker access.
  // Sub-layers    0 <--> Layer 0 (pixel)
  //               1 <--> Layer 1 (pixel)
  //           2,3,4 <--> Layer 2 (strip) 
  //           5,6,7 <--> Layer 3 (strip) 
  // (2/3 of bins in strip sub-layers are always empty. But memory is cheap)
  //
  // dv_cluster0[SVXMAXSUBLAYER+1]            sorted by sub layers
  // dv_cluster1[SVXMAXSUBLAYER+1][NPHI]      sorted by layer and phi
  // dv_cluster2[SVXMAXSUBLAYER+1][NPHI][NZ]  sorted by layer, phi, and z
  //
  // dv_used1 and dv_used2 are to record which bin in dv_cluster1 and
  // dv_cluster2 are used. They are used in clear() method to clear
  // dv_cluser1 and dv_cluser2 quickly.
  //
  int n = clusterlist.size();
  for(int i=0;i<n;i++) {
    SvxCluster *cluster = clusterlist.at(i);
    float x   = cluster->get_xyz_global(0);
    float y   = cluster->get_xyz_global(1);
    float z   = cluster->get_xyz_global(2);

    int layer = cluster->get_layer();
    int ladder = cluster->get_ladder();
    int sublayer = get_sublayer(layer,ladder);
    int iphi = calc_iphi(calc_phi(x,y));
    int iz = calc_iz(sublayer,z);

    // store the cluster in the sorted vectors based on
    // (iphi, iz).
    //
    dv_cluster0[sublayer].push_back(cluster);
    dv_cluster1[sublayer][iphi].push_back(cluster);
    dv_cluster2[sublayer][iphi][iz].push_back(cluster);

    // record which bins in the vector array are used
    //
    if(dv_cluster1[sublayer][iphi].size()==1) { //this bin is used for the first time. record it
      dv_used1.push_back(&(dv_cluster1[sublayer][iphi]));
    }
    if(dv_cluster2[sublayer][iphi][iz].size()==1) { //this bin is used for the first time. record it
      dv_used2.push_back(&(dv_cluster2[sublayer][iphi][iz]));
    }
  }

  d_initialized = true;
}


void SvxClusterContainer::load_fake_clusters(SvxClusterList *clusterlist) {
  // store swapped clusters in clusterlist into this container.

  int n = clusterlist->get_nClusters();
  for(int i=0;i<n;i++) {
    SvxCluster *cluster = clusterlist->get_Cluster(i);
    float x   = cluster->get_xyz_global(0);
    float y   = cluster->get_xyz_global(1);
    float z   = cluster->get_xyz_global(2);

    int layer = cluster->get_layer();
    int ladder = cluster->get_ladder();
    int sublayer = get_sublayer(layer,ladder);
    int iphi = calc_iphi(calc_phi(x,y));
    int iz = calc_iz(sublayer,z);

    // store the cluster in the sorted vectors based on
    // (iphi, iz).
    //
    dv_f_cluster0[sublayer].push_back(cluster);
    dv_f_cluster1[sublayer][iphi].push_back(cluster);
    dv_f_cluster2[sublayer][iphi][iz].push_back(cluster);

    // record which bins in the vector array are used    
    //
    if(dv_f_cluster1[sublayer][iphi].size()==1) { //this bin is used for the first time. record it
      dv_f_used1.push_back(&(dv_f_cluster1[sublayer][iphi]));
    }
    if(dv_f_cluster2[sublayer][iphi][iz].size()==1) { //this bin is used for the first time. record it
      dv_f_used2.push_back(&(dv_f_cluster2[sublayer][iphi][iz]));
    }
  }
}

inline int SvxClusterContainer::calc_iphi(float phi)
{
  // return phi bin index corresponding to phi.
  // return value is limited to [0,NPHI-1]
  static const float Pi_2 = Pi/2;
  static const float dPhi = 2*Pi/NPHI;

  // Note the range of phi should be [-Pi/2, 3/4*Pi].
  int iphi;
  int t_iphi = (phi + Pi_2)/dPhi;
  if(t_iphi<0)             iphi = 0;
  else if(t_iphi>= NPHI-1) iphi = NPHI-1;
  else                     iphi = t_iphi;
  return iphi;
}

inline int SvxClusterContainer::calc_iz(int sublayer, float z)
{
  //layer2 <--> sublayer 2,3,4
  //layer3 <--> sublayer 5,6,7
  static const float Zmax[SVXMAXSUBLAYER+1]={Zmax0,
				       Zmax1,
				       Zmax2,Zmax2,Zmax2,
				       Zmax3,Zmax3,Zmax3};
  static const float dZ[SVXMAXSUBLAYER+1]={2*Zmax0/NZ,
				     2*Zmax1/NZ,
				     2*Zmax2/NZ,2*Zmax2/NZ,2*Zmax2/NZ,
				     2*Zmax3/NZ,2*Zmax3/NZ,2*Zmax3/NZ};

  if(sublayer<0 || sublayer>SVXMAXSUBLAYER) return 0;
  int iz   = (z +Zmax[sublayer])/dZ[sublayer];
  
  if(iz<0) iz=0;
  if(iz>=NZ-1) iz=NZ-1;
  return iz;
}

inline int SvxClusterContainer::get_sublayer(int layer, int ladder)
{
  if ( layer<2 ) {
    return layer;
  } else if ( layer==2 ) {
    if ( ladder<8 ) {
      if( ladder%3==1 )       { return 2; }
      else if ( ladder%3==0 ) { return 3; }
      else                    { return 4; }
    } else {
      if( ladder%3==2 )       { return 2; }
      else if ( ladder%3==0 ) { return 3; }
      else                    { return 4; }
    }
  } else {
    if ( ladder<12 ) {
      if( ladder%3==0 )       { return 5; }
      else if ( ladder%3==1 ) { return 6; }
      else                    { return 7; }
    } else {
      if( ladder%3==2 )       { return 5; }
      else if ( ladder%3==1 ) { return 6; }
      else                    { return 7; }
    }
  }
}

int SvxClusterContainer::find_clusters(vector<SvxCluster*> &vcluster,
				       int sublayer)
{
  // select clusters in "sublayer" and store them in "vcluster"
  // input:
  //   sublayer:   sublayer ID (0,1,...7)
  // output
  //   vcluster:  vector of selected SvxCluster*
  // return value: # of clusters selected

  if(sublayer<0 || sublayer>SVXMAXSUBLAYER) return 0; // invalid sublayer

  vcluster.clear();  // reset output vector
  vcluster.assign(dv_cluster0[sublayer].begin(),dv_cluster0[sublayer].end());
  return vcluster.size();
}

int SvxClusterContainer::find_clusters(vector<SvxCluster*> &vcluster,
				       int sublayer,
				       float phi0, float dphi)
{
  // select hits in layer "layer" and phi ~ phi0 (+/-dphi)
  // and store them in vcluster
  // input:
  //   sublayer:   sublayer ID (0,1,...7)
  //   phi0 :   center of the selection phi angle (radian)
  //   dphi:    approximate width of phi selection angle (radian)
  // output
  //   vcluster:  vector of selected SvxCluster*
  // return value: # of clusters selected

  if(sublayer<0 || sublayer>SVXMAXSUBLAYER) return 0; // invalid sublayer

  vcluster.clear();  // reset ouput vector

  //calculate the range of iphi. search hits in iphi1<=iphi<iphi2
  if(dphi<0) dphi=0;
  int iphi1 = calc_iphi(phi0-dphi);
  int iphi2 = calc_iphi(phi0+dphi);

  // copy hits in [iphi1,iphi2] in output vector vcluster
  for(int iphi=iphi1; iphi<= iphi2; iphi++) {
    vcluster.insert(vcluster.end(),
		    dv_cluster1[sublayer][iphi].begin(),
		    dv_cluster1[sublayer][iphi].end());
  }
  return vcluster.size();
}

int SvxClusterContainer::find_clusters(vector<SvxCluster*> &vcluster,
				       int sublayer,
				       float phi0, float dphi,
				       float z0,   float dz)
{
  // select hits in layer "layer" and phi ~ phi0 (+/-dphi) and z ~ z0 (+/-dz)
  // and store them in vcluster
  // input:
  //   sublayer:   sublayer ID (0,1,...7)
  //   phi0 :   center of the selection phi angle (radian)
  //   dphi:    approximate width of phi selection angle (radian)
  //   z0  :    center of the selection z (cm)
  //   dz  :    approximate width of z selection range (cm)
  // output
  //   vcluster:  vector of selected SvxCluster*
  // return value: # of clusters selected

  if(sublayer<0 || sublayer>SVXMAXSUBLAYER) return 0; // invalid sublayer

  vcluster.clear();  // reset ouput vector

  //calculate the range for iphi.
  if(dphi<0) dphi=0;
  int iphi1 = calc_iphi(phi0-dphi);
  int iphi2 = calc_iphi(phi0+dphi);

  //calcuate the range for iz
  int iz1 = calc_iz(sublayer,z0-dz);
  int iz2 = calc_iz(sublayer,z0+dz);

  // copy hits in [iphi1,iphi2] and [iz1,iz2] in output vector vcluster
  for(int iphi=iphi1; iphi<=iphi2; iphi++) {
    for(int iz=iz1;iz<=iz2;iz++) {
      vcluster.insert(vcluster.end(),
		      dv_cluster2[sublayer][iphi][iz].begin(),
		      dv_cluster2[sublayer][iphi][iz].end());
    }
  }
  return vcluster.size();
}


int SvxClusterContainer::find_fake_clusters(vector<SvxCluster*> &vcluster,
					    int sublayer,
					    float phi0, float dphi)
{
  // select fake hits in layer "layer" and phi ~ phi0 (+/-dphi)
  // and store them in vcluster
  // input:
  //   sublayer:   sublayer ID (0,1,...7)
  //   phi0 :   center of the selection phi angle (radian)
  //   dphi:    approximate width of phi selection angle (radian)
  // output
  //   vcluster:  vector of selected SvxCluster*
  // return value: # of clusters selected

  if(sublayer<0 || sublayer>SVXMAXSUBLAYER) return 0; // invalid sublayer

  vcluster.clear();  // reset ouput vector

  //calculate the range of iphi. search hits in iphi1<=iphi<iphi2
  if(dphi<0) dphi=0;
  int iphi1 = calc_iphi(phi0-dphi);
  int iphi2 = calc_iphi(phi0+dphi);

  // copy hits in [iphi1,iphi2] in output vector vcluster
  for(int iphi=iphi1; iphi<= iphi2; iphi++) {
    vcluster.insert(vcluster.end(),
		    dv_f_cluster1[sublayer][iphi].begin(),
		    dv_f_cluster1[sublayer][iphi].end());
  }
  return vcluster.size();
}

int SvxClusterContainer::find_fake_clusters(vector<SvxCluster*> &vcluster,
					    int sublayer,
					    float phi0, float dphi,
					    float z0,   float dz)
{
  // select fake hits in layer "layer" and phi ~ phi0 (+/-dphi) and z ~ z0 (+/-dz)
  // and store them in vcluster
  // input:
  //   sublayer:   sublayer ID (0,1,...7)
  //   phi0 :   center of the selection phi angle (radian)
  //   dphi:    approximate width of phi selection angle (radian)
  //   z0  :    center of the selection z (cm)
  //   dz  :    approximate width of z selection range (cm)
  // output
  //   vcluster:  vector of selected SvxCluster*
  // return value: # of clusters selected

  if(sublayer<0 || sublayer>SVXMAXSUBLAYER) return 0; // invalid sublayer

  vcluster.clear();  // reset ouput vector

  //calculate the range for iphi.
  if(dphi<0) dphi=0;
  int iphi1 = calc_iphi(phi0-dphi);
  int iphi2 = calc_iphi(phi0+dphi);

  //calcuate the range for iz
  int iz1 = calc_iz(sublayer,z0-dz);
  int iz2 = calc_iz(sublayer,z0+dz);

  // copy hits in [iphi1,iphi2] and [iz1,iz2] in output vector vcluster
  for(int iphi=iphi1; iphi<=iphi2; iphi++) {
    for(int iz=iz1;iz<=iz2;iz++) {
      vcluster.insert(vcluster.end(),
		      dv_f_cluster2[sublayer][iphi][iz].begin(),
		      dv_f_cluster2[sublayer][iphi][iz].end());
    }
  }
  return vcluster.size();
}


// this function gets the cluster list with multiple sublayers specified by sublayerary
int SvxClusterContainer::find_clusters_multi(vector<SvxCluster*> &vcluster, vector<int> &vsublayer, 
                  const int nsublayers, const int* sublayerary,
                  float phi0, float dphi,
                  float z0,   float dz)
{
  if(nsublayers<=0||sublayerary==NULL){ // nothing happen
    return 0;
  }

  vcluster.clear(); // reset
  vsublayer.clear(); // reset
  
  vector<SvxCluster*> vtmp;
  for(int ilay=0; ilay<nsublayers; ilay++){
    int sublayer = sublayerary[ilay];
    find_clusters(vtmp, sublayer, phi0, dphi, z0, dz);
    //cout<<"multi : "<<sublayer<<" "<<vtmp.size()<<endl;

    vcluster.insert(vcluster.end(), vtmp.begin(), vtmp.end());
    vsublayer.insert(vsublayer.end(), vtmp.size(), sublayer);
  }

  return vcluster.size();
}


// this function gets the cluster list for layer(not sublayer);
int SvxClusterContainer::find_clusters_layer(vector<SvxCluster*> &vcluster, vector<int> &vsublayer, 
                  const int layer,
                  float phi0, float dphi,
                  float z0,   float dz)
{

  static const int sublayerary[4][3] = {{0,0,0},{1,0,0},{2,3,4},{5,6,7}};

  int nsublayers  = 0;
  if     (0<=layer&&layer<2){ nsublayers=1; }
  else if(2<=layer&&layer<4){ nsublayers=3; }
  else {
    cout<<"SvxClusterContainer::find_cluster_layer ERROR layer is out of range : "<<layer<<endl;
  }

  return find_clusters_multi(vcluster, vsublayer, nsublayers, sublayerary[layer], phi0, dphi, z0, dz);
}


  // this function gets the cluster for each block (not sublayer)
  //   block   sublayer
  //     7      5,6,7
  //     6      5,6 
  //     5      5   
  //     4      2,3,4
  //     3      2,3
  //     2      2
  //     1      1
  //     0      0
int SvxClusterContainer::find_clusters_block(
                    std::vector<SvxCluster*> &vcluster, std::vector<int>& vsublayer,
                    const int block,
                    float phi0, float dphi,
                    float z0,   float dz)
{

  static const int sublayerary[8][3] = {
    {0,0,0},{1,0,0},{2,0,0},{2,3,0},{2,3,4},{5,0,0},{5,6,0},{5,6,7} 
  };

  int nsublayers  = 0;
  if    ((0<=block&&block<3)||block==5){ nsublayers=1; }
  else if(block==3||block==6){ nsublayers=2; }
  else if(block==4||block==7){ nsublayers=3; }
  else {
    cout<<"SvxClusterContainer::find_cluster_block ERROR block is out of range : "<<block<<endl;
  }

  return find_clusters_multi(vcluster, vsublayer, nsublayers, sublayerary[block], phi0, dphi, z0, dz);
}



int SvxClusterContainer::get_ncluster(void) {
  int sum = 0;
  for(int isub=0;isub<SVXMAXSUBLAYER+1;isub++) {
    sum += get_ncluster(isub);
  }
  return sum;
}

int SvxClusterContainer::get_ncluster(int sublayer) {
  if(sublayer<0 || sublayer>SVXMAXSUBLAYER) return -1; //error
  return dv_cluster0[sublayer].size();
}

void SvxClusterContainer::clear(void)
{
  // clear the contents of dv_clusterI (I=0,1,2)

  // clear dv_cluster0
  for(int i=0;i<SVXMAXSUBLAYER+1;i++) {
    dv_cluster0[i].clear();
    dv_f_cluster0[i].clear();
  }

  // clear dv_cluster1 and dv_cluster2.
  // When one of the bin of dv_clusterI (I=1,2) is used, the pointer
  // to the used bin is stored in dv_usedI. So it is suffice
  // to clear those used bins stored in dv_usedI (I=1,2)
  // Note that dv_used1.size()=dv_used2.size()

  int n1 = dv_used1.size();
  for(int i=0;i<n1;i++) { dv_used1[i]->clear(); }
  int n2 = dv_used2.size();
  for(int i=0;i<n2;i++) { dv_used2[i]->clear(); }
  int fn1 = dv_f_used1.size();
  for(int i=0;i<fn1;i++) { dv_f_used1[i]->clear(); }
  int fn2 = dv_f_used2.size();
  for(int i=0;i<fn2;i++) { dv_f_used2[i]->clear(); }

  dv_used1.clear();
  dv_used2.clear();
  dv_f_used1.clear();
  dv_f_used2.clear();

  d_initialized = false;
}

void SvxClusterContainer::print(void)
{
  cout << "***** BEGIN PRINT ************"<<endl;
  for(int i=0;i<SVXMAXSUBLAYER+1;i++) {
    int nc=dv_cluster0[i].size();
    cout << "sublayer "<<i<< " ncluster="<<nc<<endl;    
    for(int j=0;j<nc;j++) {
      SvxCluster *cluster = dv_cluster0[i][j];
      float x = cluster->get_xyz_global(0);
      float y = cluster->get_xyz_global(1);
      float z = cluster->get_xyz_global(2);
      float r = sqrt(x*x+y*y);
      float phi = calc_phi(x,y);
      cout << "(" << x <<","<<y<<","<<z<<") "<<phi<<" "<<z<<" "<<z/r<<endl;
    }    
    cout << endl;
  }
  cout <<"*** END PRINT ******"<<endl;
}

void SvxClusterContainer::set_beam_center(float xcenter, float ycenter)
{
  d_xcenter = xcenter;
  d_ycenter = ycenter;

  if(m_verbosity>5){
    cout<<" SvxClusterContainer:: set_beam_center  x-y : "<<d_xcenter<<", "<<d_ycenter<<endl;
  }
}

void SvxClusterContainer::get_beam_center(float &xcenter, float &ycenter)
{
  xcenter = d_xcenter;
  ycenter = d_ycenter;
}

float SvxClusterContainer::calc_phi(float x, float y)
{
  // calculate phi in the range [-M_PI/2, 3/4*M_PI].

  float phi;
  if (x==d_xcenter) {
    return 0.;
  } else {
    if(x>0) phi = atan((y - d_ycenter)/(x - d_xcenter));
    else phi = atan((y - d_ycenter)/(x - d_xcenter)) + M_PI;
    
    return phi;
  }
}
