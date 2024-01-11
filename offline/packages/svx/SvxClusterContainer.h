#ifndef __SVXCLUSTERCONTAINER_H__
#define __SVXCLUSTERCONTAINER_H__


#include <vector>
#include "SvxParameters.h"

#include <cmath> // for M_PI definition

class SvxCluster;
class SvxClusterList;

//////////////
// modify : remove east_offset. Takashi Hachiya. 2011/Dec/11


class SvxClusterContainer {
  //
  // store all clusters in clusterlist into this container.
  // the clusters are stored sub-layers in the following three arrays for
  // quicker access.
  // Sub-layers    0 <--> Layer 0 (pixel)
  //               1 <--> Layer 1 (pixel)
  //           2,3,4 <--> Layer 2 (strip) 
  //           5,6,7 <--> Layer 3 (strip) 
  //
  // usage:
  //  void load_clusters(SvxClusterList *clusterlist);
  //       load all clusters in clusterlist into this container
  //
  //  int find_clusters(std::vector<SvxCluster*> &vcluster, int sublayer,
  //		    float phi0, float dphi,
  //		    float z0,   float dz);
  //  find all clusters in [phi0-dphi,phi0+dphi]x[z0-dz,z0+dz] in sublayer
  //  and store them in vcluster
  //
 public:
// SL: this is now moved into SvxParameters.h because of a root bug
// and renamed into SVXMAXSUBLAYER
//  static const int MAXLEVEL=7;  // maximum index of sublayer
  static const float Zmax0;  //max Z value for layer 0 (pixel) // value is set in the cc file
  static const float Zmax1;  //max Z value for layer 1 (pixel) 
  static const float Zmax2;  //max Z value for layer 2 (strip)
  static const float Zmax3;  //max Z value for layer 3 (strip)

  SvxClusterContainer();
  ~SvxClusterContainer(){}
  void load_clusters(SvxClusterList *clusterlist);
  void load_clusters(std::vector<SvxCluster*> &clusterlist);
  void load_fake_clusters(SvxClusterList *clusterlist);
  int find_clusters(std::vector<SvxCluster*> &vcluster, int sublayer);
  int find_clusters(std::vector<SvxCluster*> &vcluster, int sublayer,
		    float phi, float dphi);
  int find_clusters(std::vector<SvxCluster*> &vcluster, int sublayer,
		    float phi0, float dphi,
		    float z0,   float dz);
  int find_fake_clusters(std::vector<SvxCluster*> &vcluster, int sublayer,
			 float phi, float dphi);
  int find_fake_clusters(std::vector<SvxCluster*> &vcluster, int sublayer,
			 float phi0, float dphi,
			 float z0,   float dz);

  // this function gets the cluster list with multiple sublayers specified by sublayerary
  int find_clusters_multi(std::vector<SvxCluster*> &vcluster, std::vector<int> &vsublayer, 
                    const int nsublayers, const int* sublayerary,
		    float phi0, float dphi,
		    float z0,   float dz);
  // this function gets the cluster for each layer (not sublayer)
  int find_clusters_layer(std::vector<SvxCluster*> &vcluster, std::vector<int> &vsublayer, 
                    const int layer, 
		    float phi0, float dphi,
		    float z0,   float dz);

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
  int find_clusters_block(std::vector<SvxCluster*> &vcluster, std::vector<int> &vsublayer,
                    const int block, 
		    float phi0, float dphi,
		    float z0,   float dz);

  int get_ncluster(void);
  int get_ncluster(int sublayer);

  void clear(void);
  void print(void);
  bool is_initialized() { return d_initialized; }

  float calc_phi(float x, float y);

  void set_beam_center(float xcenter, float ycenter);
  void get_beam_center(float &xcenter, float &ycenter);

  void verbosity(const int value){ m_verbosity=value;}

  //  void Reset(){}

  
 private:
  /* private helper functions*/
  int calc_iphi(float phi);
  int calc_iz(int sublayer, float z);
  int get_sublayer(int layer, int ladder);

  /* constant parameters */
  static const int NPHI = 200;
  static const int NZ   = 200;

  /* data members*/
// SL: this is now moved into SvxParameters.h because of a root bug
// and renamed into SVXMAXSUBLAYER
  std::vector<SvxCluster*> dv_cluster0[SVXMAXSUBLAYER+1];
  std::vector<SvxCluster*> dv_cluster1[SVXMAXSUBLAYER+1][NPHI];
  std::vector<SvxCluster*> dv_cluster2[SVXMAXSUBLAYER+1][NPHI][NZ];
  std::vector<std::vector<SvxCluster*>*> dv_used1;
  std::vector<std::vector<SvxCluster*>*> dv_used2;
  std::vector<SvxCluster*> dv_f_cluster0[SVXMAXSUBLAYER+1];
  std::vector<SvxCluster*> dv_f_cluster1[SVXMAXSUBLAYER+1][NPHI];
  std::vector<SvxCluster*> dv_f_cluster2[SVXMAXSUBLAYER+1][NPHI][NZ];
  std::vector<std::vector<SvxCluster*>*> dv_f_used1;
  std::vector<std::vector<SvxCluster*>*> dv_f_used2;

  // (x,y) of the beam spot center
  float d_xcenter;
  float d_ycenter;

  // if d_initialized=true, SvxClusters are already filled in the container.
  // but if not, SvxClusters should be loaded before you use.
  bool d_initialized;

  int m_verbosity;

};

#endif
