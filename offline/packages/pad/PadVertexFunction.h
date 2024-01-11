#ifndef __PADVERTEXFUNCTION_HH__
#define __PADVERTEXFUNCTION_HH__

/*
Author: Charles F. Maguire
Initial release: July 30, 2001

Purpose: Utility function to return the Z0 based on the PC1, PC2, and PC3 cluster information

Sample use in reading a DST file (assume that the topNode pointer is defined)

        double z0PC;
	double z0PCerr;
        Int_t fault;
	fault = 0; // not verbose
        PadVertexFunction(topNode, &z0PC, &z0PCerr, &fault);

-------------

Revision: Apr 2004; David S. 
change input from pad cluster pointers to topNode;
also start using EMC hits to help beat down the combinatorics, to try 
and make it work for field-on too
- and turned this code into a class.
- and skipped using PC2; just do two attempts with PC1/3; one per arm
*/

#include <iostream>
#include <vector>
#include "PHPoint.h"

class TH1F;
class PHCompositeNode;
class dPadClusterWrapper;
class emcClusterContainer;

const int NARMS = 2;

class PadVertexFunction 
{
 public:

  PadVertexFunction();
  
  //! destructor
  virtual ~PadVertexFunction();
  
  int event(PHCompositeNode* topNode,
	    double *z0PC, double *z0PCerr,
	    int *fault);
  
  void set_match_limit_z(float val) { _match_limit_z = val; }
  void set_match_limit_phi(float val) { _match_limit_phi = val; }
  void set_match_limit_x(float val) { _match_limit_x = val; }
  void set_match_limit_y(float val) { _match_limit_y = val; }
  void set_sigma_dphi(float val) { _sigma_dphi = val; }
  void set_sigma_dz(float val) { _sigma_dz = val; }
  void set_min_cluster_energy(float val) { _min_cluster_energy = val; }

  void set_max_pc3_hits(int val) { _max_pc3_hits = val; }

  void set_n_search_bins(int val) { _n_search_bins = val; }

  void set_min_vtx_error(float val) { _min_vtx_error = val; }
  void set_max_vtx_error(float val) { _max_vtx_error = val; }

 protected:

  int Init();

  void InitPars();
  void PrintPars(std::ostream& os = std::cout) const;

  void find_pctrk(dPadClusterWrapper *dPc1Cluster,
		  dPadClusterWrapper *dPc3Cluster,
		  emcClusterContainer* fClusters);

  void remove_duplicate();

  void find_vertex(double *z0PC, double *z0PCerr,
		   int *fault);
  int verbose;
 private:

  class pctrk {
    // structure of a track constructed from
    // EMC and PC1, and checked at PC3
  public:
    pctrk(float Dz, float Dphi,
	  PHPoint &Pc1xyz, PHPoint &Pc3xyz, PHPoint &Emcxyz,
	  int Ipc1, int Ipc3, int Iemc, float Ecore)
      :dz(Dz), dphi(Dphi),
      pc1xyz(Pc1xyz),pc3xyz(Pc3xyz),emcxyz(Emcxyz),
      ipc1(Ipc1),ipc3(Ipc3),iemc(Iemc),
      ecore(Ecore), ghost(false) {}
    virtual ~pctrk() {}
    void print(std::ostream& os = std::cout) {
      os << " pc1 " << pc1xyz << std::endl
	 << " pc3 " << pc3xyz << std::endl
	 << " emc " << emcxyz << std::endl;
      os << " ipc1 " << ipc1 << std::endl
	 << " ipc3 " << ipc3 << std::endl
	 << " iemc " << iemc << std::endl
	 << " ecore " << ecore << std::endl;
      os << " dz " << dz
	 << " dphi " << dphi << std::endl;
      if (ghost) os << " GHOST " << std::endl;
    }

    float dz;     // dz at pc3
    float dphi;
    PHPoint pc1xyz;
    PHPoint pc3xyz;
    PHPoint emcxyz;
    int   ipc1;
    int   ipc3;
    int   iemc;
    float ecore;
    bool ghost;
  };
  
  std::vector<pctrk> vpctrk;

  TH1F *h1PC1PC3;  

  // cuts of various sorts
  float _match_limit_z;
  float _match_limit_phi;
  float _match_limit_x;
  float _match_limit_y;

  // for evaluation at PC3 
  float _sigma_dphi;
  float _sigma_dz;

  float _min_cluster_energy;

  int _n_search_bins;

  int _max_pc3_hits;

  float _min_vtx_error;
  float _max_vtx_error;
};

#endif
