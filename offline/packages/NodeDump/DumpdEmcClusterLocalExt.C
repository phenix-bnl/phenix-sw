//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dEmcClusterLocalExtWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdEmcClusterLocalExt.h"

using namespace std;

typedef PHIODataNode<dEmcClusterLocalExtWrapper> MyNode_t;

DumpdEmcClusterLocalExt::DumpdEmcClusterLocalExt(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdEmcClusterLocalExt::process_Node(PHNode *myNode)
{
  dEmcClusterLocalExtWrapper *demccluster = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      demccluster = thisNode->getData();
    }
  if (demccluster)
    {
      *fout << "RowCount(): " << demccluster->RowCount() << endl;
      for (unsigned int i = 0; i < demccluster->RowCount(); i++)
        {
	  *fout << "get_id: " << demccluster->get_id(i) << endl;
	  *fout << "get_runno: " << demccluster->get_runno(i) << endl;
	  *fout << "get_evno: " << demccluster->get_evno(i) << endl;
	  *fout << "get_clusno: " << demccluster->get_clusno(i) << endl;
	  *fout << "get_method: " << demccluster->get_method(i) << endl;
	  *fout << "get_type: " << demccluster->get_type(i) << endl;
	  *fout << "get_arm: " << demccluster->get_arm(i) << endl;
	  *fout << "get_sector: " << demccluster->get_sector(i) << endl;
	  *fout << "get_nsh: " << demccluster->get_nsh(i) << endl;
	  *fout << "get_twrhit: " << demccluster->get_twrhit(i) << endl;

	  *fout << "get_warnmap: " << demccluster->get_warnmap(i) << endl;
	  *fout << "get_deadmap: " << demccluster->get_deadmap(i) << endl;

	  *fout << "get_e: " << demccluster->get_e(i) << endl;
	  *fout << "get_ecore: " << demccluster->get_ecore(i) << endl;
	  *fout << "get_ecorr: " << demccluster->get_ecorr(i) << endl;
	  *fout << "get_ecent: " << demccluster->get_ecent(i) << endl;
	  *fout << "get_de: " << demccluster->get_de(i) << endl;
	  *fout << "get_tof: " << demccluster->get_tof(i) << endl;
	  *fout << "get_dtof: " << demccluster->get_dtof(i) << endl;
	  *fout << "get_qual: " << demccluster->get_qual(i) << endl;
	  *fout << "get_e9: " << demccluster->get_e9(i) << endl;
	  *fout << "get_pid: " << demccluster->get_pid(i) << endl;
	  *fout << "get_phi: " << demccluster->get_phi(i) << endl;
	  *fout << "get_theta: " << demccluster->get_theta(i) << endl;
	  *fout << "get_prob_neuhad: " << demccluster->get_prob_neuhad(i) << endl;
	  *fout << "get_re9: " << demccluster->get_re9(i) << endl;
	  *fout << "get_tofcorr: " << demccluster->get_tofcorr(i) << endl;
	  *fout << "get_tofmin: " << demccluster->get_tofmin(i) << endl;
	  *fout << "get_tofmean: " << demccluster->get_tofmean(i) << endl;
	  *fout << "get_tofmax: " << demccluster->get_tofmax(i) << endl;
	  *fout << "get_etofmin: " << demccluster->get_etofmin(i) << endl;
	  *fout << "get_etofmax: " << demccluster->get_etofmax(i) << endl;
	  *fout << "get_tofmincorr: " << demccluster->get_tofmincorr(i) << endl;
	  *fout << "get_tofmaxcorr: " << demccluster->get_tofmaxcorr(i) << endl;
	  *fout << "get_chi2_sh: " << demccluster->get_chi2_sh(i) << endl;
	  *fout << "get_prob_photon_sh: " << demccluster->get_prob_photon_sh(i) << endl;
	  *fout << "get_chi2: " << demccluster->get_chi2(i) << endl;
	  *fout << "get_prob_photon: " << demccluster->get_prob_photon(i) << endl;
	  for (short int j = 0;j<2;j++)
	    {
	      *fout << "get_ind(" << j << "," << i << "): " << demccluster->get_ind(j,i) << endl;
	      *fout << "get_e_sh(" << j << "," << i << "): " << demccluster->get_e_sh(j,i) << endl;
	      *fout << "get_de_sh(" << j << "," << i << "): " << demccluster->get_de_sh(j,i) << endl;
	      *fout << "get_ecorr_sh(" << j << "," << i << "): " << demccluster->get_ecorr_sh(j,i) << endl;
	      *fout << "get_disp(" << j << "," << i << "): " << demccluster->get_disp(j,i) << endl;
	      *fout << "get_padisp(" << j << "," << i << "): " << demccluster->get_padisp(j,i) << endl;
	      *fout << "get_yz_cg(" << j << "," << i << "): " << demccluster->get_yz_cg(j,i) << endl;
	      for (short int k=0;k<2;k++)
		{
		  *fout << "get_xyz_sh(" << k << "," << j << "," << i << "): " << demccluster->get_xyz_sh(k,j,i) << endl;
		  *fout << "get_dxyz_sh(" << k << "," << j << "," << i << "): " << demccluster->get_dxyz_sh(k,j,i) << endl;
		}
	    }
	  for (short int j = 0;j<3;j++)
	    {
	      *fout << "get_unitv(" << j << "," << i << "): " << demccluster->get_unitv(j,i) << endl;
	      *fout << "get_xyz(" << j << "," << i << "): " << demccluster->get_xyz(j,i) << endl;
	      *fout << "get_dxyz(" << j << "," << i << "): " << demccluster->get_dxyz(j,i) << endl;
	    }
	  for (short int j = 0;j<16;j++)
	    {
	      *fout << "get_partesum(" << j << "," << i << "): " << demccluster->get_partesum(j,i) << endl;
	      *fout << "get_twrlist(" << j << "," << i << "): " << demccluster->get_twrlist(j,i) << endl;
	    }
        }
    }
  return 0;
}

