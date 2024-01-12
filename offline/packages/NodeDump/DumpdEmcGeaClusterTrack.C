//INCLUDECHECKER: Removed this line: #include <fstream>
#include <string>

#include "PHIODataNode.h"
//INCLUDECHECKER: Removed this line: #include "PHNode.h"

#include "dEmcGeaClusterTrackWrapper.h"

//INCLUDECHECKER: Removed this line: #include "PHNodeDump.h"
#include "DumpdEmcGeaClusterTrack.h"

using namespace std;

typedef PHIODataNode<dEmcGeaClusterTrackWrapper> MyNode_t;

DumpdEmcGeaClusterTrack::DumpdEmcGeaClusterTrack(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpdEmcGeaClusterTrack::process_Node(PHNode *myNode)
{
  dEmcGeaClusterTrackWrapper *demcgeaclustertrack = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      demcgeaclustertrack = thisNode->getData();
    }
  if (demcgeaclustertrack)
    {
      *fout << "RowCount(): " << demcgeaclustertrack->RowCount() << endl;
      for (unsigned int i = 0; i < demcgeaclustertrack->RowCount(); i++)
        {
          *fout << "get_id(" << i << "): " << demcgeaclustertrack->get_id(i) << endl;
          *fout << "get_clusid(" << i << "): " << demcgeaclustertrack->get_clusid(i) << endl;
          *fout << "get_evno(" << i << "): " << demcgeaclustertrack->get_evno(i) << endl;
          *fout << "get_keycent(" << i << "): " << demcgeaclustertrack->get_keycent(i) << endl;
          *fout << "get_input(" << i << "): " << demcgeaclustertrack->get_input(i) << endl;
          *fout << "get_type(" << i << "): " << demcgeaclustertrack->get_type(i) << endl;
          *fout << "get_arm(" << i << "): " << demcgeaclustertrack->get_arm(i) << endl;
          *fout << "get_sector(" << i << "): " << demcgeaclustertrack->get_sector(i) << endl;
          *fout << "get_mease(" << i << "): " << demcgeaclustertrack->get_mease(i) << endl;
          *fout << "get_ecore(" << i << "): " << demcgeaclustertrack->get_ecore(i) << endl;
          *fout << "get_tof(" << i << "): " << demcgeaclustertrack->get_tof(i) << endl;
          *fout << "get_etof(" << i << "): " << demcgeaclustertrack->get_etof(i) << endl;
          *fout << "get_tofmin(" << i << "): " << demcgeaclustertrack->get_tofmin(i) << endl;
          *fout << "get_etofmin(" << i << "): " << demcgeaclustertrack->get_etofmin(i) << endl;
          *fout << "get_tofmax(" << i << "): " << demcgeaclustertrack->get_tofmax(i) << endl;
          *fout << "get_etofmax(" << i << "): " << demcgeaclustertrack->get_etofmax(i) << endl;
          *fout << "get_twrhit(" << i << "): " << demcgeaclustertrack->get_twrhit(i) << endl;
          *fout << "get_charged(" << i << "): " << demcgeaclustertrack->get_charged(i) << endl;
          *fout << "get_chi2_sh(" << i << "): " << demcgeaclustertrack->get_chi2_sh(i) << endl;
          *fout << "get_prob_photon_sh(" << i << "): " << demcgeaclustertrack->get_prob_photon_sh(i) << endl;

          for (short j = 0;j < 3;j++)
            {
              *fout << "get_trkno(" << j << "," << i << "): " << demcgeaclustertrack->get_trkno(j, i) << endl;
              *fout << "get_tracktwrhit(" << j << "," << i << "): " << demcgeaclustertrack->get_tracktwrhit(j, i) << endl;
              *fout << "get_edep_nom(" << j << "," << i << "): " << demcgeaclustertrack->get_edep_nom(j, i) << endl;
              *fout << "get_pid(" << j << "," << i << "): " << demcgeaclustertrack->get_pid(j, i) << endl;
              *fout << "get_ptot(" << j << "," << i << "): " << demcgeaclustertrack->get_ptot(j, i) << endl;
              *fout << "get_ancestry(" << j << "," << i << "): " << demcgeaclustertrack->get_ancestry(j, i) << endl;
              *fout << "get_edep(" << j << "," << i << "): " << demcgeaclustertrack->get_edep(j, i) << endl;
              *fout << "get_efrac(" << j << "," << i << "): " << demcgeaclustertrack->get_efrac(j, i) << endl;
              *fout << "get_measxyz(" << j << "," << i << "): " << demcgeaclustertrack->get_measxyz(j, i) << endl;
              *fout << "get_pc3proj(" << j << "," << i << "): " << demcgeaclustertrack->get_pc3proj(j, i) << endl;
              for (short k = 0;k < 3;k++)
                {
                  *fout << "get_vertex(" << k << "," << j << "," << i << "): " << demcgeaclustertrack->get_vertex(k, j, i) << endl;
                  *fout << "get_xyz(" << k << "," << j << "," << i << "): " << demcgeaclustertrack->get_xyz(k, j, i) << endl;
                }
            }
          for (short j = 0;j < 2;j++)
            {
              *fout << "get_disp(" << j << "," << i << "): " << demcgeaclustertrack->get_disp(j, i) << endl;
              *fout << "get_padisp(" << j << "," << i << "): " << demcgeaclustertrack->get_padisp(j, i) << endl;
              *fout << "get_e_sh(" << j << "," << i << "): " << demcgeaclustertrack->get_e_sh(j, i) << endl;
            }
          for (short j = 0;j < 8;j++)
            {
              *fout << "get_partesum(" << j << "," << i << "): " << demcgeaclustertrack->get_partesum(j, i) << endl;
              *fout << "get_chglist(" << j << "," << i << "): " << demcgeaclustertrack->get_chglist(j, i) << endl;
            }
        }
    }
  return 0;
}

