#include "DumpPHMuoTracksOut.h"

#include <PHMuoTracksOut.h>

#include <PHIODataNode.h>


#include <string>

using namespace std;

typedef PHIODataNode<PHMuoTracksOut> MyNode_t;

DumpPHMuoTracksOut::DumpPHMuoTracksOut(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPHMuoTracksOut::process_Node(PHNode *myNode)
{
  PHMuoTracksOut *phmuotracks = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      phmuotracks = thisNode->getData();
    }
  if (phmuotracks)
    {
      phmuotracks->ShutUp();
      *fout << "PHMUO get_PHMuoNTracks(): " << phmuotracks->get_npart() << endl;
      *fout << "dimuons: phmuotracks->" << phmuotracks->get_ndimu() << endl;
      for (unsigned int i = 0; i < phmuotracks->get_npart(); i++)
        {
          *fout << "get_uid(" << i << "): " << phmuotracks->get_uid(i) << endl;
          *fout << "get_PID(" << i << "): " << phmuotracks->get_PID(i) << endl;
          *fout << "get_charge(" << i << "): " << phmuotracks->get_charge(i) << endl;
          *fout << "get_nhits(" << i << "): " << phmuotracks->get_nhits(i) << endl;
          *fout << "get_MuonConfidence(" << i << "): " << phmuotracks->get_MuonConfidence(i) << endl;
          *fout << "get_PionConfidence(" << i << "): " << phmuotracks->get_PionConfidence(i) << endl;
          *fout << "get_chisquare(" << i << "): " << phmuotracks->get_chisquare(i) << endl;
          *fout << "get_ghostflag(" << i << "): " << phmuotracks->get_ghostflag(i) << endl;
          *fout << "get_hitplans(" << i << "): " << phmuotracks->get_hitplans(i) << endl;
          *fout << "get_muTRhits(" << i << "): " << phmuotracks->get_muTRhits(i) << endl;
          *fout << "get_muIDhits(" << i << "): " << phmuotracks->get_muIDhits(i) << endl;
          *fout << "get_TMutTrk_status(" << i << "): " << phmuotracks->get_TMutTrk_status(i) << endl;
        }
      for ( int i = 0; i < phmuotracks->get_ndimu(); i++)
        {
          *fout << "get_dimass(" << i << "): " << phmuotracks->get_dimass(i) << endl;
          *fout << "get_dicharge(" << i << "): " << phmuotracks->get_dicharge(i) << endl;
          *fout << "get_dipx(" << i << "): " << phmuotracks->get_dipx(i) << endl;
          *fout << "get_dipy(" << i << "): " << phmuotracks->get_dipy(i) << endl;
          *fout << "get_dipz(" << i << "): " << phmuotracks->get_dipz(i) << endl;
          *fout << "get_vtx_bp_xpos(" << i << "): " << phmuotracks->get_vtx_bp_xpos(i) << endl;
          *fout << "get_vtx_bp_ypos(" << i << "): " << phmuotracks->get_vtx_bp_ypos(i) << endl;
          *fout << "get_vtx_bp_zpos(" << i << "): " << phmuotracks->get_vtx_bp_zpos(i) << endl;
          *fout << "get_vtx_chrg_1(" << i << "): " << phmuotracks->get_vtx_chrg_1(i) << endl;
          *fout << "get_vtx_px_1(" << i << "): " << phmuotracks->get_vtx_px_1(i) << endl;
          *fout << "get_vtx_py_1(" << i << "): " << phmuotracks->get_vtx_py_1(i) << endl;
          *fout << "get_vtx_pz_1(" << i << "): " << phmuotracks->get_vtx_pz_1(i) << endl;
          *fout << "get_vtx_chrg_2(" << i << "): " << phmuotracks->get_vtx_chrg_2(i) << endl;
          *fout << "get_vtx_px_2(" << i << "): " << phmuotracks->get_vtx_px_2(i) << endl;
          *fout << "get_vtx_py_2(" << i << "): " << phmuotracks->get_vtx_py_2(i) << endl;
          *fout << "get_vtx_pz_2(" << i << "): " << phmuotracks->get_vtx_pz_2(i) << endl;
          *fout << "get_vtx_chisquare(" << i << "): " << phmuotracks->get_vtx_chisquare(i) << endl;
        }
    }
  return 0;
}

