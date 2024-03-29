#include <DumpPHCentralTrackv24.h>

#include <PHCentralTrackv24.h>
#include <PHSnglCentralTrackv24.h>

#include <PHIODataNode.h>


#include <string>

using namespace std;

typedef PHIODataNode<PHCentralTrackv24> MyNode_t;

DumpPHCentralTrackv24::DumpPHCentralTrackv24(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpPHCentralTrackv24::process_Node(PHNode *myNode)
{
  PHCentralTrackv24 *phcentraltrack = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      phcentraltrack = thisNode->getData();
    }
  if (phcentraltrack && phcentraltrack->isValid())
    {
      phcentraltrack->ShutUp();
      *fout << "PHCENTRAL get_PHCentralNTrack(): " << phcentraltrack->get_npart() << endl;
      for (unsigned int i = 0; i < phcentraltrack->get_npart(); i++)
        {
          PHSnglCentralTrackv24 *sngl = (PHSnglCentralTrackv24 *) phcentraltrack->get_track(i);
          sngl->ShutUp();
          *fout << "phparticle->get_px(" << i << "): " << sngl->get_px() << endl;
          *fout << "phparticle->get_py(" << i << "): " << sngl->get_py() << endl;
          *fout << "phparticle->get_pz(" << i << "): " << sngl->get_pz() << endl;
          *fout << "phparticle->get_E(" << i << "): " << sngl->get_E() << endl;
          *fout << "phparticle->get_charge(" << i << "): " << sngl->get_charge() << endl;
          *fout << "phparticle->get_PID(" << i << "): " << sngl->get_PID() << endl;

          *fout << "phcentraltrack->get_quality(" << i << "): " << sngl->get_quality() << endl;
          *fout << "phcentraltrack->get_zed(" << i << "): " << sngl->get_zed() << endl;
          *fout << "phcentraltrack->get_phi(" << i << "): " << sngl->get_phi() << endl;
          *fout << "phcentraltrack->get_alpha(" << i << "): " << sngl->get_alpha() << endl;
          *fout << "phcentraltrack->get_beta(" << i << "): " << sngl->get_beta() << endl;
          *fout << "phcentraltrack->get_phi0(" << i << "): " << sngl->get_phi0() << endl;
          *fout << "phcentraltrack->get_the0(" << i << "): " << sngl->get_the0() << endl;
          *fout << "phcentraltrack->get_mom(" << i << "): " << sngl->get_mom() << endl;
          *fout << "phcentraltrack->get_mompx(" << i << "): " << sngl->get_mompx() << endl;
          *fout << "phcentraltrack->get_mompy(" << i << "): " << sngl->get_mompy() << endl;
          *fout << "phcentraltrack->get_mompz(" << i << "): " << sngl->get_mompz() << endl;
//           *fout << "phcentraltrack->get_status(" << i << "): " << sngl->get_status() << endl;
//           *fout << "phcentraltrack->get_alpha1(" << i << "): " << sngl->get_alpha1() << endl;
//           *fout << "phcentraltrack->get_alpha2(" << i << "): " << sngl->get_alpha2() << endl;
//           *fout << "phcentraltrack->get_nx1hits(" << i << "): " << sngl->get_nx1hits() << endl;
//           *fout << "phcentraltrack->get_nx2hits(" << i << "): " << sngl->get_nx2hits() << endl;
//           *fout << "phcentraltrack->get_mx1dist(" << i << "): " << sngl->get_mx1dist() << endl;
//           *fout << "phcentraltrack->get_mx2dist(" << i << "): " << sngl->get_mx2dist() << endl;
          *fout << "phcentraltrack->get_chi2x1(" << i << "): " << sngl->get_chi2x1() << endl;
          *fout << "phcentraltrack->get_chi2x2(" << i << "): " << sngl->get_chi2x2() << endl;
          *fout << "phcentraltrack->get_nx1x2fit(" << i << "): " << sngl->get_nx1x2fit() << endl;
          *fout << "phcentraltrack->get_mchi2(" << i << "): " << sngl->get_mchi2() << endl;
          *fout << "phcentraltrack->get_error(" << i << "): " << sngl->get_error() << endl;
          *fout << "phcentraltrack->get_alphaf(" << i << "): " << sngl->get_alphaf() << endl;
          *fout << "phcentraltrack->get_pc1id(" << i << "): " << sngl->get_pc1id() << endl;
          *fout << "phcentraltrack->get_pc2id(" << i << "): " << sngl->get_pc2id() << endl;
          *fout << "phcentraltrack->get_pc3id(" << i << "): " << sngl->get_pc3id() << endl;
          *fout << "phcentraltrack->get_emcid(" << i << "): " << sngl->get_emcid() << endl;
          *fout << "phcentraltrack->get_tofeid(" << i << "): " << sngl->get_tofeid() << endl;
          *fout << "phcentraltrack->get_tecid(" << i << "): " << sngl->get_tecid() << endl;
          *fout << "phcentraltrack->get_mrpcid(" << i << "): " << sngl->get_mrpcid() << endl;
          *fout << "phcentraltrack->get_spc1id(" << i << "): " << sngl->get_spc1id() << endl;
          *fout << "phcentraltrack->get_spc2id(" << i << "): " << sngl->get_spc2id() << endl;
          *fout << "phcentraltrack->get_spc3id(" << i << "): " << sngl->get_spc3id() << endl;
          *fout << "phcentraltrack->get_semcid(" << i << "): " << sngl->get_semcid() << endl;
          *fout << "phcentraltrack->get_stofeid(" << i << "): " << sngl->get_stofeid() << endl;
          *fout << "phcentraltrack->get_stecid(" << i << "): " << sngl->get_stecid() << endl;
          *fout << "phcentraltrack->get_ppc1x(" << i << "): " << sngl->get_ppc1x() << endl;
          *fout << "phcentraltrack->get_ppc1y(" << i << "): " << sngl->get_ppc1y() << endl;
          *fout << "phcentraltrack->get_ppc1z(" << i << "): " << sngl->get_ppc1z() << endl;
          *fout << "phcentraltrack->get_ppc2x(" << i << "): " << sngl->get_ppc2x() << endl;
          *fout << "phcentraltrack->get_ppc2y(" << i << "): " << sngl->get_ppc2y() << endl;
          *fout << "phcentraltrack->get_ppc2z(" << i << "): " << sngl->get_ppc2z() << endl;
          *fout << "phcentraltrack->get_ptecx(" << i << "): " << sngl->get_ptecx() << endl;
          *fout << "phcentraltrack->get_ptecy(" << i << "): " << sngl->get_ptecy() << endl;
          *fout << "phcentraltrack->get_ptecz(" << i << "): " << sngl->get_ptecz() << endl;
          *fout << "phcentraltrack->get_ppc3x(" << i << "): " << sngl->get_ppc3x() << endl;
          *fout << "phcentraltrack->get_ppc3y(" << i << "): " << sngl->get_ppc3y() << endl;
          *fout << "phcentraltrack->get_ppc3z(" << i << "): " << sngl->get_ppc3z() << endl;
          *fout << "phcentraltrack->get_pemcx(" << i << "): " << sngl->get_pemcx() << endl;
          *fout << "phcentraltrack->get_pemcy(" << i << "): " << sngl->get_pemcy() << endl;
          *fout << "phcentraltrack->get_pemcz(" << i << "): " << sngl->get_pemcz() << endl;
          *fout << "phcentraltrack->get_ptofex(" << i << "): " << sngl->get_ptofex() << endl;
          *fout << "phcentraltrack->get_ptofey(" << i << "): " << sngl->get_ptofey() << endl;
          *fout << "phcentraltrack->get_ptofez(" << i << "): " << sngl->get_ptofez() << endl;
          *fout << "phcentraltrack->get_sptofex(" << i << "): " << sngl->get_sptofex() << endl;
          *fout << "phcentraltrack->get_sptofey(" << i << "): " << sngl->get_sptofey() << endl;
          *fout << "phcentraltrack->get_sptofez(" << i << "): " << sngl->get_sptofez() << endl;
          *fout << "phcentraltrack->get_pmrpcx(" << i << "): " << sngl->get_pmrpcx() << endl;
          *fout << "phcentraltrack->get_pmrpcy(" << i << "): " << sngl->get_pmrpcy() << endl;
          *fout << "phcentraltrack->get_pmrpcz(" << i << "): " << sngl->get_pmrpcz() << endl;

          *fout << "phcentraltrack->get_pltofe(" << i << "): " << sngl->get_pltofe() << endl;
          *fout << "phcentraltrack->get_plemc(" << i << "): " << sngl->get_plemc() << endl;
          *fout << "phcentraltrack->get_plmrpc(" << i << "): " << sngl->get_plmrpc() << endl;

          *fout << "phcentraltrack->get_pc1dphi(" << i << "): " << sngl->get_pc1dphi() << endl;
          *fout << "phcentraltrack->get_pc1dz(" << i << "): " << sngl->get_pc1dz() << endl;
          *fout << "phcentraltrack->get_pc2dphi(" << i << "): " << sngl->get_pc2dphi() << endl;
          *fout << "phcentraltrack->get_pc2dz(" << i << "): " << sngl->get_pc2dz() << endl;
          *fout << "phcentraltrack->get_tecdphi(" << i << "): " << sngl->get_tecdphi() << endl;
          *fout << "phcentraltrack->get_tecdalpha(" << i << "): " << sngl->get_tecdalpha() << endl;
          *fout << "phcentraltrack->get_pc3dphi(" << i << "): " << sngl->get_pc3dphi() << endl;
          *fout << "phcentraltrack->get_pc3dz(" << i << "): " << sngl->get_pc3dz() << endl;
          *fout << "phcentraltrack->get_emcdphi(" << i << "): " << sngl->get_emcdphi() << endl;
          *fout << "phcentraltrack->get_emcdz(" << i << "): " << sngl->get_emcdz() << endl;
          *fout << "phcentraltrack->get_tofdphi(" << i << "): " << sngl->get_tofdphi() << endl;
          *fout << "phcentraltrack->get_tofdz(" << i << "): " << sngl->get_tofdz() << endl;
          *fout << "phcentraltrack->get_mrpcdphi(" << i << "): " << sngl->get_mrpcdphi() << endl;
          *fout << "phcentraltrack->get_mrpcdz(" << i << "): " << sngl->get_mrpcdz() << endl;

          *fout << "phcentraltrack->get_sppc1x(" << i << "): " << sngl->get_sppc1x() << endl;
          *fout << "phcentraltrack->get_sppc1y(" << i << "): " << sngl->get_sppc1y() << endl;
          *fout << "phcentraltrack->get_sppc1z(" << i << "): " << sngl->get_sppc1z() << endl;
          *fout << "phcentraltrack->get_sppc2x(" << i << "): " << sngl->get_sppc2x() << endl;
          *fout << "phcentraltrack->get_sppc2y(" << i << "): " << sngl->get_sppc2y() << endl;
          *fout << "phcentraltrack->get_sppc2z(" << i << "): " << sngl->get_sppc2z() << endl;
          *fout << "phcentraltrack->get_sppc3x(" << i << "): " << sngl->get_sppc3x() << endl;
          *fout << "phcentraltrack->get_sppc3y(" << i << "): " << sngl->get_sppc3y() << endl;
          *fout << "phcentraltrack->get_sppc3z(" << i << "): " << sngl->get_sppc3z() << endl;
          *fout << "phcentraltrack->get_spc1dphi(" << i << "): " << sngl->get_spc1dphi() << endl;
          *fout << "phcentraltrack->get_spc1dz(" << i << "): " << sngl->get_spc1dz() << endl;
          *fout << "phcentraltrack->get_spc2dphi(" << i << "): " << sngl->get_spc2dphi() << endl;
          *fout << "phcentraltrack->get_spc2dz(" << i << "): " << sngl->get_spc2dz() << endl;
          *fout << "phcentraltrack->get_spc3dphi(" << i << "): " << sngl->get_spc3dphi() << endl;
          *fout << "phcentraltrack->get_spc3dz(" << i << "): " << sngl->get_spc3dz() << endl;
          *fout << "phcentraltrack->get_semcdphi(" << i << "): " << sngl->get_semcdphi() << endl;
          *fout << "phcentraltrack->get_semcdz(" << i << "): " << sngl->get_semcdz() << endl;
          *fout << "phcentraltrack->get_stofdphi(" << i << "): " << sngl->get_stofdphi() << endl;
          *fout << "phcentraltrack->get_stofdz(" << i << "): " << sngl->get_stofdz() << endl;
          *fout << "phcentraltrack->get_stecdphi(" << i << "): " << sngl->get_stecdphi() << endl;
          *fout << "phcentraltrack->get_stecdalpha(" << i << "): " << sngl->get_stecdalpha() << endl;
          *fout << "phcentraltrack->get_arm(" << i << "): " << sngl->get_arm() << endl;
          *fout << "phcentraltrack->get_sect(" << i << "): " << sngl->get_sect() << endl;
          *fout << "phcentraltrack->get_ysect(" << i << "): " << sngl->get_ysect() << endl;
          *fout << "phcentraltrack->get_zsect(" << i << "): " << sngl->get_zsect() << endl;
          *fout << "phcentraltrack->get_ecorr(" << i << "): " << sngl->get_ecorr() << endl;
          *fout << "phcentraltrack->get_ecore(" << i << "): " << sngl->get_ecore() << endl;
          *fout << "phcentraltrack->get_dep(" << i << "): " << phcentraltrack->get_dep(i) << endl;
          *fout << "phcentraltrack->get_emce(" << i << "): " << sngl->get_emce() << endl;

          *fout << "phcentraltrack->get_emcdispy(" << i << "): " << sngl->get_emcdispy() << endl;
          *fout << "phcentraltrack->get_emcdispz(" << i << "): " << sngl->get_emcdispz() << endl;
          *fout << "phcentraltrack->get_temc(" << i << "): " << sngl->get_temc() << endl;
          *fout << "phcentraltrack->get_prob(" << i << "): " << sngl->get_prob() << endl;
          *fout << "phcentraltrack->get_ecent(" << i << "): " << sngl->get_ecent() << endl;
          *fout << "phcentraltrack->get_twrhit(" << i << "): " << sngl->get_twrhit() << endl;
          *fout << "phcentraltrack->get_e9(" << i << "): " << sngl->get_e9() << endl;
          *fout << "phcentraltrack->get_re9(" << i << "): " << sngl->get_re9() << endl;
          *fout << "phcentraltrack->get_emcchi2(" << i << "): " << sngl->get_emcchi2() << endl;
          *fout << "phcentraltrack->get_sysect(" << i << "): " << sngl->get_sysect() << endl;
          *fout << "phcentraltrack->get_szsect(" << i << "): " << sngl->get_szsect() << endl;
          *fout << "phcentraltrack->get_secorr(" << i << "): " << sngl->get_secorr() << endl;
          *fout << "phcentraltrack->get_secore(" << i << "): " << sngl->get_secore() << endl;
          *fout << "phcentraltrack->get_semce(" << i << "): " << sngl->get_semce() << endl;
          *fout << "phcentraltrack->get_semcdispy(" << i << "): " << sngl->get_semcdispy() << endl;
          *fout << "phcentraltrack->get_semcdispz(" << i << "): " << sngl->get_semcdispz() << endl;
          *fout << "phcentraltrack->get_stemc(" << i << "): " << sngl->get_stemc() << endl;
          *fout << "phcentraltrack->get_sprob(" << i << "): " << sngl->get_sprob() << endl;
          *fout << "phcentraltrack->get_secent(" << i << "): " << sngl->get_secent() << endl;
          *fout << "phcentraltrack->get_stwrhit(" << i << "): " << sngl->get_stwrhit() << endl;
          *fout << "phcentraltrack->get_se9(" << i << "): " << sngl->get_se9() << endl;
          *fout << "phcentraltrack->get_sre9(" << i << "): " << sngl->get_sre9() << endl;
          *fout << "phcentraltrack->get_semcchi2(" << i << "): " << sngl->get_semcchi2() << endl;
          *fout << "phcentraltrack->get_slat(" << i << "): " << sngl->get_slat() << endl;
          *fout << "phcentraltrack->get_ttof(" << i << "): " << sngl->get_ttof() << endl;
          *fout << "phcentraltrack->get_etof(" << i << "): " << sngl->get_etof() << endl;
          *fout << "phcentraltrack->get_sttof(" << i << "): " << sngl->get_sttof() << endl;
          *fout << "phcentraltrack->get_setof(" << i << "): " << sngl->get_setof() << endl;
          *fout << "phcentraltrack->get_slat_mrpc(" << i << "): " << sngl->get_slat_mrpc() << endl;
          *fout << "phcentraltrack->get_ttof_mrpc(" << i << "): " << sngl->get_ttof_mrpc() << endl;
          *fout << "phcentraltrack->get_ttofd_mrpc(" << i << "): " << sngl->get_ttofd_mrpc() << endl;
          *fout << "phcentraltrack->get_qtof_mrpc(" << i << "): " << sngl->get_qtof_mrpc() << endl;
          *fout << "phcentraltrack->get_acc(" << i << "): " << sngl->get_acc() << endl;
          *fout << "phcentraltrack->get_ring(" << i << "): " << sngl->get_ring() << endl;
          *fout << "phcentraltrack->get_n0(" << i << "): " << sngl->get_n0() << endl;
          *fout << "phcentraltrack->get_npe0(" << i << "): " << sngl->get_npe0() << endl;
          *fout << "phcentraltrack->get_n1(" << i << "): " << sngl->get_n1() << endl;
          *fout << "phcentraltrack->get_npe1(" << i << "): " << sngl->get_npe1() << endl;
          *fout << "phcentraltrack->get_chi2(" << i << "): " << sngl->get_chi2() << endl;
          *fout << "phcentraltrack->get_disp(" << i << "): " << sngl->get_disp() << endl;
          *fout << "phcentraltrack->get_tcrk(" << i << "): " << sngl->get_tcrk() << endl;
          *fout << "phcentraltrack->get_cross_phi(" << i << "): " << sngl->get_cross_phi() << endl;
          *fout << "phcentraltrack->get_cross_z(" << i << "): " << sngl->get_cross_z() << endl;
          *fout << "phcentraltrack->get_center_phi(" << i << "): " << sngl->get_center_phi() << endl;
          *fout << "phcentraltrack->get_center_z(" << i << "): " << sngl->get_center_z() << endl;
          *fout << "phcentraltrack->get_sacc(" << i << "): " << sngl->get_sacc() << endl;
          *fout << "phcentraltrack->get_sring(" << i << "): " << sngl->get_sring() << endl;
          *fout << "phcentraltrack->get_sn0(" << i << "): " << sngl->get_sn0() << endl;
          *fout << "phcentraltrack->get_snpe0(" << i << "): " << sngl->get_snpe0() << endl;
          *fout << "phcentraltrack->get_sn1(" << i << "): " << sngl->get_sn1() << endl;
          *fout << "phcentraltrack->get_snpe1(" << i << "): " << sngl->get_snpe1() << endl;
          *fout << "phcentraltrack->get_schi2(" << i << "): " << sngl->get_schi2() << endl;
          *fout << "phcentraltrack->get_sdisp(" << i << "): " << sngl->get_sdisp() << endl;
          *fout << "phcentraltrack->get_stcrk(" << i << "): " << sngl->get_stcrk() << endl;
          *fout << "phcentraltrack->get_scross_phi(" << i << "): " << sngl->get_scross_phi() << endl;
          *fout << "phcentraltrack->get_scross_z(" << i << "): " << sngl->get_scross_z() << endl;
          *fout << "phcentraltrack->get_scenter_phi(" << i << "): " << sngl->get_scenter_phi() << endl;
          *fout << "phcentraltrack->get_scenter_z(" << i << "): " << sngl->get_scenter_z() << endl;
          *fout << "phcentraltrack->get_tecdedx1(" << i << "): " << sngl->get_tecdedx1() << endl;
          *fout << "phcentraltrack->get_tecdedx2(" << i << "): " << sngl->get_tecdedx2() << endl;
          *fout << "phcentraltrack->get_pc2sdphi(" << i << "): " << sngl->get_pc2sdphi() << endl;
          *fout << "phcentraltrack->get_pc2sdz(" << i << "): " << sngl->get_pc2sdz() << endl;
          *fout << "phcentraltrack->get_pc3sdphi(" << i << "): " << sngl->get_pc3sdphi() << endl;
          *fout << "phcentraltrack->get_pc3sdz(" << i << "): " << sngl->get_pc3sdz() << endl;
          *fout << "phcentraltrack->get_emcsdphi(" << i << "): " << sngl->get_emcsdphi() << endl;
          *fout << "phcentraltrack->get_emcsdz(" << i << "): " << sngl->get_emcsdz() << endl;
          *fout << "phcentraltrack->get_tofsdphi(" << i << "): " << sngl->get_tofsdphi() << endl;
          *fout << "phcentraltrack->get_tofsdz(" << i << "): " << sngl->get_tofsdz() << endl;
          *fout << "phcentraltrack->get_tecsdphi(" << i << "): " << sngl->get_tecsdphi() << endl;
          *fout << "phcentraltrack->get_tecsdalpha(" << i << "): " << sngl->get_tecsdalpha() << endl;
          *fout << "phcentraltrack->get_spc2sdphi(" << i << "): " << sngl->get_spc2sdphi() << endl;
          *fout << "phcentraltrack->get_spc2sdz(" << i << "): " << sngl->get_spc2sdz() << endl;
          *fout << "phcentraltrack->get_spc3sdphi(" << i << "): " << sngl->get_spc3sdphi() << endl;
          *fout << "phcentraltrack->get_spc3sdz(" << i << "): " << sngl->get_spc3sdz() << endl;

          *fout << "phcentraltrack->get_semcsdphi(" << i << "): " << sngl->get_semcsdphi() << endl;
          *fout << "phcentraltrack->get_semcsdz(" << i << "): " << sngl->get_semcsdz() << endl;
//           *fout << "phcentraltrack->get_stofsdphi(" << i << "): " << sngl->get_stofsdphi() << endl;
//           *fout << "phcentraltrack->get_stofsdz(" << i << "): " << sngl->get_stofsdz() << endl;
          *fout << "phcentraltrack->get_stecsdphi(" << i << "): " << sngl->get_stecsdphi() << endl;
          *fout << "phcentraltrack->get_stecsdalpha(" << i << "): " << sngl->get_stecsdalpha() << endl;
          *fout << "phcentraltrack->get_m2tof(" << i << "): " << sngl->get_m2tof() << endl;
          *fout << "phcentraltrack->get_m2emc(" << i << "): " << sngl->get_m2emc() << endl;
          *fout << "phcentraltrack->get_isPi(" << i << "): " << sngl->get_isPi() << endl;
          *fout << "phcentraltrack->get_isK(" << i << "): " << sngl->get_isK() << endl;
          *fout << "phcentraltrack->get_isP(" << i << "): " << sngl->get_isP() << endl;
          *fout << "phcentraltrack->get_dcarm(" << i << "): " << sngl->get_dcarm() << endl;
          *fout << "phcentraltrack->get_dcside(" << i << "): " << sngl->get_dcside() << endl;
          *fout << "phcentraltrack->get_pc1sect(" << i << "): " << sngl->get_pc1sect() << endl;
          *fout << "phcentraltrack->get_pc2sect(" << i << "): " << sngl->get_pc2sect() << endl;
          *fout << "phcentraltrack->get_pc3sect(" << i << "): " << sngl->get_pc3sect() << endl;
          *fout << "phcentraltrack->get_pc1phi(" << i << "): " << sngl->get_pc1phi() << endl;
          *fout << "phcentraltrack->get_pc1z(" << i << "): " << sngl->get_pc1z() << endl;
          *fout << "phcentraltrack->get_pc2phi(" << i << "): " << sngl->get_pc2phi() << endl;
          *fout << "phcentraltrack->get_pc2z(" << i << "): " << sngl->get_pc2z() << endl;
          *fout << "phcentraltrack->get_pc3phi(" << i << "): " << sngl->get_pc3phi() << endl;
          *fout << "phcentraltrack->get_pc3z(" << i << "): " << sngl->get_pc3z() << endl;
          *fout << "phcentraltrack->get_tofphi(" << i << "): " << sngl->get_tofphi() << endl;
          *fout << "phcentraltrack->get_tofz(" << i << "): " << sngl->get_tofz() << endl;
          *fout << "phcentraltrack->get_tecphi(" << i << "): " << sngl->get_tecphi() << endl;
          *fout << "phcentraltrack->get_tecalpha(" << i << "): " << sngl->get_tecalpha() << endl;
          *fout << "phcentraltrack->get_emcphi(" << i << "): " << sngl->get_emcphi() << endl;
          *fout << "phcentraltrack->get_emcz(" << i << "): " << sngl->get_emcz() << endl;
          *fout << "phcentraltrack->get_spc1phi(" << i << "): " << sngl->get_spc1phi() << endl;
          //*fout << "phcentraltrack->get_spc1x(" << i << "): " << sngl->get_spc1x() << endl;
          //*fout << "phcentraltrack->get_spc1y(" << i << "): " << sngl->get_spc1y() << endl;
          *fout << "phcentraltrack->get_spc1z(" << i << "): " << sngl->get_spc1z() << endl;
          *fout << "phcentraltrack->get_spc2phi(" << i << "): " << sngl->get_spc2phi() << endl;
          //*fout << "phcentraltrack->get_spc2x(" << i << "): " << sngl->get_spc2x() << endl;
          //*fout << "phcentraltrack->get_spc2y(" << i << "): " << sngl->get_spc2y() << endl;
          *fout << "phcentraltrack->get_spc2z(" << i << "): " << sngl->get_spc2z() << endl;
          *fout << "phcentraltrack->get_spc3phi(" << i << "): " << sngl->get_spc3phi() << endl;
          //*fout << "phcentraltrack->get_spc3x(" << i << "): " << sngl->get_spc3x() << endl;
          //*fout << "phcentraltrack->get_spc3y(" << i << "): " << sngl->get_spc3y() << endl;
          *fout << "phcentraltrack->get_spc3z(" << i << "): " << sngl->get_spc3z() << endl;
          *fout << "phcentraltrack->get_stofphi(" << i << "): " << sngl->get_stofphi() << endl;
          *fout << "phcentraltrack->get_stofz(" << i << "): " << sngl->get_stofz() << endl;
          *fout << "phcentraltrack->get_stecphi(" << i << "): " << sngl->get_stecphi() << endl;
          *fout << "phcentraltrack->get_stecalpha(" << i << "): " << sngl->get_stecalpha() << endl;
          *fout << "phcentraltrack->get_semcphi(" << i << "): " << sngl->get_semcphi() << endl;
          *fout << "phcentraltrack->get_semcz(" << i << "): " << sngl->get_semcz() << endl;
          *fout << "phcentraltrack->get_emcsdphi_e(" << i << "): " << sngl->get_emcsdphi_e() << endl;
          *fout << "phcentraltrack->get_emcsdz_e(" << i << "): " << sngl->get_emcsdz_e() << endl;
          *fout << "phcentraltrack->get_semcsdphi_e(" << i << "): " << sngl->get_semcsdphi_e() << endl;
          *fout << "phcentraltrack->get_semcsdz_e(" << i << "): " << sngl->get_semcsdz_e() << endl;
          *fout << "phcentraltrack->get_tecnhit(" << i << "): " << sngl->get_tecnhit() << endl;
          *fout << "phcentraltrack->get_n2(" << i << "): " << sngl->get_n2() << endl;
          *fout << "phcentraltrack->get_npe2(" << i << "): " << sngl->get_npe2() << endl;
          *fout << "phcentraltrack->get_n3(" << i << "): " << sngl->get_n3() << endl;
          *fout << "phcentraltrack->get_npe3(" << i << "): " << sngl->get_npe3() << endl;
          *fout << "phcentraltrack->get_sn2(" << i << "): " << sngl->get_sn2() << endl;
          *fout << "phcentraltrack->get_snpe2(" << i << "): " << sngl->get_snpe2() << endl;
          *fout << "phcentraltrack->get_sn3(" << i << "): " << sngl->get_sn3() << endl;
          *fout << "phcentraltrack->get_snpe3(" << i << "): " << sngl->get_snpe3() << endl;
          *fout << "phcentraltrack->get_deadmap(" << i << "): " << sngl->get_deadmap() << endl;
          *fout << "phcentraltrack->get_warnmap(" << i << "): " << sngl->get_warnmap() << endl;
          *fout << "phcentraltrack->get_sdeadmap(" << i << "): " << sngl->get_sdeadmap() << endl;
          *fout << "phcentraltrack->get_swarnmap(" << i << "): " << sngl->get_swarnmap() << endl;
          *fout << "phcentraltrack->get_tofecut(" << i << "): " << sngl->get_tofecut() << endl;
          *fout << "phcentraltrack->get_tofsame(" << i << "): " << sngl->get_tofsame() << endl;
          *fout << "phcentraltrack->get_slatnext(" << i << "): " << sngl->get_slatnext() << endl;
          *fout << "phcentraltrack->get_ttofnext(" << i << "): " << sngl->get_ttofnext() << endl;
          *fout << "phcentraltrack->get_etofnext(" << i << "): " << sngl->get_etofnext() << endl;
          *fout << "phcentraltrack->get_tzrid(" << i << "): " << sngl->get_tzrid() << endl;
          *fout << "phcentraltrack->get_pcrid(" << i << "): " << sngl->get_pcrid() << endl;
          *fout << "phcentraltrack->get_ptzrx(" << i << "): " << sngl->get_ptzrx() << endl;
          *fout << "phcentraltrack->get_ptzry(" << i << "): " << sngl->get_ptzry() << endl;
          *fout << "phcentraltrack->get_ptzrz(" << i << "): " << sngl->get_ptzrz() << endl;
          *fout << "phcentraltrack->get_ppcrx(" << i << "): " << sngl->get_ppcrx() << endl;
          *fout << "phcentraltrack->get_ppcry(" << i << "): " << sngl->get_ppcry() << endl;
          *fout << "phcentraltrack->get_ppcrz(" << i << "): " << sngl->get_ppcrz() << endl;
          *fout << "phcentraltrack->get_tzrtof(" << i << "): " << sngl->get_tzrtof() << endl;
          *fout << "phcentraltrack->get_tzrslat(" << i << "): " << sngl->get_tzrslat() << endl;
          *fout << "phcentraltrack->get_tzreloss(" << i << "): " << sngl->get_tzreloss() << endl;
          *fout << "phcentraltrack->get_tzrx(" << i << "): " << sngl->get_tzrx() << endl;
          *fout << "phcentraltrack->get_tzry(" << i << "): " << sngl->get_tzry() << endl;
          *fout << "phcentraltrack->get_tzrz(" << i << "): " << sngl->get_tzrz() << endl;
          *fout << "phcentraltrack->get_pcrtof(" << i << "): " << sngl->get_pcrtof() << endl;
          *fout << "phcentraltrack->get_pcrslat(" << i << "): " << sngl->get_pcrslat() << endl;
          *fout << "phcentraltrack->get_pcreloss(" << i << "): " << sngl->get_pcreloss() << endl;
          *fout << "phcentraltrack->get_pcrx(" << i << "): " << sngl->get_pcrx() << endl;
          *fout << "phcentraltrack->get_pcry(" << i << "): " << sngl->get_pcry() << endl;
          *fout << "phcentraltrack->get_pcrz(" << i << "): " << sngl->get_pcrz() << endl;
          *fout << "phcentraltrack->get_tzrsdphi(" << i << "): " << sngl->get_tzrsdphi() << endl;
          *fout << "phcentraltrack->get_tzrsdz(" << i << "): " << sngl->get_tzrsdz() << endl;
          *fout << "phcentraltrack->get_m2tzr(" << i << "): " << sngl->get_m2tzr() << endl;
          *fout << "phcentraltrack->get_m2ntctof(" << i << "): " << sngl->get_m2ntctof() << endl;
          *fout << "phcentraltrack->get_pltzr(" << i << "): " << sngl->get_pltzr() << endl;
          *fout << "phcentraltrack->get_isPitzr(" << i << "): " << sngl->get_isPitzr() << endl;
          *fout << "phcentraltrack->get_isKtzr(" << i << "): " << sngl->get_isKtzr() << endl;
          *fout << "phcentraltrack->get_isPtzr(" << i << "): " << sngl->get_isPtzr() << endl;
          *fout << "phcentraltrack->get_isPintctof(" << i << "): " << sngl->get_isPintctof() << endl;
          *fout << "phcentraltrack->get_isKntctof(" << i << "): " << sngl->get_isKntctof() << endl;
          *fout << "phcentraltrack->get_isPntctof(" << i << "): " << sngl->get_isPntctof() << endl;
          *fout << "phcentraltrack->get_tzrecut(" << i << "): " << sngl->get_tzrecut() << endl;
          *fout << "phcentraltrack->get_L1Trig(" << i << "): " << sngl->get_L1Trig() << endl;
          *fout << "phcentraltrack->get_pc1wid(" << i << "): " << sngl->get_pc1wid() << endl;
          *fout << "phcentraltrack->get_pc2wid(" << i << "): " << sngl->get_pc2wid() << endl;
          *fout << "phcentraltrack->get_pc3wid(" << i << "): " << sngl->get_pc3wid() << endl;
          *fout << "phcentraltrack->get_categoryl2eLowPt(" << i << "): " << sngl->get_categoryl2eLowPt() << endl;
          *fout << "phcentraltrack->get_categoryl2eHighPt(" << i << "): " << sngl->get_categoryl2eHighPt() << endl;
          *fout << "phcentraltrack->get_candIDl2e(" << i << "): " << sngl->get_candIDl2e() << endl;
          *fout << "phcentraltrack->get_nlvl2MatchLowOcupy(" << i << "): " << sngl->get_nlvl2MatchLowOcupy() << endl;
          *fout << "phcentraltrack->get_RawL1(" << i << "): " << sngl->get_RawL1() << endl;
          *fout << "phcentraltrack->get_LivL1(" << i << "): " << sngl->get_LivL1() << endl;
          *fout << "phcentraltrack->get_SclL1(" << i << "): " << sngl->get_SclL1() << endl;
//           *fout << "phcentraltrack->get_tofph1(" << i << "): " << sngl->get_tofph1() << endl;
//           *fout << "phcentraltrack->get_tofph2(" << i << "): " << sngl->get_tofph2() << endl;
//           *fout << "phcentraltrack->get_toftdc1(" << i << "): " << sngl->get_toftdc1() << endl;
//           *fout << "phcentraltrack->get_toftdc2(" << i << "): " << sngl->get_toftdc2() << endl;
          *fout << "phcentraltrack->get_aerindex(" << i << "): " << sngl->get_aerindex() << endl;
          *fout << "phcentraltrack->get_aersindex(" << i << "): " << sngl->get_aersindex() << endl;
          *fout << "phcentraltrack->get_aerph1(" << i << "): " << sngl->get_aerph1() << endl;
          *fout << "phcentraltrack->get_aerph2(" << i << "): " << sngl->get_aerph2() << endl;
          *fout << "phcentraltrack->get_aert1(" << i << "): " << sngl->get_aert1() << endl;
          *fout << "phcentraltrack->get_aert2(" << i << "): " << sngl->get_aert2() << endl;
          *fout << "phcentraltrack->get_aernpe(" << i << "): " << sngl->get_aernpe() << endl;
          *fout << "phcentraltrack->get_aerstatus(" << i << "): " << sngl->get_aerstatus() << endl;
          *fout << "phcentraltrack->get_aerph1_0(" << i << "): " << sngl->get_aerph1_0() << endl;
          *fout << "phcentraltrack->get_aerph2_0(" << i << "): " << sngl->get_aerph2_0() << endl;
          *fout << "phcentraltrack->get_aert1_0(" << i << "): " << sngl->get_aert1_0() << endl;
          *fout << "phcentraltrack->get_aert2_0(" << i << "): " << sngl->get_aert2_0() << endl;
          *fout << "phcentraltrack->get_aerph1_1(" << i << "): " << sngl->get_aerph1_1() << endl;
          *fout << "phcentraltrack->get_aerph2_1(" << i << "): " << sngl->get_aerph2_1() << endl;
          *fout << "phcentraltrack->get_aert1_1(" << i << "): " << sngl->get_aert1_1() << endl;
          *fout << "phcentraltrack->get_aert2_1(" << i << "): " << sngl->get_aert2_1() << endl;
          *fout << "phcentraltrack->get_aerph1_2(" << i << "): " << sngl->get_aerph1_2() << endl;
          *fout << "phcentraltrack->get_aerph2_2(" << i << "): " << sngl->get_aerph2_2() << endl;
          *fout << "phcentraltrack->get_aert1_2(" << i << "): " << sngl->get_aert1_2() << endl;
          *fout << "phcentraltrack->get_aert2_2(" << i << "): " << sngl->get_aert2_2() << endl;
          *fout << "phcentraltrack->get_aerph1_3(" << i << "): " << sngl->get_aerph1_3() << endl;
          *fout << "phcentraltrack->get_aerph2_3(" << i << "): " << sngl->get_aerph2_3() << endl;
          *fout << "phcentraltrack->get_aert1_3(" << i << "): " << sngl->get_aert1_3() << endl;
          *fout << "phcentraltrack->get_aert2_3(" << i << "): " << sngl->get_aert2_3() << endl;
          *fout << "phcentraltrack->get_aerhitid(" << i << "): " << sngl->get_aerhitid() << endl;
          *fout << "phcentraltrack->get_aerhitconfig(" << i << "): " << sngl->get_aerhitconfig() << endl;
          *fout << "phcentraltrack->get_aersph1_0(" << i << "): " << sngl->get_aersph1_0() << endl;
          *fout << "phcentraltrack->get_aersph2_0(" << i << "): " << sngl->get_aersph2_0() << endl;
          *fout << "phcentraltrack->get_aerst1_0(" << i << "): " << sngl->get_aerst1_0() << endl;
          *fout << "phcentraltrack->get_aerst2_0(" << i << "): " << sngl->get_aerst2_0() << endl;
          *fout << "phcentraltrack->get_aersph1_1(" << i << "): " << sngl->get_aersph1_1() << endl;
          *fout << "phcentraltrack->get_aersph2_1(" << i << "): " << sngl->get_aersph2_1() << endl;
          *fout << "phcentraltrack->get_aerst1_1(" << i << "): " << sngl->get_aerst1_1() << endl;
          *fout << "phcentraltrack->get_aerst2_1(" << i << "): " << sngl->get_aerst2_1() << endl;
          *fout << "phcentraltrack->get_aersph1_2(" << i << "): " << sngl->get_aersph1_2() << endl;
          *fout << "phcentraltrack->get_aersph2_2(" << i << "): " << sngl->get_aersph2_2() << endl;
          *fout << "phcentraltrack->get_aerst1_2(" << i << "): " << sngl->get_aerst1_2() << endl;
          *fout << "phcentraltrack->get_aerst2_2(" << i << "): " << sngl->get_aerst2_2() << endl;
          *fout << "phcentraltrack->get_aersph1_3(" << i << "): " << sngl->get_aersph1_3() << endl;
          *fout << "phcentraltrack->get_aersph2_3(" << i << "): " << sngl->get_aersph2_3() << endl;
          *fout << "phcentraltrack->get_aerst1_3(" << i << "): " << sngl->get_aerst1_3() << endl;
          *fout << "phcentraltrack->get_aerst2_3(" << i << "): " << sngl->get_aerst2_3() << endl;
          *fout << "phcentraltrack->get_aershitid(" << i << "): " << sngl->get_aershitid() << endl;
          *fout << "phcentraltrack->get_aershitconfig(" << i << "): " << sngl->get_aershitconfig() << endl;
          *fout << "phcentraltrack->get_tecde06(" << i << "): " << sngl->get_tecde06() << endl;
          *fout << "phcentraltrack->get_tectrklen(" << i << "): " << sngl->get_tectrklen() << endl;
          *fout << "phcentraltrack->get_tecnde(" << i << "): " << sngl->get_tecnde() << endl;
          *fout << "phcentraltrack->get_tecnhit100(" << i << "): " << sngl->get_tecnhit100() << endl;
          *fout << "phcentraltrack->get_tecnhit200(" << i << "): " << sngl->get_tecnhit200() << endl;
          *fout << "phcentraltrack->get_tecnhit50(" << i << "): " << sngl->get_tecnhit50() << endl;
          *fout << "phcentraltrack->get_tecwtb(" << i << "): " << sngl->get_tecwtb() << endl;
          *fout << "phcentraltrack->get_tecwtbsq(" << i << "): " << sngl->get_tecwtbsq() << endl;
          *fout << "phcentraltrack->get_mcid(" << i << "): " << sngl->get_mcid() << endl;
          *fout << "phcentraltrack->get_dchid(" << i << "): " << sngl->get_dchid() << endl;
          *fout << "phcentraltrack->get_emcrawtdc(" << i << "): " << sngl->get_emcrawtdc() << endl;
          *fout << "phcentraltrack->get_emcrawadc(" << i << "): " << sngl->get_emcrawadc() << endl;
          *fout << "phcentraltrack->get_emcrawadclg(" << i << "): " << sngl->get_emcrawadclg() << endl;

          *fout << "phcentraltrack->get_ptofwx(" << i << "): " << sngl->get_ptofwx() << endl;
          *fout << "phcentraltrack->get_ptofwy(" << i << "): " << sngl->get_ptofwy() << endl;
          *fout << "phcentraltrack->get_ptofwz(" << i << "): " << sngl->get_ptofwz() << endl;
          *fout << "phcentraltrack->get_sptofwx(" << i << "): " << sngl->get_sptofwx() << endl;
          *fout << "phcentraltrack->get_sptofwy(" << i << "): " << sngl->get_sptofwy() << endl;
          *fout << "phcentraltrack->get_sptofwz(" << i << "): " << sngl->get_sptofwz() << endl;
          *fout << "phcentraltrack->get_pltofw(" << i << "): " << sngl->get_pltofw() << endl;
          *fout << "phcentraltrack->get_tofwid(" << i << "): " << sngl->get_tofwid() << endl;
          *fout << "phcentraltrack->get_stofwid(" << i << "): " << sngl->get_stofwid() << endl;
          *fout << "phcentraltrack->get_striptofw(" << i << "): " << sngl->get_striptofw() << endl;
          *fout << "phcentraltrack->get_tofwx(" << i << "): " << sngl->get_tofwx() << endl;
          *fout << "phcentraltrack->get_tofwy(" << i << "): " << sngl->get_tofwy() << endl;
          *fout << "phcentraltrack->get_tofwz(" << i << "): " << sngl->get_tofwz() << endl;
          *fout << "phcentraltrack->get_ttofw(" << i << "): " << sngl->get_ttofw() << endl;
          *fout << "phcentraltrack->get_qtofw(" << i << "): " << sngl->get_qtofw() << endl;
          *fout << "phcentraltrack->get_tofwadcup(" << i << "): " << sngl->get_tofwadcup() << endl;
          *fout << "phcentraltrack->get_tofwadcdw(" << i << "): " << sngl->get_tofwadcdw() << endl;
          *fout << "phcentraltrack->get_tofwtdcup(" << i << "): " << sngl->get_tofwtdcup() << endl;
          *fout << "phcentraltrack->get_tofwtdcdw(" << i << "): " << sngl->get_tofwtdcdw() << endl;
          *fout << "phcentraltrack->get_tofwdphi(" << i << "): " << sngl->get_tofwdphi() << endl;
          *fout << "phcentraltrack->get_tofwdz(" << i << "): " << sngl->get_tofwdz() << endl;
          *fout << "phcentraltrack->get_tofwsdphi(" << i << "): " << sngl->get_tofwsdphi() << endl;
          *fout << "phcentraltrack->get_tofwsdz(" << i << "): " << sngl->get_tofwsdz() << endl;
          *fout << "phcentraltrack->get_m2tofw(" << i << "): " << sngl->get_m2tofw() << endl;
          *fout << "phcentraltrack->get_hbdid(" << i << "): " << sngl->get_hbdid() << endl;
          *fout << "phcentraltrack->get_hbdsector(" << i << "): " << sngl->get_hbdsector() << endl;
          *fout << "phcentraltrack->get_hbdsize(" << i << "): " << sngl->get_hbdsize() << endl;
          *fout << "phcentraltrack->get_hbdcharge(" << i << "): " << sngl->get_hbdcharge() << endl;
          *fout << "phcentraltrack->get_hbdx(" << i << "): " << sngl->get_hbdx() << endl;
          *fout << "phcentraltrack->get_hbdy(" << i << "): " << sngl->get_hbdy() << endl;
          *fout << "phcentraltrack->get_hbdz(" << i << "): " << sngl->get_hbdz() << endl;
          *fout << "phcentraltrack->get_phbdx(" << i << "): " << sngl->get_phbdx() << endl;
          *fout << "phcentraltrack->get_phbdy(" << i << "): " << sngl->get_phbdy() << endl;
          *fout << "phcentraltrack->get_hbdz(" << i << "): " << sngl->get_hbdz() << endl;
          *fout << "phcentraltrack->get_phbdx(" << i << "): " << sngl->get_phbdx() << endl;
          *fout << "phcentraltrack->get_phbdy(" << i << "): " << sngl->get_phbdy() << endl;
          *fout << "phcentraltrack->get_phbdz(" << i << "): " << sngl->get_phbdz() << endl;
          *fout << "phcentraltrack->get_hbddphi(" << i << "): " << sngl->get_hbddphi() << endl;
          *fout << "phcentraltrack->get_hbddz(" << i << "): " << sngl->get_hbddz() << endl;
          *fout << "phcentraltrack->get_hbdsdphi(" << i << "): " << sngl->get_hbdsdphi() << endl;
          *fout << "phcentraltrack->get_hbdsdz(" << i << "): " << sngl->get_hbdsdz() << endl;

          *fout << "phcentraltrack->get_Px(" << i << "): " << sngl->get_Px() << endl;
          *fout << "phcentraltrack->get_Py(" << i << "): " << sngl->get_Py() << endl;
          *fout << "phcentraltrack->get_Pz(" << i << "): " << sngl->get_Pz() << endl;

        }
    }
  return 0;
}

