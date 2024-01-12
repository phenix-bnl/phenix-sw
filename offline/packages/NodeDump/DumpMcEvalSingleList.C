#include "DumpMcEvalSingleList.h"

#include <PHIODataNode.h>

#include <McEvalSingleList.h>


#include <string>

using namespace std;

typedef PHIODataNode<McEvalSingleList> MyNode_t;

DumpMcEvalSingleList::DumpMcEvalSingleList(const string &NodeName): DumpObject(NodeName)
{
  return ;
}

int DumpMcEvalSingleList::process_Node(PHNode *myNode)
{
  McEvalSingleList *mcevalsinglelist = NULL;
  MyNode_t *thisNode = static_cast <MyNode_t *> (myNode);
  if (thisNode)
    {
      mcevalsinglelist = thisNode->getData();
    }
  if (mcevalsinglelist)
    {
      *fout << "get_McEvalSingleTrackN(): " << mcevalsinglelist->get_McEvalSingleTrackN() << endl;
      for (unsigned int i = 0; i < mcevalsinglelist->get_McEvalSingleTrackN(); i++)
        {
          *fout << "mcevalsinglelist->get_eventid(" << i << "): " << mcevalsinglelist->get_eventid(i) << endl;
          *fout << "mcevalsinglelist->get_mctrackid(" << i << "): " << mcevalsinglelist->get_mctrackid(i) << endl;
          *fout << "mcevalsinglelist->get_generation(" << i << "): " << mcevalsinglelist->get_generation(i) << endl;
          *fout << "mcevalsinglelist->get_particleid(" << i << "): " << mcevalsinglelist->get_particleid(i) << endl;
          *fout << "mcevalsinglelist->get_parentid(" << i << "): " << mcevalsinglelist->get_parentid(i) << endl;
          *fout << "mcevalsinglelist->get_primaryid(" << i << "): " << mcevalsinglelist->get_primaryid(i) << endl;
          *fout << "mcevalsinglelist->get_vertexx(" << i << "): " << mcevalsinglelist->get_vertexx(i) << endl;
          *fout << "mcevalsinglelist->get_vertexy(" << i << "): " << mcevalsinglelist->get_vertexy(i) << endl;
          *fout << "mcevalsinglelist->get_vertexz(" << i << "): " << mcevalsinglelist->get_vertexz(i) << endl;
          *fout << "mcevalsinglelist->get_parentvertexx(" << i << "): " << mcevalsinglelist->get_parentvertexx(i) << endl;
          *fout << "mcevalsinglelist->get_parentvertexy(" << i << "): " << mcevalsinglelist->get_parentvertexy(i) << endl;
          *fout << "mcevalsinglelist->get_parentvertexz(" << i << "): " << mcevalsinglelist->get_parentvertexz(i) << endl;
          *fout << "mcevalsinglelist->get_primaryvertexx(" << i << "): " << mcevalsinglelist->get_primaryvertexx(i) << endl;
          *fout << "mcevalsinglelist->get_primaryvertexy(" << i << "): " << mcevalsinglelist->get_primaryvertexy(i) << endl;
          *fout << "mcevalsinglelist->get_primaryvertexz(" << i << "): " << mcevalsinglelist->get_primaryvertexz(i) << endl;
          *fout << "mcevalsinglelist->get_momentumx(" << i << "): " << mcevalsinglelist->get_momentumx(i) << endl;
          *fout << "mcevalsinglelist->get_momentumy(" << i << "): " << mcevalsinglelist->get_momentumy(i) << endl;
          *fout << "mcevalsinglelist->get_momentumz(" << i << "): " << mcevalsinglelist->get_momentumz(i) << endl;
          *fout << "mcevalsinglelist->get_parentmomentumx(" << i << "): " << mcevalsinglelist->get_parentmomentumx(i) << endl;
          *fout << "mcevalsinglelist->get_parentmomentumy(" << i << "): " << mcevalsinglelist->get_parentmomentumy(i) << endl;
          *fout << "mcevalsinglelist->get_parentmomentumz(" << i << "): " << mcevalsinglelist->get_parentmomentumz(i) << endl;
          *fout << "mcevalsinglelist->get_primarymomentumx(" << i << "): " << mcevalsinglelist->get_primarymomentumx(i) << endl;
          *fout << "mcevalsinglelist->get_primarymomentumy(" << i << "): " << mcevalsinglelist->get_primarymomentumy(i) << endl;
          *fout << "mcevalsinglelist->get_primarymomentumz(" << i << "): " << mcevalsinglelist->get_primarymomentumz(i) << endl;
          *fout << "mcevalsinglelist->get_quality(" << i << "): " << mcevalsinglelist->get_quality(i) << endl;
          *fout << "mcevalsinglelist->get_momentumr(" << i << "): " << mcevalsinglelist->get_momentumr(i) << endl;
          *fout << "mcevalsinglelist->get_theta0(" << i << "): " << mcevalsinglelist->get_theta0(i) << endl;
          *fout << "mcevalsinglelist->get_phi0(" << i << "): " << mcevalsinglelist->get_phi0(i) << endl;
          *fout << "mcevalsinglelist->get_phi(" << i << "): " << mcevalsinglelist->get_phi(i) << endl;
          *fout << "mcevalsinglelist->get_alpha(" << i << "): " << mcevalsinglelist->get_alpha(i) << endl;
          *fout << "mcevalsinglelist->get_zed(" << i << "): " << mcevalsinglelist->get_zed(i) << endl;
          *fout << "mcevalsinglelist->get_beta(" << i << "): " << mcevalsinglelist->get_beta(i) << endl;
          *fout << "mcevalsinglelist->get_Nreco(" << i << "): " << mcevalsinglelist->get_Nreco(i) << endl;
	  for (int j=0; j<mcevalsinglelist->get_Nreco(i);j++)
	    {
	      *fout << "mcevalsinglelist->get_recoid(" << i << ", " << j << "): " << mcevalsinglelist->get_recoid(i,j) << endl;
	      *fout << "mcevalsinglelist->get_quality(" << i << ", " << j << "): " << mcevalsinglelist->get_quality(i,j) << endl;
	      *fout << "mcevalsinglelist->get_momentum(" << i << ", " << j << "): " << mcevalsinglelist->get_momentum(i,j) << endl;
	      *fout << "mcevalsinglelist->get_theta0(" << i << ", " << j << "): " << mcevalsinglelist->get_theta0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_phi0(" << i << ", " << j << "): " << mcevalsinglelist->get_phi0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_phi(" << i << ", " << j << "): " << mcevalsinglelist->get_phi(i,j) << endl;
	      *fout << "mcevalsinglelist->get_alpha(" << i << ", " << j << "): " << mcevalsinglelist->get_alpha(i,j) << endl;
	      *fout << "mcevalsinglelist->get_zed(" << i << ", " << j << "): " << mcevalsinglelist->get_zed(i,j) << endl;
	      *fout << "mcevalsinglelist->get_beta(" << i << ", " << j << "): " << mcevalsinglelist->get_beta(i,j) << endl;
	      *fout << "mcevalsinglelist->get_averagetime(" << i << ", " << j << "): " << mcevalsinglelist->get_averagetime(i,j) << endl;
	      *fout << "mcevalsinglelist->get_xhits(" << i << ", " << j << "): " << mcevalsinglelist->get_xhits(i,j) << endl;
	      *fout << "mcevalsinglelist->get_uvhits(" << i << ", " << j << "): " << mcevalsinglelist->get_uvhits(i,j) << endl;
	      *fout << "mcevalsinglelist->get_mulmain(" << i << ", " << j << "): " << mcevalsinglelist->get_mulmain(i,j) << endl;
	      *fout << "mcevalsinglelist->get_mulxmain(" << i << ", " << j << "): " << mcevalsinglelist->get_mulxmain(i,j) << endl;
	      *fout << "mcevalsinglelist->get_muluvmain(" << i << ", " << j << "): " << mcevalsinglelist->get_muluvmain(i,j) << endl;
	      *fout << "mcevalsinglelist->get_main(" << i << ", " << j << "): " << mcevalsinglelist->get_main(i,j) << endl;
	      *fout << "mcevalsinglelist->get_xmain(" << i << ", " << j << "): " << mcevalsinglelist->get_xmain(i,j) << endl;
	      *fout << "mcevalsinglelist->get_uvmain(" << i << ", " << j << "): " << mcevalsinglelist->get_uvmain(i,j) << endl;
	      *fout << "mcevalsinglelist->get_ambiguity(" << i << ", " << j << "): " << mcevalsinglelist->get_ambiguity(i,j) << endl;
	      *fout << "mcevalsinglelist->get_purity(" << i << ", " << j << "): " << mcevalsinglelist->get_purity(i,j) << endl;
	      *fout << "mcevalsinglelist->get_xpurity(" << i << ", " << j << "): " << mcevalsinglelist->get_xpurity(i,j) << endl;
	      *fout << "mcevalsinglelist->get_uvpurity(" << i << ", " << j << "): " << mcevalsinglelist->get_uvpurity(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc1clusid(" << i << ", " << j << "): " << mcevalsinglelist->get_pc1clusid(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc2clusid(" << i << ", " << j << "): " << mcevalsinglelist->get_pc2clusid(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc3clusid(" << i << ", " << j << "): " << mcevalsinglelist->get_pc3clusid(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc1clusidtrue(" << i << ", " << j << "): " << mcevalsinglelist->get_pc1clusidtrue(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc2clusidtrue(" << i << ", " << j << "): " << mcevalsinglelist->get_pc2clusidtrue(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc3clusidtrue(" << i << ", " << j << "): " << mcevalsinglelist->get_pc3clusidtrue(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc1clusidg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc1clusidg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc2clusidg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc2clusidg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc3clusidg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc3clusidg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc1pointxg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc1pointxg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc1pointyg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc1pointyg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc1pointzg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc1pointzg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc2pointxg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc2pointxg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc2pointyg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc2pointyg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc2pointzg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc2pointzg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc3pointxg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc3pointxg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc3pointyg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc3pointyg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_pc3pointzg(" << i << ", " << j << "): " << mcevalsinglelist->get_pc3pointzg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofid(" << i << ", " << j << "): " << mcevalsinglelist->get_tofid(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofidtrue(" << i << ", " << j << "): " << mcevalsinglelist->get_tofidtrue(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofpointxg(" << i << ", " << j << "): " << mcevalsinglelist->get_tofpointxg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofidg(" << i << ", " << j << "): " << mcevalsinglelist->get_tofidg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofpointxg(" << i << ", " << j << "): " << mcevalsinglelist->get_tofpointxg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofpointyg(" << i << ", " << j << "): " << mcevalsinglelist->get_tofpointyg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofpointzg(" << i << ", " << j << "): " << mcevalsinglelist->get_tofpointzg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofg(" << i << ", " << j << "): " << mcevalsinglelist->get_tofg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_tofelossg(" << i << ", " << j << "): " << mcevalsinglelist->get_tofelossg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcclusid(" << i << ", " << j << "): " << mcevalsinglelist->get_emcclusid(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcclusidtrue(" << i << ", " << j << "): " << mcevalsinglelist->get_emcclusidtrue(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcclusidg(" << i << ", " << j << "): " << mcevalsinglelist->get_emcclusidg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcanctrk0(" << i << ", " << j << "): " << mcevalsinglelist->get_emcanctrk0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcanctrk1(" << i << ", " << j << "): " << mcevalsinglelist->get_emcanctrk1(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcanctrk2(" << i << ", " << j << "): " << mcevalsinglelist->get_emcanctrk2(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcanctwrhit0(" << i << ", " << j << "): " << mcevalsinglelist->get_emcanctwrhit0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcanctwrhit1(" << i << ", " << j << "): " << mcevalsinglelist->get_emcanctwrhit1(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcanctwrhit2(" << i << ", " << j << "): " << mcevalsinglelist->get_emcanctwrhit2(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancpid0(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancpid0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancpid1(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancpid1(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancpid2(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancpid2(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancedep0(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancedep0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancedep1(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancedep1(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancedep2(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancedep2(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancptot0(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancptot0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancptot1(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancptot1(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcancptot2(" << i << ", " << j << "): " << mcevalsinglelist->get_emcancptot2(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcpointxg(" << i << ", " << j << "): " << mcevalsinglelist->get_emcpointxg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcpointyg(" << i << ", " << j << "): " << mcevalsinglelist->get_emcpointyg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcpointzg(" << i << ", " << j << "): " << mcevalsinglelist->get_emcpointzg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcefracg(" << i << ", " << j << "): " << mcevalsinglelist->get_emcefracg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcecoreg(" << i << ", " << j << "): " << mcevalsinglelist->get_emcecoreg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emcmeaseg(" << i << ", " << j << "): " << mcevalsinglelist->get_emcmeaseg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_emctofg(" << i << ", " << j << "): " << mcevalsinglelist->get_emctofg(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crkacc(" << i << ", " << j << "): " << mcevalsinglelist->get_crkacc(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crknpmt0(" << i << ", " << j << "): " << mcevalsinglelist->get_crknpmt0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crknpmt1(" << i << ", " << j << "): " << mcevalsinglelist->get_crknpmt1(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crknpmt3(" << i << ", " << j << "): " << mcevalsinglelist->get_crknpmt3(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crknpe0(" << i << ", " << j << "): " << mcevalsinglelist->get_crknpe0(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crknpe1(" << i << ", " << j << "): " << mcevalsinglelist->get_crknpe1(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crknpe3(" << i << ", " << j << "): " << mcevalsinglelist->get_crknpe3(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crkchi2(" << i << ", " << j << "): " << mcevalsinglelist->get_crkchi2(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crkdisp(" << i << ", " << j << "): " << mcevalsinglelist->get_crkdisp(i,j) << endl;
	      *fout << "mcevalsinglelist->get_crkpath(" << i << ", " << j << "): " << mcevalsinglelist->get_crkpath(i,j) << endl;
	    }
	}
    }
  return 0;
}

