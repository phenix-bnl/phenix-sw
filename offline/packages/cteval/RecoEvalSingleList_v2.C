#include <iostream>
#include "RecoEvalSingleTrack_v2.h"
#include "RecoEvalSingleList_v2.h"

#define RECOEVALSINGLELISTSIZE  200

ClassImp(RecoEvalSingleList_v2);

using namespace std;

RecoEvalSingleList_v2::RecoEvalSingleList_v2()
{
  RecoEvalSingleTrackN = 0;
  RecoList = new TClonesArray("RecoEvalSingleTrack_v2",RECOEVALSINGLELISTSIZE);
  return ;
}

RecoEvalSingleList_v2::~RecoEvalSingleList_v2()
{
  if (RecoList)
    {
      RecoList->Clear();
      delete RecoList;
    }
  return ;
}

int RecoEvalSingleList_v2::AddRecoEvalSingleTrack(RecoEvalSingleTrack 
						  *track, const int itrk)
{
  if(itrk<RecoList->GetSize()){
    TClonesArray &newhit = *RecoList;
    new (newhit[itrk]) RecoEvalSingleTrack_v2(track);
    return itrk;
  }else{
    cout << PHWHERE << "TClonesArray size of "
	 << RecoList->GetSize() 
	 << " too small for adding track" << endl;
    return -1;
  }
}

void RecoEvalSingleList_v2::Clear(Option_t *option) 
{
  RecoList->Clear();
  if (RecoEvalSingleTrackN > RECOEVALSINGLELISTSIZE)
    {
      RecoList->Expand(RECOEVALSINGLELISTSIZE);
    }
  RecoEvalSingleTrackN = 0;
  return;
}

void RecoEvalSingleList_v2::Reset()
{
  Clear();
  return;
}

int RecoEvalSingleList_v2::set_TClonesArraySize(const unsigned int fullsize)
{
  if (fullsize > RECOEVALSINGLELISTSIZE)
    {
      RecoList->Expand(fullsize);
    }
  return fullsize;
}

RecoEvalSingleTrack* RecoEvalSingleList_v2::get_RecoEvalSingleTrack(const int i) const
{
  if (i < RecoList->GetSize())
    {
      return (RecoEvalSingleTrack *) (RecoList->UncheckedAt(i));
    }
  else
    {
      cout << PHWHERE << "RecoEval " << i 
	   << " does not exist, number of tracks: " 
	   << RecoEvalSingleTrackN << endl;
    }
  return NULL;
}

void RecoEvalSingleList_v2::set_recoid(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_recoid(val);
}
void RecoEvalSingleList_v2::set_quality(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_quality(val);
}
void RecoEvalSingleList_v2::set_momentum(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_momentum(val);
}
void RecoEvalSingleList_v2::set_theta0(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_theta0(val);
}
void RecoEvalSingleList_v2::set_phi0(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_phi0(val);
}
void RecoEvalSingleList_v2::set_phi(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_phi(val);
}
void RecoEvalSingleList_v2::set_alpha(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_alpha(val);
}
void RecoEvalSingleList_v2::set_zed(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_zed(val);
}
void RecoEvalSingleList_v2::set_beta(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_beta(val);
}
void RecoEvalSingleList_v2::set_averagetime(const unsigned int i, 
					    const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_averagetime(val);
}
void RecoEvalSingleList_v2::set_xhits(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_xhits(val);
}
void RecoEvalSingleList_v2::set_uvhits(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_uvhits(val);
}
void RecoEvalSingleList_v2::set_mulmain(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_mulmain(val);
}
void RecoEvalSingleList_v2::set_mulxmain(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_mulxmain(val);
}
void RecoEvalSingleList_v2::set_muluvmain(const unsigned int i, 
					  const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_muluvmain(val);
}
void RecoEvalSingleList_v2::set_main(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_main(val);
}
void RecoEvalSingleList_v2::set_xmain(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_xmain(val);
}
void RecoEvalSingleList_v2::set_uvmain(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_uvmain(val);
}
void RecoEvalSingleList_v2::set_ambiguity(const unsigned int i, 
					  const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_ambiguity(val);
}
void RecoEvalSingleList_v2::set_purity(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_purity(val);
}
void RecoEvalSingleList_v2::set_xpurity(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_xpurity(val);
}
void RecoEvalSingleList_v2::set_uvpurity(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_uvpurity(val);
}
void RecoEvalSingleList_v2::set_pc1clusid(const unsigned int i, 
					  const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc1clusid(val);
}
void RecoEvalSingleList_v2::set_pc2clusid(const unsigned int i, 
					  const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc2clusid(val);
}
void RecoEvalSingleList_v2::set_pc3clusid(const unsigned int i, 
					  const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc3clusid(val);
}
void RecoEvalSingleList_v2::set_pc1clusidtrue(const unsigned int i, 
					      const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc1clusidtrue(val);
}
void RecoEvalSingleList_v2::set_pc2clusidtrue(const unsigned int i, 
					      const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc2clusidtrue(val);
}
void RecoEvalSingleList_v2::set_pc3clusidtrue(const unsigned int i, 
					      const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc3clusidtrue(val);
}
void RecoEvalSingleList_v2::set_pc1clusidg(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc1clusidg(val);
}
void RecoEvalSingleList_v2::set_pc2clusidg(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc2clusidg(val);
}
void RecoEvalSingleList_v2::set_pc3clusidg(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc3clusidg(val);
}
void RecoEvalSingleList_v2::set_pc1pointxg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc1pointxg(val);
}
void RecoEvalSingleList_v2::set_pc1pointyg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc1pointyg(val);
}
void RecoEvalSingleList_v2::set_pc1pointzg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc1pointzg(val);
}
void RecoEvalSingleList_v2::set_pc2pointxg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc2pointxg(val);
}
void RecoEvalSingleList_v2::set_pc2pointyg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc2pointyg(val);
}
void RecoEvalSingleList_v2::set_pc2pointzg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc2pointzg(val);
}
void RecoEvalSingleList_v2::set_pc3pointxg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc3pointxg(val);
}
void RecoEvalSingleList_v2::set_pc3pointyg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc3pointyg(val);
}
void RecoEvalSingleList_v2::set_pc3pointzg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_pc3pointzg(val);
}
void RecoEvalSingleList_v2::set_tofid(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_tofid(val);
}
void RecoEvalSingleList_v2::set_tofidtrue(const unsigned int i, 
					  const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_tofidtrue(val);
}
void RecoEvalSingleList_v2::set_tofidg(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_tofidg(val);
}
void RecoEvalSingleList_v2::set_tofpointxg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_tofpointxg(val);
}
void RecoEvalSingleList_v2::set_tofpointyg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_tofpointyg(val);
}
void RecoEvalSingleList_v2::set_tofpointzg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_tofpointzg(val);
}
void RecoEvalSingleList_v2::set_tofg(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_tofg(val);
}
void RecoEvalSingleList_v2::set_tofelossg(const unsigned int i, 
					  const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_tofelossg(val);
}
void RecoEvalSingleList_v2::set_emcclusid(const unsigned int i, 
					  const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcclusid(val);
}
void RecoEvalSingleList_v2::set_emcclusidtrue(const unsigned int i, 
					      const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcclusidtrue(val);
}
void RecoEvalSingleList_v2::set_emcclusidg(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcclusidg(val);
}
void RecoEvalSingleList_v2::set_emcanctrk0(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcanctrk0(val);
}
void RecoEvalSingleList_v2::set_emcanctrk1(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcanctrk1(val);
}
void RecoEvalSingleList_v2::set_emcanctrk2(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcanctrk2(val);
}
void RecoEvalSingleList_v2::set_emcanctwrhit0(const unsigned int i, 
					      const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcanctwrhit0(val);
}
void RecoEvalSingleList_v2::set_emcanctwrhit1(const unsigned int i, 
					      const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcanctwrhit1(val);
}
void RecoEvalSingleList_v2::set_emcanctwrhit2(const unsigned int i, 
					      const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcanctwrhit2(val);
}
void RecoEvalSingleList_v2::set_emcancpid0(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancpid0(val);
}
void RecoEvalSingleList_v2::set_emcancpid1(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancpid1(val);
}
void RecoEvalSingleList_v2::set_emcancpid2(const unsigned int i, 
					   const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancpid2(val);
}
void RecoEvalSingleList_v2::set_emcancedep0(const unsigned int i, 
					    const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancedep0(val);
}
void RecoEvalSingleList_v2::set_emcancedep1(const unsigned int i, 
					    const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancedep1(val);
}
void RecoEvalSingleList_v2::set_emcancedep2(const unsigned int i, 
					    const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancedep2(val);
}
void RecoEvalSingleList_v2::set_emcancptot0(const unsigned int i, 
					    const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancptot0(val);
}
void RecoEvalSingleList_v2::set_emcancptot1(const unsigned int i, 
					    const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancptot1(val);
}
void RecoEvalSingleList_v2::set_emcancptot2(const unsigned int i, 
					    const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcancptot2(val);
}
void RecoEvalSingleList_v2::set_emcpointxg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcpointxg(val);
}
void RecoEvalSingleList_v2::set_emcpointyg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcpointyg(val);
}
void RecoEvalSingleList_v2::set_emcpointzg(const unsigned int i, 
					   const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcpointzg(val);
}
void RecoEvalSingleList_v2::set_emcefracg(const unsigned int i, 
					  const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcefracg(val);
}
void RecoEvalSingleList_v2::set_emcecoreg(const unsigned int i, 
					  const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcecoreg(val);
}
void RecoEvalSingleList_v2::set_emcmeaseg(const unsigned int i, 
					  const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emcmeaseg(val);
}
void RecoEvalSingleList_v2::set_emctofg(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_emctofg(val);
}
void RecoEvalSingleList_v2::set_crkacc(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crkacc(val);
}
void RecoEvalSingleList_v2::set_crknpmt0(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crknpmt0(val);
}
void RecoEvalSingleList_v2::set_crknpmt1(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crknpmt1(val);
}
void RecoEvalSingleList_v2::set_crknpmt3(const unsigned int i, const short val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crknpmt3(val);
}
void RecoEvalSingleList_v2::set_crknpe0(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crknpe0(val);
}
void RecoEvalSingleList_v2::set_crknpe1(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crknpe1(val);
}
void RecoEvalSingleList_v2::set_crknpe3(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crknpe3(val);
}
void RecoEvalSingleList_v2::set_crkchi2(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crkchi2(val);
}
void RecoEvalSingleList_v2::set_crkdisp(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crkdisp(val);
}
void RecoEvalSingleList_v2::set_crkpath(const unsigned int i, const float val)
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) trk->set_crkpath(val);
}

short RecoEvalSingleList_v2::get_recoid(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_recoid();
  else return -999;
}
short RecoEvalSingleList_v2::get_quality(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_quality();
  else return -999;
}
float RecoEvalSingleList_v2::get_momentum(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_momentum();
  else return -999;
}
float RecoEvalSingleList_v2::get_theta0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_theta0();
  else return -999;
}
float RecoEvalSingleList_v2::get_phi0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_phi0();
  else return -999;
}
float RecoEvalSingleList_v2::get_phi(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_phi();
  else return -999;
}
float RecoEvalSingleList_v2::get_alpha(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_alpha();
  else return -999;
}
float RecoEvalSingleList_v2::get_zed(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_zed();
  else return -999;
}
float RecoEvalSingleList_v2::get_beta(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_beta();
  else return -999;
}
float RecoEvalSingleList_v2::get_averagetime(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_averagetime();
  else return -999;
}
short RecoEvalSingleList_v2::get_xhits(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_xhits();
  else return -999;
}
short RecoEvalSingleList_v2::get_uvhits(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_uvhits();
  else return -999;
}
short RecoEvalSingleList_v2::get_mulmain(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_mulmain();
  else return -999;
}
short RecoEvalSingleList_v2::get_mulxmain(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_mulxmain();
  else return -999;
}
short RecoEvalSingleList_v2::get_muluvmain(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_muluvmain();
  else return -999;
}
short RecoEvalSingleList_v2::get_main(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_main();
  else return -999;
}
short RecoEvalSingleList_v2::get_xmain(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_xmain();
  else return -999;
}
short RecoEvalSingleList_v2::get_uvmain(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_uvmain();
  else return -999;
}
short RecoEvalSingleList_v2::get_ambiguity(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_ambiguity();
  else return -999;
}
float RecoEvalSingleList_v2::get_purity(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_purity();
  else return -999;
}
float RecoEvalSingleList_v2::get_xpurity(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_xpurity();
  else return -999;
}
float RecoEvalSingleList_v2::get_uvpurity(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_uvpurity();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc1clusid(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc1clusid();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc2clusid(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc2clusid();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc3clusid(const unsigned int i) const 
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc3clusid();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc1clusidtrue(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc1clusidtrue();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc2clusidtrue(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc2clusidtrue();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc3clusidtrue(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc3clusidtrue();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc1clusidg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc1clusidg();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc2clusidg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc2clusidg();
  else return -999;
}
short RecoEvalSingleList_v2::get_pc3clusidg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc3clusidg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc1pointxg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc1pointxg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc1pointyg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc1pointyg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc1pointzg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc1pointzg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc2pointxg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc2pointxg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc2pointyg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc2pointyg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc2pointzg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc2pointzg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc3pointxg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc3pointxg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc3pointyg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc3pointyg();
  else return -999;
}
float RecoEvalSingleList_v2::get_pc3pointzg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_pc3pointzg();
  else return -999;
}
short RecoEvalSingleList_v2::get_tofid(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_tofid();
  else return -999;
}
short RecoEvalSingleList_v2::get_tofidtrue(const unsigned int i) const 
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_tofidtrue();
  else return -999;
}
short RecoEvalSingleList_v2::get_tofidg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_tofidg();
  else return -999;
}
float RecoEvalSingleList_v2::get_tofpointxg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_tofpointxg();
  else return -999;
}
float RecoEvalSingleList_v2::get_tofpointyg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_tofpointyg();
  else return -999;
}
float RecoEvalSingleList_v2::get_tofpointzg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_tofpointzg();
  else return -999;
}
float RecoEvalSingleList_v2::get_tofg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_tofg();
  else return -999;
}
float RecoEvalSingleList_v2::get_tofelossg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_tofelossg();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcclusid(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcclusid();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcclusidtrue(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcclusidtrue();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcclusidg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcclusidg();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcanctrk0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcanctrk0();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcanctrk1(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcanctrk1();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcanctrk2(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcanctrk2();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcanctwrhit0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcanctwrhit0();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcanctwrhit1(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcanctwrhit1();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcanctwrhit2(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcanctwrhit2();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcancpid0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancpid0();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcancpid1(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancpid1();
  else return -999;
}
short RecoEvalSingleList_v2::get_emcancpid2(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancpid2();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcancedep0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancedep0();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcancedep1(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancedep1();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcancedep2(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancedep2();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcancptot0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancptot0();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcancptot1(const unsigned int i) const 
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancptot1();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcancptot2(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcancptot2();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcpointxg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcpointxg();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcpointyg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcpointyg();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcpointzg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcpointzg();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcefracg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcefracg();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcecoreg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcecoreg();
  else return -999;
}
float RecoEvalSingleList_v2::get_emcmeaseg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emcmeaseg();
  else return -999;
}
float RecoEvalSingleList_v2::get_emctofg(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_emctofg();
  else return -999;
}
short RecoEvalSingleList_v2::get_crkacc(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crkacc();
  else return -999;
}
short RecoEvalSingleList_v2::get_crknpmt0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crknpmt0();
  else return -999;
}
short RecoEvalSingleList_v2::get_crknpmt1(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crknpmt1();
  else return -999;
}
short RecoEvalSingleList_v2::get_crknpmt3(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crknpmt3();
  else return -999;
}
float RecoEvalSingleList_v2::get_crknpe0(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crknpe0();
  else return -999;
}
float RecoEvalSingleList_v2::get_crknpe1(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crknpe1();
  else return -999;
}
float RecoEvalSingleList_v2::get_crknpe3(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crknpe3();
  else return -999;
}
float RecoEvalSingleList_v2::get_crkchi2(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crkchi2();
 else return -999;
}
float RecoEvalSingleList_v2::get_crkdisp(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crkdisp();
  else return -999;
}
float RecoEvalSingleList_v2::get_crkpath(const unsigned int i) const
{
  RecoEvalSingleTrack* trk = get_RecoEvalSingleTrack(i);
  if(trk) return trk->get_crkpath();
  else return -999;
}








