#include <iostream>
#include "McEvalSingleTrack_v2.h"
#include "McEvalSingleList_v2.h"
#include "RecoEvalSingleList.h"

#define MCEVALSINGLELISTSIZE  4000

ClassImp(McEvalSingleList_v2);

using namespace std;

McEvalSingleList_v2::McEvalSingleList_v2()
{
  McEvalSingleTrackN = 0;
  McList = new TClonesArray("McEvalSingleTrack_v2",MCEVALSINGLELISTSIZE);
  return ;
}

McEvalSingleList_v2::~McEvalSingleList_v2()
{
  if (McList)
    {
      Clear();
      delete McList;
    }
  return ;
}

int McEvalSingleList_v2::AddMcEvalSingleTrack(McEvalSingleTrack *track, 
					      const int itrk)
{
  if(itrk<McList->GetSize()){
    TClonesArray &newhit = *McList;
    new (newhit[itrk]) McEvalSingleTrack_v2(track);
    return itrk;
  }else{
    cout << PHWHERE << "TClonesArray size of "
	 << McList->GetSize() 
	 << " too small for adding track" << endl;
    return -1;
  }
}

void McEvalSingleList_v2::Clear(Option_t *option) 
{
  McList->Clear();
  if (McEvalSingleTrackN > MCEVALSINGLELISTSIZE)
    {
      McList->Expand(MCEVALSINGLELISTSIZE);
    }
  McEvalSingleTrackN = 0;
  return;
}

void McEvalSingleList_v2::Reset()
{
  Clear();
  return;
}

int McEvalSingleList_v2::set_TClonesArraySize(const unsigned int fullsize)
{
  if (fullsize > MCEVALSINGLELISTSIZE)
    {
      McList->Expand(fullsize);
    }
  return fullsize;
}

McEvalSingleTrack* McEvalSingleList_v2::get_McEvalSingleTrack(const int i) const
{
  if (i < McList->GetSize())
    {
      return (McEvalSingleTrack *) (McList->UncheckedAt(i));
    }
  else
    {
      cout << PHWHERE << "McEvalSingleList " << i 
	   << " does not exist, number of tracks: " 
	   << McEvalSingleTrackN << endl;
    }
  return NULL;
}

void McEvalSingleList_v2::set_eventid(const unsigned int i, const int val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_eventid(val);
}
void McEvalSingleList_v2::set_mctrackid(const unsigned int i, const int val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_mctrackid(val);
}
void McEvalSingleList_v2::set_generation(const unsigned int i, 
					 const int val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_generation(val);
}
void McEvalSingleList_v2::set_particleid(const unsigned int i, 
					 const int val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_particleid(val);
}
void McEvalSingleList_v2::set_parentid(const unsigned int i, const int val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_parentid(val);
}
void McEvalSingleList_v2::set_primaryid(const unsigned int i, const int val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_primaryid(val);
}
void McEvalSingleList_v2::set_vertexx(const unsigned int i, const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_vertexx(val);
}
void McEvalSingleList_v2::set_vertexy(const unsigned int i, const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_vertexy(val);
}
void McEvalSingleList_v2::set_vertexz(const unsigned int i, const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_vertexz(val);
}
void McEvalSingleList_v2::set_parentvertexx(const unsigned int i, 
					    const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_parentvertexx(val);
}
void McEvalSingleList_v2::set_parentvertexy(const unsigned int i, 
					    const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_parentvertexy(val);
}
void McEvalSingleList_v2::set_parentvertexz(const unsigned int i, 
					    const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_parentvertexz(val);
}
void McEvalSingleList_v2::set_primaryvertexx(const unsigned int i, 
					     const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_primaryvertexx(val);
}
void McEvalSingleList_v2::set_primaryvertexy(const unsigned int i, 
					     const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_primaryvertexy(val);
}
void McEvalSingleList_v2::set_primaryvertexz(const unsigned int i, 
					     const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_primaryvertexz(val);
}
void McEvalSingleList_v2::set_momentumx(const unsigned int i, 
					const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_momentumx(val);
}
void McEvalSingleList_v2::set_momentumy(const unsigned int i, 
					const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_momentumy(val);
}
void McEvalSingleList_v2::set_momentumz(const unsigned int i, 
					const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_momentumz(val);
}
void McEvalSingleList_v2::set_parentmomentumx(const unsigned int i, 
					      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_parentmomentumx(val);
}
void McEvalSingleList_v2::set_parentmomentumy(const unsigned int i, 
					      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_parentmomentumy(val);
}
void McEvalSingleList_v2::set_parentmomentumz(const unsigned int i, 
					      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_parentmomentumz(val);
}
void McEvalSingleList_v2::set_primarymomentumx(const unsigned int i, 
					       const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_primarymomentumx(val);
}
void McEvalSingleList_v2::set_primarymomentumy(const unsigned int i, 
					       const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_primarymomentumy(val);
}
void McEvalSingleList_v2::set_primarymomentumz(const unsigned int i, 
					       const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_primarymomentumz(val);
}
void McEvalSingleList_v2::set_quality(const unsigned int i, const int val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_quality(val);
}
void McEvalSingleList_v2::set_momentumr(const unsigned int i, 
					const double val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_momentumr(val);
}
void McEvalSingleList_v2::set_theta0(const unsigned int i, const double val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_theta0(val);
}
void McEvalSingleList_v2::set_phi0(const unsigned int i, const double val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_phi0(val);
}
void McEvalSingleList_v2::set_phi(const unsigned int i, const double val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_phi(val);
}
void McEvalSingleList_v2::set_alpha(const unsigned int i, const double val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_alpha(val);
}
void McEvalSingleList_v2::set_zed(const unsigned int i, const double val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_zed(val);
}
void McEvalSingleList_v2::set_beta(const unsigned int i, const double val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) trk->set_beta(val);
}

void McEvalSingleList_v2::set_recoid(const unsigned int i,
				     const unsigned int j, const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_recoid(j,val);
  }
}
void McEvalSingleList_v2::set_quality(const unsigned int i, 
				      const unsigned int j,
				      const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_quality(j,val);
  }
}
void McEvalSingleList_v2::set_momentum(const unsigned int i, 
				       const unsigned int j,
				       const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_momentum(j,val);
  }
}
void McEvalSingleList_v2::set_theta0(const unsigned int i, 
				     const unsigned int j,
				     const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_theta0(j,val);
  }
}
void McEvalSingleList_v2::set_phi0(const unsigned int i,
				   const unsigned int j, 
				   const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_phi0(j,val);
  }
}
void McEvalSingleList_v2::set_phi(const unsigned int i,
				  const unsigned int j, 
				  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_phi(j,val);
  }
}
void McEvalSingleList_v2::set_alpha(const unsigned int i, 
				    const unsigned int j,
				    const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_alpha(j,val);
  }
}
void McEvalSingleList_v2::set_zed(const unsigned int i,
				  const unsigned int j, 
				  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_zed(j,val);
  }
}
void McEvalSingleList_v2::set_beta(const unsigned int i,
				   const unsigned int j, 
				   const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_beta(j,val);
  }
}
void McEvalSingleList_v2::set_averagetime(const unsigned int i, 
					  const unsigned int j, 
					  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_averagetime(j,val);
  }
}
void McEvalSingleList_v2::set_xhits(const unsigned int i, 
				    const unsigned int j,
				    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_xhits(j,val);
  }
}
void McEvalSingleList_v2::set_uvhits(const unsigned int i, 
				     const unsigned int j,
				     const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_uvhits(j,val);
  }
}
void McEvalSingleList_v2::set_mulmain(const unsigned int i, 
				      const unsigned int j,
				      const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_mulmain(j,val);
  }
}
void McEvalSingleList_v2::set_mulxmain(const unsigned int i, 
				       const unsigned int j,
				       const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_mulxmain(j,val);
  }
}
void McEvalSingleList_v2::set_muluvmain(const unsigned int i, 
					const unsigned int j,
					const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_muluvmain(j,val);
  }
}
void McEvalSingleList_v2::set_main(const unsigned int i,
				   const unsigned int j,
				   const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_main(j,val);
  }
}
void McEvalSingleList_v2::set_xmain(const unsigned int i,
				    const unsigned int j,
				    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_xmain(j,val);
  }
}
void McEvalSingleList_v2::set_uvmain(const unsigned int i, 
				     const unsigned int j,
				     const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_uvmain(j,val);
  }
}
void McEvalSingleList_v2::set_ambiguity(const unsigned int i, 
					const unsigned int j,
					const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_ambiguity(j,val);
  }
}
void McEvalSingleList_v2::set_purity(const unsigned int i, 
				     const unsigned int j,
				     const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_purity(j,val);
  }
}
void McEvalSingleList_v2::set_xpurity(const unsigned int i, 
				      const unsigned int j,
				      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_xpurity(j,val);
  }
}
void McEvalSingleList_v2::set_uvpurity(const unsigned int i, 
				       const unsigned int j,
				       const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_uvpurity(j,val);
  }
}
void McEvalSingleList_v2::set_pc1clusid(const unsigned int i, 
					const unsigned int j,
					const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc1clusid(j,val);
  }
}
void McEvalSingleList_v2::set_pc2clusid(const unsigned int i, 
					const unsigned int j,
					const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc2clusid(j,val);
  }
}
void McEvalSingleList_v2::set_pc3clusid(const unsigned int i, 
					const unsigned int j,
					const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc3clusid(j,val);
  }
}
void McEvalSingleList_v2::set_pc1clusidtrue(const unsigned int i, 
					    const unsigned int j,
					    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc1clusidtrue(j,val);
  }
}
void McEvalSingleList_v2::set_pc2clusidtrue(const unsigned int i, 
					    const unsigned int j,
					    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc2clusidtrue(j,val);
  }
}
void McEvalSingleList_v2::set_pc3clusidtrue(const unsigned int i, 
					    const unsigned int j,
					    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc3clusidtrue(j,val);
  }
}
void McEvalSingleList_v2::set_pc1clusidg(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc1clusidg(j,val);
  }
}
void McEvalSingleList_v2::set_pc2clusidg(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc2clusidg(j,val);
  }
}
void McEvalSingleList_v2::set_pc3clusidg(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc3clusidg(j,val);
  }
}
void McEvalSingleList_v2::set_pc1pointxg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc1pointxg(j,val);
  }
}
void McEvalSingleList_v2::set_pc1pointyg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc1pointyg(j,val);
  }
}
void McEvalSingleList_v2::set_pc1pointzg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc1pointzg(j,val);
  }
}
void McEvalSingleList_v2::set_pc2pointxg(const unsigned int i, 
					 const unsigned int j,
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc2pointxg(j,val);
  }
}
void McEvalSingleList_v2::set_pc2pointyg(const unsigned int i, 
					 const unsigned int j,
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc2pointyg(j,val);
  }
}
void McEvalSingleList_v2::set_pc2pointzg(const unsigned int i, 
					 const unsigned int j,
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc2pointzg(j,val);
  }
}
void McEvalSingleList_v2::set_pc3pointxg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc3pointxg(j,val);
  }
}
void McEvalSingleList_v2::set_pc3pointyg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc3pointyg(j,val);
  }
}
void McEvalSingleList_v2::set_pc3pointzg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_pc3pointzg(j,val);
  }
}
void McEvalSingleList_v2::set_tofid(const unsigned int i,
				    const unsigned int j,
				    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_tofid(j,val);
  }
}
void McEvalSingleList_v2::set_tofidtrue(const unsigned int i, 
					const unsigned int j,
					const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_tofidtrue(j,val);
  }
}
void McEvalSingleList_v2::set_tofidg(const unsigned int i, 
				     const unsigned int j,
				     const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_tofidg(j,val);
  }
}
void McEvalSingleList_v2::set_tofpointxg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_tofpointxg(j,val);
  }
}
void McEvalSingleList_v2::set_tofpointyg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_tofpointyg(j,val);
  }
}
void McEvalSingleList_v2::set_tofpointzg(const unsigned int i, 
					 const unsigned int j, 
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_tofpointzg(j,val);
  }
}
void McEvalSingleList_v2::set_tofg(const unsigned int i,
				   const unsigned int j,
				   const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_tofg(j,val);
  }
}
void McEvalSingleList_v2::set_tofelossg(const unsigned int i, 
					const unsigned int j,
					const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_tofelossg(j,val);
  }
}
void McEvalSingleList_v2::set_emcclusid(const unsigned int i, 
					const unsigned int j, 
					const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcclusid(j,val);
  }
}
void McEvalSingleList_v2::set_emcclusidtrue(const unsigned int i, 
					    const unsigned int j,
					    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcclusidtrue(j,val);
  }
}
void McEvalSingleList_v2::set_emcclusidg(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcclusidg(j,val);
  }
}
void McEvalSingleList_v2::set_emcanctrk0(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcanctrk0(j,val);
  }
}
void McEvalSingleList_v2::set_emcanctrk1(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcanctrk1(j,val);
  }
}
void McEvalSingleList_v2::set_emcanctrk2(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcanctrk2(j,val);
  }
}
void McEvalSingleList_v2::set_emcanctwrhit0(const unsigned int i, 
					    const unsigned int j,
					    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcanctwrhit0(j,val);
  }
}
void McEvalSingleList_v2::set_emcanctwrhit1(const unsigned int i, 
					    const unsigned int j,
					    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcanctwrhit1(j,val);
  }
}
void McEvalSingleList_v2::set_emcanctwrhit2(const unsigned int i, 
					    const unsigned int j,
					    const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcanctwrhit2(j,val);
  }
}
void McEvalSingleList_v2::set_emcancpid0(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancpid0(j,val);
  }
}
void McEvalSingleList_v2::set_emcancpid1(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancpid1(j,val);
  }
}
void McEvalSingleList_v2::set_emcancpid2(const unsigned int i, 
					 const unsigned int j,
					 const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancpid2(j,val);
  }
}
void McEvalSingleList_v2::set_emcancedep0(const unsigned int i, 
					  const unsigned int j,
					  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancedep0(j,val);
  }
}
void McEvalSingleList_v2::set_emcancedep1(const unsigned int i, 
					  const unsigned int j,
					  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancedep1(j,val);
  }
}
void McEvalSingleList_v2::set_emcancedep2(const unsigned int i, 
					  const unsigned int j,
					  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancedep2(j,val);
  }
}
void McEvalSingleList_v2::set_emcancptot0(const unsigned int i, 
					  const unsigned int j,
					  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancptot0(j,val);
  }
}
void McEvalSingleList_v2::set_emcancptot1(const unsigned int i, 
					  const unsigned int j,
					  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancptot1(j,val);
  }
}
void McEvalSingleList_v2::set_emcancptot2(const unsigned int i, 
					  const unsigned int j,
					  const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcancptot2(j,val);
  }
}
void McEvalSingleList_v2::set_emcpointxg(const unsigned int i, 
					 const unsigned int j,
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcpointxg(j,val);
  }
}
void McEvalSingleList_v2::set_emcpointyg(const unsigned int i, 
					 const unsigned int j,
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcpointyg(j,val);
  }
}
void McEvalSingleList_v2::set_emcpointzg(const unsigned int i, 
					 const unsigned int j,
					 const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcpointzg(j,val);
  }
}
void McEvalSingleList_v2::set_emcefracg(const unsigned int i, 
					const unsigned int j,
					const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcefracg(j,val);
  }
}
void McEvalSingleList_v2::set_emcecoreg(const unsigned int i, 
					const unsigned int j,
					const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcecoreg(j,val);
  }
}
void McEvalSingleList_v2::set_emcmeaseg(const unsigned int i, 
					const unsigned int j,
					const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emcmeaseg(j,val);
  }
}
void McEvalSingleList_v2::set_emctofg(const unsigned int i, 
				      const unsigned int j,
				      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_emctofg(j,val);
  }
}
void McEvalSingleList_v2::set_crkacc(const unsigned int i, 
				     const unsigned int j,
				     const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crkacc(j,val);
  }
}
void McEvalSingleList_v2::set_crknpmt0(const unsigned int i, 
				       const unsigned int j,
				       const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crknpmt0(j,val);
  }
}
void McEvalSingleList_v2::set_crknpmt1(const unsigned int i, 
				       const unsigned int j,
				       const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crknpmt1(j,val);
  }
}
void McEvalSingleList_v2::set_crknpmt3(const unsigned int i, 
				       const unsigned int j,
				       const short val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crknpmt3(j,val);
  }
}
void McEvalSingleList_v2::set_crknpe0(const unsigned int i, 
				      const unsigned int j,
				      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crknpe0(j,val);
  }
}
void McEvalSingleList_v2::set_crknpe1(const unsigned int i, 
				      const unsigned int j,
				      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crknpe1(j,val);
  }
}
void McEvalSingleList_v2::set_crknpe3(const unsigned int i, 
				      const unsigned int j,
				      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crknpe3(j,val);
  }
}
void McEvalSingleList_v2::set_crkchi2(const unsigned int i, 
				      const unsigned int j,
				      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crkchi2(j,val);
  }
}
void McEvalSingleList_v2::set_crkdisp(const unsigned int i,
				      const unsigned int j,
				      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crkdisp(j,val);
  }
}
void McEvalSingleList_v2::set_crkpath(const unsigned int i,
				      const unsigned int j,
				      const float val)
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if(trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    tmparray->set_crkpath(j,val);
  }
}

int McEvalSingleList_v2::get_eventid(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_eventid();
  else return -999;
}
int McEvalSingleList_v2::get_mctrackid(const unsigned int i) const  
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_mctrackid();
  else return -999;
}
int McEvalSingleList_v2::get_generation(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_generation();
  else return -999;
}
int McEvalSingleList_v2::get_particleid(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_particleid();
  else return -999;
}
int McEvalSingleList_v2::get_parentid(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_parentid();
  else return -999;
}
int McEvalSingleList_v2::get_primaryid(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_primaryid();
  else return -999;
}
float McEvalSingleList_v2::get_vertexx(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_vertexx();
  else return -999;
}
float McEvalSingleList_v2::get_vertexy(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_vertexy();
  else return -999;
}
float McEvalSingleList_v2::get_vertexz(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_vertexz();
  else return -999;
}
float McEvalSingleList_v2::get_parentvertexx(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_parentvertexx();
  else return -999;
}
float McEvalSingleList_v2::get_parentvertexy(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_parentvertexy();
  else return -999;
}
float McEvalSingleList_v2::get_parentvertexz(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_parentvertexz();
  else return -999;
}
float McEvalSingleList_v2::get_primaryvertexx(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_primaryvertexx();
  else return -999;
}
float McEvalSingleList_v2::get_primaryvertexy(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_primaryvertexy();
  else return -999;
}
float McEvalSingleList_v2::get_primaryvertexz(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_primaryvertexz();
  else return -999;
}
float McEvalSingleList_v2::get_momentumx(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_momentumx();
  else return -999;
}
float McEvalSingleList_v2::get_momentumy(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_momentumy();
  else return -999;
}
float McEvalSingleList_v2::get_momentumz(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_momentumz();
  else return -999;
}
float McEvalSingleList_v2::get_parentmomentumx(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_parentmomentumx();
  else return -999;
}
float McEvalSingleList_v2::get_parentmomentumy(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_parentmomentumy();
  else return -999;
}
float McEvalSingleList_v2::get_parentmomentumz(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_parentmomentumz();
  else return -999;
}
float McEvalSingleList_v2::get_primarymomentumx(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_primarymomentumx();
  else return -999;
}
float McEvalSingleList_v2::get_primarymomentumy(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_primarymomentumy();
  else return -999;
}
float McEvalSingleList_v2::get_primarymomentumz(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_primarymomentumz();
  else return -999;
}
int McEvalSingleList_v2::get_quality(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_quality();
  else return -999;
}
double McEvalSingleList_v2::get_momentumr(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_momentumr();
  else return -999;
}
double McEvalSingleList_v2::get_theta0(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_theta0();
  else return -999;
}
double McEvalSingleList_v2::get_phi0(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_phi0();
  else return -999;
}
double McEvalSingleList_v2::get_phi(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_phi();
  else return -999;
}
double McEvalSingleList_v2::get_alpha(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_alpha();
  else return -999;
}
double McEvalSingleList_v2::get_zed(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_zed();
  else return -999;
}
double McEvalSingleList_v2::get_beta(const unsigned int i) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) return trk->get_beta();
  else return -999;
}


short McEvalSingleList_v2::get_Nreco(const unsigned int i) const  
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_RecoEvalSingleTrackN();
  }
  else {
    return -999;
  }
}


short McEvalSingleList_v2::get_recoid(const unsigned int i,
				      const unsigned int j) const  
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_recoid(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_quality(const unsigned int i, 
				       const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_quality(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_momentum(const unsigned int i, 
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_momentum(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_theta0(const unsigned int i, 
				       const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_theta0(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_phi0(const unsigned int i, 
				     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_phi0(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_phi(const unsigned int i, 
				    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_phi(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_alpha(const unsigned int i,
				      const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_alpha(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_zed(const unsigned int i,
				    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_zed(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_beta(const unsigned int i,
				     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_beta(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_averagetime(const unsigned int i,
					    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_averagetime(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_xhits(const unsigned int i,
				     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_xhits(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_uvhits(const unsigned int i,
				      const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_uvhits(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_mulmain(const unsigned int i,
				       const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_mulmain(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_mulxmain(const unsigned int i,
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_mulxmain(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_muluvmain(const unsigned int i,
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_muluvmain(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_main(const unsigned int i, 
				    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_main(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_xmain(const unsigned int i,
				     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_xmain(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_uvmain(const unsigned int i, 
				      const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_uvmain(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_ambiguity(const unsigned int i, 
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_ambiguity(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_purity(const unsigned int i, 
				       const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_purity(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_xpurity(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_xpurity(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_uvpurity(const unsigned int i,
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_uvpurity(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc1clusid(const unsigned int i,
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc1clusid(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc2clusid(const unsigned int i,
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc2clusid(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc3clusid(const unsigned int i,
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc3clusid(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc1clusidtrue(const unsigned int i,
					     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc1clusidtrue(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc2clusidtrue(const unsigned int i,
					     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc2clusidtrue(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc3clusidtrue(const unsigned int i,
					     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc3clusidtrue(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc1clusidg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc1clusidg(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc2clusidg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc2clusidg(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_pc3clusidg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc3clusidg(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_pc1pointxg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc1pointxg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_pc1pointyg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc1pointyg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_pc1pointzg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc1pointzg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_pc2pointxg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc2pointxg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_pc2pointyg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc2pointyg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_pc2pointzg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc2pointzg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_pc3pointxg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc3pointxg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_pc3pointyg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc3pointyg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_pc3pointzg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_pc3pointzg(j);
  }
  else return -999;
}
short McEvalSingleList_v2::get_tofid(const unsigned int i,
				     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_tofid(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_tofidtrue(const unsigned int i,
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_tofidtrue(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_tofidg(const unsigned int i,
				      const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_tofidg(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_tofpointxg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_tofpointxg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_tofpointyg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_tofpointyg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_tofpointzg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_tofpointzg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_tofg(const unsigned int i,
				     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_tofg(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_tofelossg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_tofelossg(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcclusid(const unsigned int i,
					 const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcclusid(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcclusidtrue(const unsigned int i,
					     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcclusidtrue(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcclusidg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcclusidg(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcanctrk0(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcanctrk0(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcanctrk1(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcanctrk1(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcanctrk2(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcanctrk2(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcanctwrhit0(const unsigned int i,
					     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcanctwrhit0(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcanctwrhit1(const unsigned int i,
					     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcanctwrhit1(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcanctwrhit2(const unsigned int i,
					     const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcanctwrhit2(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcancpid0(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancpid0(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcancpid1(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancpid1(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_emcancpid2(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancpid2(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcancedep0(const unsigned int i,
					    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancedep0(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcancedep1(const unsigned int i,
					    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancedep1(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcancedep2(const unsigned int i,
					    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancedep2(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcancptot0(const unsigned int i,
					    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancptot0(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcancptot1(const unsigned int i,
					    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancptot1(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcancptot2(const unsigned int i,
					    const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcancptot2(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcpointxg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcpointxg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_emcpointyg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcpointyg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_emcpointzg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcpointzg(j);
  }
  else return -999;
}
float McEvalSingleList_v2::get_emcefracg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcefracg(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcecoreg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcecoreg(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emcmeaseg(const unsigned int i,
					  const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emcmeaseg(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_emctofg(const unsigned int i,
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_emctofg(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_crkacc(const unsigned int i, 
				      const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crkacc(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_crknpmt0(const unsigned int i, 
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crknpmt0(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_crknpmt1(const unsigned int i, 
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crknpmt1(j);
  }
  else {
    return -999;
  }
}
short McEvalSingleList_v2::get_crknpmt3(const unsigned int i, 
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crknpmt3(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_crknpe0(const unsigned int i, 
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crknpe0(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_crknpe1(const unsigned int i, 
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crknpe1(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_crknpe3(const unsigned int i,
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crknpe3(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_crkchi2(const unsigned int i,
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crkchi2(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_crkdisp(const unsigned int i,
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crkdisp(j);
  }
  else {
    return -999;
  }
}
float McEvalSingleList_v2::get_crkpath(const unsigned int i,
					const unsigned int j) const
{
  McEvalSingleTrack* trk = get_McEvalSingleTrack(i);
  if (trk) {
    RecoEvalSingleList *tmparray = trk->get_RecoEvalSingleList();
    return tmparray->get_crkpath(j);
  }
  else {
    return -999;
  }
}







