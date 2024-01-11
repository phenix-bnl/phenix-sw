#include <TClonesArray.h>

#include "BbcMultipleVtx_v2.hh"
#include "BbcMultipleVtxList_v2.hh"

using namespace std;

ClassImp(BbcMultipleVtx_v2);

BbcMultipleVtx_v2::BbcMultipleVtx_v2(const Int_t kNtimebin)
  : vtxlists( new TClonesArray("BbcMultipleVtxList_v2", kNtimebin) )
{
  vtxlists->SetOwner(kTRUE);
  TClonesArray &lists = *vtxlists;
  for(Int_t ibin=0; ibin<kNtimebin; ibin++) {
    new(lists[ibin]) BbcMultipleVtxList_v2();
  }
}


BbcMultipleVtx_v2::~BbcMultipleVtx_v2() {
  vtxlists->Delete();
  delete vtxlists;
}

void BbcMultipleVtx_v2::Reset() {
  for(Int_t ilist = 0; ilist < vtxlists->GetEntries(); ilist++) {
    BbcMultipleVtxList *list = (BbcMultipleVtxList*)(vtxlists->UncheckedAt(ilist));
    list->Reset();
  }
  fBbcOut_zvtx = -9999.;
  fBbcOut_t0 = -9999.;
}


BbcMultipleVtxList* BbcMultipleVtx_v2::get_vtxlist(const int tune) const {
  if( tune < vtxlists->GetEntries() ) {
    return (BbcMultipleVtxList*)(vtxlists->UncheckedAt(tune));
  } else {
    return 0;
  }
}

const int BbcMultipleVtx_v2::get_size() const {
  return vtxlists->GetEntries();
}

const float BbcMultipleVtx_v2::get_bbcout_zvtx() const {
  return fBbcOut_zvtx;
}

const float BbcMultipleVtx_v2::get_bbcout_t0() const {
  return fBbcOut_t0;
}

void BbcMultipleVtx_v2::set_bbcout_zvtx(const float zvtx) {
  fBbcOut_zvtx = zvtx;
}

void BbcMultipleVtx_v2::set_bbcout_t0(const float t0) {
  fBbcOut_t0 = t0;
}

void BbcMultipleVtx_v2::print() const {
  cout << Form("------------------------  ** BbcMultipleVtx **  ------------------------") << endl;
  cout << Form("  BbcOut:: zvtx = %8.2f cm t0 = %8.2f ns\n", fBbcOut_zvtx, fBbcOut_t0) << endl;
  for(Int_t ilist = 0; ilist < vtxlists->GetEntries(); ilist++) {
    BbcMultipleVtxList *list = (BbcMultipleVtxList*)(vtxlists->UncheckedAt(ilist));
    list->print();
  }
  cout << endl;
}

