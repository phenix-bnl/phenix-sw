#include <TObjArray.h>

#include "BbcMultipleVtx_v1.hh"
#include "BbcMultipleVtxList.hh"

using namespace std;

ClassImp(BbcMultipleVtx_v1);

BbcMultipleVtx_v1::BbcMultipleVtx_v1()
  : vtxlists( new TObjArray() )
{
  vtxlists->SetOwner(kTRUE);
}


BbcMultipleVtx_v1::~BbcMultipleVtx_v1() {
  vtxlists->Delete();
  delete vtxlists;
}

void BbcMultipleVtx_v1::Reset() {
  for(Int_t ilist = 0; ilist < vtxlists->GetEntries(); ilist++) {
    BbcMultipleVtxList *list = (BbcMultipleVtxList*)(vtxlists->At(ilist));
    list->Reset();
  }
  fBbcOut_zvtx = -9999.;
  fBbcOut_t0 = -9999.;
}


BbcMultipleVtxList* BbcMultipleVtx_v1::get_vtxlist(const int tune) const {
  if( tune < vtxlists->GetEntries() ) {
    return (BbcMultipleVtxList*)(vtxlists->At(tune));
  } else {
    return 0;
  }
}

const int BbcMultipleVtx_v1::get_size() const {
  return vtxlists->GetEntries();
}

const float BbcMultipleVtx_v1::get_bbcout_zvtx() const {
  return fBbcOut_zvtx;
}

const float BbcMultipleVtx_v1::get_bbcout_t0() const {
  return fBbcOut_t0;
}

void BbcMultipleVtx_v1::add_vtxlist(BbcMultipleVtxList *vtxlist) {
  vtxlists->Add( vtxlist );
}


void BbcMultipleVtx_v1::set_bbcout_zvtx(const float zvtx) {
  fBbcOut_zvtx = zvtx;
}

void BbcMultipleVtx_v1::set_bbcout_t0(const float t0) {
  fBbcOut_t0 = t0;
}

void BbcMultipleVtx_v1::print() const {
  cout << Form("------------------------  ** BbcMultipleVtx **  ------------------------") << endl;
  cout << Form("  BbcOut:: zvtx = %8.2f cm t0 = %8.2f ns\n", fBbcOut_zvtx, fBbcOut_t0) << endl;
  for(Int_t ilist = 0; ilist < vtxlists->GetEntries(); ilist++) {
    BbcMultipleVtxList *list = (BbcMultipleVtxList*)(vtxlists->At(ilist));
    list->print();
  }
  cout << endl;
}

