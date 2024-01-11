#include "BbcMultipleVtx.hh"
#include "BbcMultipleVtxList.hh"
#include <cmath>

using namespace std;

ClassImp(BbcMultipleVtx);

BbcMultipleVtx::BbcMultipleVtx() {}

BbcMultipleVtx::~BbcMultipleVtx() {}

void BbcMultipleVtx::Reset() {
}


BbcMultipleVtxList* BbcMultipleVtx::get_vtxlist(const int tune) const {
  return 0;
}

const int BbcMultipleVtx::get_size() const {
  return 0;
}

const float BbcMultipleVtx::get_bbcout_zvtx() const {
  return NAN;
}

const float BbcMultipleVtx::get_bbcout_t0() const {
  return NAN;
}

void BbcMultipleVtx::add_vtxlist(BbcMultipleVtxList *vtxlist) {
  return;
}


void BbcMultipleVtx::set_bbcout_zvtx(const float zvtx) {
  return;
}

void BbcMultipleVtx::set_bbcout_t0(const float t0) {
  return;
}

void BbcMultipleVtx::print() const {
}

