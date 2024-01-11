#ifndef __DDCHRECONSTRUCTIONPARWRAPPER_H__
#define __DDCHRECONSTRUCTIONPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dDchReconstructionPar.h"
class dDchReconstructionParWrapper: public PHTable
{
public:
  dDchReconstructionParWrapper(const char* name = "dDchReconstructionPar", const size_t& max_rows = 1);
  ~dDchReconstructionParWrapper();

  void* RawTableData();
  DDCHRECONSTRUCTIONPAR_ST* TableData();

  DDCHRECONSTRUCTIONPAR_ST& operator[](const size_t& row);
  const DDCHRECONSTRUCTIONPAR_ST& operator[](const size_t& row) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_mirrorHitAnalysis(size_t n, short v) {
    fTableData[n].mirrorHitAnalysis = v;
  }
  short get_mirrorHitAnalysis(size_t n) const {
    return fTableData[n].mirrorHitAnalysis;
  }
  void set_houghThresholdOnXCell(size_t n, short v) {
    fTableData[n].houghThresholdOnXCell = v;
  }
  short get_houghThresholdOnXCell(size_t n) const {
    return fTableData[n].houghThresholdOnXCell;
  }
  void set_houghThresholdOnXMask(size_t n, short v) {
    fTableData[n].houghThresholdOnXMask = v;
  }
  short get_houghThresholdOnXMask(size_t n) const {
    return fTableData[n].houghThresholdOnXMask;
  }
  void set_houghThresholdOnUVCell(size_t n, short v) {
    fTableData[n].houghThresholdOnUVCell = v;
  }
  short get_houghThresholdOnUVCell(size_t n) const {
    return fTableData[n].houghThresholdOnUVCell;
  }
  void set_houghThresholdOnUVMask(size_t n, short v) {
    fTableData[n].houghThresholdOnUVMask = v;
  }
  short get_houghThresholdOnUVMask(size_t n) const {
    return fTableData[n].houghThresholdOnUVMask;
  }
  void set_purgeCandidateThreshold(size_t n, short v) {
    fTableData[n].purgeCandidateThreshold = v;
  }
  short get_purgeCandidateThreshold(size_t n) const {
    return fTableData[n].purgeCandidateThreshold;
  }
  void set_minimumNumberOfXHits(size_t n, short v) {
    fTableData[n].minimumNumberOfXHits = v;
  }
  short get_minimumNumberOfXHits(size_t n) const {
    return fTableData[n].minimumNumberOfXHits;
  }
  void set_minimumNumberOfUVHits(size_t n, short v) {
    fTableData[n].minimumNumberOfUVHits = v;
  }
  short get_minimumNumberOfUVHits(size_t n) const {
    return fTableData[n].minimumNumberOfUVHits;
  }
  void set_firstXHoughThreshold(size_t n, short v) {
    fTableData[n].firstXHoughThreshold = v;
  }
  short get_firstXHoughThreshold(size_t n) const {
    return fTableData[n].firstXHoughThreshold;
  }
  void set_secondXHoughThreshold(size_t n, short v) {
    fTableData[n].secondXHoughThreshold = v;
  }
  short get_secondXHoughThreshold(size_t n) const {
    return fTableData[n].secondXHoughThreshold;
  }
  void set_XHitsThreshold(size_t n, short v) {
    fTableData[n].XHitsThreshold = v;
  }
  short get_XHitsThreshold(size_t n) const {
    return fTableData[n].XHitsThreshold;
  }
  void set_numberOfAlphaBins(size_t n, int v) {
    fTableData[n].numberOfAlphaBins = v;
  }
  int get_numberOfAlphaBins(size_t n) const {
    return fTableData[n].numberOfAlphaBins;
  }
  void set_numberOfPhiBins(size_t n, int v) {
    fTableData[n].numberOfPhiBins = v;
  }
  int get_numberOfPhiBins(size_t n) const {
    return fTableData[n].numberOfPhiBins;
  }
  void set_numberOfBetaBins(size_t n, int v) {
    fTableData[n].numberOfBetaBins = v;
  }
  int get_numberOfBetaBins(size_t n) const {
    return fTableData[n].numberOfBetaBins;
  }
  void set_numberOfZedBins(size_t n, int v) {
    fTableData[n].numberOfZedBins = v;
  }
  int get_numberOfZedBins(size_t n) const {
    return fTableData[n].numberOfZedBins;
  }
  void set_cellDifferenceCut(size_t n, short v) {
    fTableData[n].cellDifferenceCut = v;
  }
  short get_cellDifferenceCut(size_t n) const {
    return fTableData[n].cellDifferenceCut;
  }
  void set_maxAlpha(size_t n, float v) {
    fTableData[n].maxAlpha = v;
  }
  float get_maxAlpha(size_t n) const {
    return fTableData[n].maxAlpha;
  }
  void set_minAlpha(size_t n, float v) {
    fTableData[n].minAlpha = v;
  }
  float get_minAlpha(size_t n) const {
    return fTableData[n].minAlpha;
  }
  void set_maxPhi(size_t n, float v) {
    fTableData[n].maxPhi = v;
  }
  float get_maxPhi(size_t n) const {
    return fTableData[n].maxPhi;
  }
  void set_minPhi(size_t n, float v) {
    fTableData[n].minPhi = v;
  }
  float get_minPhi(size_t n) const {
    return fTableData[n].minPhi;
  }
  void set_maxBeta(size_t n, float v) {
    fTableData[n].maxBeta = v;
  }
  float get_maxBeta(size_t n) const {
    return fTableData[n].maxBeta;
  }
  void set_minBeta(size_t n, float v) {
    fTableData[n].minBeta = v;
  }
  float get_minBeta(size_t n) const {
    return fTableData[n].minBeta;
  }
  void set_maxZed(size_t n, float v) {
    fTableData[n].maxZed = v;
  }
  float get_maxZed(size_t n) const {
    return fTableData[n].maxZed;
  }
  void set_minZed(size_t n, float v) {
    fTableData[n].minZed = v;
  }
  float get_minZed(size_t n) const {
    return fTableData[n].minZed;
  }
  void set_delBetaCut(size_t n, float v) {
    fTableData[n].delBetaCut = v;
  }
  float get_delBetaCut(size_t n) const {
    return fTableData[n].delBetaCut;
  }
  void set_deltaBetaCut(size_t n, float v) {
    fTableData[n].deltaBetaCut = v;
  }
  float get_deltaBetaCut(size_t n) const {
    return fTableData[n].deltaBetaCut;
  }
  void set_wireResolution(size_t n, float v) {
    fTableData[n].wireResolution = v;
  }
  float get_wireResolution(size_t n) const {
    return fTableData[n].wireResolution;
  }
  void set_initUVChi2(size_t n, float v) {
    fTableData[n].initUVChi2 = v;
  }
  float get_initUVChi2(size_t n) const {
    return fTableData[n].initUVChi2;
  }
  void set_initXChi2(size_t n, float v) {
    fTableData[n].initXChi2 = v;
  }
  float get_initXChi2(size_t n) const {
    return fTableData[n].initXChi2;
  }
  void set_deltaBetaVertexCut(size_t n, float v) {
    fTableData[n].deltaBetaVertexCut = v;
  }
  float get_deltaBetaVertexCut(size_t n) const {
    return fTableData[n].deltaBetaVertexCut;
  }

private:
  DDCHRECONSTRUCTIONPAR_ST* fTableData;

  ClassDef(dDchReconstructionParWrapper,1)
};
#endif /*__DDCHRECONSTRUCTIONPARWRAPPER_H__*/
