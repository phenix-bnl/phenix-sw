#include "dDchReconstructionParWrapper.h"

ClassImp(dDchReconstructionParWrapper)

dDchReconstructionParWrapper::dDchReconstructionParWrapper(const char* name, const size_t& max_rows)
  : PHTable(name,max_rows)
{
  size_t rowSize = sizeof(DDCHRECONSTRUCTIONPAR_ST);
  if (max_rows > 0) { // Avoid allocating a space of zero size!
     fTableData = new DDCHRECONSTRUCTIONPAR_ST[max_rows];
     SetMaxRowCount(max_rows);
  }
  else {
     fTableData = new DDCHRECONSTRUCTIONPAR_ST[1];
     SetMaxRowCount(1);
  }

  SetRowSize(rowSize);
  SetType("dDchReconstructionPar");
  fTableHeader->dsl_pointer  = (long)this;
  fTableHeader->data_pointer = (long)fTableData;
}

dDchReconstructionParWrapper::~dDchReconstructionParWrapper()
{
  delete [] fTableData;
}

void*
dDchReconstructionParWrapper::RawTableData()
{
  return static_cast<void*>(fTableData);
}

DDCHRECONSTRUCTIONPAR_ST*
dDchReconstructionParWrapper::TableData()
{
  return fTableData;
}

DDCHRECONSTRUCTIONPAR_ST&
dDchReconstructionParWrapper::operator[](const size_t& row)
{
  return fTableData[row];
}

const DDCHRECONSTRUCTIONPAR_ST&
dDchReconstructionParWrapper::operator[](const size_t& row) const
{
  return fTableData[row];
}

void
dDchReconstructionParWrapper::SetMaxRowCount(const size_t& max_rows)
{
  // Avoid reallocing a space of zero size!
  if (max_rows <= 0) {
     return;
  }

  // Ensure that the current row count is not out of range.
  if ((size_t) fTableHeader->nok > max_rows) {
     fTableHeader->nok = max_rows;
  }

  // If table needs to grow, allocate a new area for it.
  if (max_rows > (size_t) fTableHeader->maxlen) {
     DDCHRECONSTRUCTIONPAR_ST* newData = new DDCHRECONSTRUCTIONPAR_ST[max_rows];
     if (fTableData) {
        for (long i = 0; i < fTableHeader->nok; i++) {
           newData[i] = fTableData[i];
        }
        delete [] fTableData;
     }
     fTableData = newData;
     fTableHeader->data_pointer = (long)fTableData;
  }

  fTableHeader->maxlen = max_rows;
}
void
dDchReconstructionParWrapper::SetRowCount(const size_t& n)
{
  if (n > (size_t) fTableHeader->maxlen) {
     fTableHeader->nok = fTableHeader->maxlen;
  }
  else if (n >= 0) {
     fTableHeader->nok = n;
  }
}

void
dDchReconstructionParWrapper::SetRowSize(const size_t& row_size)
{
  if (row_size > 0) {
     fTableHeader->rbytes = row_size;
  }
}

void
dDchReconstructionParWrapper::Streamer(TBuffer& b)
{
   // Stream an object of class dDchReconstructionParWrapper.
   // What should be done on output if the table is empty?

   if (b.IsReading()) {
     b.ReadVersion();
     PHTable::Streamer(b);         // Read the table header.
     if (RowSize() != sizeof(DDCHRECONSTRUCTIONPAR_ST)) {
       // Sanity check failed.  Need some error message here.
       return;
     }

 	   // Reallocate the table explicitly here; the size of the data array
 	   // may be inconsistent with the max. row count variable in the header
 	   // (since the ROOT I/O default-constructs the former, and reads
 	   // the header for the latter).
 	   size_t max_rows = MaxRowCount();
 	   if (max_rows <= 0) { // Avoid allocating a space of zero size!
 	      max_rows = 1;
 	   }

 	   delete [] fTableData;
 	   fTableData = new DDCHRECONSTRUCTIONPAR_ST[max_rows];
 	   fTableHeader->data_pointer = (long)fTableData;

     SetMaxRowCount(max_rows);
     SetType("dDchReconstructionParWrapper");

     for (unsigned long i=0; i<RowCount(); i++) {
        b >> fTableData[i].mirrorHitAnalysis;
        b >> fTableData[i].houghThresholdOnXCell;
        b >> fTableData[i].houghThresholdOnXMask;
        b >> fTableData[i].houghThresholdOnUVCell;
        b >> fTableData[i].houghThresholdOnUVMask;
        b >> fTableData[i].purgeCandidateThreshold;
        b >> fTableData[i].minimumNumberOfXHits;
        b >> fTableData[i].minimumNumberOfUVHits;
        b >> fTableData[i].firstXHoughThreshold;
        b >> fTableData[i].secondXHoughThreshold;
        b >> fTableData[i].XHitsThreshold;
        b >> fTableData[i].numberOfAlphaBins;
        b >> fTableData[i].numberOfPhiBins;
        b >> fTableData[i].numberOfBetaBins;
        b >> fTableData[i].numberOfZedBins;
        b >> fTableData[i].cellDifferenceCut;
        b >> fTableData[i].maxAlpha;
        b >> fTableData[i].minAlpha;
        b >> fTableData[i].maxPhi;
        b >> fTableData[i].minPhi;
        b >> fTableData[i].maxBeta;
        b >> fTableData[i].minBeta;
        b >> fTableData[i].maxZed;
        b >> fTableData[i].minZed;
        b >> fTableData[i].delBetaCut;
        b >> fTableData[i].deltaBetaCut;
        b >> fTableData[i].wireResolution;
        b >> fTableData[i].initUVChi2;
        b >> fTableData[i].initXChi2;
        b >> fTableData[i].deltaBetaVertexCut;
     }
   }
   else {
     b.WriteVersion(IsA());
     PHTable::Streamer(b);         // Write the table header.
     for (unsigned long i=0; i<RowCount(); i++) {
        b << fTableData[i].mirrorHitAnalysis;
        b << fTableData[i].houghThresholdOnXCell;
        b << fTableData[i].houghThresholdOnXMask;
        b << fTableData[i].houghThresholdOnUVCell;
        b << fTableData[i].houghThresholdOnUVMask;
        b << fTableData[i].purgeCandidateThreshold;
        b << fTableData[i].minimumNumberOfXHits;
        b << fTableData[i].minimumNumberOfUVHits;
        b << fTableData[i].firstXHoughThreshold;
        b << fTableData[i].secondXHoughThreshold;
        b << fTableData[i].XHitsThreshold;
        b << fTableData[i].numberOfAlphaBins;
        b << fTableData[i].numberOfPhiBins;
        b << fTableData[i].numberOfBetaBins;
        b << fTableData[i].numberOfZedBins;
        b << fTableData[i].cellDifferenceCut;
        b << fTableData[i].maxAlpha;
        b << fTableData[i].minAlpha;
        b << fTableData[i].maxPhi;
        b << fTableData[i].minPhi;
        b << fTableData[i].maxBeta;
        b << fTableData[i].minBeta;
        b << fTableData[i].maxZed;
        b << fTableData[i].minZed;
        b << fTableData[i].delBetaCut;
        b << fTableData[i].deltaBetaCut;
        b << fTableData[i].wireResolution;
        b << fTableData[i].initUVChi2;
        b << fTableData[i].initXChi2;
        b << fTableData[i].deltaBetaVertexCut;
     }
   }

}
/* Automatically generated.  Do not edit. */
