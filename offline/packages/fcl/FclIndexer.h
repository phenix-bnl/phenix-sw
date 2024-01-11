#ifndef __FCLINDEXER__
#define __FCLINDEXER__

#include "FclConsts.h"

class FclIndexer {

 public:
  static FclIndexer* Instance();

  int getRowSouth(int k) {return sArrayRow[k];}
  int getRowNorth(int k) {return nArrayRow[k];}
  int getRow(int iSide, int k);
  int getColumnSouth(int k) {return sArrayColumn[k];}
  int getColumnNorth(int k) {return nArrayColumn[k];}
  int getColumn(int iSide, int k);
  int getLVSouth(int k) {return sArrayLV[k];}
  int getLVNorth(int k) {return nArrayLV[k];}
  int getLVSouth(int row, int col) {return sArrayLV[sChannel[row][col]];}
  int getLVNorth(int row, int col) {return nArrayLV[nChannel[row][col]];}

 protected:
  FclIndexer();

 private:
  static FclIndexer* _instance;
  
  int sChannel[ROWTOT][COLTOT]; 
  int nChannel[ROWTOT][COLTOT]; 
  
  int board[ROWTOT][2]; // a single board with 20 + 4 gap channels (first two columns of channel numbers in one board)
  
  // gives row and column for a given channel number (144 channels total)
  
  int nArrayRow[CHANTOT];
  int nArrayColumn[CHANTOT];
  
  int sArrayRow[CHANTOT];
  int sArrayColumn[CHANTOT];

  int sArrayLV[CHANTOT];
  int nArrayLV[CHANTOT];
  
  
};


#endif
