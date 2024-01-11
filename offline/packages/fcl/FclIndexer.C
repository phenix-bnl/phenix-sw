
//
// The indexer provides the interface to the database that provides
// the indexing scheme between row/column and signal channel.  It will
// eventually include the LV channel map also.
//
// SCJ (17 January 2002) First iteration.


#include "FclIndexer.h"
//INCLUDECHECKER: Removed this line: #include <iostream>

using namespace std;

FclIndexer* FclIndexer::_instance = 0;

FclIndexer* FclIndexer::Instance () {

  if (_instance ==0) {
    _instance = new FclIndexer;
  }
  return _instance;

}

FclIndexer::FclIndexer() {
  
  //cout << "Now you have the indexer" << endl;
  for (int ir = 0; ir < ROWTOT; ir++)
    {
      for (int ic = 0; ic < COLTOT; ic++)
        {
          nChannel[ir][ic] = INIT;
          sChannel[ir][ic] = INIT;
        }
    }
  
  
  // gives row and column for a given channel number (144 channels total)
  
  for (int ichan = 0; ichan < CHANTOT; ichan++)
    {
      nArrayRow[ichan] = INIT;
      nArrayColumn[ichan] = INIT;
      
      sArrayRow[ichan] = INIT;
      sArrayColumn[ichan] = INIT;
    }
  
  board[11][0] = INIT;
  board[11][1] = INIT;
  board[10][0] = INIT;
  board[10][1] = INIT;
  board[9][0] = 16;
  board[9][1] = 17;
  board[8][0] = 19;
  board[8][1] = 18;
  board[7][0] = 12;
  board[7][1] = 13;
  board[6][0] = 15;
  board[6][1] = 14;
  board[5][0] = 8;
  board[5][1] = 9;
  board[4][0] = 11;
  board[4][1] = 10;
  board[3][0] = 4;
  board[3][1] = 5;
  board[2][0] = 7;
  board[2][1] = 6;
  board[1][0] = 0;
  board[1][1] = 1;
  board[0][0] = 3;
  board[0][1] = 2;


  // loop over boards (6 possible but we only use 5)

  for (int boardNum = 0; boardNum < BOARDTOT - 1; boardNum++)
    {

      // loop over the rows in stack (10 of them)

      for (int row = 0; row < ROWTOT - 2; row++)
        {
          // North
          nChannel[row][2*boardNum] = board[row][0] + boardNum * 24;
          nChannel[row][2*boardNum + 1] = board[row][1] + boardNum * 24;
          // South
          sChannel[row][2*boardNum] = board[row][0] + boardNum * 24;
          sChannel[row][2*boardNum + 1] = board[row][1] + boardNum * 24;

        }
    }

  // North patch due to the missing signal cables, the
  // two corner channels are in channel 2 and 3 or j2 and j4

  nChannel[9][8] = 109;
  nChannel[8][8] = 110;
  nChannel[1][8] = 101;
  nChannel[0][8] = 102;

  // South patch after access on 15th of January

  sChannel[5][1] = 5;
  sChannel[5][0] = 4;
  sChannel[4][1] = 6;
  sChannel[4][0] = 7;

  sChannel[3][1] = 9;
  sChannel[3][0] = 8;
  sChannel[2][1] = 10;
  sChannel[2][0] = 11;


  sChannel[1][5] = 50;
  sChannel[0][5] = 97; // patch to 23/3

  sChannel[9][6] = 114; // patch to 44/2
  sChannel[8][6] = 88;

  // this column does not exist for the last board

  for (int ir = 0; ir < ROWTOT; ir++)
    {
      nChannel[ir][9] = INIT;
      sChannel[ir][9] = INIT;
    }

  // now get indices in grid for our channel configuration (10 rows x 9 cols ).
  for (int ic = 0; ic < COLTOT; ic++)
    {
      for (int ir = 0 ; ir < ROWTOT; ir++)
        {
          // North
          if (nChannel[ir][ic] != -1)
            {
              nArrayRow[nChannel[ir][ic]] = ir;
              nArrayColumn[nChannel[ir][ic]] = ic;
              // South
            }
          if (sChannel[ir][ic] != -1)
            {
              sArrayRow[sChannel[ir][ic]] = ir;
              sArrayColumn[sChannel[ir][ic]] = ic;
            }
        }
    }


  // Now do the LV channel map:

  //Initialize arrays:
  for (int i = 0; i<CHANTOT; i++){

    sArrayLV[i] = -1;
    nArrayLV[i] = -1;

  }

  int tempLVChan=0;

  //The south detector:
  for (int ir = 0; ir< 10; ir++){
    for (int ic = 0; ic< 9; ic++){

      sArrayLV[sChannel[ir][ic]] = tempLVChan;
      tempLVChan++;
    }
  }

  // One change in the LV distribution in the south:
  // The cable to channel 55 (row 6 column 1) goes to channel 92 in the board:
  sArrayLV[sChannel[6][1]] = 92;

  tempLVChan=0;
  // The north detector is swapped
  for (int ir = 0; ir< 10; ir++){
    for (int ic = 9-1; ic>-1; ic--){

      nArrayLV[nChannel[ir][ic]] = tempLVChan;
      tempLVChan++;
      
    }
  }


#ifdef DEBUG
  // output to double-check the above
  cout << "NORTH - Module map employed in all displays, beam is to the left: " << endl;
  for (int ir = ROWTOT - 1; ir > -1; ir--)
    {
      for (int ic = 0; ic < COLTOT; ic++)
        {
          if (nChannel[ir][ic] != -1)
            {
              cout << nArrayLV[nChannel[ir][ic]] << " ";
            }
        }
      cout << endl;
      
    }
  cout << "SOUTH Module map employed in all displays, beam is to the left: " << endl;
  for (int ir = ROWTOT - 1; ir > -1; ir--)
    {
      for (int ic = 0; ic < COLTOT; ic++)
        {
          if (sChannel[ir][ic] != -1)
            {
              cout << sArrayLV[sChannel[ir][ic]] << " ";
            }
        }
      cout << endl;
      
    }
#endif
  
  
}

int FclIndexer::getRow(int iSide, int k) {
  if (iSide==0) return getRowNorth(k);
  else if(iSide==1) return getRowSouth(k);
  else return -1;
}
int FclIndexer::getColumn(int iSide, int k) {
  if (iSide==0) return getColumnNorth(k);
  else if(iSide==1) return getColumnSouth(k);
  else return -1;
}
