#include <iostream>
#include <fstream>
#include <cstdio>
#include <sys/stat.h>
#include <sys/types.h>

//
// this makes the required arcnet hex files for download
// to the bbc/zdc/ntc trigger look up tables
// the input file is a ZdcCalib.tdc1lut
//

using namespace std;

static const int nch = 8;		// 8 fem channels
static const int ntdcval = 2048;	// 11 bit tdc1 value

int main(int argc, char **argv)
{
  int board = 1;

  ifstream textfile( argv[1] );
  cout << "reading from file " << argv[1] << endl;

  char fname[1000];
  while ( !textfile.eof() )
    {
      // allocate space for lookup table
      // which is 8 channels * 11 bits of address per channel
      int level1map[nch][ntdcval];

      for (int ich=0; ich<nch; ich++)
        {
          for (int itdc=0; itdc<ntdcval; itdc++)
            {
	      textfile >> level1map[ich][itdc];
	      if (textfile.eof()) 
	        {
	          exit(0);
	        }
            }
        }

      // create board directory
      cout << "processing board " << board << endl;
      sprintf(fname,"board%02d",board);
      if ( mkdir(fname,0755) != 0 )
        {
	  cout << fname << " found, overwriting files" << endl;
	}

      // keep track of number of files in directory
      int fcount = 0;	// filecount

      // init fem for lut download
      sprintf(fname,"board%02d/%03d_board%02d_init.hex",board,fcount,board);
      ofstream initfile(fname);
      initfile << "1 30 0 0" << endl;
      initfile << "0 0 0 14" << endl;
      initfile << "1 90 " << board << endl;	// set board to talk to
      initfile << "1 b0 0" << endl;		// set board offline
      initfile << "1 b0 80" << endl;		// send reset pulse, puts board in ready state
      initfile << "1 b0 42" << endl;		// set board to write trigger map
      initfile.close();
      fcount++;

     ofstream level1mapfile;
     for (int ich=0; ich<nch; ich++)
       {
         if (ich==0)
           {
	     sprintf(fname,"board%02d/%03d_ch%03d.hex",board,fcount,ich);
	     level1mapfile.open(fname);
	     level1mapfile << "1 30 0 0" << endl;
	     level1mapfile << "0 0 0 b" << endl;
	     level1mapfile << "1 b0 c5" << endl;	// enable chip select for mem1 (ch 0-3)
	     level1mapfile.close();
	     fcount++;
           }

         if (ich==4)
           {
             sprintf(fname,"board%02d/%03d_ch%03d.hex",board,fcount,ich);
	     level1mapfile.open(fname);
	     level1mapfile << "1 30 0 0" << endl;
	     level1mapfile << "0 0 0 b" << endl;
	     level1mapfile << "1 b0 c6" << endl;    // enable chip select for mem1 (ch 0-3)
	     level1mapfile.close();
	     fcount++;
	   }

         for (int ifile=0; ifile<32; ifile++)
            {
	      sprintf(fname,"board%02d/%03d_ch%03d.hex",board,fcount,ich);
	      level1mapfile.open(fname);
	      level1mapfile << "1 30 0 0" << endl;
	      level1mapfile << "0 0 0 c8" << endl;	// 0xc8 = 200 bytes in hex file
              for (int isubtdc=0; isubtdc<64; isubtdc++)
                {
	          level1mapfile << "1 d0 " << hex << level1map[ich][ifile*64+isubtdc] << dec << endl;
	        }
	      level1mapfile.close();
	      fcount++;
	    }
        }

      sprintf(fname,"board%02d/%03d_board%02d_end.hex",board,fcount,board);
      initfile.open(fname);
      initfile << "1 30 0 0" << endl;
      initfile << "0 0 0 e" << endl;
      initfile << "1 b0 2" << endl;
      initfile << "1 b0 40" << endl;
      initfile.close();

      board++;
    }

  return 0;
}
