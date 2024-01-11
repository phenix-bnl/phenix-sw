#include <iostream>
#include <fstream>
#include <cstdio>

//
// generate a zdc LL1 test map with aa 55 as outputs
//
int main(int argc, char **argv)
{
  ofstream initfile("000_board01_init.hex");
  initfile << "1 30 0 0" << endl;
  initfile << "0 0 0 14" << endl;
  initfile << "1 90 1" << endl;		// set board to talk to
  initfile << "1 b0 0" << endl;		// set board offline
  initfile << "1 b0 80" << endl;	// send reset pulse, puts board in ready state
  initfile << "1 b0 42" << endl;	// set board to write trigger map
  initfile.close();

  // allocate space for lookup table
  // which is 8 channels * 11 bits of address per channel
  const int nch = 8;		// 8 fem channels
  const int ntdcval = 2048;	// 11 bit tdc1 value
  int level1map[nch][ntdcval];

  for (int ich=0; ich<nch; ich++)
    {
      for (int itdc=0; itdc<ntdcval; itdc++)
        {
	  if ( (itdc%2)==0 )
	    level1map[ich][itdc] = 0x55;
	  else
	    level1map[ich][itdc] = 0x2a;
        } 
    }

  ofstream level1mapfile;
  char fname[1000];
  int fcount = 1;
  for (int ich=0; ich<nch; ich++)
    {
      if (ich==0)
        {
	  sprintf(fname,"%03d_ch%03d.hex",fcount,ich);
	  level1mapfile.open(fname);
	  level1mapfile << "1 30 0 0" << endl;
	  level1mapfile << "0 0 0 b" << endl;
	  level1mapfile << "1 b0 c5" << endl;	// enable chip select for mem1 (ch 0-3)
	  level1mapfile.close();
	  fcount++;
        }

      if (ich==4)
        {
	  sprintf(fname,"%03d_ch%03d.hex",fcount,ich);
	  level1mapfile.open(fname);
	  level1mapfile << "1 30 0 0" << endl;
	  level1mapfile << "0 0 0 b" << endl;
	  level1mapfile << "1 b0 c6" << endl;    // enable chip select for mem1 (ch 0-3)
	  level1mapfile.close();
	  fcount++;
	}

      for (int ifile=0; ifile<32; ifile++)
        {
	  sprintf(fname,"%03d_ch%03d.hex",fcount,ich);
	  level1mapfile.open(fname);
	  level1mapfile << "1 30 0 0" << endl;
	  level1mapfile << "0 0 0 c8" << endl;	// 0xc8 = 200 bytes in hex file
          for (int isubtdc=0; isubtdc<64; isubtdc++)
            {
	      level1mapfile << "1 d0 " << hex << level1map[ich][isubtdc] << endl;
	    }
	  level1mapfile.close();
	  fcount++;
	}
    }

  sprintf(fname,"%03d_board01_end.hex",fcount);
  initfile.open(fname);
  initfile << "1 30 0 0" << endl;
  initfile << "0 0 0 e" << endl;
  initfile << "1 b0 2" << endl;
  initfile << "1 b0 40" << endl;
  initfile.close();

  return 0;
}
