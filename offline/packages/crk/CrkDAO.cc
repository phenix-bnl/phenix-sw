#include "CrkDAO.h"
#include "phool.h"

#include <cctype>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>

using namespace std;

#define SW 0
#define NW 1
#define SE 2
#define NE 3
#define LEFT  0
#define RIGHT 1

static bool skip_comment(istream &is) {
  char c;

  while(! is.eof()) {
    is.get(c);
    if( isspace(c) ) {
      continue;
    } else if( c == '#') {
      is.ignore(10000,'\n');
    } else { // not white space characters or comment line starting with #
      is.unget();
      break;
    }
  }

  if(is.eof() || !is.good()) return false;
  else return true;
}

static int read_packet(istream &is, CrkPacket &packet) {
  string s_sector;
  string s_LR;

  if(!skip_comment(is)) return 0;
  is >> packet.id; 
  //cout << packet.id << endl;
  if(!skip_comment(is)) return -1;
  is >> s_sector; is.ignore(10000,'\n');

  if(s_sector == "SW") packet.sector = SW;
  else if(s_sector == "NW") packet.sector = NW;
  else if(s_sector == "SE") packet.sector = SE;
  else if(s_sector == "NE") packet.sector = NE;
  else return -2;

  if(!skip_comment(is)) return -3;
  is >> s_LR; is.ignore(10000,'\n');
  if(s_LR == "LEFT") packet.LR = LEFT;
  else if(s_LR == "RIGHT") packet.LR = RIGHT;
  else return -4;

  int j;
  for(j=0;j<2;j++) {
    if(!skip_comment(is)) return -5;
    for(int i=0;i<5;i++) {
      is >> packet.card[i][j];
      //      cout << packet.card[i][j] << " ";
    }
    //    cout << endl;
    is.ignore(10000,'\n');
  }
  return 1;
}

static void print_packet(CrkPacket *p, ostream &os) {
  const char *sect_name[4] = {"SW","NW","SE","NE"};
  const char *LR_name[2] = {"LEFT","RIGHT"};

  os << "id     = "<<p->id;
  os << " sector = "<<p->sector<<" ("<<sect_name[p->sector]<<")";
  os << " LR = "<<p->LR <<" ("<<LR_name[p->LR]<<")"<< endl;

  for(int j=0;j<2;j++) {
    for(int i = 0; i<5; i++) cout << p->card[i][j] <<" ";
    cout << endl;
  }
}

CrkDAO::CrkDAO(const char *initfile) {
  ifstream ifile(initfile);
  if (!ifile.good())
    {
      cout << PHWHERE 
	   << " Cable map file " << initfile 
           << " not found, no point in continuing" << endl;
      cout << "Exiting now" << endl;
      exit(1);
    }
  CrkPacket packet;
  int status;
  string s_format;

  d_packet_cache = 0;
  d_p_cache = NULL;

  skip_comment(ifile);
  ifile >> s_format;
  if(s_format == "PASS_THROUGH_FEE") d_format = PASS_THROUGH_FEE;
  else if(s_format == "PASS_THROUGH_MC") d_format = PASS_THROUGH_MC;
  else {
    cout << "Error in reading format word: "<<s_format <<endl;
  }
  //  cout << s_format << endl;
  // cout <<" FORMAT: "<<d_format<<endl;
  ifile.ignore(1000,'\n');

  while((status = read_packet(ifile,packet)) == 1)
    d_packets.push_back(packet);
  if(status < 0) {
    cout << "Error in initialization of CrKDAO"<<endl;
    cout << "Error code: "<<status;
    cout << " at reading "<<d_packets.size()<<"-th packet"<<endl;
  }
}

void CrkDAO::print(ostream &os) {
  std::vector<CrkPacket>::iterator p;
  cout << "# of packets "<<d_packets.size()<<endl;
  for(p = d_packets.begin(); p != d_packets.end(); p++)
    print_packet(&(*p),os);
}

int
CrkDAO::get_PMTid(int packet, int channel)
{
  // translate (packet, channel) pair to PMTid (position) from cabling
  // information
  //
  if (channel < 0 || channel >= 160)
    {
      return -1; // invalid input
    }

  std::vector<CrkPacket>::iterator p;
  if (packet != d_packet_cache)
    {
      for (p = d_packets.begin(); p != d_packets.end(); ++p)
        {
          if (p->id == packet)
            {
              d_packet_cache = packet;
              d_p_cache = &(*p);
              break;
            }
        }
      if (p == d_packets.end())
        {
	  return -1; // failed to find the packet
	}
    }
  CrkPacket *pp = d_p_cache;

  int card = channel / 32;
  int subaddr = channel % 32;
  int up_down = subaddr / 16;
  int ch = subaddr % 16;

  //Now I know that the updown order is reversed in half-FEE card
  up_down = 1 - up_down;

  //Now get the cable number from the cabling map
  int cable;
  if (pp->LR == RIGHT)
    { // RIGHT part...reverse order of the card
      cable = pp->card[4 - card][up_down];
    }
  else
    { // should be LEFT, and the order of the card is normal
      cable = pp->card[card][up_down];
    }
  // In RICH numbering convension, cable number is identical to the
  // row number of PMT array.

  return 1280*(pp->sector) + 16*cable + ch;
}

bool CrkDAO::VRDC_format(void) {
  return d_format == PASS_THROUGH_MC;
}
