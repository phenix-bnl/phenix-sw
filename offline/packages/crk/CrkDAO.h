#ifndef CRKDAO_H_INCLUDED
#define CRKDAO_H_INCLUDED

#include <iostream>
#include <vector>


struct CrkPacket {
  // structure that fold cabling information of one packet of RICH FEE
  // One packet corresponds to
  //    1/2 of half crate
  //    = upper or lower half of 5 AMU/ADC card
  //    = 5*4 flat coaxial cables
  // 
  int id;      // packet ID. 6001 to 6038
  int sector;  // sector id. 0(SW),1(NW),2(SE),3(NE)
  int LR;      // Left (0) or Right(1)
  int card[5][2];   // cable id of AMU/ADC card.
                    // there are 5 half-cards in one packet, numbered
                    // from Left to Righ (in this convention)
                    // each card accepts 4 signal cables. They are
                    // numbered from top to bottom
};

class CrkDAO {
 public:
  CrkDAO(const char *initfile);
  void print() {print(std::cout);}
  void print(std::ostream& os);
  int get_PMTid(int packet, int ch);
  bool VRDC_format(void);

 private:
  enum format_t {PASS_THROUGH_FEE=1, PASS_THROUGH_MC};
  // Format type of RICH_DCM output
  // PASS_THROUGH_FEE: pass through mode for "real" RICH FEE
  // PASS_THROUGH_MC: pass through mode used in MC (VRDC 2000)
  // more format will be added as DCM software evolve....

  CrkDAO() {}  // disable default constructor
  format_t           d_format;
  std::vector <CrkPacket> d_packets;

  int         d_packet_cache;
  CrkPacket*  d_p_cache;
};

#endif
