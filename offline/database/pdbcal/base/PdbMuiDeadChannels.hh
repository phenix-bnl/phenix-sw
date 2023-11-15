#ifndef __PDBMUIDEADCHANNELS_HH__
#define __PDBMUIDEADCHANNELS_HH__

#include "PdbCalChan.hh"
#include <iostream>

class PdbMuiDeadChannels : public PdbCalChan 
{
public:

  PdbMuiDeadChannels();
  virtual ~PdbMuiDeadChannels();
  //prints out all 256 channels to cout
  virtual void print()const;
  //prints out all 256 channels to ostream
  virtual void print(std::ostream&)const;
  //prints out ith channel to ostream
  virtual void print(int i, std::ostream& os=std::cout)const;
  //reads in ith channel from istream
  virtual void Read(int, std::istream&);
  //reads in all channels from istream
  virtual void Read(std::istream&);

  enum {size=256};
 
  /**prints out all 256 channels to ostream
   *cint requires operator<< and operator>> to be friends
   */
  friend std::ostream& operator<<(std::ostream& os, 
				  const PdbMuiDeadChannels&);
  //reads in from istream
  friend std::istream& operator>>(std::istream& is, PdbMuiDeadChannels&);

private:
  short index[size];
  short arm[size];
  short plane[size];
  short panel[size];
  short orientation[size];
  short twopack[size];
  
};

#endif

