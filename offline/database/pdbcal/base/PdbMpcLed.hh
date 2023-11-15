#ifndef __PDBMPCLED_HH__
#define __PDBMPCLED_HH__

#include <PdbCalChan.hh>
#include <iostream>
#include <ctime>
#include <phool.h>

class PdbMpcLed : public PdbCalChan {
public:
  PdbMpcLed();
  virtual ~PdbMpcLed() {}

  PdbMpcLed& operator = (const PdbMpcLed &p);

  void set_status(short tstatus) { status=tstatus; }
  void set_fee576ch(short tfee576ch) { fee576ch=tfee576ch; }
  void set_mean(float tmean) { mean=tmean; }
  void set_dmean(float tdmean) { dmean=tdmean; }
  void set_chisquare(float tchisquare) { chisquare=tchisquare; }
  void set_ndf(short tndf) { ndf=tndf; }

  short get_status() const { return status; }
  short get_fee576ch() const { return fee576ch; }
  float get_mean() const { return mean; }
  float get_dmean() const { return dmean; }
  float get_chisquare() const { return chisquare; }
  float get_ndf() const { return ndf; }

  virtual void reset();
  virtual void print() const;

private:

  short status;
  short fee576ch;

  float mean;        // mean of the quantity
  float dmean;       // error on the mean (variance / sqrt(N) )
  float chisquare;   // chisquare of the sub-run means compared to the run-mean
  short ndf;         // number of sub-run means - 1

  ClassDef(PdbMpcLed,1);
};

#endif // __PDBMPCLED_HH__ 

