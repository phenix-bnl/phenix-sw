#ifndef __PDBMPCSHAPE_HH__
#define __PDBMPCSHAPE_HH__

#include <PdbCalChan.hh>
#include <vector>

class PdbMpcShape : public PdbCalChan
{
  public:
  PdbMpcShape(){ nsamples = 0; }
  virtual ~PdbMpcShape();

  void set_fee576ch(const short c) {
    fee576ch = c;
  }
  void set_nsamples(const short n) {
    nsamples = n;
  }
  void set_start_time(const float t) {
    start_time = t;
  }
  void set_end_time(const float t) {
    end_time = t;
  }
  void add_float(float fl);
  virtual void print() const;
  
  std::vector<float> getVector() const  { return _valvec; }
  float getValue(int pos) const;
  int getLength() const {return _valvec.size(); }
  short get_fee576ch() { return fee576ch; }
  short get_nsamples() { return nsamples; }
  float get_start_time() { return start_time; }
  float get_end_time() { return end_time; }

  void  reset();

private:
  
  short fee576ch;
  short nsamples;
  float start_time;
  float end_time;
  std::vector<float> _valvec; // the value (ampl or err)
  
  ClassDef(PdbMpcShape,1);
};

#endif /* __PDBMPCSHAPE_HH__ */
