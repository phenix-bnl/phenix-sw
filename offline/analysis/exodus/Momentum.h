//-----------------------------------------------------------------------------
//
//  Declaration of the classes Mom3 and Mom4
//
//-----------------------------------------------------------------------------

#ifndef MOMENTUM_H
#define MOMENTUM_H

#include <boost/operators.hpp>

class Mom3:
  boost::addable<Mom3>,
  boost::subtractable<Mom3>,
  boost::multipliable<Mom3, double>
{
  public:
    Mom3();
    Mom3(const double px, const double py, const double pz);
    double Getpx() const {return itsPx;}
    double Getpy() const {return itsPy;}
    double Getpz() const {return itsPz;}
    double Theta() const;
    double Phi() const;
    void Set(const double px, const double py, const double pz);
    double Abs() const;
    Mom3 operator+=(Mom3 const&);
    Mom3 operator-=(Mom3 const&);
    double operator* (const Mom3 &) const;
    Mom3 operator*=(double const&);
  protected:
    double itsPx, itsPy, itsPz;
};

class Mom4:
  public Mom3,
  boost::addable<Mom4>,
  boost::subtractable<Mom4>,
  boost::multipliable<Mom4, double>
{
  public:
    Mom4();
    Mom4(const double E, const Mom3& p);
    Mom4(const double E, const double px,const double py, const double pz);
    double GetE() const {return itsE;}
    //void SetE(const double E){itsE=E;}
    void Set(const double E, const Mom3& p) { Set(E, p.Getpx(), p.Getpy(), p.Getpz()); }
    void Set(const double E, const double px, const double py, const double pz)
    {
      itsE = E;
      Mom3::Set(px,py,pz);
    }
    double Abs() const;
    Mom4 operator+=(Mom4 const&);
    Mom4 operator-=(Mom4 const&);
    Mom4 operator*=(double const&);
    double operator* (const Mom4 &) const;
  private:
    double itsE;
};

#endif /* MOMENTUM_H */
