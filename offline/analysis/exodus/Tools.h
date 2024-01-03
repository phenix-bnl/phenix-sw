#ifndef Tools_h
#define Tools_h

#include <sstream>
#include <string>

class Mom3;
class Mom4;
class ParticlePropertyList;

Mom3 invert(const Mom3& input);
Mom4 boost_vector(const Mom4& pinit, const Mom4& pframe);
Mom3 Rotate(const Mom3& pold, const double costheta, const double sintheta,
    const double cosphi, const double sinphi);
double GetMass(const int id, const ParticlePropertyList& Index);
double GetWidth(const int id, const ParticlePropertyList& Index);

double phiPHENIX(const double phi);

template<class T1, class T2>
const std::string createString(const T1& p1, const T2& p2) {
  // return a std::string "p1_p2"
  std::stringstream ss;
  std::string s;

  ss << p1 <<"_"<< p2;
  ss >> s;
  return s;
}

std::string getDataFileName(const std::string& basename);

#endif
