#ifndef PARTICLEPROPERTY_H
#define PARTICLEPROPERTY_H

#include <string>

class ParticleProperty
{
  public:
    ParticleProperty();
    void Set(const int ID, const double mass, const double width,
        const int charge);
    int GetID() const {return itsID;}
    void SetID(const int ID) {itsID = ID;}
    std::string GetName() const {return itsName;}
    void SetName(const std::string& name) {itsName = name;}
    double GetMass() const {return itsMass;}
    void SetMass(const double mass) {itsMass = mass;}
    double GetWidth() const {return itsWidth;}
    void SetWidth(const double width) {itsWidth = width;}
    int GetCharge() const {return itsCharge;}
    void SetCharge(const int charge) {itsCharge = charge;}
  private:
    double      itsMass, itsWidth;
    std::string itsName;
    int         itsID, itsCharge;
};

#endif
