#ifndef PARTICLEGENERATOR_H
#define PARTICLEGENERATOR_H

class TF2;
class TH1;

class ParticleGenerator
{
  public:
    ParticleGenerator(const int id=0, const double weight=0);
    int GetID() const {return itsID;}
    void SetID(const int ID) {itsID = ID;}
    double GetWeight() const {return itsWeight;}
    void SetWeight(const double weight){itsWeight = weight;}
    void SetPtYFunction(TF2 *function){itsPtYFunction=function;}
    TF2* GetPtYFunction() const {return itsPtYFunction;}
    void SetPtHistogram(TH1 *histogram){itsPtHistogram=histogram;}
    TH1* GetPtHistogram() const {return itsPtHistogram;}
    void SetYHistogram(TH1 *histogram){itsYHistogram=histogram;}
    TH1* GetYHistogram() const {return itsYHistogram;}
    void SetMHistogram(TH1 *histogram){itsMHistogram=histogram;}
    TH1* GetMHistogram() const {return itsMHistogram;}
  private:
    int    itsID;
    double itsWeight;
    TF2*   itsPtYFunction;
    TH1*   itsPtHistogram;
    TH1*   itsYHistogram;
    TH1*   itsMHistogram;
};

#endif
