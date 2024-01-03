#ifndef Filler_h
#define Filler_h

#include <fstream>
#include <string>

class ParticleList;
class ParticlePropertyList;
class TFile;

enum FileFormat {root, oscar, NONE};

class Filler {
  private:
    Filler();
    bool fill_primaries;
    bool fill_singles;
    bool fill_single_photon;
    bool fill_electron_pair;
    int setup;
    unsigned int Nevents;
    double dNdy_pi0;
    double N_coll;
    ParticlePropertyList* PPList;
    std::string output_file;

    // only one of these is used, depending on outputFormat
    TFile* root_file;
    std::ofstream* oscar_file;

    int outputFormat;
    void setOutputFormat(const FileFormat format);

  public:
    static Filler& getInstance();
    void Close();

    void setFillPrimaries(const bool fill_primaries);
    void setFillSingles(const bool fill_singles);
    void setFillSinglePhoton(const bool fill_single_photon);
    void setFillElectronPair(const bool fill_electron_pair);
    void setSetup(const int setup);
    void setParticlePropertyList(ParticlePropertyList* PPList);
    void setNevents(const unsigned int Nevents);
    void setdNdy_pi0(const double dNdy_pi0);
    void setNcoll(const int N_coll);
    void setOutputFile(const std::string& output_file);

    void initDefaultROOTObjects();
    void Fill(const ParticleList& PList, const unsigned int evtnum=0);
};
#endif
