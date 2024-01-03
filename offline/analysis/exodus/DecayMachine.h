#ifndef DecayMachine_h
#define DecayMachine_h

#include <boost/scoped_ptr.hpp>

class ParticleList;
class ParticlePropertyList;
class ParticleGeneratorList;
class DecayList;

class DecayMachine {
  private:
    DecayMachine();
    ~DecayMachine();

    int setup;
    boost::scoped_ptr<ParticlePropertyList> PPList;
    boost::scoped_ptr<ParticleGeneratorList> PGList;
    boost::scoped_ptr<DecayList> Decays;
    boost::scoped_ptr<ParticleList> event;
    bool useFlow;       // use elliptic flow?
    int dNch_dy;        // dN_ch/dy at y=0
    double vertexRange; // +-cm

  public:
    static DecayMachine& getInstance();
    ParticlePropertyList& getParticlePropertyList();
    void setGenerator(int setup);
    void setFlow(const bool useFlow);
    void setdNch_dy(const int dNch_dy);
    void setVertexRange(const double range);

    ParticleList& run();

    DecayList& getDecayProperties();

};
#endif
