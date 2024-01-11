#ifndef __SIMULATIONREADER_H__
#define __SIMULATIONREADER_H__

#include <functional>
#include <iostream>
#include <string>
#include <vector>

class PHNodeIOManager;
class PHCompositeNode;

class SimulationReader
{
 public:

  enum EReturnCode { kOK,
		     kNoMatch,
		     kSimulationPoolExhausted,
		     kMaxIterationReached };


  SimulationReader(const char* inputfile,
		   const char* topnode="SIMU",
		   float deltazlimit=5 /*cm*/,
		   size_t maxnumberofiterations=30,
		   float simupoolfractiontouse=1.0);

  ~SimulationReader();

  size_t eventNumber() const { return fCurrentEvent; }

  std::string filename() const { return fInputFile; }

  EReturnCode findEvent(float realvertex);

  size_t NumberOfEventsUsed() const { return fNused; }
  
  size_t NumberOfRewind() const { return fNrewind; }

  void print(std::ostream& os = std::cout, int level=0) const;

  float vertex() const;

 private:

  /// Whether the event is already used.
  bool alreadyUsed(size_t eventnumber) const;
  /// Whether we already know this event.
  bool alreadyHave(size_t eventnumber) const;
  /// Whether our event pool is exhausted
  bool isPoolExhausted() const;

  EReturnCode invalidate(EReturnCode code)
  { fIsValid=false; return code; }

  std::string fInputFile;
  std::string fTopNode;
  float fDeltaZlimit;
  size_t fMaxNumberOfIterations;
  float fPoolFractionToUse;
  
  PHNodeIOManager* fIManager;
  PHCompositeNode* fDstNode;

  size_t fCurrentEvent;   
  size_t fNrewind;
  size_t fNused;
  size_t fNevents;

  bool fIsValid;

  class Event 
  {
  public:
    Event(size_t n, float z, bool v) 
      : fNumber(n), fVertex(z), fUsed(v) {}
    
    size_t number() const { return fNumber; }
    void number(size_t n) { fNumber=n;}

    float vertex() const { return fVertex; }
    void vertex(float z) { fVertex=z; }

    bool used() const { return fUsed; }
    void used(bool value) { fUsed=value; }
    void print(std::ostream& os = std::cout) const
    {
      os << "#" << fNumber << " z=" << fVertex 
	 << " used=" << fUsed << std::endl;
    }

  private:
    size_t fNumber;
    float fVertex;
    bool fUsed;
  };

  struct eventFinder : public std::unary_function<bool,Event>
  {
    eventFinder(size_t n) : n_(n) {}

    bool operator()(const Event& e)
    {
      return e.number()==n_;
    }
    size_t n_;
  };

  typedef std::vector<Event> EventVector;
  EventVector fEvents;;

  friend std::ostream& operator<<(std::ostream& os, const SimulationReader::Event& e);
};

std::ostream& operator<<(std::ostream& os, const SimulationReader::Event& e);

#endif
