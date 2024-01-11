#ifndef __EventSorting_h__
#define __EventSorting_h__

#include <vector>
#include <string>
#include <iostream>
#include "PHObject.h"
#include "TMatrix.h"

using namespace std;

class EventSorting : public PHObject
{

public:

  EventSorting();
  virtual ~EventSorting();

  EventSorting& operator+=(const EventSorting& es);
  EventSorting operator+(const EventSorting& es) 
  { return EventSorting(*this)+=es; }

  EventSorting(const EventSorting& es);
  EventSorting& operator=(const EventSorting& es);

  std::vector<int> getCentralities(void) const;

  size_t getCentralityIndex(const int centrality) const;

  void getCentralityClassLimits(size_t index, int& centMin, 
				int& centMax) const;

  void getCentralityDescription(size_t index, std::string& title) const;

  bool getIndices(int centrality, double zvertex,
		  int& ic, int& iv) const;

  size_t getNCentralities(void) const;

  float getNEvents(void) const;
  float getNEvents(int centrality) const;
  float getNEvents(int centrality, double zvertex) const;

  bool getPath(int centrality, std::string& path) const;

  int getVertexIndex(const double zvertex) const;

  std::vector<double> getVertices(void) const;

  void identify(ostream& os = cout) const {}

  float incrNevents(const int centrality, const double zvertex, float n=1);

  int isValid() const { return 1; }

  void init(void);

  std::ostream& print(std::ostream& out = std::cout) const;

  void reset(void);

  void set(const std::vector<int>& cent,
	   const std::vector<double>& vert);

private:
  void copyTo(EventSorting& es) const;

private:

  std::vector<int> fCentralities;
  std::vector<double> fVertices;
  std::vector<std::string> fPathNames;
  TMatrix fNEvents;

  ClassDef(EventSorting,1)
};

#endif
