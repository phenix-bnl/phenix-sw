
#ifndef __RGRAPH_H__
#define __RGRAPH_H__

/** 
This is the graph class. I emulates one of the old "paper graph" devices
which are used to record the history of temperature, humidity, pressure, 
or the Dow Jones index.

In spy movies, you have seen them in the notorious polygraph tests
(although here we record only one quantity).

It will keep a specified amount of old values and optionally fill it into a histogram which
we specify. 
 

*/



class TH1;


class rGraph  {

public:
  rGraph( const int NumberofChannels, const int bins =1);
  virtual ~rGraph();


  /// Reset will reset the whole thing
  virtual int Reset();

  virtual int Add (const int i);
  virtual int Add (const float f);

  virtual int Fill (TH1 *h) const;

protected:

  int current_channel;
  int NumberofChannels;
  int multiplex;
  int current_bin;
  float * array;

};

#endif


