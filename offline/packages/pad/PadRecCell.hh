#ifndef __PADRECCELL_HH__
#define __PADRECCELL_HH__

class PadRecCell {

public:
  short w;        // Cell number across the wire
  short z;        // Cell number along the wire
  short celltype; // Number of hot pads
  short id;       // Simulation id
};

#endif
