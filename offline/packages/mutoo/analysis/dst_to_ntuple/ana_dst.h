// PHENIX includes
//
#include "ezdst.h"
#include <iostream>
#include "pdst.h"
#include "Event.h"

// ROOT includes
//
#include "TDataType.h"
#include "TFile.h"
#include "TH1.h"
#include "TNtuple.h"
#include "TH1.h"

// Prototypes
//
int process_event (PHCompositeNode *topNode); //++CINT
void setup_display(); //++CINT
int setup_all(DstContent *dst); //++CINT
int end_all(); //++CINT
void set_ntuple_name(char* ntuple_name);//++CINT

