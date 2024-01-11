#include "pdst.h"
#include "Event.h"
#include "TDataType.h"
#include "ezdst.h"
int process_event (PHCompositeNode *topNode); //++CINT
void setup_display(); //++CINT
int setup_all(DstContent *dst); //++CINT
int end_all(); //++CINT
void draw_plane(UShort_t arm, UShort_t octant); //++CINT
void set_ntuple_name(char* ntuple_filename);//++CINT

