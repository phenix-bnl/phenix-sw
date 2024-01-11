#include "emcOMAsciiT.h"
#include "emcQAFEM.h"
#include "emcDataManager.h"
#include "EmcIndexer.h"
#include "asciitimestamp.h"
#include "dirfilemanip.h"
#include <string>
#include <vector>
#include <cassert>

namespace
{
  bool reader(emcQAFEM& qaFEM, int code)
  {
    // We read 4 status files for this FEM.
    // Each file containts:
    // PHTimeStamp begin
    // PHTimeStamp end
    // channel error warning
    // channel error warning
    // ...
    // where channel is between 0 and 143, error and warning are 6 bits flags
    // From the 4 6-bits flags, we hereby construct a 32 bits
    // (6x4+some reserved ones for future use) flag for each
    // channel. Ordering of the bytes is governed by the producers vector
    // below.
    //
    // NOTE: only absPosition is used here (pinNumber,post_pre and tac_pre
    // are dummy).

    int absPosition;
    int idummy;

    if ( code < 0 )
      {
	code = qaFEM.AbsolutePosition();
      }

    emcCalFEM::FEMDecode(code,absPosition,idummy,idummy,idummy);

    int SN;
    int SM144N;

    EmcIndexer::PXSM144_iSiSM144( absPosition, SN, SM144N );

    std::vector<std::string> producers;
    // The order of this procuders vector IS crucial
    // as it determines the status bit positions in the
    // final ERROR and WARNING arrays

    producers.push_back("PED");
    producers.push_back("HLR");
    producers.push_back("ToF");
    producers.push_back("GAINS");

    // Typically the file names are like
    // SxSMyy.FEMzzz.tac-pre.producer.QA
    // e.g.W1SM15.PED.QA

    emcDataManager* dm = emcDataManager::GetInstance();

    size_t i;
    char filename[256];
    INT32 ERROR[144], WARNING[144];

    PHTimeStamp begin;
    PHTimeStamp end;
    PHTimeStamp insert;
    end.setToFarFuture();

    for ( i = 0; i < 144; i++ )
      {
        ERROR[i] = WARNING[i] = 0;
      }

    std::string dir = expand(dm->GetSourceDir());

    for ( i = 0; i < producers.size(); i++ )
      {

        if ( (absPosition >= 172) &&
             ( producers[i] == "ToF" ||
               producers[i] == "GAINS") )
          continue;

        snprintf(filename, 256, "%s/QA/%sSM%02d.%s.QA",
		 dir.c_str(),
                 EmcIndexer::EmcSectorId(SN),
                 SM144N,
                 producers[i].c_str());

        std::ifstream fs(filename);

        if (!fs)
          {
            continue;
          }

        begin = getTimeStamp(fs);
        insert = getTimeStamp(fs);

        int channel;
        INT32 error , warning;

        while ( fs >> channel >> error >> warning )
          {
            assert (channel >= 0 && channel < 144);
            assert (error < 64);
            assert (warning < 64);
            ERROR[channel] |= (error << 6 * i);
            WARNING[channel] |= (warning << 6 * i);
          }

        fs.close();
      }

    for ( i = 0; i < 144; i ++ )
      {
        if ( ERROR[i] || WARNING[i] )
          {
            qaFEM.AppendOneChannel(i, ERROR[i], WARNING[i]);
          }
      }

    qaFEM.SetValidityPeriod(begin, end);

    return true;
  }

  emcOMAsciiT<emcQAFEM> gemcOMQAFEM("emcOMQA",
				    "Read emcQAFEM objects",
				    reader);
}
