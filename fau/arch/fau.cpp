// fau's Faust architecture: render a generator (a piece without inputs) to a
// wav file. Controls are set by their exact labels, the param names.
//
//   NAME OUT.wav SAMPLE_RATE FRAMES PREROLL BIT_DEPTH [PARAM VALUE]...
//
// PREROLL frames are thrown away before writing to .wav.
// BIT_DEPTH is 16, 24, or 32 (integer PCM).

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

#include <sndfile.h>

#include "faust/dsp/dsp.h"
#include "faust/gui/MapUI.h"
#include "faust/gui/meta.h"

<< includeIntrinsic >>

    << includeclass >>

    int main(int argc, char *argv[]) {
    if (argc < 6 || (argc - 6) % 2 != 0) {
        std::cerr << "usage: " << argv[0]
                  << " OUT.wav SAMPLE_RATE FRAMES PREROLL BIT_DEPTH [PARAM "
                     "VALUE]...\n";
        return 2;
    }
    const char *out = argv[1];
    int rate = std::atoi(argv[2]);
    long long frames = std::atoll(argv[3]);
    long long preroll = std::atoll(argv[4]);
    int depth = std::atoi(argv[5]);

    mydsp dsp;
    MapUI ui;
    dsp.buildUserInterface(&ui);
    dsp.init(rate);
    for (int i = 6; i < argc; i += 2) {
        FAUSTFLOAT *zone = ui.getParamZone(argv[i]);
        if (!zone) {
            std::cerr << argv[0] << ": no control named " << argv[i] << "\n";
            return 2;
        }
        *zone = FAUSTFLOAT(std::strtod(argv[i + 1], nullptr));
    }

    int channels = dsp.getNumOutputs();
    SF_INFO info = {};
    info.samplerate = rate;
    info.channels = channels;
    info.format = SF_FORMAT_WAV | (depth == 16   ? SF_FORMAT_PCM_16
                                   : depth == 24 ? SF_FORMAT_PCM_24
                                                 : SF_FORMAT_PCM_32);
    SNDFILE *file = sf_open(out, SFM_WRITE, &info);
    if (!file) {
        std::cerr << argv[0] << ": can't write " << out << ": "
                  << sf_strerror(nullptr) << "\n";
        return 1;
    }
    if (depth == 32)
        sf_command(file, SFC_SET_CLIPPING, nullptr, SF_TRUE);

    const int block = 512;
    std::vector<std::vector<FAUSTFLOAT>> buffers(
        channels, std::vector<FAUSTFLOAT>(block));
    std::vector<FAUSTFLOAT *> outputs(channels);
    for (int c = 0; c < channels; c++)
        outputs[c] = buffers[c].data();
    std::vector<FAUSTFLOAT> interleaved(block * channels);

    for (long long done = 0; done < preroll;) {
        int n = int(std::min<long long>(block, preroll - done));
        dsp.compute(n, nullptr, outputs.data());
        done += n;
    }
    for (long long done = 0; done < frames;) {
        int n = int(std::min<long long>(block, frames - done));
        dsp.compute(n, nullptr, outputs.data());
        for (int i = 0; i < n; i++)
            for (int c = 0; c < channels; c++)
                interleaved[i * channels + c] = buffers[c][i];
        sf_writef_float(file, interleaved.data(), n);
        done += n;
    }
    sf_close(file);
    return 0;
}
