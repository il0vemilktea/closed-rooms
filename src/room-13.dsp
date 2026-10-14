declare samplerate "44100";

import("stdfaust.lib");
fau = library("fau.lib");
fit = library("data/room-13-fit.csv");
tones = library("data/room-13-tones.csv");
bells = library("data/room-13-bells.csv");

//@params
dur  = 137;     // duration
gain = 1;    // [0, 1]
//@end

N = 4;

// single filter bank for both channels
floorShape(n) = n
    : fi.highpass(2, fit.hipassFreq)
    : fi.spectral_tilt(4, 50, fit.knee1-50, fit.alpha1 / 2)
    : fi.spectral_tilt(4, 50, fit.knee1-50, fit.alpha1 / 2)
    : fi.spectral_tilt(6, fit.knee1, fit.knee2-fit.knee1, fit.alpha2 / 2)
    : fi.spectral_tilt(6, fit.knee1, fit.knee2-fit.knee1, fit.alpha2 / 2)
    : fi.spectral_tilt(6, fit.knee2, 20000 - fit.knee2, fit.alpha3 / 4)
    : fi.spectral_tilt(6, fit.knee2, 20000 - fit.knee2, fit.alpha3 / 4)
    : fi.spectral_tilt(6, fit.knee2, 20000 - fit.knee2, fit.alpha3 / 4)
    : fi.spectral_tilt(6, fit.knee2, 20000 - fit.knee2, fit.alpha3 / 4);

noiseFloor = shape, hiss :> _, _
with {
    hiss = no.noises(N,2), no.noises(N,3)
         : _ * ba.db2linear(fit.hissL),
           _ * ba.db2linear(fit.hissR);
    shape = no.noises(N,0), no.noises(N,1)
          : fau.lowcorr(130, 0.85)
          : fau.lowcorr(700, 0.25)
          : floorShape, floorShape
          : fau.bellBank(bells)
          : _ * ba.db2linear(fit.chainGainL),
            _ * ba.db2linear(fit.chainGainR);
};


process = noiseFloor , fau.phasedToneSet(tones)
    :> *(gain), *(gain)
    : fau.fade(dur, 0.5, 0.5);
