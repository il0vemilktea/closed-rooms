declare samplerate "44100";

import("stdfaust.lib");
fau = library("fau.lib");
fit = library("data/room-05-fit.csv");
filterTones = library("data/room-05-filtertones.csv");
bells = library("data/room-05-bells.csv");

//@params
dur  = 365;     // duration
gain = 0.8;    // [0, 1]
//@end

N = 2 + ba.count(filterTones.freq);

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

noiseFloor = no.noises(N,0), no.noises(N,1)
    : fau.lowcorr(55, 0.3)
    : fau.lowcorr(700, 0.3)
    : fau.lowcorr(2200, 0.25)
    : floorShape, floorShape
    : fau.bellBank(bells)
    : _ * ba.db2linear(fit.chainGainL), _ * ba.db2linear(fit.chainGainR);


process = noiseFloor , fau.filterToneSet(filterTones, N, 2)
    :> *(gain), *(gain)
    : fau.fade(dur, 0.5, 0.5);
