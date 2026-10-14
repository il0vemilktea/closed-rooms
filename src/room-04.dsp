declare samplerate "44100";

import("stdfaust.lib");
fau = library("fau.lib");
fit = library("data/room-04-fit.csv");
tones = library("data/room-04-tones.csv");

//@params
dur  = 366;     // duration
gain = 1;    // [0, 1]
//@end

N = 4;

floorShape(i, n)  = n
      : fi.spectral_tilt(4, 12, knee-12, loAlpha)
      : fi.spectral_tilt(6, knee, 9000-knee, hiAlpha/2)
      : fi.spectral_tilt(6, knee, 9000-knee, hiAlpha/2)
      : fi.svf.bell(140, dentQ, dent)
      : fi.svf.bell(200, bumpQ, bump)
      : _ * cGain
with {
  loAlpha = ba.take(i+1, fit.loAlpha);
  hiAlpha = ba.take(i+1, fit.hiAlpha);
  knee = ba.take(i+1, fit.knee);
  dent = ba.take(i+1, fit.dent);
  dentQ = ba.take(i+1, fit.dentQ);
  bump = ba.take(i+1, fit.bump);
  bumpQ = ba.take(i+1, fit.bumpQ);
  cGain = ba.take(i+1, fit.chainGain) : ba.db2linear;
};

noiseFloor = shape, hiss :> lp
with {
  shape = no.noises(N,0), no.noises(N, 1)
        // see notes in the lib file
        : fau.lowcorr(200, 0.45)
        : fau.lowcorr(1400, 0.5)
        : floorShape(0), floorShape(1);
  hiss = no.noises(N,2), no.noises(N,3)
       : _ * ba.db2linear(ba.take(1, fit.wnGain)),
         _ * ba.db2linear(ba.take(2, fit.wnGain));
  lp = fi.lowpass(4, ba.take(1, fit.lopassFreq)),
       fi.lowpass(4, ba.take(2, fit.lopassFreq));
};



process = noiseFloor, fau.toneSet(tones)
        :> *(gain), *(gain)
        : fau.fade(dur, 0.5, 0.5);
