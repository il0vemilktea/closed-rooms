import numpy as np
import scipy as sp

def quad_interp(f, db, k):
    delta = f[1] - f[0]

    a, b, c = db[k - 1], db[k], db[k + 1]

    p = 0.5 * (a - c) / (a - 2 * b + c)

    return f[k] + p * delta, b - 0.25 * (a - c) * p

# tf2s, boost (fi.peak_eq), tf1s and spectral_tilt are ported from Faust's
# filters.lib, Copyright (C) 2003-2019 by Julius O. Smith III
# <jos@ccrma.stanford.edu>, under the STK 4.3 license
# (LICENSES/LicenseRef-STK-4.3.txt).

def tf2s(b2, b1, b0, a1, a0, w1, sr=44_100):
    c = 1 / np.tan(w1 * 0.5 / sr)
    csq = c * c
    d = a0 + a1 * c + csq
    b0d = (b0 + b1 * c + b2 * csq) / d
    b1d = 2 * (b0 - b2 * csq) / d
    b2d = (b0 - b1 * c + b2 * csq) / d
    a1d = 2 * (a0 - csq) / d
    a2d = (a0 - a1 * c + csq) / d

    a = np.array([1, a1d, a2d])
    b = np.array([b0d, b1d, b2d])
    return b, a


def boost(gain, freq, B, sr=44_100):
    # decibel to linear
    g = 10 ** (np.abs(gain) / 20)

    T = 1.0 / sr
    wx = 2 * np.pi * freq
    Bw = B * T / np.sin(wx * T)
    a1 = np.pi * Bw
    b1 = g * a1
    if gain <= 0:
        a1s, b1s = b1, a1
    else:
        a1s, b1s = a1, b1
    return tf2s(1, b1s, 1, a1s, 1, wx, sr)


def bell(freq, Q, gain, sr=44_100):
    # abs: a cut and a boost of the same Q have the same width
    A = 10 ** (abs(gain) / 40)
    B = np.sin(2 * np.pi * freq / sr) / (A * np.pi * Q / sr)
    return boost(gain, freq, B, sr)


def tf1s(b1, b0, a0, w1, sr=44_100):
    c = 1 / np.tan(w1 * 0.5 / sr)
    d = a0 + c
    b1d = (b0 - b1 * c) / d
    b0d = (b0 + b1 * c) / d
    a1d = (a0 - c) / d
    return np.array([b0d, b1d]), np.array([1, a1d])


def spectral_tilt(N, f0, bw, alpha, sr=44_100):
    T, w0 = 1 / sr, 2 * np.pi * f0
    r = ((f0 + bw) / f0) ** (1 / (N - 1))

    prewarp = lambda w: w0 * np.tan(w * T / 2) / np.tan(w0 * T / 2)

    # build the second-order sections
    rows = []
    for i in range(N):
        b0 = prewarp(w0 * r ** (i - alpha))
        a0 = prewarp(w0 * r**i)

        g = a0 / b0

        [b0d, b1d], [a0d, a1d] = tf1s(1, b0, a0, 1, sr)

        rows.append((np.array([g * b0d, g * b1d, 0]), np.array([a0d, a1d, 0])))

    return np.array(rows)


def sos(*filters):
    return np.vstack([np.reshape(f, (-1, 6)) for f in filters])


def db_curve(sos_rows, freqs, sr=44_100):
    _, h = sp.signal.sosfreqz(sos_rows, worN=freqs, fs=sr)
    return 20 * np.log10(np.abs(h))
