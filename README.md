# erika milkea - _closed rooms_

**Album links**: [Bandcamp](https://erikamilktea.bandcamp.com/album/closed-rooms); [Soundcloud](https://soundcloud.com/erika-milktea/sets/closed-rooms).

[**Post link**](https://erikamilktea.substack.com/p/stanford-laptop-orchestra-a-hit-piece).

[**Github mirror**](https://github.com/il0vemilktea/closed-rooms/blob) (for web-rendered notebooks).

Okay if you reached here you clearly are interested in this more than just as a substack bit.

Check `notebooks/` for spectral analyses, then `src/` for the Faust code. And `lib/` has the helper functions. 
The rest is dev scaffolding for emacs.

To render the album, first GO AND INSTALL NIX(OS) and then run

```
nix build .#renders
```

### Notes

* Samples examined are 4, 5, 13 from CD10 on The Works library (Sound Ideas).

* Code is licensed as MIT. Except the `src/*.dsp` and `data/*.csv` which are CC-BY-NC to match the release.
