linkmap
=======

Display and investigate GNU ld/FPC internal linker maps. To create linker maps with fpc, use `fpc` option `-Xm`.

Usage
-----

```
linkmap MAPFILE
```

The Map Display
----------------
Each linked entity is drawn in a defragmentation-style map. The colour is derieved from 
the object file name.
Click a pixel to highlight the corresponding object file in the legend view.

Keyboard Hotkeys
----------------

* `R`: reload the current map file
* `L`: show/hide legend
* `1`: Scale to 1 byte/pixel
* `2`: Scale to 2 byte/pixel
* `4`: Scale to 4 byte/pixel
* `8`: Scale to 8 byte/pixel


Compiling
---------
Made with Lazarus 1.9 on FPC 3.1.

Libraries:

* [bitSpaceUtils](https://git.ccs-baumann.de/bitspace/utils/) for SScanf. A copy is included under ext/ in this repository.

