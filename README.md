## Automated Detection and Tracking of solar filaments observed in full-disc H-alpha images

### This is a package of programs written in IDL. The input is a full-disc image of the Sun observed (mostly by ground-based observatories) in the H-alpha line in the visible region of spectrum at 6563 Angstrom. The image is first corrected for limb darkening, and optionally corrected for foreshortening as well. The greyscale image is then converted into a binary image using advanced local thresholding technique as well as size threshold, which identifies only the filaments on the disc.

### Filaments in a single image are extracted and labelled uniquely in a descending order. All of the small "fragments" that a filament is split into are assigned the same label. This is done by the Grouping Criterion. Filaments in subsequent images are also identified, and labels in the previous image are carried forward consistently without regard to the filamnet sizes in later images, by making use of the Labelling Criterion.

### Finally the output for each filament label is produced as a text file which gives the length, size and number of fragments that a filament is split into at each time, along with location of its centroid in physical coordinates. To help visualize evolution of the all the filaments on the disc, a jpeg image for each colour-coded binary image is saved in a folder.
