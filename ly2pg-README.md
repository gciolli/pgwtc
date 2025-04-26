# Overview

This extension includes code for extracting music from LilyPond source
files.

It is known to work on the LilyPond sources in [Rother, K. 2015. An
'open source' open score edition of Bach's Well-tempered
Clavier. Music score. University of Cape
Town.](hdl.handle.net/11427/13106), but it should be usable to load
many other Lilypond source files.

# Example: the Well-Tempered Clavier

First, we unpack the source archives:

    unzip bach_WTCbook1_openscore.zip 
    unzip bach_WTCbook2_openscore.zip 

Then, we rename the LilyPond sources to easier, consistent file names:

    mkdir ly
    mv book1/fugue1_846/fugue1_expanded.ly              ly/fuga-bwv846.ly
    mv book1/fugue2_847/fugue2_expanded.ly              ly/fuga-bwv847.ly
    mv book1/fugue3_848/fugue3_expanded.ly              ly/fuga-bwv848.ly
    mv book1/fugue4_849/fugue4_expanded.ly              ly/fuga-bwv849.ly
    mv book1/fugue5_850/fugue5_expanded.ly              ly/fuga-bwv850.ly
    mv book1/fugue6_851/fugue6_expanded.ly              ly/fuga-bwv851.ly
    mv book1/fugue7_852_with_prelude/fugue7_expanded.ly ly/fuga-bwv852.ly
    mv book1/fugue8_853/fugue8_expanded.ly              ly/fuga-bwv853.ly
    mv book1/fugue9_854/fugue9_expanded.ly              ly/fuga-bwv854.ly
    mv book1/fugue10_855/fugue10_basic.ly               ly/fuga-bwv855.ly
    mv book1/fugue11_856/fugue11_expanded.ly            ly/fuga-bwv856.ly
    mv book1/fugue12_857/fugue12_expanded.ly            ly/fuga-bwv857.ly
    mv book1/fugue13_858/fugue13_expanded.ly            ly/fuga-bwv858.ly
    mv book1/fugue14_859/fugue14_expanded.ly            ly/fuga-bwv859.ly
    mv book1/fugue15_860/fugue15_expanded.ly            ly/fuga-bwv860.ly
    mv book1/fugue16_861/fugue16_expanded.ly            ly/fuga-bwv861.ly
    mv book1/fugue17_862/fugue17_expanded.ly            ly/fuga-bwv862.ly
    mv book1/fugue18_863/fugue18_expanded.ly            ly/fuga-bwv863.ly
    mv book1/fugue19_864/fugue19_expanded.ly            ly/fuga-bwv864.ly
    mv book1/fugue20_865/fugue20_expanded.ly            ly/fuga-bwv865.ly
    mv book1/fugue21_866/fugue21_expanded.ly            ly/fuga-bwv866.ly
    mv book1/fugue22_867/fugue22_expanded.ly            ly/fuga-bwv867.ly
    mv book1/fugue23_868/fugue23_expanded.ly            ly/fuga-bwv868.ly
    mv book1/fugue24_869/fugue24_expanded.ly            ly/fuga-bwv869.ly
    mv book2/fugue1_870/fugue1_expanded.ly              ly/fuga-bwv870.ly
    mv book2/fugue2_871/fugue2_expanded.ly              ly/fuga-bwv871.ly
    mv book2/fugue3_872/fugue3_expanded.ly              ly/fuga-bwv872.ly
    mv book2/fugue4_873/fugue4_expanded.ly              ly/fuga-bwv873.ly
    mv book2/fugue5_874/fugue5_expanded.ly              ly/fuga-bwv874.ly
    mv book2/fugue6_875/fugue6_expanded.ly              ly/fuga-bwv875.ly
    mv book2/fugue7_876/fugue7_expanded.ly              ly/fuga-bwv876.ly
    mv book2/fugue8_877/fugue8_expanded.ly              ly/fuga-bwv877.ly
    mv book2/fugue9_878/fugue9_expanded.ly              ly/fuga-bwv878.ly
    mv book2/fugue10_879/fugue10_expanded.ly            ly/fuga-bwv879.ly
    mv book2/fugue11_880/fugue11_expanded.ly            ly/fuga-bwv880.ly
    mv book2/fugue12_881/fugue12_expanded.ly            ly/fuga-bwv881.ly
    mv book2/fugue13_882/fugue13_expanded.ly            ly/fuga-bwv882.ly
    mv book2/fugue14_883/fugue14_expanded.ly            ly/fuga-bwv883.ly
    mv book2/fugue15_884/fugue15_expanded.ly            ly/fuga-bwv884.ly
    mv book2/fugue16_885/fugue16_expanded.ly            ly/fuga-bwv885.ly
    mv book2/fugue17_886/fugue17_expanded.ly            ly/fuga-bwv886.ly
    mv book2/fugue18_887/fugue18_expanded.ly            ly/fuga-bwv887.ly
    mv book2/fugue19_888/fugue19_expanded.ly            ly/fuga-bwv888.ly
    mv book2/fugue20_889/fugue20_expanded.ly            ly/fuga-bwv889.ly
    mv book2/fugue21_890/fugue21_expanded.ly            ly/fuga-bwv890.ly
    mv book2/fugue22_891/fugue22_expanded.ly            ly/fuga-bwv891.ly
    mv book2/fugue23_892/fugue23_expanded.ly            ly/fuga-bwv892.ly
    mv book2/fugue24_893/fugue24_expanded.ly            ly/fuga-bwv893.ly

At this point we can run the `ly2pg-wtc-voces.sh` shell script, which
uses LilyPond to extract voices into individual files, such as e.g.
`ly/voces/fuga-bwv871-alto.ly`:

    for bwv in $(seq 846 893); do
      bash /usr/share/postgresql/17/extension/ly2pg-wtc-voces.sh $bwv;
    done

On a standard laptop, the 48 fugues are processed in about 7 minutes.

Finally, we can load the voice files into PostgreSQL, and extract them
into a single CSV file `pgwtc-notes.csv`:

    psql -f /usr/share/postgresql/17/extension/ly2pg-wtc-load.sql

Note that this is the same file which is included in the `pgwtc`
extension.
