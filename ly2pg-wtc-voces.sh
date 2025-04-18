#!/bin/bash

usage () {
    cat <<EOF
Synopsis:

    $0 NNN [--simplify]

where NNN is an integer between 846 and 893.

This command extracts LilyPond voices from the fugues in the
Well-Tempered Clavier; it reads from the following input file:

    ly/fuga-bwv${NNN}.ly

and creates the following voice files:

    ly/voces/fuga-bwv${NNN}-${V}.ly

where V is one of soprano, alto, mezzo, tenor, bass, depending on
which voices are used by that piece.

If --simplify is specified, each voice file is transposed to a key
that has no alterations, i.e. to either C major or A minor.

EOF
}

voces () {
    case $1 in
	855)
	    echo "soprano bass";;
	849|867)
	    echo "soprano alto mezzo tenor bass";;
	847|848)
	    echo "soprano alto bass";;
	846|850|857|859|861|862|863|865|868|869|871|874|876|877|878|885|886)
	    echo "soprano alto tenor bass";;
	891|892)
	    echo "soprano alto tenor bass";;
	851|852|853|854|856|858|860|864|866|870|872|873|875|879|880|881|882)
	    echo "soprano mezzo bass";;
	883|884|887|888|889|890|893)
	    echo "soprano mezzo bass";;
	*)
	    echo "unknown fuga '$1'"
	    exit 1;;
    esac
}

clavis_maior () {
    # We assume that the first argument is 845 + n with n=1,...,48
    local n=$(($1 - 845))
    if [[ $n -gt 24 ]]; then
	n=$((n - 24))
    fi
    case $n in
	1|20)  echo "c"  ;;
	3)     echo "cis";;
	22)    echo "des";;
	5|24)  echo "d"  ;;
	7|2)   echo "ees";;
	9|4)   echo "e"  ;;
	11|6)  echo "f"  ;;
	13|8)  echo "fis";;
	15|10) echo "g"  ;;
	17|12) echo "aes";;
	19|14) echo "a"  ;;
	21|16) echo "bes";;
	23|18) echo "b"  ;;
	*)
	    echo "ERROR: $n" > /dev/stderr
	    exit 1
    esac
}

elice_vocem () {
    local bwv=$1
    local v=$2
    shift 2
    if [[ $# -eq 1 && $1 == '--simplify' ]]; then
	local cm=$(clavis_maior $bwv)
	options="\\transpose $cm c"
    fi
    local lf=tmp-${bwv}-${v}.ly
    local lo=ly/voces/fuga-bwv${bwv}-${v}.ly
    cat >$lf <<EOF
\\include "ly/fuga-bwv${bwv}.ly"
{
  \\void \\displayLilyMusic $options \\$v
}
EOF
    lilypond $lf > $lo 2>/dev/null
    rm $lf ${lf%.ly}.pdf
    echo "Extracted $v voice from BWV${bwv}"
}

if [[ $# -lt 1 ]]; then
    usage
    exit 1
fi

BWV=$1
shift 1

for v in $(voces $BWV); do
    elice_vocem $BWV $v $@
done
