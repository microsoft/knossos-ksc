#!perl -an

# Usage:
#  perl -an scripts/parse-nvidia-smi-log.pl log.txt > gpu-stats.csv
#
# Should produce a CSV something like this:
#
#   time,temp,pwr,mem,util
#   1604493629,24,25,0,0
#   1,24,25,0,0
#   2,24,25,0,0
#   3,24,25,0,0
#   4,24,25,0,0
#   5,24,31,15325,2
#   ....
# The first datetime is the start time (in seconds since 1970), subsequents are deltas.
#

BEGIN { 
    use Date::Parse;
    $\ = "\n";
    $, = ",";
    # CSV header
    print("time", "temp", "pwr", "mem", "util")
}

if (/NVIDIA-SMI/) { 
    # Target line number is 7 ahead
    $target = $. + 7;

    # Last date-like line was the actual datetime
    $date = str2time($nm1);
    if ($date0) { $date -= $date0 } else { $date0=$date} 
}

if ($.==$target) {
    print($date, $F[2]+0, $F[4]+0, $F[8]+0, $F[12]+0)
}

if (/^[A-Z]/) { 
    # Date-like line, remember it
    chomp;
    $nm1 = $_ 
}
