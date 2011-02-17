#!/bin/sh

#
# Anand Avati <avati@gluster.com>
#

problem="7 0 0  0 9 0  0 0 3
         0 0 5  8 0 2  6 0 0
         0 8 0  3 0 1  0 9 0

         0 5 0  7 0 4  0 1 0
         3 0 0  0 0 0  0 0 4
         0 4 0  5 0 9  0 8 0

         0 2 0  9 0 8  0 5 0
         0 0 9  6 0 7  4 0 0
         5 0 0  0 2 0  0 0 8";

space=;


function display9()
{
    echo " ==== Sudoku ===="

    for ((i=0; $i < 9; i++)); do
        for ((j=0; $j < 9; j++)); do
            idx=$(($i * 9 + $j + 1));
            echo -n " "
            echo -n "${space[$idx]}"
        done
        echo
    done
}


function initialize()
{
    for ((i=0; $i < 9; i++)); do
        for ((j=0; $j < 9; j++)); do
            idx=$(($i * 9 + j + 1));
            space[$idx]="1 2 3 4 5 6 7 8 9";
        done
    done
}


function fix_if_necessary()
{
    local i;
    local j;
    local idx;

    i=$1;
    j=$2;
    val=$3;

    idx=$(($i * 9 + $j + 1));

    set ${space[$idx]};

    if [ $# -eq 1 ]; then
        fix_position $i $j $1;
    fi
}


function not_possible()
{
    local i;
    local j;
    local val;
    local idx;
    local v;

    i=$1;
    j=$2;
    val=$3;
    new=;

    idx=$(($i * 9 + $j + 1));

    set ${space[$idx]};

#    echo "Unsetting from $i,$j ($@) => $val";

    if [ $# -eq 1 ]; then
        if [ $1 -eq $val ]; then
            echo "ERROR !! $i,$j had $1, but now unsetting $val"
            exit 1;
        fi
        return;
    fi

    for v in $@; do
        if [ $v -eq $val ]; then
            continue;
        fi
        new="$new $v";
    done

#    echo "Setting to $i,$j => ($new)";
    space[$idx]="$new";

    fix_if_necessary $i $j;
}


function spread_vertical_awareness()
{
    local i;
    local j;
    local val;
    local k;

    i=$1;
    j=$2;
    val=$3;

    for ((k=0; $k < 9; k++)); do
        if [ $k -eq $i ]; then
            continue;
        fi
        not_possible $k $j $val;
    done
}


function spread_horizontal_awareness()
{
    local i;
    local j;
    local val;
    local k;

    i=$1;
    j=$2;
    val=$3;

    for ((k=0; $k < 9; k++)); do
        if [ $k -eq $j ]; then
            continue;
        fi
        not_possible $i $k $val;
    done
}


function spread_block_awareness()
{
    local i;
    local j;
    local val;
    local k;
    local l;
    local myblk;

    i=$1;
    j=$2;

    val=$3;

    myblk=$(( ($i / 3) * 3 + ($j / 3)));

    for ((k=0; $k < 9; k++)); do
        for ((l=0; $l < 9; l++)); do
            blk=$(( ($k / 3) * 3 + ($l / 3)));

            if [ $blk -ne $myblk ]; then
                continue;
            fi

            if [ $k -eq $i -a $l -eq $j ]; then
                continue;
            fi
            not_possible $k $l $val;
        done
    done
}


function spread_awareness()
{
    spread_vertical_awareness "$@";
    spread_horizontal_awareness "$@";
    spread_block_awareness "$@";
}


function fix_position()
{
    local i;
    local j;
    local idx;
    local val;

    i=$1;
    j=$2;
    val=$3;

    idx=$(($i * 9 + j + 1));

#    echo "Fixing $i,$j ($idx) => $val";

    space[$idx]=$val;

    spread_awareness $i $j $val;
}


function start_play()
{
    local i;
    local j;
    local idx;

    set $1;

    for ((i=0; $i < 9; i++)); do
        for ((j=0; $j < 9; j++)); do
            idx=$(($i * 9 + j + 1));
            if [ ${!idx} -gt 0 ]; then
                fix_position $i $j ${!idx};
            fi
        done
    done
}


function main()
{
    initialize;

    start_play "$problem";

    display9;
}

main "$@";

