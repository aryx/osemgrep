<?php

function foo(int $a, string $s): void {
    //ERROR:
    echo $a;
    echo $s;
}
