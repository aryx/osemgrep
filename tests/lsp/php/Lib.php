<?php
// Helper class for --lsp PHP test.
// Lib::add() returns int, Lib::ratio() returns float.
// These types are only known to the LSP server (Intelephense).

class Lib {
    public static function add(int $a, int $b): int {
        return $a + $b;
    }

    public static function ratio(int $a, int $b): float {
        return $a / $b;
    }
}
