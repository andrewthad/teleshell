{ package ? "teleshell", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).teleshell
