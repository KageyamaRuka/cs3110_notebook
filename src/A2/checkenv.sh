#!/bin/bash
# DO NOT EDIT THIS FILE

environment=good

OPAM_LOCATION="$(command -v opam)"
if [[ "$OPAM_LOCATION" == "" ]]; then
  echo "OPAM is NOT available.  This is bad."
  environment=bad
else
  echo "OPAM is available.  Good."
fi

OCAMLC_VERSION="$(ocamlc --version 2>&1)"
if [[ "$OCAMLC_VERSION" == "4.12.0" ]]; then
  echo "OCaml compiler version 4.12.0 is active.  Good."
else
  echo "OCaml compiler version 4.12.0 is NOT active.  This is bad."
  environment=bad
fi

OUNIT_VERSION="$(opam info ounit -f installed-version 2>&1)"
if [[ "$OUNIT_VERSION" =~ "2.2.4" && "$OCAMLC_VERSION" =~ "4.12.0" ]]; then
  echo "OUnit version 2.2.4 is active.  Good."
else
  echo "OUnit version 2.2.4 is NOT active.  This is bad."
  environment=bad
fi

YOJSON_VERSION="$(opam info yojson -f installed-version 2>&1)"
if [[ "$YOJSON_VERSION" =~ "1.7.0" && "$OCAMLC_VERSION" =~ "4.12.0" ]]; then
  echo "Yojson version 1.7.0 is active.  Good."
else
  echo "Yojson version 1.7.0 is NOT active.  This is bad."
  environment=bad
fi

ANSITERMINAL_VERSION="$(opam info ANSITerminal -f installed-version 2>&1)"
if [[ "$ANSITERMINAL_VERSION" =~ "0.8.2" && "$OCAMLC_VERSION" =~ "4.12.0" ]]; then
  echo "ANSITerminal version 0.8.2 is active.  Good."
else
  echo "ANSITerminal version 0.8.2 is NOT active.  This is bad."
  environment=bad
fi

if [[ "$environment" == good ]]; then
  cat <<EOF
===========================================================
Your OCaml environment looks good to me.  Congratulations!
===========================================================
EOF
else
  cat <<EOF
===========================================================
WARNING

Your OCaml environment looks broken to me.  The code that
you submit might not compile on the grader's machine,
leading to heavy penalties.  Please fix your OCaml
environment. Check the error messages above carefully to
determine what is wrong with your environment.  See a
consultant for help if you cannot determine what is wrong.
===========================================================
EOF
fi
