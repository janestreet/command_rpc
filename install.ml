#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"command_rpc"
  [ oasis_lib "command_rpc"
  ; file "META" ~section:"lib"
  ]
