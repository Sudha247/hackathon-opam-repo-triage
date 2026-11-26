(* Regression tests for [opam-ci-triage suggest], without network.

   Each case runs [suggest --json] on a recorded CI run with
   OPAM_CI_OFFLINE=1: every page, log, opam file, doc lookup and git object
   must come from tests/fixtures/<case>/, and a cache miss fails the test.

     dune test                                     # all the tests (offline)
     dune exec tests/test_cases.exe -- test why3find   # one case
     dune exec tests/test_cases.exe -- --record [case ...]
                                                   # (re)record fixtures online
     dune exec tests/test_cases.exe -- --top-up [case ...]
                                                   # add missing MCP answers

   (--record and --top-up are run from the repository's root.)

   The expectations are fixes checked by hand against the CI logs, upstream
   history, or the change the PR author actually made. *)

open Triage
open Suggestion.T

(* An expectation matches a suggestion whose fields contain the given
   strings ([Has]); [Not]: no suggestion matches. *)
type expectation =
  | Has of (string * string) list
  | Not of (string * string) list

(* name, commit, extra arguments, expectations *)
let cases =
  [
    ( {|ortools-lower-bound|},
      {|55ad97d637d37d80b5d186bd7b3850ad263d05f1|},
      [],
      [
        Has
          [
            ({|failing|}, {|ortools_solvers.9.15.0-3|});
            ({|new|}, {|"ortools" {>= "9.15.0-2"}|});
            ({|alternative|}, {|{= version}|});
          ];
      ] );
    ( {|colibri2-stdlib|},
      {|e162f8cea2ee791d6b3c418bb56611b92a93e286|},
      [ {|--max|}; {|1|} ],
      [
        Has
          [
            ({|failing|}, {|colibri2.0.7|});
            ({|rule|}, {|stdlib|});
            ({|new|}, {|"ocaml" {>= "5.1"}|});
          ];
      ] );
    ( {|why3find-gitlab-fallback|},
      {|00f688e6e6e43a9d4720c3fbf49df8f262e6f232|},
      [ {|--max|}; {|1|} ],
      [
        Has
          [
            ({|failing|}, {|why3find.1.3.1|});
            ({|new|}, {|"why3" {>= "1.8.0" & < "1.9.0"}|});
          ];
      ] );
    ( {|hol_light-lint|},
      {|d9fecfe04f055d0029dcea6042a23c2c6c04925b|},
      [],
      [
        Has
          [
            ({|failing|}, {|hol_light.3.2.0|});
            ({|new|}, {|license: "BSD-2-Clause"|});
          ];
        Has [ ({|failing|}, {|hol_light.3.2.0|}); ({|new|}, {|%{_:VAR}%|}) ];
      ] );
    ( {|binaryen-uninstallable|},
      {|ffd281c8603d21293b920f7cd26b60bbab4dec39|},
      [ {|--max|}; {|1|} ],
      [
        Has
          [
            ({|rule|}, {|uninstallable|});
            ({|note|}, {|js_of_ocaml-compiler < 5.0.0|});
          ];
      ] );
    ( {|oonnxx-platforms|},
      {|f60998e840bc2ad6a3e0f26fe2d465c4ba781551|},
      [ {|--max|}; {|3|} ],
      [
        Has
          [
            ({|failing|}, {|oonnxx.0.2.0|});
            ({|new|}, {|x-ci-accept-failures: ["centos-9" "ubuntu-22.04"]|});
          ];
        Has [ ({|failing|}, {|oonnxx.0.2.0|}); ({|new|}, {|arch != "s390x"|}) ];
      ] );
    ( {|chatoyant-melange-awk|},
      {|8bd7f3b5699246c467c2698927e1a3e8e8f5b43d|},
      [ {|--max|}; {|3|} ],
      [
        Has
          [
            ({|failing|}, {|melange.7.0.1-55|});
            ({|new|}, {|depexts: ["gawk"] {os-distribution = "cygwin"}|});
          ];
      ] );
    ( {|lwt-revdeps|},
      {|1e2d45eb8b297fea701407d41e2d2af656bd5d9a|},
      [
        {|--max|};
        {|6|};
        {|--match|};
        {|^compilers,(4\.14,lwt.5.10.2,revdeps,kinetic-client|5\.5,lwt.5.10.2,revdeps,passage\.0\.1\.7)|};
      ],
      [
        Has
          [
            ({|failing|}, {|kinetic-client.0.0.9|});
            ({|rule|}, {|safe-string|});
            ({|new|}, {|< "4.06"|});
          ];
        Has
          [
            ({|failing|}, {|passage.0.1.7|});
            ({|candidate|}, {|"cmdliner" {with-test & < "2.0.0"}|});
            ({|changelog|}, {|no longer allows command names|});
          ];
      ] );
    ( {|graphql-async-license|},
      {|765646df4b8a6b4fcf3b68949aa3a82236d89e0d|},
      [ {|--max|}; {|6|} ],
      [
        Has
          [
            ({|failing|}, {|graphql-async.0.13.0|});
            ({|new|}, {|license: "MIT"|});
          ];
      ] );
    ( {|ocaml-r-unmaintained|},
      {|ab3adebc3709da97cafdc3546a50c8158b229dcd|},
      [ {|--max|}; {|6|} ],
      [
        Has
          [
            ({|failing|}, {|ocaml-r.0.5.0|});
            ({|alternative|}, {|x-maintained: false|});
          ];
      ] );
    ( {|git-paf-missing-library|},
      {|1b03be6378915db91ffe28eaa264a242da693e1c|},
      [ {|--max|}; {|6|} ],
      [ Has [ ({|failing|}, {|git-paf.3.19.0|}); ({|new|}, {|"bstr"|}) ] ] );
    ( {|websocket-bound|},
      {|f290556e5d03adbda8dc847188358e06abe16436|},
      [ {|--max|}; {|6|} ],
      [
        Has
          [
            ({|failing|}, {|websocket-lwt-unix.2.18|});
            ({|new|}, {|"mirage-crypto-rng" {>= "1.2.0"}|});
          ];
      ] );
    (* The same run with the LLM fallback, its answer in the fixture (an
       --llm call recorded on 2026-11-26; offline, a missing answer is a
       cache miss, never an API call). The answer puts the fix in another
       package: the place names it. Its version bound is text to check, not
       a diff. Re-recording this case calls the API (with
       ANTHROPIC_API_KEY set) and may get another answer. *)
    ( {|websocket-bound-llm|},
      {|f290556e5d03adbda8dc847188358e06abe16436|},
      [ {|--max|}; {|6|}; {|--llm|}; {|--llm-max|}; {|1|} ],
      [
        Has
          [
            ({|rule|}, {|llm|});
            ({|failing|}, {|ojs_filetree.0.8.0|});
            ({|where|}, {|ojs_filetree.0.8.0 (another package)|});
            ({|where_pkg|}, {|ojs_filetree.0.8.0|});
          ];
        Not [ ({|rule|}, {|llm|}); ({|new|}, {|js_of_ocaml|}) ];
      ] );
    ( {|lambdapi-bound|},
      {|5fe799b6cebb0d22cd7b963e1103bd17aa545bae|},
      [ {|--max|}; {|6|} ],
      [
        Has
          [
            ({|failing|}, {|lambdapi.4.0.0|}); ({|new|}, {|"sedlex" {>= "3.3"}|});
          ];
      ] );
    ( {|unikraft-pr-context|},
      {|a2a330c034bbf7e443ef9eadad19fd68ceeb991d|},
      [ {|--max|}; {|8|} ],
      [
        Has
          [
            ({|failing|}, {|unikraft.0.20.0|});
            ({|rule|}, {|pr-relaxed-available|});
            ({|new|}, {|os = "linux"|});
          ];
        Has
          [
            ({|rule|}, {|builder-specific|});
            ({|note|}, {|conf-bison declares a depext for centos|});
          ];
        Has
          [
            ({|rule|}, {|platform-matrix|});
            ({|note|}, {|relaxed `available` of unikraft.0.20.0|});
          ];
        Has
          [
            ({|failing|}, {|ocaml-unikraft.1.2.1|});
            ({|rule|}, {|lint|});
            ({|note|}, {|#30209|});
          ];
      ] );
    (* The error is printed before ~850 lines of warnings (it used to be cut from the excerpt);
       pgn_parser < 1.0.2 does not install its library (no `(package ...)`). *)
    ( {|pgn-parser-sibling-bound|},
      {|d524ea6c8a2ed6050068c8cedb293e595330dc67|},
      [ {|--max|}; {|6|}; {|--match|}; {|lower-bounds|} ],
      [
        Has
          [
            ({|failing|}, {|chess_com_api.1.0.2|});
            ({|rule|}, {|missing-library|});
            ({|new|}, {|"pgn_parser" {>= "1.0.2"}|});
          ];
        Has
          [
            ({|failing|}, {|lichess_api.1.0.2|});
            ({|rule|}, {|missing-library|});
            ({|new|}, {|"pgn_parser" {>= "1.0.2"}|});
          ];
      ] );
    ( {|dns-certify-bound|},
      {|d1951c34e014d331928d961b53113a61c93b17fe|},
      [ {|--max|}; {|6|} ],
      [
        Has
          [
            ({|failing|}, {|dns-certify.10.2.7|});
            ({|new|}, {|"ca-certs-nss" {>= "3.113.1"}|});
          ];
      ] );
    ( {|optint-revdeps|},
      {|83910a8f6a20733de90fcdd822827279971ac3ba|},
      [ {|--max|}; {|12|} ],
      [
        Has
          [
            ({|failing|}, {|topojsone.0.1.0|});
            ({|rule|}, {|test-output-ocaml|});
            ({|new|}, {|< "5.5" | !with-test|});
          ];
        Has
          [
            ({|rule|}, {|rerun|});
            ({|error|}, {|Segmentation fault of the CI tool day10-install|});
          ];
        Has [ ({|rule|}, {|rerun|}); ({|error|}, {|7 cancelled|}) ];
        Has
          [
            ({|failing|}, {|solo5-bindings-hvt.0.6.9|});
            ({|rule|}, {|newer-c-compiler|});
            ({|note|}, {|-Werror|});
          ];
        Has
          [ ({|failing|}, {|tar-eio.3.5.0|}); ({|rule|}, {|builder-specific|}) ];
        Has
          [
            ({|rule|}, {|health-check-preexisting|}); ({|failing|}, {|cca.0.7|});
          ];
      ] );
    ( {|frama-clang-llvm|},
      {|8f970cb1aaa9971525c904c59e168a4ab5169d00|},
      [ {|--max|}; {|8|} ],
      [
        Has
          [
            ({|failing|}, {|conf-libclang.22|});
            ({|rule|}, {|system-library-version|});
            ({|error|}, {|archlinux has LLVM 23|});
          ];
        Has [ ({|rule|}, {|rerun|}); ({|error|}, {|HTTP 503|}) ];
        Has
          [
            ({|failing|}, {|frama-c.34.0~beta|});
            ({|rule|}, {|opam-same-version-reinstall|});
          ];
        Not [ ({|error|}, {|brew|}) ];
      ] );
    ( {|glfw-accepted-failures|},
      {|bac9c3858cff51b3d7c9a6f6990799bbe7582c01|},
      [ {|--max|}; {|6|} ],
      [
        Has
          [
            ({|failing|}, {|conf-glfw3.3|});
            ({|rule|}, {|accept-failures-ignored|});
            ({|note|}, {|cygwin|});
          ];
        Has
          [
            ({|failing|}, {|imguiml.v1.90.6|}); ({|rule|}, {|builder-specific|});
          ];
        Not [ ({|new|}, {|os != "win32"|}) ];
      ] );
    ( {|irmin-watcher-bound-only|},
      {|79e6e2140ee3e82c58d607f1e294e74b3123089a|},
      [ {|--max|}; {|8|} ],
      [
        Has
          [
            ({|failing|}, {|slipshow.0.2.0|});
            ( {|note|},
              {|Pre-existing: this PR only adds irmin-watcher {< "0.6.0"}|} );
          ];
        Has
          [
            ({|failing|}, {|irmin-watcher.0.6.0|});
            ({|rule|}, {|platform-matrix|});
          ];
        Not
          [
            ({|failing|}, {|irmin-watcher.0.6.0|}); ({|note|}, {|Pre-existing|});
          ];
        Has [ ({|rule|}, {|rerun|}); ({|error|}, {|9 cancelled|}) ];
      ] );
    ( {|goblint-cil-baseline|},
      {|eafb2a69786279b98b31f3e5cd8a62ce432cf343|},
      [ {|--max|}; {|6|} ],
      [
        Has
          [
            ({|rule|}, {|platform-matrix|});
            ( {|note|},
              {|goblint-cil.2.2.0 failed the same job in the CI of #31249|} );
          ];
        Has
          [
            ({|rule|}, {|accept-failures-ignored|}); ({|note|}, {|freebsd-15.1|});
          ];
      ] );
    ( {|aslref-sources|},
      {|5a2833e2b7ed465175d6685cd3de0960d38cbabc|},
      [ {|--max|}; {|4|} ],
      [
        Has
          [
            ({|rule|}, {|sources|});
            ({|error|}, {|tar cannot create the symlink|});
            ({|note|}, {|msys2|});
          ];
      ] );
    ( {|melange-npm-lint|},
      {|4615b601a5d1860114fb109cff54ec33bec76e27|},
      [ {|--max|}; {|2|} ],
      [ Has [ ({|rule|}, {|lint|}); ({|note|}, {|npm-version|}) ] ] );
    ( {|binaryen-cstdint|},
      {|6818539645347aa49316df894e8160a9db5a7789|},
      [ {|--max|}; {|2|}; {|--match|}; {|binaryen|} ],
      [
        Has
          [
            ({|failing|}, {|libbinaryen.109.0.1|});
            ({|rule|}, {|newer-c-compiler|});
            ({|note|}, {|<cstdint>|});
          ];
      ] );
    ( {|bun-afl-implicit-int|},
      {|eb1089be5f315d8d5f5d749ad06f42bd92ae5b4b|},
      [ {|--max|}; {|2|}; {|--match|}; {|revdeps,bun|} ],
      [
        Has
          [
            ({|failing|}, {|afl.2.52b|});
            ({|rule|}, {|newer-c-compiler|});
            ({|note|}, {|-Wimplicit-int|});
          ];
        Has
          [
            ({|rule|}, {|health-check-preexisting|});
            ({|failing|}, {|xen-gnt-unix.4.0.2|});
          ];
        Has
          [
            ({|rule|}, {|health-check-new|});
            ({|failing|}, {|jext.0.1.1|});
            ({|new|}, {|conflicts: [ "lwt" {>= "6.1.3"} ]|});
          ];
      ] );
    (* Rules for causes found by grading the LLM fallback's answers. *)
    ( {|ordma-git-describe|},
      {|3670238c506b9ba55e76e0956102b5a5eca57118|},
      [ {|--max|}; {|1|}; {|--match|}; {|^compilers,4.11|} ],
      [
        Has
          [ ({|failing|}, {|ordma.0.0.6|}); ({|rule|}, {|needs-git-checkout|}) ];
      ] );
    ( {|llvm-python3|},
      {|e3b282a9c0eb1ef824245e3ffb935dc8fcc9b1ea|},
      [ {|--max|}; {|2|}; {|--match|}; {|llvm.20-shared|} ],
      [
        Has
          [
            ({|rule|}, {|missing-tool|}); ({|new|}, {|"conf-python-3" {build}|});
          ];
        Not [ ({|error|}, {|llvm-config-|}) ];
      ] );
    ( {|slipshow-cmdliner2|},
      {|5697d67430433b6e7f81fe26a2cc05f4a4f816b6|},
      [ {|--max|}; {|2|}; {|--match|}; {|slipshow.0.2.0|} ],
      [ Has [ ({|failing|}, {|slipshow.0.2.0|}); ({|new|}, {|< "2.0.0"|}) ] ] );
    ( {|mirage-crypto-riscv64|},
      {|79e6e2140ee3e82c58d607f1e294e74b3123089a|},
      [ {|--max|}; {|1|}; {|--match|}; {|riscv64.*current_git.0.6.2|} ],
      [
        Has
          [
            ({|rule|}, {|unsupported-arch|});
            ({|new|}, {|arch != "riscv64"|});
            ({|fix_in|}, {|mirage-crypto.0.8.0|});
          ];
      ] );
    ( {|biocaml-core-kernel-unix|},
      {|eb1089be5f315d8d5f5d749ad06f42bd92ae5b4b|},
      [ {|--max|}; {|1|}; {|--match|}; {|revdeps,biocaml|} ],
      [
        Has
          [
            ({|failing|}, {|biocaml.0.11.0|});
            ({|rule|}, {|library-api|});
            ({|note|}, {|Core_kernel|});
          ];
      ] );
    ( {|mlcuddidl-win32|},
      {|e162f8cea2ee791d6b3c418bb56611b92a93e286|},
      [ {|--max|}; {|1|}; {|--match|}; {|cygwin|} ],
      [
        Has [ ({|rule|}, {|platform-unsupported|}); ({|error|}, {|mlcuddidl|}) ];
      ] );
    ( {|alt-ergo-prefix|},
      {|6818539645347aa49316df894e8160a9db5a7789|},
      [ {|--max|}; {|1|}; {|--match|}; {|frama_c_codex|} ],
      [
        Has
          [
            ({|rule|}, {|install-outside-prefix|});
            ({|error|}, {|/usr/local/bin|});
          ];
      ] );
    ( {|frama-clang-clang19|},
      {|5f64e545c42823195503816088208e2d89a257c4|},
      [ {|--max|}; {|1|}; {|--match|}; {|revdeps,frama-clang.0.0.16|} ],
      [
        Has
          [
            ({|rule|}, {|system-library-version|});
            ({|note|}, {|Clang 19 removed|});
          ];
        Not [ ({|new|}, {|conf-libclang|}) ];
      ] );
    ( {|mlfront-labelled-tuples|},
      {|fa5fb3a6a806e832e4c7791145889a6bbe8b64ef|},
      [ {|--max|}; {|1|}; {|--match|}; {|revdeps,MlFront_Config.2.3.1|} ],
      [
        Has [ ({|failing|}, {|MlFront_Config.2.3.1|}); ({|new|}, {|< "5.4"|}) ];
      ] );
    ( {|pds-sqlite-unixepoch|},
      {|95c70e7638f27c185540b2efc1bb6fe8f64dcdfa|},
      [ {|--max|}; {|1|}; {|--match|}; {|centos-9.*tests|} ],
      [
        Has [ ({|rule|}, {|system-library-version|}); ({|error|}, {|3.34.1|}) ];
      ] );
    ( {|dspml-day10-excerpt|},
      {|d721bc4476e2693a8ae955aa0c8e8647d207ca16|},
      [ {|--max|}; {|1|}; {|--match|}; {|^day10|} ],
      [
        Has
          [
            ({|failing|}, {|dspml.1.0.2|});
            ({|rule|}, {|missing-library|});
            ({|new|}, {|"gnuplot"|});
          ];
      ] );
    (* Lower-bounds jobs: a downgraded dependency that no longer builds, and the package's own failure
       with its downgraded dependencies as candidates. *)
    ( {|colibri2-lower-bound-dep|},
      {|e0a519e15c3951b6ed6a838cda48da8742cdad33|},
      [ {|--max|}; {|1|}; {|--match|}; {|4.14,colibri2.0.6,lower-bounds|} ],
      [
        Has
          [
            ({|failing|}, {|colibri2.0.6|});
            ({|rule|}, {|lower-bound|});
            ({|new|}, {|"mlcuddidl" {>= "3.0.8"}|});
          ];
      ] );
    ( {|orrandomforest-lower-bound-chain|},
      {|41de505ec8ff3637ab3cb06a41ccbc29683f566c|},
      [ {|--max|}; {|1|}; {|--match|}; {|4.14,orrandomForest.3.0.0,lower|} ],
      [
        Has
          [
            ({|rule|}, {|lower-bound|});
            ({|new|}, {|"parany" {>= "14.0.1"}|});
            ({|error|}, {|pulls in ocamlnet.4.1.8|});
          ];
      ] );
    ( {|miaou-lower-bound-candidates|},
      {|a8e39569b4d59572ae01a7800cf55dcd9d854d1e|},
      [ {|--max|}; {|1|}; {|--match|}; {|5.2,miaou-core.0.6.0,lower|} ],
      [
        Has
          [
            ({|failing|}, {|miaou-registry.0.6.0|});
            ({|rule|}, {|lower-bound|});
            ({|candidate|}, {|"dune" {>= "3.24.2"}|});
          ];
      ] );
    (* The previous release bounds dolog, and dolog 6 is installed, but the job fails in an R script
       that downloads keras: no previous-release bound. *)
    ( {|odnnr-previous-release-unrelated|},
      {|6b62b33f94fa0d7f64aa4da5cb0c75e78fa3b3ee|},
      [ {|--max|}; {|1|}; {|--match|}; {|^compilers,4.11,odnnr.2.0.3$|} ],
      [ Not [ ({|rule|}, {|previous-release-bound|}) ] ] );
    (* Platforms other versions of the package already accept failures on. *)
    ( {|ocaml-r-accepted-elsewhere|},
      {|e77d15c9d7a33bd5ce6b889bcaff4fd2ee733e9c|},
      [ {|--max|}; {|6|} ],
      [
        Has
          [
            ({|failing|}, {|ocaml-r.0.7.0|});
            ({|rule|}, {|platform-matrix|});
            ( {|note|},
              {|Other versions already accept failures there: ocaml-r.0.4.0|} );
          ];
      ] );
    (* A new release breaking reverse dependencies through another one: the bound goes on the package
       that actually breaks (git-mirage), as the author did, not on the 14 packages that fail through
       it. *)
    ( {|awa-revdeps-through-dependency|},
      {|b00f7d80717a0cc3f6d2777cb5e55331607d53d3|},
      [ {|--max|}; {|8|} ],
      [
        Has
          [
            ({|rule|}, {|health-check-new|});
            ({|failing|}, {|git-mirage.3.18.0|});
            ({|new|}, {|< "0.7.0"|});
          ];
        Not [ ({|rule|}, {|health-check-new|}); ({|failing|}, {|diffast.0.4|}) ];
      ] );
    (* From a scan of 190 merged PRs whose first version failed. *)
    ( {|diffast-incomplete-install|},
      {|1b03be6378915db91ffe28eaa264a242da693e1c|},
      [
        {|--max|};
        {|1|};
        {|--match|};
        {|^compilers,4.14,.*revdeps,diffast.0.1.1$|};
      ],
      [
        Has [ ({|rule|}, {|broken-install|}); ({|error|}, {|uutf/META|}) ];
        Not [ ({|rule|}, {|missing-library|}); ({|new|}, {|"uutf"|}) ];
      ] );
    ( {|cohttp-curl-x86-flags|},
      {|9a18be928fec732b4b376fff7cfcbbcdfddb9c0e|},
      [ {|--max|}; {|1|}; {|--match|}; {|^macos,.*cohttp-curl-async|} ],
      [
        Has [ ({|rule|}, {|platform-unsupported|}); ({|error|}, {|-mpopcnt|}) ];
      ] );
    ( {|owl-link-error|},
      {|2cadc04b9adb2b758b70dab842ca5094c42c543b|},
      [
        {|--max|}; {|1|}; {|--match|}; {|^compilers,4.14,.*revdeps,owl.1.0.2$|};
      ],
      [ Has [ ({|rule|}, {|link-error|}); ({|error|}, {|LAPACKE_|}) ] ] );
    ( {|ocsipersist-freebsd-script|},
      {|094df7d8541218ef134342c3d82e9e191ffb2c59|},
      [ {|--max|}; {|1|}; {|--match|}; {|^freebsd,.*ocsipersist-pgsql|} ],
      [
        Has
          [
            ({|rule|}, {|platform-unsupported|}); ({|error|}, {|genconfig.sh|});
          ];
      ] );
    ( {|alcobar-flambda-stack|},
      {|952c1bf57515980ee939ade07e8a09b3040e161e|},
      [ {|--max|}; {|1|}; {|--match|}; {|^extras,ocaml-4.14-flambda,alcobar|} ],
      [ Has [ ({|rule|}, {|test-platform|}); ({|error|}, {|flambda|}) ] ] );
    ( {|janestreet-cpuid-old-day10-log|},
      {|e1268d425980a62d80b8507a0148af0f5c54d2c2|},
      [
        {|--max|};
        {|1|};
        {|--match|};
        {|^day10,day10-archlinux-ocaml-5.5,janestreet_cpuid|};
      ],
      [
        Has
          [
            ({|rule|}, {|ocaml-runtime-api|}); ({|error|}, {|caml_alloc_local|});
          ];
      ] );
    (* Every build job skipped: the solver chain is "pkg >= 0.7 -> …", not "pkg = 0.7 -> …". *)
    ( {|opam-dune-lint-uninstallable|},
      {|1c9cedbb0aed129d82fd932138d24126c7e06ebe|},
      [],
      [
        Has
          [
            ({|rule|}, {|uninstallable|});
            ({|note|}, {|`dune-private-libs < 3.0`|});
          ];
      ] );
    ( {|obs-loki-eio-no-matching-version|},
      {|feb8cffb7befd056276f7fe38943de8faef7eac8|},
      [],
      [
        Has
          [
            ({|rule|}, {|uninstallable|});
            ({|note|}, {|No version of `obs-eio`|});
          ];
      ] );
    (* A dependency missing from opam-repository, and a lint job that failed before linting. *)
    ( {|omni-irc-unknown-package-lint-ci-error|},
      {|7386318651c3e60f273db07ad6d31c6c377ca249|},
      [],
      [
        Has
          [
            ({|rule|}, {|uninstallable|});
            ({|note|}, {|`omni-irc-sig` is not in opam-repository|});
          ];
        Has
          [
            ({|rule|}, {|lint-ci-error|});
            ({|class|}, {|elsewhere|});
            ({|note|}, {|git could not check out|});
          ];
      ] );
  ]

let fixtures =
  if Sys.file_exists "tests/fixtures" then "tests/fixtures"
    (* from the repository's root *)
  else "fixtures" (* dune test: _build/default/tests *)

let tool =
  match Sys.getenv_opt "OPAM_CI_TRIAGE_TOOL" with
  | Some t when t <> "" -> t
  | _ ->
      Filename.concat
        (Filename.dirname Sys.executable_name)
        "../bin/opam_repo_ci_triage.exe"

let find_case case = List.find (fun (c, _, _, _) -> c = case) cases

let run ?(offline = true) case =
  let _, sha, extra, _ = find_case case in
  Proc.run ~timeout:900.
    ~env:
      [
        ("OPAM_CI_OFFLINE", if offline then "1" else "0");
        ("OPAM_CI_RECORD", if offline then "0" else "1");
      ]
    ([
       tool;
       "suggest";
       sha;
       "--json";
       "--cache-dir";
       Filename.concat fixtures case;
     ]
    @ extra)

let field_text (v : Json.t) =
  match v with
  | `String s -> s
  | `Null -> ""
  | v -> if Json.truthy v then Json.py_str v else ""

let matches s exp =
  List.for_all
    (fun (k, v) ->
      match k with
      | "candidate" ->
          List.exists
            (fun c -> Util.contains (Json.str "new" c) v)
            (Json.list "candidates" s)
      | "changelog" ->
          List.exists
            (fun c ->
              Util.contains (Json.str "text" (Json.member "changelog" c)) v)
            (Json.list "candidates" s)
      | _ -> Util.contains (field_text (Json.member k s)) v)
    exp

(** Every suggestion says where its fix goes ([where_kind]), and the same place
    is derived from its [fix_in] alone, as for results stored before
    [where_kind] existed. *)
let check_where sugg =
  List.iter
    (fun s ->
      let d = Suggestion.of_json s in
      let kind = Json.str_opt "where_kind" s in
      Alcotest.(check bool)
        (Printf.sprintf "where_kind of %s" (Json.dumps s))
        true
        (match kind with
        | Some k -> List.mem k Where.where_kinds
        | None -> false);
      let old = Suggestion.copy d in
      old.where_kind <- None;
      old.where_pkg <- Absent;
      Alcotest.(check (pair string (option string)))
        (Printf.sprintf "where of %s [%s]" (Json.str "rule" s)
           (Json.str "fix_in" s))
        (Option.get kind, Json.str_opt "where_pkg" s)
        (Where.where_kind old, Where.where_pkg old))
    sugg

let test_case case () =
  let _, _, _, exps = find_case case in
  if not (Util.is_dir (Filename.concat fixtures case)) then
    Alcotest.skip () (* no fixture (run with --record) *);
  let out = run case in
  let tail n s = Util.suffix_chars s n in
  Alcotest.(check int) ("exit status: " ^ tail 1000 out.stderr) 0 out.code;
  Alcotest.(check bool)
    ("offline cache misses: " ^ tail 1500 out.stderr)
    true
    (Util.contains out.stderr "offline cache misses: 0");
  let sugg = Json.list "suggestions" (Json.parse out.stdout) in
  check_where sugg;
  List.iter
    (function
      | Not exp ->
          let bad = List.filter (fun s -> matches s exp) sugg in
          if bad <> [] then
            Alcotest.failf "unexpected suggestion matching %s: %s"
              (Json.dumps
                 (`Assoc (List.map (fun (k, v) -> (k, `String v)) exp)))
              (Json.dumps (List.hd bad))
      | Has exp ->
          if not (List.exists (fun s -> matches s exp) sugg) then
            Alcotest.failf "no suggestion matching %s; got:\n%s"
              (Json.dumps
                 (`Assoc (List.map (fun (k, v) -> (k, `String v)) exp)))
              (String.concat "\n"
                 (List.map
                    (fun s ->
                      Printf.sprintf "  %s [%s] %s"
                        (field_text (Json.member "failing" s))
                        (field_text (Json.member "rule" s))
                        (match Json.str_opt "new" s with
                        | Some n when n <> "" -> n
                        | _ -> field_text (Json.member "note" s)))
                    sugg)))
    exps

(* Compress the pages, logs and JSON answers of a fixture directory. *)
let compress ?(only = fun _ -> true) d =
  List.iter
    (fun name ->
      let p = Filename.concat d name in
      if
        (String.ends_with ~suffix:".html" name
        || String.ends_with ~suffix:".json" name)
        && only name
      then begin
        Util.write_gzip (p ^ ".gz")
          (In_channel.with_open_bin p In_channel.input_all);
        Util.remove p
      end)
    (Util.listdir d)

let rec du p =
  if Util.is_dir p then
    List.fold_left (fun a n -> a + du (Filename.concat p n)) 0 (Util.listdir p)
  else try (Unix.lstat p).Unix.st_size with Unix.Unix_error _ -> 0

let record names =
  List.iter
    (fun case ->
      let d = Filename.concat fixtures case in
      Util.rmtree d;
      let out = run ~offline:false case in
      Printf.printf "%s: rc=%d %s\n%!" case out.code
        (match Util.last_opt (Util.splitlines (String.trim out.stderr)) with
        | Some l -> Util.repr l
        | None -> "");
      compress d;
      Printf.printf "  fixture size: %d KiB\n%!" (du d / 1024))
    names

(** Add to existing fixtures the ocaml.org MCP answers they miss (after a change
    that makes new MCP calls), without fetching anything else again. *)
let top_up ?(rounds = 4) names =
  Net.offline_ref := false;
  List.iter
    (fun case ->
      let d = Filename.concat fixtures case in
      let added = ref 0 in
      (try
         for _ = 1 to rounds do
           let out = run case in
           let misses =
             Rx.findall ~flags:[ Rx.M ] "^  miss: mcp (.*)$" out.stderr
           in
           let others =
             List.filter
               (fun l ->
                 String.starts_with ~prefix:"  miss: " l
                 && not (String.starts_with ~prefix:"  miss: mcp " l))
               (Util.splitlines out.stderr)
           in
           if others <> [] then
             Printf.printf "%s: %d other misses, re-record it: %s\n%!" case
               (List.length others)
               (String.concat ", " (Util.take 2 others));
           if misses = [] then raise Exit;
           let src = Sources.create d in
           List.iter
             (fun key ->
               match Json.parse key with
               | `List [ `String tool; `Assoc args ] ->
                   ignore (Sources.mcp src tool args);
                   incr added
               | _ -> ())
             (Util.dedup misses)
         done
       with Exit -> ());
      compress ~only:(fun name -> String.starts_with ~prefix:"mcp-" name) d;
      Printf.printf "%s: %d MCP answers added\n%!" case !added)
    names

let () =
  match Array.to_list Sys.argv with
  | _ :: "--record" :: names ->
      record
        (if names = [] then List.map (fun (c, _, _, _) -> c) cases else names)
  | _ :: "--top-up" :: names ->
      top_up
        (if names = [] then List.map (fun (c, _, _, _) -> c) cases else names)
  | _ ->
      Alcotest.run "cases"
        (List.map
           (fun (case, _, _, _) ->
             (case, [ Alcotest.test_case "suggest" `Quick (test_case case) ]))
           cases)
